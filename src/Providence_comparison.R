# read in providence (Naima 11-22mos) phon file 
phon <- scan("doc/Naima_11to22m.phon", what = character(), sep = "\n")
orth <- scan("doc/Naima_11to22m.plain", what = character(), sep = "\n")[-23592]
# one exceptionally long utterance is excluded from analyses:
# "hi it's I hope you had good holidays and I was calling to see if we are both still on for a play date for Thursday the ninth this Thursday my phone home phone number is we had said two which is fine our nap time here is somewhat unpredictable but you can tell me what works for you and we are at in which is practically right down the street South Main Street from Four Seas Ice Cream if you know that so I I'll look forward to hearing from you at some point and hope we can still do this thanks bye"

df_prov <- data.frame(utt = paste0("naima_", 1:length(phon)), orth = orth, phon = phon)

# get LDA topics
df_prov$wn.count <- gl(n=ceiling(nrow(df_prov)/30), k=30, nrow(df_prov))


# collapse utterances from within each wn-utterance window
doc.data <- group_by(df_prov, wn.count) %>%
  dplyr::select(orth, wn.count) %>%
  do({
    doc.data <- data.frame(documents=paste(.$orth, collapse=" "),
                           stringsAsFactors=FALSE)
  }) 

# prep
processed <- stm::textProcessor(doc.data$documents, 
                                metadata = doc.data, 
                                removestopwords=TRUE, 
                                wordLengths=c(1, Inf))
out <- stm::prepDocuments(processed$documents, 
                          processed$vocab, 
                          processed$meta,
                          lower.thresh = 2) 
# removes infrequent terms depending on user-set parameter lower.thresh (the minimum number of documents a word needs to appear in order for the word to be kept within the vocabulary)

# Need to reindex docs to start counting vocab at 0 instead of 1 for lda package
docs.lda <- out$documents
for(i in 1:length(docs.lda)){
  docs.lda[[i]][1,] <- as.integer(out$documents[[i]][1,]-1)
  if(dim(docs.lda[[i]])[1] != 2) message(paste("wrong number of rows in document", i))
  if(dim(docs.lda[[i]])[2] < 1) message(paste("wrong number of columns in document", i))
}


lda <- lda::lda.collapsed.gibbs.sampler(docs.lda,
                                        K=7,
                                        vocab=out$vocab,
                                        num.iterations = 500, # 25 is the number used in the lda demo
                                        alpha = 0.1, # from demo
                                        eta = 0.1, # from demo
                                        burnin = 5,
                                        compute.log.likelihood=TRUE)

loadings <- t(lda$document_sums) / colSums(lda$document_sums)
colnames(loadings) <- paste0("LDAtopic_", 1:ncol(loadings))
loadings <- cbind(meta, loadings)
loadings <- dplyr::select(loadings, -documents)
df_prov_prop <- left_join(df_prov, loadings, by=c( "wn.count" )) %>% 
  dplyr::select(utt, orth, phon, starts_with("LDAtopic_"))


# any additional values to be saved in the plot name can be included as extra arguments
LDA.threshold <- threshold_plots(df_prov_prop, 
                                 thresholds=seq(0,.75,.05), # what thresholds to try, 
                                 method="LDA",
                                 save.to=file.path("graphs", "LDA"),
                                 Providence=TRUE) 

# apply threshold
df_prov_bin <- apply_threshold(df_prov_prop, 
                              LDA.threshold, 
                              plot=FALSE, 
                              method="LDA", 
                              save.to=file.path("graphs", "LDA"))

cache('df_prov_bin')
