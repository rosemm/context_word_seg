# read in providence (Naima 11-22mos) phon file 
phon <- scan("doc/Naima_11to22m.phon", what = character(), sep = "\n")
orth <- scan("doc/Naima_11to22m.plain", what = character(), sep = "\n")[-23592]
# one exceptionally long utterance is excluded from analyses:
# "hi it's I hope you had good holidays and I was calling to see if we are both still on for a play date for Thursday the ninth this Thursday my phone home phone number is we had said two which is fine our nap time here is somewhat unpredictable but you can tell me what works for you and we are at in which is practically right down the street South Main Street from Four Seas Ice Cream if you know that so I I'll look forward to hearing from you at some point and hope we can still do this thanks bye"

sin_topics <- scan("doc/Synnaeve2014/naima_topic_11to22m.sin", what = character(), sep = "\n") 
sin_topics <- data.frame(sin = sin_topics) %>% 
  tidyr::extract(sin, into=c("topic", "sin"), regex ="_(t[[:digit:]]) (.*)")

sin <- scan("doc/Synnaeve2014/Naima_11to22m.sin", what = character(), sep = "\n")

# counter for sin_topics
t <- 1
df_sin <- data.frame(orth = orth, sin=sin, topic=NA)
sin_topics$match <- NA
for(r in 1:nrow(df_sin)){
  # matches in what remains of sin_topics
  remains <- sin_topics[t:nrow(sin_topics), ]
  matches <- grep(pattern = df_sin$sin[r], x = remains$sin, fixed = TRUE)
  if(length(matches) > 0){
    # use first match only
    df_sin$topic[r] <- remains[matches[1], ]$topic
    # record which row matched (doesn't work properly)
    sin_topics$match[t + matches[1] - 1] <- r
    # advance in sin_topics
    t <- t + 1
  } 
}
df_synn <- df_sin %>% 
  dplyr::mutate(hit=1, utt = 1:nrow(.)) %>% 
  tidyr::spread(key=topic, value=hit, fill = 0) %>% 
  dplyr::select(starts_with("t")) 
df_both <- cbind(df_prov_bin, df_synn)  
  df_prov_bin %>% 
  left_join(df_sin, by = "orth") %>% 
  dplyr::select(-sin) %>% 
  

df_synn <- data.frame(utt = paste0("naima_", 1:nrow(sin_topics)), sin = sin_topics$sin, topic = sin_topics$topic) %>% 
  dplyr::mutate(hit=1) %>% 
  tidyr::spread(key=topic, value=hit, fill = 0)
coling_dict <- read.table("doc/coling_phoneSet.txt")
colnames(coling_dict) <- c("output", "input")
# Translate phon from df into cm_phon using coling dict
df_synn$phon <- df_synn$sin
df_synn$phon <- gsub(x=df_synn$phon, pattern = " ", replacement = "\t") # mark word boundaries with \t
for(p in 1:nrow(coling_dict)){
  df_synn$phon <- gsub(x=df_synn$phon, pattern = coling_dict$input[p], replacement = paste0(coling_dict$output[p], " "), fixed = TRUE)
}
df_synn_bin <- df_synn %>% 
  dplyr::select(utt, orth=sin, phon, TOP=starts_with("t"))
cache('df_synn_bin')

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

lda::top.topic.words(lda$topics, by.score=TRUE)

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

N.utts <- df_prov_bin %>% dplyr::select(-utt, -orth, -phon) %>% colSums()

# run on AWS

# --------------------------------------------
# get output
dir <- "computational_models" # the folder where the output folders are 

message("processing Providence (Naima) adaptor grammar output")

outputdirs <- file.path(dir, grep(x=list.files(dir), pattern = "naima.*output", value=TRUE))

con_dir <- grep(x=outputdirs, pattern = "context", value=TRUE)
non_dir <- grep(x=outputdirs, pattern = "nontext", value=TRUE)

# read context results
con_files <- file.path(con_dir, list.files(path=con_dir, pattern=".*[.]trscore", recursive = TRUE))
con_files <- grep(x=con_files, pattern="n500_", value = TRUE) # only keep runs with 500 iterations (as per Synnaeve2014)

contexts <- gsub(x=con_files, pattern = ".*output/(.*)Eval/.*", replacement = "\\1")

# read all of the results files and compile into a dataframe with context names
for(f in 1:length(con_files)){
  this.result <- read.table(con_files[f], header = TRUE)
  this.result$context <- contexts[f]
  if(f == 1){
    results_coling <- this.result
  } else {
    results_coling <- rbind(results_coling, this.result)      
  }
}
context_est_coling <- results_coling %>% 
  gather(key="measure", value="value", token_f.score:boundary_recall) %>% 
  # remove the iter number from the end of the context name
  extract(context, into=c("context", "iter"), regex="(.*_[[:digit:]])([[:digit:]]+)") %>% 
  group_by(context, measure) %>% 
  # context_est is the mean of all iterations
  summarize(context_est=mean(value, na.rm=TRUE),
            context_iters=n(),
            context_SD=sd(value, na.rm=TRUE),
            context_SE=context_SD/sqrt(context_iters))

# read nontext results
non_files <- file.path(non_dir, list.files(path=non_dir, pattern=".*[.]trscore", recursive = TRUE))
non_files <- grep(x=non_files, pattern = "n500_", value=TRUE)  # only keep runs with 500 iterations (as per Synnaeve2014)

nontexts <- gsub(x=non_files, pattern = ".*output/(.*)Eval/.*", replacement = "\\1")
# read all of the results files and compile into a dataframe with context names
for(f in 1:length(non_files)){
  this.result <- read.table(non_files[f], header = TRUE)
  this.result$context <- nontexts[f]
  
  if(f == 1){
    results_coling <- this.result
  } else {
    results_coling <- rbind(results_coling, this.result)      
  }  
}

N.utts <- data.frame(N.utts = N.utts, context = names(N.utts))

cm_results <- results_coling %>% 
  dplyr::as.tbl() %>% 
  tidyr::extract(col=context, into=c("context", "iter"), regex = "N([[:alpha:]]+[[:digit:]])([[:digit:]]+)") %>% 
  tidyr::gather(key = "measure", value="value", token_f.score:boundary_recall) %>% 
  # add "topic" back into context names (accidentally removed during computational modeling run)
  dplyr::mutate(context =  gsub(x=context, pattern="([[:alpha:]]+)([[:digit:]])", replacement = "\\1topic_\\2")) %>% 
  # add context results to nontext results
  dplyr::left_join(context_est_coling, by=c("context", "measure")) %>% 
  # add in N.utts
  left_join(N.utts, by = "context") %>% 
  tidyr::extract(col=context, into=c("method", "context"), regex="([[:upper:]]+)(.*)") %>% 
  mutate(model="coling") 


cm_results$value <- as.numeric(cm_results$value)
cm_results$iter <- as.numeric(cm_results$iter)
cm_results$context_est <- as.numeric(cm_results$context_est)
cm_results$measure <- as.factor(cm_results$measure)
cm_results$context <- as.factor(cm_results$context)
cm_results$method <- as.factor(cm_results$method)
cm_results$model <- as.factor(cm_results$model)

# coling returns model results between 0 and 1 while dpseg converts to percent (0 to 100)
# standardize these for easier comparison
cm_results$value <- ifelse(cm_results$value < 1, 100*cm_results$value, cm_results$value)
cm_results$context_est <- ifelse(cm_results$context_est < 1, 100*cm_results$context_est, cm_results$context_est)

# add Z scored versions of context_est and bootstrap values, for easier comparison across contexts and methods
cm_results <- bootstrap_Z(cm_results, value="value", est="context_est", by=c("model", "method", "context", "measure"))


cm_results_prov <- cm_results %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(model=factor(model, levels=c("coling", "dpseg"), labels=c("Adaptor_Grammar", "HDP")))

cache('cm_results_prov')

# calculate p vlaues for each context and measure
cm_boot_tests_prov <- cm_results_prov %>% 
  dplyr::mutate(above=ifelse(value >= context_est, 1, ifelse(value < context_est, 0, NA))) %>% 
  dplyr::group_by(model, method, context, measure, context_est, Z_est, 
                  N.utts) %>% 
  dplyr::summarize(iters = n(), 
                   n.above = sum(above),
                   n.below = iters - n.above,
                   p.hi = n.above/iters,
                   p.lo = n.below/iters,
                   p.val = min(p.hi, p.lo),
                   # is p.val significantly different from alpha? Davidson, R., & MacKinnon, J. G. (2000). Bootstrap tests: How many bootstraps?. Econometric Reviews, 19(1), 55-68.
                   p.boot.unsure = ifelse(p.hi < p.lo & p.hi < .025, pbinom(n.above, iters, .025),
                                          ifelse(p.lo < p.hi & p.lo < .025, pbinom(n.below, iters, .025),
                                                 ifelse(p.hi < p.lo & p.hi >= .025, pbinom(n.above, iters, .025, lower.tail = FALSE),
                                                        ifelse(p.lo < p.hi & p.lo >= .025, pbinom(n.below, iters, .025, lower.tail = FALSE), NA))))) %>% 
  add_stars()

cache('cm_boot_tests_prov')
