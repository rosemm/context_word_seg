
# set number of topics
nK <- 12

# Need to reindex docs to start counting vocab at 0 instead of 1 for lda package
docs.lda <- TM_doc_prep_out$documents
for(i in 1:length(docs.lda)){
  docs.lda[[i]][1,] <- as.integer(docs[[i]][1,]-1)
  if(dim(docs.lda[[i]])[1] != 2) message(paste("wrong number of rows in document", i))
  if(dim(docs.lda[[i]])[2] < 1) message(paste("wrong number of columns in document", i))
}

lda <- lda.collapsed.gibbs.sampler(docs.lda,
                                   K=nK,
                                   vocab=vocab,
                                   num.iterations = 500, # 25 is the number used in the lda demo
                                   0.1, # from demo
                                   0.1, # from demo
                                   burnin = 5,
                                   compute.log.likelihood=TRUE)
cache('lda')
df_LDA_prop <- doc_loadings(method="lda", 
                       model=lda, 
                       meta=TM_doc_prep_out$meta, 
                       df.wn.count)   

write.table(df_LDA_prop, file="context_codes/topic_models/df_LDA_prop.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
cache('df_LDA_prop')

stm <- stm(TM_doc_prep_out$documents, # the documents
           TM_doc_prep_out$vocab, # the words
                K = nK, # number of topics
                prevalence =~ child, 
                content =~ child,
                max.em.its = 75, 
                data = TM_doc_prep_out$meta, 
                init.type = "Spectral")
cache('stm')

df_STM_prop <- doc_loadings(method="stm", 
                            model=stm, 
                            meta=TM_doc_prep_out$meta, 
                            df.wn.count) 

write.table(df_STM_prop, file="context_codes/topic_models/df_STM_prop.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
cache('df_STM_prop')
