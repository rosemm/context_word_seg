# 
# contexts.list <- as.list(WL_contexts)
# for(i in 1:length(names(contexts.list))){
#   # words.in.utts <- sapply(df$orth, strsplit, split=" ")
#   this.context <- contexts.list[[i]][contexts.list[[i]] %in% global.data$streams$orth.stream] # the context words that show up in the corpus
#   this.context <- c(as.character(this.context), rep("", nrow(contexts)-length(this.context))) # add empty values at the end to make all of the columns the same length
#   contexts.list[[i]] <- this.context
# }
# contexts.occuring <- as.data.frame(contexts.list)
# contexts.occuring <- contexts.occuring[-which(apply(contexts.occuring,1,function(x)all(x==""))),] # remove empty rows at the end
# 
# write.table(contexts.occuring, file="context_codes/word_lists/WL_contexts_occuring.csv", sep=",", row.names=FALSE)
