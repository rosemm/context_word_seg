
# to speed up processing, only code each unique utterance once (then we'll join it back to the full df)
temp.codes <- data.frame(orth = unique(df$orth), stringsAsFactors = FALSE)
for(k in names(WL_contexts) ){
  
  temp.codes[[k]] <- 0 # make a new column for this context
  words <- WL_contexts[[k]] # this word list
  words <- words[words !=""] # drop any empty character elements in the list
  words <- gsub(x = words, pattern = "_", replacement = " ") # remove underscore from bigram key words, so they can be recognized in orth
  
  for(w in 1:length(words)){
    # for every orth entry that contains this word, put a 1 in this context's column
    temp.codes[[k]][grep(pattern=paste0("\\<", words[w], "\\>"), x=temp.codes$orth )] <- 1 
  }
}
df_WL <- left_join(df, temp.codes, by="orth") # join temp.codes back to full df

df_WL <- expand_windows(df_WL, context.names=names(WL_contexts)) # extend context codes 2 utterances before and after each hit

write.table(df_WL, file="context_codes/word_lists/df_WL.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
cache('df_WL')

df_WL_bin <- df_WL %>% 
  mutate_each(funs(ifelse(. > 0, 1, 0)), -utt, -orth, -phon) # make all contexts either 1 or 0 (smoothing over 1.5's from expand_windows)

cache('df_WL_bin')
