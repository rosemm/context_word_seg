cdi_words <- read.csv("/Users/TARDIS/Documents/STUDIES/context_word_seg/doc/oxford_CDI_words.csv", header=1, stringsAsFactors = FALSE)

cdi_words <- cdi_words %>% 
  dplyr::filter(context != "") %>% 
  dplyr::select(context, word) 

WL_contexts <- vector("list", length(unique(cdi_words$context)))
names(WL_contexts) <- unique(cdi_words$context)
for(k in unique(cdi_words$context) ){
  key.words <- filter(cdi_words, context == k)$word
  WL_contexts[[k]] <- key.words
}

cache('WL_contexts')
