cloud_from_df <- function(df, which.context, min.freq=1){
  this.df <- df %>% 
    dplyr::select(orth, context=which(colnames(df)==which.context)) %>% 
    dplyr::filter(context == 1)
  word.stream <- strsplit(paste(this.df$orth, collapse = " "), split = "[[:space:]]+")[[1]]
  freq <- table(word.stream)
  words <- names(freq)
  wordcloud::wordcloud(words, freq, min.freq=min.freq)
}
