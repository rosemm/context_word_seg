cloud_from_df <- function(df, which.context, min.freq=0, size.breaks, save.to, note=NULL, standardize.font=TRUE, color=FALSE, title=FALSE){

  word.stream <- strsplit(paste(df$orth, collapse = " "), split = "[[:space:]]+")[[1]]
  freq <- table(word.stream)
  words <- names(freq)

  plot.data <- as.data.frame(freq) %>% 
    dplyr::filter(Freq >= min.freq)
  if(nrow(plot.data) > 0){
    p <- ggplot(plot.data, aes(x=1, y=1, label=word.stream ))  + 
      theme_void() +
      theme(legend.title=element_text()) 
    if(title){
      p <- p + ggtitle(bquote(underline(~.(which.context))))
    }
    if(color){
      p <- p + geom_text_repel(aes(size=Freq, color=Freq), segment.size=0)
    } else {
      p <- p + geom_text_repel(aes(size=Freq), segment.size=0)
    }
    if(standardize.font){
      p <- p + scale_size(name="Frequency", breaks=size.breaks, limits = c(min(size.breaks), max(size.breaks)))
    } else {
      p <- p + scale_size(name="Frequency")
    }
    p  
    
    which.context <- gsub(x=which.context, pattern = " ", replacement = "")
    ggsave(file.path(save.to, paste0("wordcloud_",  note, "_", which.context, ".png")), width = 6, height = 5, units="in" )
  } else warning(paste("No plot for", which.context, "because no words occur at least", min.freq, "times"))
}
