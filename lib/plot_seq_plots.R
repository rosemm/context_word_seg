#' @export
seq_plots <- function(df, method=c("WL", "HJ", "LDA", "STM", "LCA"), min.utts=0){
  
  df_seq <- gather(df, key=context, value=value, -orth, -phon, -utt) %>%
    separate(col=utt, into=c("file", "UttNum"), sep="_", remove=FALSE)
  
  context_counts <- df_seq %>% 
    group_by(context) %>% 
    summarise(N.utts = sum(value, na.rm=TRUE))
  contexts_keep <- dplyr::filter(context_counts, N.utts > min.utts) 
  if( nrow(contexts_keep) < nrow(context_counts) ) message(paste("Removing", nrow(context_counts) - nrow(contexts_keep), "contexts because they have fewer than", min.utts, "total utterances."))
  df_seq <- dplyr::filter(df_seq, context %in% contexts_keep$context)
  
  df_seq$context <- as.factor(df_seq$context)
  df_seq$utt <- as.factor(df_seq$utt)
  df_seq$file <- as.factor(df_seq$file)
  df_seq$UttNum <- as.numeric(df_seq$UttNum)
  
  # sequence plots for each file separately
  files <- unique(df_seq$file)
  for (f in files){
    data <- dplyr::filter(df_seq, file == f & context != "misc" & context !="none") %>% 
      dplyr::filter(!is.na(value))
    
    ggplot(data) +
      geom_point(aes(x=UttNum, y=context, color=context, alpha=value), size=4, shape="|", show.legend=F) + 
      scale_shape_identity() +
      labs(y=NULL, x="Utterance number") + 
      theme_classic()
    ggsave( file.path("graphs", method, paste0("seqplot-", method, f, ".png")), width=9, height=3, units="in" )
  }
}
