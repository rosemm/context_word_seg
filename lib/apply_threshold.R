apply_threshold <- function(df.model, threshold, plots=FALSE, method=NULL, save.to=NULL){
  
  stopifnot(require(dplyr), require(tidyr))
  if(plots) stopifnot(require(ggplot2), !is.null(method), !is.null(save.to))
  
  df.long <- df.model %>% 
    gather(key="topic", value="loading", -utt, -orth, -phon)
  df.long$include <- ifelse(df.long$loading > threshold, 1, 0)
  df_bin <- df.long %>% 
    dplyr::select(-loading) 
  if(plots){
    ggplot(df.long, aes(x=loading)) + 
      geom_histogram() + 
      facet_wrap(~topic) + 
      geom_vline(xintercept=threshold, color="red", lty=2) + 
      theme(text = element_text(size=20)) + 
      ggtitle("Topic loadings on utterances\nthreshold shown in red")
    ggsave(filename=paste0(save.to, "/threshold_", method, "_hists_by_context.png"), width=16, height=12, units="in")
    ggplot(dplyr::filter(df.long, loading > 0), aes(x=loading)) + 
      geom_histogram() + 
      facet_wrap(~topic, scales="free_y") + 
      geom_vline(xintercept=threshold, color="red", lty=2) + 
      theme(text = element_text(size=20)) + 
      ggtitle("Topic loadings (excluding 0) on utterances\nthreshold shown in red")
    ggsave(filename=paste0(save.to, "/threshold_", method, "_hists_by_context_drop0s.png"), width=16, height=12, units="in")
  }
  
  # any contexts that are all 0's (i.e. any contexts which have no utterances assigned)?
  topics.occurring <- unique(dplyr::filter(df_bin, include==1)$topic)
  message("\nDropping ", length(unique(df_bin$topic)) - length(topics.occurring), " contexts because they are not above threshold on any utterance.\n")
  
  df_bin <- df_bin %>%
    dplyr::filter(topic %in% topics.occurring) %>% 
    spread(key=topic, value=include)
  
  return(df_bin)
}
