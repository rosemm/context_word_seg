#' @export
threshold_plots <- function(df_prop, thresholds, method, save.to, ...){
  
  stopifnot(require(ggplot2), require(dplyr), require(tidyr))
  
  if(toupper(method) == "HJ" | toupper(method) == "LCA"){
    colnames(df_prop)[4:ncol(df_prop)] <- paste0("topic_", colnames(df_prop)[4:ncol(df_prop)])
  }
  
  df.threshold <- gather(df_prop, key="topic", value="loading", starts_with("topic_")) %>% 
    na.omit() %>% 
    as.tbl() # for speed
  
  for(threshold in thresholds){
    df.threshold[[paste0("thresh_",threshold)]] <- ifelse(df.threshold$loading > threshold, 1, 0)
  }
  
  df.threshold <- df.threshold %>% 
    dplyr::select(-loading) %>% 
    gather(key="threshold", value="include", starts_with("thresh_")) %>% 
    spread(key=topic, value=include) %>% 
    extract(col=threshold, into="threshold", regex="_(0*[.]*[[:digit:]]+)", convert=TRUE) 
  
  df.threshold$num_topics <- rowSums(dplyr::select(df.threshold, starts_with("topic_")), na.rm=TRUE)
  
  plot.data <- df.threshold %>% 
    group_by(threshold) %>% 
    summarize(mean_num_topics=mean(num_topics, na.rm=T), 
              perc_zero_topic=length(num_topics[num_topics==0])/length(num_topics),
              perc_one_topic=length(num_topics[num_topics==1])/length(num_topics),
              perc_one_or_two_topic=length(num_topics[num_topics < 3 & num_topics > 0])/length(num_topics),
              perc_two_topic=length(num_topics[num_topics==2])/length(num_topics),
              perc_more_topic=length(num_topics[num_topics>2])/length(num_topics)) %>% 
    gather(key="measure", value="value", -threshold)
  plot.data$measure <- factor(plot.data$measure, 
                              levels = c("mean_num_topics", "perc_zero_topic", "perc_one_topic", "perc_one_or_two_topic", "perc_two_topic", "perc_more_topic"),
                              labels = c("mean_num_topics", "zero contexts", "one context", "one or two contexts", "two contexts", "more than two contexts"))
  
  method <- toupper(method)
  additional_args <- as.data.frame(list(...), stringsAsFactors=FALSE)
  additional_args <- paste0(colnames(additional_args), additional_args, collapse="_")
  
  ggplot(dplyr::filter(plot.data, measure=="mean_num_topics"), aes(x=threshold, y=value)) + 
    geom_line() + 
    scale_y_continuous(breaks=0:12) +
    scale_x_continuous(breaks=round(seq(min(thresholds), max(thresholds), by=.05), 2)) +
    geom_line(y=1, linetype=2) +
    labs(y=NULL) +
    ggtitle(paste0(method, ": mean number of contexts per utterance")) + 
    theme(text = element_text(size=20)) + 
    theme_classic()
  ggsave(filename=paste0(save.to, "/thresholds_", method, "_means_", additional_args ,".png"), width=12, height=8, units="in")
  
  ggplot(dplyr::filter(plot.data, measure!="mean_num_topics"), aes(x=threshold, y=value, color=measure)) + 
    geom_line() + 
    scale_x_continuous(breaks=round(seq(min(thresholds), max(thresholds), by=.05), 2)) +
    labs(y="Percent of total utterances") +
    ggtitle(paste0(method, ": number of contexts per utterance")) + 
    theme(text = element_text(size=20))
  ggsave(filename=paste0(save.to, "/thresholds_", method, "_perc_", additional_args ,".png"), width=12, height=8, units="in")
  
  # best threshold to maximize one or two topics per utt?
 
  best.threshold <- filter(plot.data, measure == "one or two contexts")$threshold[which.max(filter(plot.data, measure == "one or two contexts")$value)]
  if( length(best.threshold) > 1 ) {
    message(paste("More than one optimal threshold:", best.threshold, "\nUsing highest."))
    best.threshold <- max(best.threshold)
  }  
  return(best.threshold)
}
