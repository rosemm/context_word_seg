doc_loadings <- function(method=c("lda", "stm"), model, meta, df.wn.count){
  
  stopifnot(require(dplyr))
  
  if(method=="lda"){
    loadings <- t(model$document_sums) / colSums(model$document_sums)
  } else if(method=="stm"){
    loadings <- as.data.frame(model$theta)
  } else stop("method must be lda or stm.")
  
  colnames(loadings) <- paste0("topic_", 1:ncol(loadings))
  loadings <- cbind(meta, loadings)
  loadings <- select(loadings, -documents)
  
  df.loadings <- left_join(df.wn.count, loadings, by=c( "child", "age_weeks", "wn.count" )) %>% 
    select(utt, orth, phon, starts_with("topic_"))
  
  return(df.loadings)
}

threshold_plots <- function(df.model, thresholds, method, save.to, ...){
  
  stopifnot(require(ggplot2), require(dplyr), require(tidyr))
  
  df.threshold <- gather(df.model, key="topic", value="loading", starts_with("topic_"))
  
  for(threshold in thresholds){
    df.threshold[[paste0("thresh_",threshold)]] <- ifelse(df.threshold$loading > threshold, 1, 0)
  }
  
  df.threshold <- df.threshold %>% 
    select(-loading) %>% 
    gather(key="threshold", value="include", starts_with("thresh_")) %>% 
    spread(key=topic, value=include) %>% 
    extract(col=threshold, into="threshold", regex="_(0*[.]*[[:digit:]]+)", convert=TRUE) 
  
  df.threshold$num_topics <- rowSums(select(df.threshold, starts_with("topic_")))
  
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
  additional_args <- as.data.frame(list(...))
  additional_args <- paste0(colnames(additional_args), additional_args, collapse="_")
  
  ggplot(filter(plot.data, measure=="mean_num_topics"), aes(x=threshold, y=value)) + 
    geom_line() + 
    scale_y_continuous(breaks=0:12) +
    scale_x_continuous(breaks=round(seq(min(thresholds), max(thresholds), by=.05), 2)) +
    geom_line(y=1, linetype=2) +
    labs(y=NULL) +
    ggtitle(paste0(method, ": mean number of contexts per utterance"))
  ggsave(filename=paste0(save.to, "/topic_thresholds_", method, "_means_", additional_args ,".png"), width=8, height=8, units="in")
  
  ggplot(filter(plot.data, measure!="mean_num_topics"), aes(x=threshold, y=value, color=measure)) + 
    geom_line() + 
    scale_x_continuous(breaks=round(seq(min(thresholds), max(thresholds), by=.05), 2)) +
    labs(y="Percent of total utterances") +
    ggtitle(paste0(method, ": number of contexts per utterance"))
 ggsave(filename=paste0(save.to, "/topic_thresholds_", method, "_perc_", additional_args ,".png"), width=8, height=8, units="in")
}