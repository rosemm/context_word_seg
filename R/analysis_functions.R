
threshold_plots <- function(df.model, thresholds, method, save.to, ...){
  
  stopifnot(require(ggplot2), require(dplyr), require(tidyr))
  
  if(toupper(method) == "HJ"){
    colnames(df.model)[4:ncol(df.model)] <- paste0("topic_", colnames(df.model)[4:ncol(df.model)])
  }
  
  df.threshold <- gather(df.model, key="topic", value="loading", starts_with("topic_")) %>% 
    na.omit() %>% 
    as.tbl() # for speed
  
  for(threshold in thresholds){
    df.threshold[[paste0("thresh_",threshold)]] <- ifelse(df.threshold$loading > threshold, 1, 0)
  }
  
  df.threshold <- df.threshold %>% 
    select(-loading) %>% 
    gather(key="threshold", value="include", starts_with("thresh_")) %>% 
    spread(key=topic, value=include) %>% 
    extract(col=threshold, into="threshold", regex="_(0*[.]*[[:digit:]]+)", convert=TRUE) 
  
  df.threshold$num_topics <- rowSums(select(df.threshold, starts_with("topic_")), na.rm=T)
  
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
  
  ggplot(filter(plot.data, measure=="mean_num_topics"), aes(x=threshold, y=value)) + 
    geom_line() + 
    scale_y_continuous(breaks=0:12) +
    scale_x_continuous(breaks=round(seq(min(thresholds), max(thresholds), by=.05), 2)) +
    geom_line(y=1, linetype=2) +
    labs(y=NULL) +
    ggtitle(paste0(method, ": mean number of contexts per utterance"))
  ggsave(filename=paste0(save.to, "/thresholds_", method, "_means_", additional_args ,".png"), width=8, height=8, units="in")
  
  ggplot(filter(plot.data, measure!="mean_num_topics"), aes(x=threshold, y=value, color=measure)) + 
    geom_line() + 
    scale_x_continuous(breaks=round(seq(min(thresholds), max(thresholds), by=.05), 2)) +
    labs(y="Percent of total utterances") +
    ggtitle(paste0(method, ": number of contexts per utterance"))
  ggsave(filename=paste0(save.to, "/thresholds_", method, "_perc_", additional_args ,".png"), width=8, height=8, units="in")
}

apply_threshold <- function(df.model, threshold){
  
  stopifnot(require(dplyr), require(tidyr))
  
  df.bin <- df.model %>% 
    gather(key="topic", value="loading", -utt, -orth, -phon)
  df.bin$include <- ifelse(df.bin$loading > threshold, 1, 0)
  df.bin <- df.bin %>% 
    select(-loading) 
  
  # any contexts that are all 0's (i.e. any contexts which have no utterances assigned)?
  topics.occurring <- unique(filter(df.bin, include==1)$topic)
  
  df.bin <- df.bin %>%
    filter(topic %in% topics.occurring) %>% 
    spread(key=topic, value=include)
  
  return(df.bin)
}

# agreement analysis functions
cat_agreement <- function(cat.codes){
  
  stopifnot(length(cat.codes) == 2, require(dplyr), require(tidyr), require(vcd))
  
  message("Assessing agreement between ", paste(c(names(cat.codes)[1],names(cat.codes)[2]), collapse=" and "))
  
  cat1 <- cat.codes[[1]]
  cat2 <- cat.codes[[2]]
  
  cat1.long <- gather(cat1, key="cat1", value="include", -utt, -orth, -phon) %>% 
    filter(include==1) %>% 
    select(-include)
  cat2.long <- gather(cat2, key="cat2", value="include", -utt, -orth, -phon) %>% 
    filter(include==1) %>% 
    select(-include)
  
  freqs <- full_join(cat1.long, cat2.long, by="utt")
  
  freqs <- freqs %>% 
    count(cat1, cat2)
  
  xt <- xtabs(n ~ cat1 + cat2, data=freqs) # cross-tabs
  names(attr(xt, "dimnames")) <- c(names(cat.codes)[1],names(cat.codes)[2])
  
  chi.sq <- summary(xt) # chi-squared test of independence
  v <- assocstats(xt) # cramers v
  
  return(list(xt, chi.sq, v))
}

logistic_regressions <- function(all.methods, outcome_method, predictor_method){
  stopifnot(require(dplyr), require(tidyr))
  dvs <- all.methods %>% 
    select(starts_with(outcome_method)) %>% 
    as.list()

  predictors <- all.methods %>% 
    select(starts_with(predictor_method)) %>% 
    as.matrix()
    
  models <- list()
  summaries <- list()
  plot.data <- data.frame(est=NULL, se=NULL, method=NULL, context=NULL, outcome=NULL)
  for(i in 1:length(dvs)){
    dv <- dvs[[i]]
    model <- glm(dv ~ predictors - 1,  
                family=binomial(link = "logit"))
    # save the models
    models[[names(dvs)[i]]] <- model
    # save the model summaries
    summaries[[names(dvs)[i]]] <- summary(model)
    # save the coefficient estimates and se's
    p <- summary(model)$coefficients %>% 
      as.data.frame() %>% 
      select(1,2)
    colnames(p) <- c("est", "se")
    p$context <- row.names(p)
    row.names(p) <- NULL
    if( grepl(x=predictor_method, pattern="STM|LDA") ) { # for topic modeling results
      p <- extract(p, context, into=c("method", "context"), regex="predictors(.*)[_](topic.*)")
    } else {
      p <- extract(p, context, into=c("method", "context"), regex="predictors(.*)[_](.*)")
    }
    p$context <- factor(p$context, levels=unique(p$context))
    p$outcome <- names(dvs)[i]
    plot.data <- rbind(plot.data, p)
  }
  plot <- ggplot(plot.data, aes(y=context, x=est)) +
    geom_point() +
    geom_errorbarh(aes(xmin = est-(2*se), xmax = est+(2*se)), height = .2) +
    facet_wrap(~ outcome, scales = "free_x", nrow=2) +
    labs(y=NULL, x="Logistic regression coefficients (2SE error bars)" )
  output <- list(models=models, summaries=summaries, plot.data=plot.data, plot=plot)
  return(output)
}
