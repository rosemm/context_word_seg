
logistic_regressions <- function(all.methods, outcome_method, predictor_method, min.N.utt=0, save.to, ...){
  stopifnot(require(dplyr), require(tidyr))
  
  vars <- all.methods %>% 
    dplyr::select(starts_with(outcome_method), starts_with(predictor_method)) %>% 
    na.omit() # listwise deletion
  
  dvs <- vars %>% 
    dplyr::select(starts_with(outcome_method))
  
  # only keep contexts for dv that have at least min.N.utt utterances
  keep_dvs <- colnames(dvs)[colSums(dvs, na.rm = TRUE) > min.N.utt]
  
  dvs <- dvs %>% 
    dplyr::select(one_of(keep_dvs)) %>% 
    as.list()
  
  # only keep contexts for predictrs that have at least min.N.utt utterances in bin method
  predictor_bin <- gsub(x=predictor_method, pattern = "[.]con", replacement = ".bin")
  preds_bin <- all.methods %>% 
    dplyr::select(starts_with(predictor_bin))
  keep_predictors <- colnames(preds_bin)[colSums(preds_bin, na.rm = TRUE) > min.N.utt] # extract the column names with greater than min.N.utt 
  keep_predictors <- gsub(x=keep_predictors, pattern = "[.]bin", replacement = ".con") # switch them back to continuous column names
  
  predictors <- vars %>% 
    dplyr::select(one_of(keep_predictors)) %>% 
    as.matrix() %>% 
    scale() # centering and scaling predictors
  
  # for naming the plots  
  additional_args <- as.data.frame(list(...), stringsAsFactors=FALSE)
  additional_args <- paste0(colnames(additional_args), additional_args, collapse="_")
  
  models <- list()
  summaries <- list()
  plot.data <- data.frame(est=NULL, se=NULL, pval=NULL, method=NULL, context=NULL, sig=NULL, outcome=NULL, N.outcome=NULL)
  for(i in 1:length(dvs)){
    dv <- dvs[[i]]
    N.outcome <- sum(dvs[[i]], na.rm = TRUE)
    message(names(dvs)[i])
    # http://stats.stackexchange.com/questions/131456/confused-about-0-intercept-in-logistic-regression-in-r
    # http://stats.stackexchange.com/questions/35071/what-is-rank-deficiency-and-how-to-deal-with-it
    if( grepl(x=predictor_method, pattern = "STM|LDA") ){
      # topic modeling contexts are constrained to be related (sum across all topics = 1 for each utterance), so can't estimate an intercept and all topics
      # design matrix is not full rank; columns are not linearly independent
      # drop first topic
      predictors <- predictors[,-1]
    } 
    model <- glm(dv ~ predictors, 
                 family=binomial(link = "logit"))
    # save the models
    models[[names(dvs)[i]]] <- model
    # save the model summaries
    summaries[[names(dvs)[i]]] <- summary(model)
    # save the coefficient estimates and se's
    p <- summary(model)$coefficients %>% 
      as.data.frame() %>% 
      dplyr::select(1,2,4)
    colnames(p) <- c("est", "se", "pval")
    p$context <- row.names(p)
    row.names(p) <- NULL
    
    if( grepl(x=predictor_method, pattern = "STM|LDA") ){
      p$context[1] <- "predictors_topic_Intercept"
      p <- extract(p, context, into=c("method", "context"), regex="predictors(.*)[_](topic_.*)")
    } else {
      p$context[1] <- "predictors_Intercept"
      p <- extract(p, context, into=c("method", "context"), regex="predictors(.*)[_](.*)")
    }
    
    p$context <- factor(p$context, levels=unique(p$context))
    p$sig <- ifelse(p$pval < .05, "sig", "not sig")
    p$pval <- NULL
    p$outcome <- names(dvs)[i]
    p$N.outcome <- N.outcome
    plot.data <- rbind(plot.data, p)
    
    plot <- ggplot(p, aes(x=est, y=reorder(context, est), color=sig)) +
      geom_point(show.legend = F) +
      scale_color_manual(values=c("sig"="black", "not sig"="grey")) +
      geom_errorbarh(aes(xmin = est-(2*se), xmax = est+(2*se)), height = .2, show.legend = FALSE) +
      labs(y=NULL, x="Logistic regression coefficients (2SE error bars)", title=names(dvs)[i])
    ggsave(plot, filename=paste0(save.to, "/logisticreg_", names(dvs)[i],"_from_", predictor_method, additional_args ,".png"), width=8, height=8, units="in")
  } # end for loop
  
  output <- list(models=models, summaries=summaries, plot.data=plot.data)
  return(output)
}
