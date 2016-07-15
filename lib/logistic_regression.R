
logistic_regressions <- function(all.methods, outcome_method, predictor_method, save.to, ...){
  stopifnot(require(dplyr), require(tidyr))
  
  vars <- all.methods %>% 
    dplyr::select(starts_with(outcome_method), starts_with(predictor_method)) %>% 
    na.omit() # listwise deletion
  
  dvs <- vars %>% 
    dplyr::select(starts_with(outcome_method)) %>% 
    as.list()
  
  predictors <- vars %>% 
    dplyr::select(starts_with(predictor_method)) %>% 
    as.matrix()
  
  # for naming the plots  
  additional_args <- as.data.frame(list(...), stringsAsFactors=FALSE)
  additional_args <- paste0(colnames(additional_args), additional_args, collapse="_")
  
  models <- list()
  summaries <- list()
  plot.data <- data.frame(est=NULL, se=NULL, method=NULL, context=NULL, sig=NULL, outcome=NULL)
  for(i in 1:length(dvs)){
    dv <- dvs[[i]]
    message(names(dvs)[i])
    model <- glm(dv ~ 0 + predictors,  # not estimating an intercept, so we get the expected value for each predictor
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
    if( grepl(x=predictor_method, pattern="STM|LDA") ) { # for topic modeling results
      p <- extract(p, context, into=c("method", "context"), regex="predictors(.*)[_](topic.*)")
    } else {
      p <- extract(p, context, into=c("method", "context"), regex="predictors(.*)[_](.*)")
    }
    p$context <- factor(p$context, levels=unique(p$context))
    p$sig <- ifelse(p$pval < .05, "sig", "not sig")
    p$pval <- NULL
    p$outcome <- names(dvs)[i]
    p$outcome.N <- sum(dvs[[i]], na.rm = TRUE)
    plot.data <- rbind(plot.data, p)
    
    plot <- ggplot(p, aes(x=est, y=reorder(context, est), color=sig)) +
      geom_point(show.legend = F) +
      scale_color_manual(values=c("sig"="black", "not sig"="grey")) +
      geom_errorbarh(data=dplyr::filter(p, sig=="sig"), aes(xmin = est-(2*se), xmax = est+(2*se)), height = .2, show.legend = F) +
      labs(y=NULL, x="Logistic regression coefficients (2SE error bars)", title=names(dvs)[i])
    ggsave(plot, filename=paste0(save.to, "/logisticreg_", names(dvs)[i],"_from_", predictor_method, additional_args ,".png"), width=8, height=8, units="in")
    
  } # end for loop
  
  output <- list(models=models, summaries=summaries, plot.data=plot.data)
  return(output)
}
