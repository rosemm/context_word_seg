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
