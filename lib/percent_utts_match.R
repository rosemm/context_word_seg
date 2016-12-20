percent_utts_match <- function(df, class_contexts, x){
  
  match_classes <- substitute(dplyr::filter(class_contexts, x == 1), list(x = as.name(x))) %>% 
    eval() %>% 
    dplyr::select(-class) %>% 
    colSums(na.rm = TRUE)
  # the contexts that appear in the same class(es) as the target context
  match_classes <- names(match_classes[match_classes > 0])
  match_classes <- match_classes[match_classes != x] # drop the target context itself
  
  matches <- substitute(dplyr::filter(df, x == 1), list(x = as.name(x))) %>% 
    eval() %>%
    dplyr::select(one_of(match_classes))
  message(nrow(matches), " utterances in ", x)
  props <- colMeans(matches, na.rm = TRUE)
  message("Proportion match for the following contexts...\n", paste(paste(names(props), round(props, 3), sep = ": "), collapse="\n"))
  
return(props)  
}