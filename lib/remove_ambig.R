#' @export
remove_ambig <- function(df){
  # remove cases that have ambiguous codes within context approach (more than 1 context tagged)
  df$tot <- df %>% 
    dplyr::select(-utt, -orth, -phon) %>% 
    rowSums()
  
  df <- df %>% 
    dplyr::filter(tot < 2) %>% 
    dplyr::select(-tot)
  
  return(df)
}
