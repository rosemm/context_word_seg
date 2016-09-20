fuzzy_code <- function(df_bin){
  # fuzzy coding a la Greenacre, 1984
  
  context_cols <- dplyr::select(df_bin, -utt, -orth, -phon)
  df_bin$tot <- rowSums(context_cols)
  df_bin <- as.tbl(df_bin) %>% 
    mutate(tot=ifelse(tot==0, 1, tot)) %>% # don't want to divide by 0
    mutate_if(is.numeric, funs(./tot)) %>% 
    dplyr::select(-tot)
  return(df_bin)
}