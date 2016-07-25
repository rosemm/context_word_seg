extract_contexts <- function(df){
  df$context <- ifelse(df$N.contexts > 1, "ambiguous", 
                       ifelse(df$N.contexts == 0, "no context tag", 
                              ifelse(df$N.contexts == 1, "xxx", NA )))
  df_clear_contexts <- df %>% 
    gather(key="key", value="value",-utt, -orth, -phon, -N.contexts, -context) %>% 
    filter(value==1 & context == "xxx") %>% 
    mutate(context=key) %>% 
    select(-key, -value, -orth, -phon, -N.contexts) 
  
  df_contexts <- df %>% 
    dplyr::select(utt, context) %>% 
    filter(context != "xxx") %>% 
    full_join(df_clear_contexts, by=c("utt", "context")) 
  
  df <- df %>% 
    dplyr::select(-context) %>% 
    left_join(df_contexts, by="utt")
  
  df$context <- as.factor(df$context)
  
  return(df)
}

