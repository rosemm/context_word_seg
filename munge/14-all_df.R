# combine coding from all methods into one dataframe

WL <- df_WL_bin 
colnames(WL)[4:ncol(WL)] <- paste0("WL_", colnames(WL)[4:ncol(WL)])
HJ <- df_HJ_bin
colnames(HJ)[4:ncol(HJ)] <- paste0("HJ_", colnames(HJ)[4:ncol(HJ)])
LDA <- df_LDA_bin
colnames(LDA)[4:ncol(LDA)] <- paste0("LDA_", colnames(LDA)[4:ncol(LDA)])
STM <- df_STM_bin
colnames(STM)[4:ncol(STM)] <- paste0("STM_", colnames(STM)[4:ncol(STM)])

df_all <- WL %>% 
  full_join(HJ, by=c("utt")) %>% 
  full_join(LDA, by=c("utt")) %>% 
  full_join(STM, by=c("utt")) %>% 
  as.tbl() %>% 
  dplyr::select( -starts_with("orth"), -starts_with("phon")) %>% 
  left_join(df, by="utt")

cache('df_all')

# REMOVE AMBIGUOUS UTTERANCES
df_all_noambig <- remove_ambig(WL) %>% 
  full_join(remove_ambig(HJ), by=c("utt")) %>% 
  full_join(remove_ambig(LDA), by=c("utt")) %>% 
  full_join(remove_ambig(STM), by=c("utt")) %>% 
  as.tbl() %>% 
  dplyr::select( -starts_with("orth"), -starts_with("phon")) %>% 
  left_join(df, by="utt")

cache('df_all_noambig')