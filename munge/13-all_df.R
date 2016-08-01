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
  left_join(HJ, by=c("utt", "orth", "phon")) %>% 
  left_join(LDA, by=c("utt", "orth", "phon")) %>% 
  left_join(STM, by=c("utt", "orth", "phon")) %>% 
  as.tbl()

cache('df_all')

colnames(df_all)
nrow(df_all)
