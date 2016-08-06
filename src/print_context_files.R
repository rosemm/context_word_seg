# Printing context files for sharing with CF:

library(ProjectTemplate)
load.project()

WL <- df_WL_bin
WL$N.contexts <- rowSums(dplyr::select(WL, -utt, -orth, -phon), na.rm = TRUE)
WL <- extract_contexts(WL)

HJ <- df_HJ_bin
HJ$N.contexts <- rowSums(dplyr::select(HJ, -utt, -orth, -phon), na.rm = TRUE)
HJ <- extract_contexts(HJ)

LDA <- df_LDA_bin
LDA$N.contexts <- rowSums(dplyr::select(LDA, -utt, -orth, -phon), na.rm = TRUE)
LDA <- extract_contexts(LDA)

STM <- df_STM_bin
STM$N.contexts <- rowSums(dplyr::select(STM, -utt, -orth, -phon), na.rm = TRUE)
STM <- extract_contexts(STM)

WL <- dplyr::select(WL, utt, N.contexts, context) %>% 
  mutate(method = "word list")
HJ <- dplyr::select(HJ, utt, N.contexts, context) %>% 
  mutate(method = "coder judgments")
LDA <- dplyr::select(LDA, utt, N.contexts, context) %>% 
  mutate(method = "LDA")
STM <- dplyr::select(STM, utt, N.contexts, context) %>% 
  mutate(method = "STM")
  
all <- rbind(WL, HJ, LDA, STM) %>% 
  dplyr::select(utt, context, method) %>% 
  tidyr::extract(utt, into = c("child", "age", "utt.num"), regex = "^([[:alpha:]]{2})([[:digit:]]{2})[.]cha_([[:digit:]]+)$")

all$utt.num <- as.numeric(all$utt.num)

all %>% 
  dplyr::filter(method=="word list") %>% 
  tidyr::spread(key=utt.num, value=context) %>% 
  dplyr::select(-method) %>% 
  write.csv("/Users/TARDIS/Dropbox/2_RoseM_TP/context_files/contexts_file_WL.csv", row.names=FALSE)
all %>% 
  dplyr::filter(method=="coder judgments") %>% 
  tidyr::spread(key=utt.num, value=context) %>% 
  dplyr::select(-method) %>% 
  write.csv("/Users/TARDIS/Dropbox/2_RoseM_TP/context_files/contexts_file_HJ.csv", row.names=FALSE)  
all %>% 
  dplyr::filter(method=="LDA") %>% 
  tidyr::spread(key=utt.num, value=context) %>% 
  dplyr::select(-method) %>% 
  write.csv("/Users/TARDIS/Dropbox/2_RoseM_TP/context_files/contexts_file_LDA.csv", row.names=FALSE)
all %>% 
  dplyr::filter(method=="STM") %>% 
  tidyr::spread(key=utt.num, value=context) %>% 
  dplyr::select(-method) %>% 
  write.csv("/Users/TARDIS/Dropbox/2_RoseM_TP/context_files/contexts_file_STM.csv", row.names=FALSE)  
