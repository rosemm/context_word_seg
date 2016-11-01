####################################################################################
# creates the dataframe df_all
# which has columns utt, orth, phon, and all context columns from all approaches
# and drop the very small contexts
# 
# generates another dataframe df_all_noambig that excludes utterances with more than one
# context within an approach (for use in xtab analysis)
####################################################################################

# combine coding from all methods into one dataframe

# get context columns from each method (also retain utt column, for matching them up later)
# for each, append the method abbreviation to the beginning of each context column name
WL <- df_WL_bin %>% 
  dplyr::select(-orth, -phon)
colnames(WL)[2:ncol(WL)] <- paste0("WL_", colnames(WL)[2:ncol(WL)])
HJ <- df_HJ_bin %>% 
  dplyr::select(-orth, -phon)
colnames(HJ)[2:ncol(HJ)] <- paste0("HJ_", colnames(HJ)[2:ncol(HJ)])
LDA <- df_LDA_bin %>% 
  dplyr::select(-orth, -phon)
colnames(LDA)[2:ncol(LDA)] <- paste0("LDA_", colnames(LDA)[2:ncol(LDA)])
STM <- df_STM_bin %>% 
  dplyr::select(-orth, -phon)
colnames(STM)[2:ncol(STM)] <- paste0("STM_", colnames(STM)[2:ncol(STM)])

df_all <- df %>% 
  full_join(WL, by=c("utt")) %>% 
  full_join(HJ, by=c("utt")) %>% 
  full_join(LDA, by=c("utt")) %>% 
  full_join(STM, by=c("utt")) %>% 
  as.tbl() 
# remove spaces in column names
colnames(df_all) <- gsub(x=colnames(df_all), pattern = " ", replacement = "")

# DROP CONTEXTS WITH FEWER THAN 100 UTTERANCES
all_utts <- df_all %>% 
  dplyr::select(-utt, -orth, -phon) %>% 
  colSums(na.rm=TRUE)
contexts_keep <- names(all_utts[all_utts > 100])
df_all <- df_all %>% 
  # one_of matches column names to a list of names in a character vector (in this case, contexts_keep)
  dplyr::select(utt, orth, phon, one_of(contexts_keep))

cache('df_all')

# REMOVE AMBIGUOUS UTTERANCES
df_all_noambig <- df %>% 
  full_join(remove_ambig(WL), by=c("utt")) %>% 
  full_join(remove_ambig(HJ), by=c("utt")) %>% 
  full_join(remove_ambig(LDA), by=c("utt")) %>% 
  full_join(remove_ambig(STM), by=c("utt")) %>% 
  as.tbl() %>% 
  # drop contexts with fewer than 100 utterances
  dplyr::select(utt, orth, phon, one_of(contexts_keep))

cache('df_all_noambig')
