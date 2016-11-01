
# Need to open R first and install the following packages:
# install.packages(c("devtools", "dplyr", "tidyr", "doParallel", "poLCA"))

devtools::source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/lib/utils.R")

# download data and dict
load_url("https://github.com/rosemm/context_word_seg/raw/master/cache/df_all.RData")


library(dplyr)
library(tidyr)

# context columns should be coded as 0 or 1 at this point
method_contexts <- df_all %>% 
  tidyr::gather(key="context", value="value", -utt, -orth, -phon) %>% 
  tidyr::extract(context, into=c("method", "context"), regex="([[:upper:]]+)_(.*)") %>% 
  # only keep positive context codes (1 means context is occuring, 0 means not)
  dplyr::filter(value == 1) 
method_counts <- method_contexts %>% 
  dplyr::count(utt, orth, phon, method) # returns number of occurrences per method per utterance as n
df_by_method <- method_counts %>% 
  # only keeping occurrences where there is exactly one context listed per method
  dplyr::filter(n == 1) %>% 
  dplyr::select(-n) %>% 
  # join the contexts themselves back in
  dplyr::left_join(method_contexts, by=c("utt", "orth", "phon", "method")) %>% 
  dplyr::select(-value) %>% 
  # reformat to wide
  tidyr::spread(key=method, value=context) %>% 
  dplyr::select(utt, orth, phon, HJ, STM, WL) %>% # keep context columns for HJ, STM, and WL
  dplyr::mutate_at(vars(HJ, STM, WL), as.factor) # turn all context columns into factors if they're not already (poLCA needs factors)

# f <- cbind(WL_bath, WL_bed, WL_body_touch, WL_diaper_dressing, WL_fussing, WL_meal, WL_play, HJ_bathtime, HJ_diaperchange, HJ_dressing, HJ_fussing, HJ_housework, HJ_interaction, HJ_mealtime, HJ_playtime, HJ_sleep, LDA_topic_1, LDA_topic_10, LDA_topic_11, LDA_topic_12, LDA_topic_2, LDA_topic_3, LDA_topic_4, LDA_topic_5, LDA_topic_6, LDA_topic_7, LDA_topic_8, LDA_topic_9, STM_topic_1, STM_topic_10, STM_topic_11, STM_topic_12, STM_topic_2, STM_topic_3, STM_topic_4, STM_topic_5, STM_topic_6, STM_topic_7, STM_topic_8, STM_topic_9) ~ 1 

library(doParallel)
registerDoParallel() 
lca_models <- foreach(i=3:29, 
                      .inorder=TRUE,
                      .errorhandling="pass",
                      .verbose=TRUE,
                      .packages=c("poLCA", "MASS") ) %dopar% poLCA(cbind(HJ, STM, WL) ~ 1, df_by_method, nclass=i, maxiter = 100000, nrep=i, na.rm=FALSE)

save(lca_models, file="lca_models.RData")

# http://www.cyberciti.biz/tips/nohup-execute-commands-after-you-exit-from-a-shell-prompt.html
# nohup Rscript r_LCA.R &
