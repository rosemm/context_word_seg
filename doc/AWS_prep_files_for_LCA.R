
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
  # if an approach has more than one context for a given utterance, this drops that approach (but not the others) for that utterance
  dplyr::filter(n == 1) %>% 
  dplyr::select(-n) %>% 
  # join the contexts themselves back in
  dplyr::left_join(method_contexts, by=c("utt", "orth", "phon", "method")) %>% 
  dplyr::select(-value) %>% 
  # reformat to wide with approach as columns (HJ, STM, WL) and context as content in columns
  tidyr::spread(key=method, value=context) %>% 
  dplyr::select(utt, orth, phon, HJ, STM, WL) %>% # keep context columns for HJ, STM, and WL
  dplyr::mutate_at(vars(HJ, STM, WL), as.factor) # turn all context columns into factors if they're not already (poLCA needs factors)

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
