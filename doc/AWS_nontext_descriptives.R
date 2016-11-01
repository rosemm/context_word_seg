# Rscript for running nontext descriptives on AWS

install.packages(c("devtools", "dplyr", "tidyr", "BBmisc", "doParallel"))

devtools::source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/lib/nontexts_descriptives.R")
devtools::source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/lib/data_processing_functions.r")
devtools::source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/lib/utils.R")

# download data and dict
load_url("https://github.com/rosemm/context_word_seg/raw/master/cache/df_all_cm.RData")
load_url("https://github.com/rosemm/context_word_seg/raw/master/cache/dict.RData")


iter <- 5000 # the number of times to generate random samples
date <- Sys.time()

run_analysis_args_WL <- list(dataframe=dplyr::select(df_all_cm, utt, orth, phon, dplyr::starts_with("WL")), 
                          dict=dict, 
                          seg.utts=TRUE, 
                          verbose=FALSE, 
                          nontext=TRUE, 
                          quiet=TRUE,
                          date=date)
run_analysis_args_STM <- list(dataframe=dplyr::select(df_all_cm, utt, orth, phon, dplyr::starts_with("STM")), 
                          dict=dict, 
                          seg.utts=TRUE, 
                          verbose=FALSE, 
                          nontext=TRUE, 
                          quiet=TRUE,
                          date=date)                          
run_analysis_args_HJ <- list(dataframe=dplyr::select(df_all_cm, utt, orth, phon, dplyr::starts_with("HJ")), 
                          dict=dict, 
                          seg.utts=TRUE, 
                          verbose=FALSE, 
                          nontext=TRUE, 
                          quiet=TRUE,
                          date=date)
run_analysis_args_context <- list(dataframe=dplyr::select(df_all_cm, -phon_cm), 
                             dict=dict, 
                             seg.utts=TRUE, 
                             verbose=FALSE, 
                             nontext=FALSE, 
                             quiet=TRUE,
                             date=date)
r_context <- nontexts_descriptives(run_analysis_args_context) 
write.csv(r_context, "contexts_descriptives.csv", row.names = FALSE)

library(doParallel)
registerDoParallel()
r_WL <- foreach(1:iter, 
             .inorder=FALSE,
             .combine = rbind,
             .verbose=TRUE,
             .packages=c("dplyr", "tidyr", "devtools", "BBmisc") ) %dopar% nontexts_descriptives(run_analysis_args_WL)
r_STM <- foreach(1:iter, 
             .inorder=FALSE,
             .combine = rbind,
             .verbose=TRUE,
             .packages=c("dplyr", "tidyr", "devtools", "BBmisc") ) %dopar% nontexts_descriptives(run_analysis_args_STM)
r_HJ <- foreach(1:iter, 
             .inorder=FALSE,
             .combine = rbind,
             .verbose=TRUE,
             .packages=c("dplyr", "tidyr", "devtools", "BBmisc") ) %dopar% nontexts_descriptives(run_analysis_args_HJ)

r <- rbind(r_WL, r_STM, r_HJ)

write.csv(r, "nontexts_descriptives.csv", row.names = FALSE)

