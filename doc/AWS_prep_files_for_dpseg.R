
# Need to open R first and install the following packages:
# install.packages(c("devtools", "dplyr", "tidyr", "BBmisc", "doParallel"))

devtools::source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/lib/data_processing_functions.r")
devtools::source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/lib/utils.R")
devtools::source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/lib/print_for_cm.R")

# download data
load_url("https://github.com/rosemm/context_word_seg/raw/master/cache/df_all_cm.RData")

run_dpseg <- function(df, r){
	# setting nontext = TRUE takes a random sample of the same size as each context
	print_for_dpseg(df, nontext = TRUE, dir = getwd(), save.to=file.path("dpseg_input"), r)
	system(paste0('bash ./', r, 'dpseg_contexts.sh'))
}

library(dplyr)
library(tidyr)

df <- df_all_cm %>% 
  dplyr::select(-phon_cm) 

start <- 6000
end <- 6999

library(doParallel)
registerDoParallel() # 40 cores available on node
r_cm <- foreach(i=start:end, 
             .inorder=FALSE,
             .verbose=TRUE,
             .packages=c("dplyr", "tidyr", "devtools", "BBmisc") ) %dopar% run_dpseg(df, r=i)

# http://www.cyberciti.biz/tips/nohup-execute-commands-after-you-exit-from-a-shell-prompt.html
# nohup Rscript r_dpseg.R &
