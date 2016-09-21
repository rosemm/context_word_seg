#' @export
aciss_function <- function(date, id, starts, iter, run_analysis_args, walltime=9600){
  
  batch_function <- function(start, run_analysis_args){
    devtools::install_github(repo = "rosemm/context_word_seg", subdir = "pkg")
    iter <- iter # the number of times to generate random samples
    
    library(doParallel)
    registerDoParallel()
    r <- foreach(1:iter, 
                 .inorder=FALSE,
                 .combine = rbind,
                 .verbose=TRUE,
                 .packages=c("dplyr", "tidyr", "devtools", "BBmisc") ) %dopar% run_analysis(run_analysis_args)
  }
  
  # create a registry
  reg <- makeRegistry(id = id)
  
  # map function and data to jobs and submit
  ids  <- batchMap(reg, batch_function, starts, more.args=list(run_analysis_args=run_analysis_args))
  done <- submitJobs(reg, resources = list(nodes = 1, ppn=1)) 
}
