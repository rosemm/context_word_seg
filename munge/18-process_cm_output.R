
dir <- "computational_models" # the folder where the output folders are 

outputdirs <- file.path(dir, grep(x=list.files(dir), pattern = "coling.*output", value=TRUE))

con_dir <- grep(x=outputdirs, pattern = "context", value=TRUE)
non_dir <- grep(x=outputdirs, pattern = "nontext", value=TRUE)

# read context results
con_files <- file.path(con_dir, list.files(path=con_dir, pattern=".*[.]trscore", recursive = TRUE))

contexts <- gsub(x=con_files, pattern = ".*output/(.*)Eval/.*", replacement = "\\1")

# read all of the results files and compile into a dataframe with context names
for(f in 1:length(con_files)){
  this.result <- read.table(con_files[f], header = TRUE)
  this.result$context <- contexts[f]
  if(f == 1){
    results <- this.result
  } else {
    results <- rbind(results, this.result)      
  }
}
context_est <- results %>% 
  gather(key="measure", value="context_est", token_f.score:boundary_recall) %>% 
  group_by(context, measure) %>% 
  summarize(context_est=mean(context_est)) # if there is more than one estimate for the same context (from running it multiple times), average them

# read nontext results
non_files <- file.path(non_dir, list.files(path=non_dir, pattern=".*[.]trscore", recursive = TRUE))
nontexts <- gsub(x=non_files, pattern = ".*output/(.*)Eval/.*", replacement = "\\1")
# read all of the results files and compile into a dataframe with context names
for(f in 1:length(non_files)){
  this.result <- read.table(non_files[f], header = TRUE)
  this.result$context <- nontexts[f]
  
  if(f == 1){
    results <- this.result
  } else {
    results <- rbind(results, this.result)      
  }     
}
cm_results <- results %>% 
  dplyr::as.tbl() %>% 
  tidyr::extract(col=context, into=c("context", "iter"), regex = "([[:alnum:]]+[[:alpha:]])([[:digit:]]*)") %>% 
  tidyr::gather(key = "measure", value="value", token_f.score:boundary_recall) %>% 
  dplyr::left_join(context_est, by=c("context", "measure")) # add context results to nontext results
cache('cm_results')

# calculate p vlaues for each context and measure
cm_boot_tests <- cm_results %>% 
  dplyr::mutate(above=ifelse(value >= context_est, 1, ifelse(value < context_est, 0, NA))) %>% 
  dplyr::group_by(context, measure, context_est) %>% 
  dplyr::summarize(iters = n(), 
                   p.val = mean(above)) %>% 
  add_stars()
cache('cm_boot_tests')
