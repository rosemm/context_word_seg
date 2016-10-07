
dir <- "computational_models" # the folder where the output folders are 

#----------------------------
## get colingFinal output

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
    results_coling <- this.result
  } else {
    results_coling <- rbind(results_coling, this.result)      
  }
}
context_est_coling <- results_coling %>% 
  gather(key="measure", value="context_est", token_f.score:boundary_recall) %>% 
  group_by(context, measure) %>% 
  summarize(context_est=mean(context_est, na.rm=TRUE)) # if there is more than one estimate for the same context (from running it multiple times), average them

# read nontext results
non_files <- file.path(non_dir, list.files(path=non_dir, pattern=".*[.]trscore", recursive = TRUE))
nontexts <- gsub(x=non_files, pattern = ".*output/(.*)Eval/.*", replacement = "\\1")
# read all of the results files and compile into a dataframe with context names
for(f in 1:length(non_files)){
  this.result <- read.table(non_files[f], header = TRUE)
  this.result$context <- nontexts[f]
  
  if(f == 1){
    results_coling <- this.result
  } else {
    results_coling <- rbind(results_coling, this.result)      
  }  
}

cm_results_coling <- results_coling %>% 
  dplyr::as.tbl() %>% 
  tidyr::extract(col=context, into=c("context", "iter"), regex = "([[:alnum:]]+[[:alpha:]])([[:digit:]]*)") %>% 
  tidyr::gather(key = "measure", value="value", token_f.score:boundary_recall) %>% 
  dplyr::left_join(context_est_coling, by=c("context", "measure")) %>% # add context results to nontext results
  tidyr::extract(col=context, into=c("method", "context"), regex="([[:upper:]]{2,3})([[:alnum:]]+)") %>% 
  mutate(model="coling")

#----------------------------
## get dpseg-1.2.1 output

outputdirs <- file.path(dir, grep(x=list.files(dir), pattern = "dpseg.*output", value=TRUE))

con_dir <- grep(x=outputdirs, pattern = "context", value=TRUE)
non_dir <- grep(x=outputdirs, pattern = "nontext", value=TRUE)

# read context results
con_files <- file.path(con_dir, list.files(path=con_dir, pattern=".*[.]out", recursive = TRUE))

contexts <- gsub(x=con_files, pattern = ".*/([[:alnum:]]+)[.]out", replacement = "\\1")

# read all of the results files and compile into a dataframe with context names
failed <- NULL
for(f in 1:length(con_files)){
  this.result <- readLines(con_files[f])
  this.result <- this.result[grepl(x=this.result, pattern = "^P ")]
  if(length(this.result) > 0){
    # insert _ between statistics
    this.result <- gsub(x=this.result, pattern=" ([[:upper:]]{1,2})", replacement="_\\1")
    # use the _'s to split the result into each of its statistics
    this.result <- stringr::str_split(this.result, pattern="_", simplify=TRUE)
    measures <- as.character(gsub(x=this.result, pattern="([[:upper:]]{1,2}) .*", replacement = "\\1"))
    values <- as.numeric(gsub(x=this.result, pattern="[[:upper:]]{1,2} (.*)", replacement = "\\1"))
    names(values) <- measures
    this.result <- t(as.data.frame(values))
    this.result <- cbind(this.result, context=contexts[f])
    row.names(this.result) <- NULL
    if(f == 1){
      results_dpseg <- this.result
    } else {
      results_dpseg <- rbind(results_dpseg, this.result)
    }
  } else {
    failed <- c(failed, f)
    message("No results for ", contexts[f])
  }
} # end of for loop
message(length(failed), " failed files. ", 100*round(length(failed)/length(contexts), 3), "% of total.")

context_est_dpseg <- results_dpseg %>% 
  data.frame(stringsAsFactors=FALSE) %>% 
  dplyr::mutate_at(vars(-context), as.numeric) %>% 
  dplyr::select(token_f.score=F, token_precision=P, token_recall=R, 
                boundary_f.score=BF, boundary_precision=BP, boundary_recall=BR,
                LP, LR, LF, context) %>% 
  # if there are any trailing numbers, drop them
  tidyr::extract(col=context, into=c("context"), regex = "([[:alnum:]]+[[:alpha:]])[[:digit:]]*") %>% 
  tidyr::gather(key = "measure", value="context_est", token_f.score:LF) %>% 
  group_by(context, measure) %>% 
  # if there is more than one estimate for the same context (from running it multiple times), average them
  summarize(context_est=mean(context_est)) 

# read nontext results
non_files <- file.path(non_dir, list.files(path=non_dir, pattern=".*[.]out", recursive = TRUE))
nontexts <- gsub(x=non_files, pattern = ".*/([[:alnum:]]+)[.]out", replacement = "\\1")
# read all of the results files and compile into a dataframe with context names
failed <- NULL
for(f in 1:length(non_files)){
  this.result <- readLines(non_files[f])
  this.result <- this.result[grepl(x=this.result, pattern = "^P ")]
  
  if(length(this.result) > 0){
    # insert _ between statistics
    this.result <- gsub(x=this.result, pattern=" ([[:upper:]]{1,2})", replacement="_\\1")
    # use the _'s to split the result into each of its statistics
    this.result <- stringr::str_split(this.result, pattern="_", simplify=TRUE)
    
    # every once in a while, the program records two sets of scores
    # if so, check that the measures are the same, and average
    for(r in 1:nrow(this.result)){
      this.measures <- as.character(gsub(x=this.result[r, ], pattern="([[:upper:]]{1,2}) .*", replacement = "\\1"))
      this.values <- as.numeric(gsub(x=this.result[r, ], pattern="[[:upper:]]{1,2} (.*)", replacement = "\\1"))
      if(r==1){
        measures <- this.measures
        values <- matrix(this.values, nrow=1)
      } else {
        measures <- measures[measures %in% this.measures]
        values <- rbind(values, this.values)
      }
    }
    if(length(measures) == ncol(values)){
      values <- colMeans(values)
      names(values) <- measures
      this.result <- t(as.data.frame(values))
      this.result <- cbind(this.result, context=nontexts[f])
      row.names(this.result) <- NULL
      if(f == 1){
        results_dpseg <- this.result
      } else {
        if(colnames(this.result) == colnames(results_dpseg)){
          results_dpseg <- rbind(results_dpseg, this.result)
        } else {
          message("Error: ", nontexts[f], "\n", colnames(this.result), "\n", this.result, "\n")
          failed <- c(failed, f)
        }
      }
    } else {
      failed <- c(failed, f)
      message("Unclear results for ", nontexts[f])
    }
  } else {
    failed <- c(failed, f)
    # message("No results for ", nontexts[f])
  }
}
message(length(failed), " failed files. ", 100*round(length(failed)/length(nontexts), 3), "% of total.")

cm_results_dpseg <- results_dpseg %>% 
  data.frame(stringsAsFactors=FALSE) %>% 
  dplyr::as.tbl() %>% 
  dplyr::select(token_f.score=F, token_precision=P, token_recall=R, 
                boundary_f.score=BF, boundary_precision=BP, boundary_recall=BR,
                LP, LR, LF, context) %>% 
  tidyr::extract(col=context, into=c("context", "iter"), regex = "([[:alnum:]]+[[:alpha:]])([[:digit:]]*)") %>% 
  tidyr::gather(key = "measure", value="value", token_f.score:LF) %>% 
  dplyr::left_join(context_est_dpseg, by=c("context", "measure")) %>% # add context results to nontext results
  tidyr::extract(col=context, into=c("method", "context"), regex="([[:upper:]]{2,3})([[:alnum:]]+)") %>% 
  mutate(model="dpseg")

#----------------------------
## combine results from colingFinal and dpseg-1.2.1
cm_results <- rbind(cm_results_dpseg, cm_results_coling) 

cm_results$value <- as.numeric(cm_results$value)
cm_results$iter <- as.numeric(cm_results$iter)
cm_results$measure <- as.factor(cm_results$measure)
cm_results$context <- as.factor(cm_results$context)
cm_results$method <- as.factor(cm_results$method)
cm_results$model <- as.factor(cm_results$model)

# coling returns model results between 0 and 1 while dpseg converts to percent (0 to 100)
# standardize these for easier comparison
cm_results$value <- ifelse(cm_results$value < 1, 100*cm_results$value, cm_results$value)
cm_results$context_est <- ifelse(cm_results$context_est < 1, 100*cm_results$context_est, cm_results$context_est)
   
# add Z scored versions of context_est and bootstrap values, for easier comparison across contexts and methods
cm_results <- bootstrap_Z(cm_results, value="value", est="context_est", by=c("model", "method", "context", "measure"))
                
# add in some information about each context subset
cm_results <- ds_results %>% 
  # add in information from nontext descriptives dataframe
  # dropping the bootstrapped nontext values (we just want the context estimates here)
  dplyr::select(method, context, measure, context_est) %>% 
  unique() %>% 
  # reformat to wide, so each context descriptive measure is a column
  tidyr::spread(key=measure, value=context_est) %>% 
  # add these descriptive columns to the cm_results dataframe
  left_join(cm_results, by=c("method", "context")) %>% 
  ungroup() %>% 
  dplyr::mutate(model=factor(model, levels=c("coling", "dpseg"), labels=c("Adaptor_Grammar", "HDP")))

cache('cm_results')

# calculate p vlaues for each context and measure
cm_boot_tests <- cm_results %>% 
  dplyr::mutate(above=ifelse(value >= context_est, 1, ifelse(value < context_est, 0, NA))) %>% 
  dplyr::group_by(model, method, context, measure, context_est, Z_est, N.utts, TTR, mean.words.per.utt, prop.one.word.utt) %>% 
  dplyr::summarize(iters = n(), 
                   p.val = mean(above)) %>% 
  add_stars()
cache('cm_boot_tests')

hist(dplyr::filter(cm_boot_tests, measure == "token_f.score")$iters, breaks=30)
View(cm_boot_tests)
