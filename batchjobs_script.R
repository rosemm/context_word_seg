# http://blogs.uoregon.edu/rclub/2015/06/02/easy-massively-parallel-r-on-uos-aciss-cluster/
# https://nsaunders.wordpress.com/2015/04/01/configuring-the-r-batchjobs-package-for-torque-batch-queues/

# http://aciss-computing.uoregon.edu/2013/09/04/how-to-submission-queues/

###################################
# First time: install BatchJobs
###################################
library(devtools)
install_github("tudo-r/BatchJobs")
library(BatchJobs)

###########################################################
# configure BatchJobs (do this on your local machine, and then use sftp to send .Batchjobs.R and simple.tmpl to your working directory on ACISS)
###########################################################
# set up batchjobs configuration for this project to override the global settings
batch.conf <- readLines("/Library/Frameworks/R.framework/Versions/3.1/Resources/library/BatchJobs/etc/BatchJobs_global_config.R")
batch.conf[1] <- "cluster.functions = makeClusterFunctionsTorque('simple.tmpl')"
## to run debug function for BatchJobs, change the configuration file:
# batch.conf[1] <- "cluster.functions = makeClusterFunctionsInteractive()"
# batch.conf[7] <- "debug = TRUE"

writeLines(batch.conf, ".BatchJobs.R", sep="\n") # write this file to the current working directory

# from https://raw.githubusercontent.com/tudo-r/BatchJobs/master/examples/cfTorque/simple.tmpl
simple <- "#PBS -N <%= job.name %>
## merge standard error and output
#PBS -j oe
## direct streams to our logfile
#PBS -o <%= log.file %>
#PBS -l walltime=<%= resources$walltime %>,nodes=<%= resources$nodes %>,vmem=<%= resources$memory %>M
## remove this line if your cluster does not support arrayjobs
#PBS -t 1-<%= arrayjobs %>

## Run R:
## we merge R output with stdout from PBS, which gets then logged via -o option
R CMD BATCH --no-save --no-restore '<%= rscript %>' /dev/stdout
"

writeLines(simple, "simple.tmpl", sep="\n")


###########################################################
# the model (run this in R on ACISS)
###########################################################
library(BatchJobs)
getConfig()

# install.packages("dplyr", "tidyr", "doParallel")
library(dplyr); library(tidyr), library(doParallel); library(devtools)

starts <- 1:10

# copy utt_orth_phon_KEY.txt to server
# copy /Users/TARDIS/Documents/STUDIES/context_word_seg/words by contexts.csv to server
# copy dict_all3_updated.txt to server
# copy simple.tmpl
# copy .BatchJobs.R


batch_function <- function(starts){
  library(dplyr)
  library(tidyr)
  library(devtools)
  source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/data_processing_functions.r")
  
  evaluation <- function(df, contexts){ # this is the function that should be done in parallel on the 12 cores of each node
    # contexts <- df[1, c(4:ncol(df))]
    
    # pick nontexts
    results <- nontext_cols(df=df, context_names=colnames(contexts)) # add the nontext col
    non <- results[[1]]
    nontexts <- results[[2]]
    names(nontexts) <- paste("non.", colnames(contexts), sep="")
    
    # add nontext columns to dataframe
    colnames(non) <- colnames(contexts)
    df.non <- cbind(df[,1:3], non)
    
    # expand windows to + - 2 utterances before and after
    df.non <- expand_windows(df.non)
    
    # calculate MIs and TPs
    nontext.data <- context_results(contexts, df=df.non) # calls make_streams() and calc_MI()
    
    # segment speech
    for(k in 1:length(names(nontext.data))){
      
      nontext.data[[k]]$TP85$seg.phon.stream <- segment_speech(cutoff=.85, stat="TP", nontext.data[[k]]$unique.phon.pairs, nontext.data[[k]]$streams$phon.stream)
      
      nontext.data[[k]]$MI85$seg.phon.stream <- segment_speech(cutoff=.85, stat="MI", nontext.data[[k]]$unique.phon.pairs, nontext.data[[k]]$streams$phon.stream)
    }
    
    # assess segmentation
    stat.results <- data.frame(recall=NULL, precision=NULL, stat=NULL, nontext=NULL)
    for(k in 1:length(names(nontext.data))){
      
      nontext.data[[k]]$TP85$seg.results <- assess_seg(seg.phon.stream=nontext.data[[k]]$TP85$seg.phon.stream, words=nontext.data[[k]]$streams$words, dict=dict)
      
      TPresults <- colMeans(nontext.data[[k]]$TP85$seg.results[,3:4], na.rm=T)
      TPresults$stat <- "TP"

      nontext.data[[k]]$MI85$seg.results <- assess_seg(seg.phon.stream=nontext.data[[k]]$MI85$seg.phon.stream, words=nontext.data[[k]]$streams$words, dict=dict)
      
      MIresults <- colMeans(nontext.data[[k]]$MI85$seg.results[,3:4], na.rm=T)
      MIresults$stat <- "MI"
      
      this.result <- as.data.frame(rbind(TPresults, MIresults))
      row.names(this.result) <- NULL
      this.result$stat <- as.factor(as.character(this.result$stat))
      this.result$nontext <- names(nontext.data)[[k]]
      stat.results <- rbind(stat.results,this.result)
    } 
    stat.results$nontext <- as.factor(as.character(stat.results$nontext))
    stat.results$recall <- as.numeric(stat.results$recall)
    stat.results$precision <- as.numeric(stat.results$precision)
    return(stat.results)
  }
  
  # note that key should have the context columns already (from lists, human coding, or topic modeling, etc.)
  key <- read.table("utt_orth_phon_KEY.txt", header=1, sep="\t", stringsAsFactors=F, quote="", comment.char ="")
  df <- key
  if(nrow(df) == 0) stop("df didn't load")
  
  dict <- read.table("dict_all3_updated.txt", sep="\t", quote="", comment.char ="", header=1, stringsAsFactors=F)
  cols <- ncol(dict)
  if(nrow(dict) == 0) stop("dict didn't load")
  
  contexts <- read.csv("words by contexts.csv")
  if(nrow(contexts) == 0) stop("contexts didn't load")
    
  iter <- 12 # the number of times to generate random samples

  library(doParallel)
  registerDoParallel()
  r <- foreach(1:iter, .combine = rbind) %dopar% evaluation(df, contexts)
  # save(r, file="bootstrap_results.data")
}


# create a registry
id <- "bootstrap"
reg <- makeRegistry(id = id)

# map function and data to jobs and submit
ids  <- batchMap(reg, batch_function, starts)
done <- submitJobs(reg, resources = list(nodes = 10, ppn=iter))

showStatus(reg)

results <- data.frame(V1=NULL)
nodes <- list.files(paste0(id, "-files/jobs"))
for(i in 1:length(nodes)){
  if (i < 10){
    load(paste0(id, "-files/jobs/0", i, "/", i, "-result.RData"))
  } else {
    load(paste0(id, "-files/jobs/", i, "/", i, "-result.RData"))
  }
  results <- rbind(results, result)
}
# save(results, file="batchresults.RData") # not working
