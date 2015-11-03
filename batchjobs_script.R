# http://blogs.uoregon.edu/rclub/2015/06/02/easy-massively-parallel-r-on-uos-aciss-cluster/
# https://nsaunders.wordpress.com/2015/04/01/configuring-the-r-batchjobs-package-for-torque-batch-queues/

# http://aciss-computing.uoregon.edu/2013/09/04/how-to-submission-queues/


# # typed in console:
# cp /Library/Frameworks/R.framework/Versions/3.1/Resources/library/BatchJobs/etc/BatchJobs_global_config.R /Users/TARDIS/Documents/STUDIES/context_word_seg/.Batchjobs.R
# # edit the first line of the resulting file (.Batchjobs.R) to look like this:    cluster.functions = makeClusterFunctionsTorque("simple.tmpl")

###################################
# the code below gets pasted into the terminal after launching R on an ACISS node
###################################
library(devtools)
install_github("tudo-r/BatchJobs")
library(BatchJobs)

# define the data and the function
starts <- replicate(60, rnorm(100), simplify = FALSE)
myFun  <- function(start) { 
  mean(start) 
}

# create a registry
reg <- makeRegistry(id = "batchtest")

# map function and data to jobs and submit
ids  <- batchMap(reg, myFun, starts)
done <- submitJobs(reg, resources = list(nodes = 5, ppn=12))

## if it all goes badly wrong run this to delete and start over
removeRegistry(reg, ask="no")




###################################
# the code below gets pasted into the terminal after launching R on an ACISS node
###################################
library(BatchJobs)
library(dplyr)
library(tidyr)
library(doParallel)
starts <- replicate(60, rnorm(100), simplify = FALSE)

# copy utt_orth_phon_KEY.txt to server
# copy data_processing_functions.r to server
# copy /Users/TARDIS/Documents/STUDIES/context_word_seg/words by contexts.csv to server
# copy dict_all3_updated.txt to server

batch_function <- function(){
  #note that key should have the context columns already (from human coding or topic modeling, etc.)
  key <- read.table("utt_orth_phon_KEY.txt", header=1, sep="\t", stringsAsFactors=F, quote="", comment.char ="")
  df <- key
  
  dict <- read.table("dict_all3_updated.txt", sep="\t", quote="", comment.char ="", header=1, stringsAsFactors=F)
  cols <- ncol(dict)
  ####################################
  # add defined context for each word in dict
  ####################################
  # for context lists...
  contexts <- read.csv("words by contexts.csv")
  for(i in 1:length(colnames(contexts))){
    
    dict <- cbind(dict, as.numeric(rep(NA, nrow(dict)))) # add a column of NAs to dict
    colnames(dict)[cols+i] <- colnames(contexts)[i] # name that column after the current context
    
    key.words <- as.vector(contexts[,i][contexts[,i] != ""]) # drop empty cells from context columns to save just a vector of key words for this context
    
    dict[,cols+i] <- ifelse(dict$word %in% key.words, 1, 0) # if word occurs in the key.words, then mark 1 for this column, otherwise 0
  }
  ####################################
  
  source("data_processing_functions.r")
  
  iter <- 12 # the number of times to generate random samples
  evaluation <- function(){ # this is the function that should be done in parallel on the 12 cores of each node
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
      
      message(paste("Segment speech! Processing ", names(nontexts)[k], "...", sep=""))
      
      nontext.data[[k]]$TP85$seg.phon.stream <- segment_speech(cutoff=.85, stat="TP", nontext.data[[k]]$unique.phon.pairs, nontext.data[[k]]$streams$phon.stream)
      
      nontext.data[[k]]$MI85$seg.phon.stream <- segment_speech(cutoff=.85, stat="MI", nontext.data[[k]]$unique.phon.pairs, nontext.data[[k]]$streams$phon.stream)
    }
    
    # assess segmentation
    stat.results <- data.frame(recall=NULL, precision=NULL, stat=NULL, nontext=NULL)
    for(k in 1:length(names(nontext.data))){
      
      message(paste("Assess segmentation! Processing ", names(nontexts)[k], "...", sep=""))
      message("TPs...")
      
      nontext.data[[k]]$TP85$seg.results <- assess_seg(seg.phon.stream=nontext.data[[k]]$TP85$seg.phon.stream, words=nontext.data[[k]]$streams$words, dict=dict)
      
      TPresults <- colMeans(nontext.data[[k]]$TP85$seg.results[,3:4], na.rm=T)
      TPresults$stat <- "TP"
      
      message("MIs...")
      nontext.data[[k]]$MI85$seg.results <- assess_seg(seg.phon.stream=nontext.data[[k]]$MI85$seg.phon.stream, words=nontext.data[[k]]$streams$words, dict=dict)
      
      MIresults <- colMeans(nontext.data[[k]]$MI85$seg.results[,3:4], na.rm=T)
      MIresults$stat <- "MI"
      
      this.result <- as.data.frame(rbind(TPresults, MIresults))
      row.names(this.result) <- NULL
      this.result$nontext <- names(nontext.data)[[k]]
      stat.results <- rbind(stat.results,this.result)
    } 
    return(stat.results)
  }
  registerDoParallel()
  r <- foreach(1:iter, .combine = rbind) %dopar% evaluation()
}


# create a registry
reg <- makeRegistry(id = "batchtest")

# map function and data to jobs and submit
ids  <- batchMap(reg, batch_function, starts)
done <- submitJobs(reg, resources = list(nodes = 5, ppn=iter))

