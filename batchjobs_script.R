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

install.packages("dplyr", "tidyr", "doParallel")

starts <- 1:10

# copy utt_orth_phon_KEY.txt to server
# copy data_processing_functions.r to server
# copy /Users/TARDIS/Documents/STUDIES/context_word_seg/words by contexts.csv to server
# copy dict_all3_updated.txt to server
# copy simple.tmpl
# copy .BatchJobs.R


batch_function <- function(starts){
  library(dplyr)
  library(tidyr)
  
  evaluation <- function(df, contexts){ # this is the function that should be done in parallel on the 12 cores of each node
    #contexts <- df[1, c(4:ncol(df))]
    
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
  
  # source("data_processing_functions.r")
  nontext_cols <- function(df, context_names){
    nontexts<- vector("list", length(context_names)) # storage variable
    non <- NULL
    
    for(k in 1:length(context_names)){  
      N.hits <- length(which(df[[context_names[k]]] == 1)) # the number of utterances that contain a keyword for that context
      nontexts[[k]] <- sample(x=as.numeric(row.names(df)), size=N.hits)
      col <- rep(0, nrow(df))
      col[nontexts[[k]]] <- 1
      non <- cbind(non, col)
      colnames(non)[k] <- paste("non.", context_names[k], sep="") 
    }
    return(list(non, nontexts))
  }
  expand_windows <- function(df){
    for(i in 3:(nrow(df)-2)){
      for(j in which(colnames(df) == colnames(contexts)[1]):ncol(df)){
        df[i,j] <- ifelse(df[i,j]==1, df[i,j], # if it is already marked 1, leave it
                          ifelse(df[(i-2),j]==1, 1.5, # if the utterance 2 before it is marked 1, mark 1.5
                                 ifelse(df[(i-1),j]==1, 1.5, # if the utterance 1 before it is marked 1, mark 1.5
                                        ifelse(df[(i+1),j]==1, 1.5, # if the utterance 1 after it is marked 1, mark 1.5
                                               ifelse(df[(i+2),j]==1, 1.5, 0))))) # if the utterance 2 after it is marked 1, mark 1.5. Otherwise mark 0.
        # note that utterances classified as a context based on their proximity to an utterance with a key word must be marked with something other than 1 to prevent them being used as key utterances in the next row
        
      }
    }
    return(df)
  }
  calc_MI = function(phon.pairs, phon.stream){
    # mutual information, and transitional probabilty. See Swingley (2005) p97
    for(i in 1:nrow(phon.pairs)){
      AB <- filter(phon.pairs, syl1==syl1[i] & syl2==syl2[i])
      p.AB <- nrow(AB)/nrow(phon.pairs)
      p.A <- length(which(phon.stream == phon.pairs$syl1[i]))/length(phon.stream)
      p.B <- length(which(phon.stream == phon.pairs$syl2[i]))/length(phon.stream)
      
      phon.pairs$MI[i] <- ifelse(p.AB==0, NA, log2(p.AB/(p.A * p.B))) # if AB never occurs, enter NA, otherwise calculate MI
      phon.pairs$TP[i] <- ifelse(p.AB==0, NA, p.AB/(p.A)) # if AB never occurs, enter NA, otherwise calculate TP
      phon.pairs$freq[i] <- ifelse(p.AB==0, NA, nrow(AB)) # if AB never occurs, enter NA, otherwise enter freq
    }
    output <- unique(phon.pairs) # Only keep one instance of each syllable pair
    return(output)
  }
  make_streams = function(df){
    # add utterance boundary marker 
    phon.utts <- paste(df$phon, "##")
    # collapse phonological utterances into one continuous stream
    phon.stream <- unlist(strsplit(phon.utts, " "))
    # delete "syllables" that are just empty space
    phon.stream <- phon.stream[ !grepl(pattern="^$", x=phon.stream) ]
    
    # how many unique syllables are there?
    syllables <- unique(phon.stream)
    # syllables <- sample(syllables, 200) # for testing, just use some random syllables
    N.syl <- length(syllables)
    
    # make phone stream into a list of all of the bisyllable pairs that occur
    phon.pairs <- data.frame(syl1=phon.stream[1:length(phon.stream)-1], syl2=phon.stream[2:length(phon.stream)])
    # delete rows that code for utterance boundary (the result is that syllable pairs across utterance boundaries are simply unattested)
    phon.pairs <- dplyr::filter(phon.pairs, syl1 !="##" & syl2 !="##")
    
    # collapse orthographic utterances into one stream
    orth.stream <- unlist(strsplit(df$orth, " "))
    
    # how many unique words are there?
    words <- unique(orth.stream)
    N.words <- length(words)
    
    output <- list(phon.stream=phon.stream, syllables=syllables, N.syl=N.syl, phon.pairs=phon.pairs, orth.stream=orth.stream, words=words, N.words=N.words)
    return(output)
  }
  context_results <- function(contexts, df){
    context.data <- vector("list", length(names(contexts))) # storage variable
    names(context.data) <- names(contexts)
    
    for(k in 1:length(names(contexts))){
      
      message(paste("processing ", names(contexts)[k], "...", sep=""))
      
      df.context <- filter(df, df[ , which(colnames(df)==names(contexts)[k])] > 0) # select cases that have any value greater than 0 in the column that matches the name of context k
      
      context.data[[k]]$N.hits <- nrow(filter(df, df[ , which(colnames(df)==names(contexts)[k])] == 1)) # the number of utterances that contained a key word (does not include utterances before and after)
      
      context.data[[k]]$N.utterances <- nrow(df.context)
      
      context.data[[k]]$streams <- make_streams(df.context)
      context.data[[k]]$unique.phon.pairs <- calc_MI(context.data[[k]]$streams$phon.pairs, context.data[[k]]$streams$phon.stream)
      
      context.data[[k]]$freq.bigrams <- dplyr::summarise(group_by(context.data[[k]]$streams$phon.pairs, syl1, syl2), count=n()) # frequency of bigrams
      context.data[[k]]$freq.words <- table(context.data[[k]]$streams$orth.stream) # frequency of words
      context.data[[k]]$freq.syl <- table(context.data[[k]]$streams$phon.stream) # frequency of syllables
    }
    return(context.data)
  }
  segment_speech <- function(cutoff, stat, unique.phon.pairs, phon.stream, consider.freq=FALSE){
    
    if(stat=="TP") {
      TP.cutoff <- quantile(unique.phon.pairs$TP, cutoff)
      message(paste("...TP cutoff is", round(TP.cutoff, 3)))
      unique.phon.pairs$TPseg <- ifelse(unique.phon.pairs$TP > TP.cutoff, 0, 1)
    } else if(stat=="MI") {
      MI.cutoff <- quantile(unique.phon.pairs$MI, cutoff)
      message(paste("...MI cutoff is", round(MI.cutoff, 3)))
      unique.phon.pairs$MIseg <- ifelse(unique.phon.pairs$MI > MI.cutoff, 0, 1)
    } else {stop("ERROR: Enter stat='TP' or stat='MI' only")}
    
    # to consider frequency as well, only segment units that are above freqency threshold as well as above TP/MI threshold
    if(consider.freq){
      freq.cutoff <- quantile(unique.phon.pairs$freq, cutoff)
      message(paste("...frequency cutoff is", round(freq.cutoff, 3)))
      unique.phon.pairs$seg <- ifelse(is.na(unique.phon.pairs$freq), NA,
                                      ifelse(unique.phon.pairs$freq < freq.cutoff, 0, 
                                             ifelse(unique.phon.pairs$MIseg==1 | unique.phon.pairs$TPseg==1, 1, 0)))
    } else if(stat=="TP") {
      unique.phon.pairs$seg <- unique.phon.pairs$TPseg
    } else if(stat=="MI") {
      unique.phon.pairs$seg <- unique.phon.pairs$MIseg
    } else {stop("ERROR: Enter stat='TP' or stat='MI' only")}
    
    
    seg.phon.stream <- phon.stream
    
    for(i in 2:length(phon.stream)){
      
      seg <- ifelse(phon.stream[i]=="##" | phon.stream[i-1]=="##", 1, # utterance boundaries are given as word boundaries
                    dplyr::filter(unique.phon.pairs, syl1==phon.stream[i-1] & syl2==phon.stream[i])$seg)
      
      if(length(seg) > 1) stop(paste("ERROR at ", i, "th element of phon.stream: more than one entry for seg", sep=""))
      
      seg.phon.stream[i]<- ifelse(seg==1, 
                                  paste0(",", phon.stream[i]), 
                                  phon.stream[i]) # if seg=1 for this phon pair, then insert a comma before the second syllable
    }
    
    # drop utterance boundary markers (the segmentation is still coded on the syllable after the utt boundary)
    seg.phon.stream <- seg.phon.stream[ seg.phon.stream != ",##"]
    
    return(seg.phon.stream)
  }
  assess_seg <- function(seg.phon.stream, words, dict){
    # extract units from segmented stream
    collapsed.temp <- paste(seg.phon.stream, collapse="-")
    collapsed <- paste(collapsed.temp, "-", sep="") # add a - to the very end, so every unit will have - as the last character
    collapsed.unseg <- gsub(",", "", collapsed, fixed=T) # make a version of the collapsed stream with commas removed, for frequency counts
    units.temp <- strsplit(collapsed, ",", fixed=T)[[1]]
    units <- gsub('.{1}$', "", units.temp) # remove the trailing - on each segmented unit
    unique.units <- data.frame(phon=unique(units) )
    
    # compare extracted units to dictionary words
    this.dict <- filter(dict, word %in% words)[,c("word", "phon")] 
    
    # number of hits and false alarms
    unique.units$precision <- ifelse(unique.units$phon %in% this.dict$phon, 1, 0) # if this segmented unit is in the dict it's a hit, if it's not in the dictionary it's a false alarm
    # number of hits and misses
    this.dict$recall <- ifelse(this.dict$phon %in% unique.units$phon, 1, 0)
    
    results <- merge(this.dict, unique.units, by="phon", all=T)
    
    # segmentation result
    results$seg.result <- as.factor(ifelse(results$recall==1 & results$precision==1, "hit", 
                                           ifelse(results$recall==0 & is.na(results$precision), "miss",
                                                  ifelse(is.na(results$recall) & results$precision==0, "false alarm",
                                                         NA))))
    
    # add number of syllables and frequency for each word
    results$N.syl <- NA
    for(i in 1:nrow(results)){
      results$N.syl[i] <- length(strsplit(as.character(results$phon[i]), split="-", fixed=TRUE)[[1]])
      results$freq[i] <- length(gregexpr(pattern=as.character(results$phon[i]), text=collapsed.unseg, fixed=TRUE)[[1]])
      results$freq.segd[i] <- length(gregexpr(pattern=paste(",",as.character(results$phon[i]), "-,", sep=""), text=collapsed, fixed=TRUE)[[1]])
    }
    freq.breaks <- quantile(results$freq, probs=seq(0,1, 1/3))
    results$freq.bins <- ifelse(results$freq <= freq.breaks[1], "low",
                                ifelse(results$freq <= freq.breaks[2], "med",
                                       ifelse(results$freq > freq.breaks[2], "high", NA)))
    results$freq.bins <- as.factor(results$freq.bins)
    # break N.syl into monosyllabic, disyllabic and multisyllabic
    results$syl.bins <- cut(results$N.syl, breaks=c(0,1,2,3,max(results$N.syl)+1), labels=c("one", "two", "three", "more"), ordered_result=T)
    results$N.segd.units <- length(units)  # how many "words" were found in this corpus?
    
    return(results)
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
save(results, file="batchresults.RData")