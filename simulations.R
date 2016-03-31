####################################################################################
# how does the cutoff affect precision and recall?
####################################################################################

starts <- seq(from=.05, to=.95, by=.05)

batch_function <- function(start){
  library(dplyr)
  library(tidyr)
  library(devtools)
  library(RCurl)
  fun.version <- "15cf033206483"
  source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/data_processing_functions.r", 
             sha1=fun.version)
  
  # note that df should have the context columns already (from lists, human coding, or topic modeling, etc.)
  # df <- get_from_https("https://raw.githubusercontent.com/rosemm/context_word_seg/master/contexts_HJ_voting.txt"); prop=FALSE; expand=FALSE
  # df <- get_from_https("https://raw.githubusercontent.com/rosemm/context_word_seg/master/contexts_HJ_prop.txt"); prop=TRUE; expand=FALSE
  df <- get_from_https("https://raw.githubusercontent.com/rosemm/context_word_seg/master/contexts_WL.txt") ; prop=FALSE; expand=TRUE
   
  if(nrow(df) == 0) stop("df didn't load")
  
  dict <- read.table("dict_all3_updated.txt", sep="\t", quote="", comment.char ="", header=1, stringsAsFactors=F)
  if(nrow(dict) == 0) stop("dict didn't load")
  
  iter <- 20 # the number of times to generate random samples
  
  library(doParallel)
  registerDoParallel()
  r <- foreach(1:iter, 
               .combine = rbind,
               .inorder=FALSE,
               .errorhandling='remove',
               .packages=c("dplyr", "tidyr", "devtools", "BBmisc") ) %dopar% par_function(dataframe=df,
                                                                                dict=dict,
                                                                                expand=expand,
                                                                                seg.utts=TRUE,
                                                                                TP=FALSE,
                                                                                MI=TRUE, 
                                                                                verbose=FALSE, 
                                                                                prop=prop,
                                                                                cutoff=start, 
                                                                                nontext=TRUE,
                                                                                fun.version=fun.version)
}

# create a registry
id <- "bootstrapCutoffWLnontext"
reg.cutoff <- makeRegistry(id = id)

# map function and data to jobs and submit
ids  <- batchMap(reg.cutoff, batch_function, starts)
done <- submitJobs(reg.cutoff, resources = list(nodes = 20, walltime=28800)) # expected to run for 8 hours (28800 seconds)

showStatus(reg.cutoff)

#######################
# get context estimates
df <- get_from_https("https://raw.githubusercontent.com/rosemm/context_word_seg/master/contexts_WL.txt"); prop=FALSE
dict <- read.table("dict_all3_updated.txt", sep="\t", quote="", comment.char ="", header=1, stringsAsFactors=F)

# settings
expand=TRUE
seg.utts=TRUE
TP=FALSE
MI=TRUE 
verbose=FALSE 
prop=prop
nontext=FALSE

context.names <- colnames(df[4:ncol(df)])

df.non <- df

# calculate MIs and TPs
data <- context_results(context.names, df=df.non, seg.utts=seg.utts) # calls make_streams() and calc_MI()

r <- NULL
for(c in 1:length(starts)){
  cutoff=starts[c] 
  if(nrow(df) == 0) stop("df didn't load")
  if(nrow(dict) == 0) stop("dict didn't load")
  message(paste("cutoff is", starts[c]))
  # segment speech
  for(k in 1:length(names(data))){
    message(paste("segmenting speech for", names(data)[k]))
    data[[k]]$MI85$seg.phon.stream <- segment_speech(cutoff=cutoff, 
                                                           stat="MI", 
                                                           data[[k]]$unique.phon.pairs, 
                                                           data[[k]]$streams$phon.stream, 
                                                           seg.utts=seg.utts)
  }
  # assess segmentation
  stat.results <- data.frame(recall=NULL, precision=NULL, stat=NULL, nontext=NULL)
  for(k in 1:length(names(data))){
    message(paste("assessing segmentation for", names(data)[k]))
    if(TP){
      data[[k]]$TP85$seg.results <- assess_seg(seg.phon.stream=data[[k]]$TP85$seg.phon.stream, words=data[[k]]$streams$words, dict=dict)
      TPresults <- colMeans(data[[k]]$TP85$seg.results[,3:4], na.rm=T)
      TPresults$stat <- "TP"
    }
    if(MI){
      data[[k]]$MI85$seg.results <- assess_seg(seg.phon.stream=data[[k]]$MI85$seg.phon.stream, words=data[[k]]$streams$words, dict=dict)
      MIresults <- colMeans(data[[k]]$MI85$seg.results[,3:4], na.rm=T)
      MIresults$stat <- "MI"
    }
    if(TP & MI){ 
      this.result <- as.data.frame(rbind(TPresults, MIresults))
      this.result$TPN.segd.units <- median(data[[k]]$TP85$seg.results$N.segd.units)
      this.result$MIN.segd.units <- median(data[[k]]$MI85$seg.results$N.segd.units)
    } else if(TP){
      this.result <- as.data.frame(rbind(TPresults))
      this.result$TPN.segd.units <- median(data[[k]]$TP85$seg.results$N.segd.units)
    } else if(MI){
      this.result <- as.data.frame(rbind(MIresults))
      this.result$MIN.segd.units <- median(data[[k]]$MI85$seg.results$N.segd.units)
    } else stop("At least one of MI and TP must be true.")
    
    row.names(this.result) <- NULL
    this.result$stat <- as.factor(as.character(this.result$stat))
    this.result$nontext <- names(data)[[k]]
    this.result$cutoff <- cutoff
    this.result$N.utts <- data[[k]]$N.utterances
    this.result$N.words <- data[[k]]$streams$N.words
    stat.results <- rbind(stat.results,this.result)
  } 
  stat.results$nontext <- as.factor(as.character(stat.results$nontext))
  stat.results$recall <- as.numeric(stat.results$recall)
  stat.results$precision <- as.numeric(stat.results$precision)
  
  if(!verbose){
    this.r <- stat.results
  } else {
    this.r <- list(stat.results, data) 
  }
    
  r <- rbind(r, this.r)
}

context.ests <- r %>%
  gather(key=measure, value=context.est, recall:precision) %>%
  rename(context=nontext)

cutoff.results1 <- process_batch_results(id="bootstrapCutoffWLnontext", dir="nontexts_CutoffSim")
cutoff.results1$round <- "one"
cutoff.results2 <- process_batch_results(id="bootstrapCutoffWLnontext2", dir="nontexts_CutoffSim")
cutoff.results2$round <- "two"

cutoff.results <- rbind(cutoff.results1, cutoff.results2)
# cutoff.results2 <- process_batch_results(id="bootstrapCutoffWLnontext2", dir="expandFALSE/nontexts_CutoffSim")
# cutoff.results2 <- process_batch_results(id="bootstrapCutoffWLnontext0", dir="old output/nontexts_CutoffSim")
# cutoff.results3 <- process_batch_results(id="bootstrapCutoffWLnontext1", dir="old output/nontexts_CutoffSim")
# cutoff.results4 <- process_batch_results(id="bootstrapCutoffWLnontext2", dir="old output/nontexts_CutoffSim")
# cutoff.results <- rbind(cutoff.results2, cutoff.results3, cutoff.results4)


cutoff.results <- cutoff.results %>%
  gather(key=measure, value=value, recall:precision) %>%
  rename(context=nontext)

# # add the context estimates to the nontext results
# cutoff.results <- left_join(cutoff.results, context.ests, by=c("cutoff", "context", "stat", "measure"))
# 
# cutoff.results$cutoff <- as.factor(cutoff.results$cutoff)

ggplot(context.ests, aes(x=cutoff, y=context.ests)) +
  geom_point(data=cutoff.results, aes(x=cutoff, y=value, color=round), size=2, alpha=.3) + 
  # geom_point(aes(x=cutoff, y=context.est, color=context), size=4, show_guide=F) + 
  facet_grid(measure ~ context, scales="free_y") +
  theme(text = element_text(size=20), axis.ticks = element_blank()) +
  labs(y=NULL)


####################################################################################
# how does corpus size affect precision and recall?
####################################################################################
# the model (run this in R on ACISS)
###########################################################
library(BatchJobs)
# getConfig()

# install.packages("dplyr", "tidyr", "doParallel")
library(dplyr); library(tidyr); library(doParallel); library(devtools)

starts <- 1:50

batch_function <- function(start, verbose=FALSE, dataframe, TTR){
  library(dplyr)
  library(tidyr)
  library(devtools)
  fun.version <- "c7e0f2c7cff0d" # refers to the current commit for data_processing_functions.r
  source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/data_processing_functions.r", 
             sha1=fun.version)
  
#   # note that df should have the context columns already (from lists, human coding, or topic modeling, etc.)
#   corpus <- get_from_https("https://raw.githubusercontent.com/rosemm/context_word_seg/master/utt_orth_phon_KEY.txt")
#   df <- contexts_by_size(corpus, N.sizes=20, min.utt=200)
#   if(nrow(df) == 0) stop("df didn't load")
  
  dict <- read.table("dict_all3_updated.txt", sep="\t", quote="", comment.char ="", header=1, stringsAsFactors=F)
  cols <- ncol(dict)
  if(nrow(dict) == 0) stop("dict didn't load")

  reps <- 20
  if (TTR){
    N.types <- round(seq(from=250, to=1800, length.out=reps), 0) # these are good N.types values for make_corpus(), for manipulating TTR
  } else {
    N.types <- rep(1800, reps) # 1800 is a sensible default
  }
  
  library(doParallel)
  registerDoParallel()
  r <- foreach(i=N.types, 
               .errorhandling="pass",
               .verbose=TRUE) %dopar% par_function(dataframe=dataframe,
                                                   N.types=i,
                                                   N.utts=500, # if this argument is left NULL, it will use 1000 utts
                                                   dict=dict,
                                                   expand=FALSE,
                                                   seg.utts=TRUE, 
                                                   TP=FALSE,
                                                   MI=TRUE, 
                                                   verbose=verbose, 
                                                   prop=FALSE,
                                                   cutoff=.85,
                                                   nontext=TRUE,
                                                   fun.version=fun.version)
  return(r)
}

# create a registry
id <- "bootSizeSim_skew"
reg.size.skew <- makeRegistry(id = id)
# system('rm -r *-files')
# removeRegistry(reg)
# map function and data to jobs and submit
ids  <- batchMap(reg.size.skew, batch_function, starts, more.args=list(verbose=TRUE, dataframe="skewed", TTR=FALSE))
done <- submitJobs(reg.size.skew, resources = list(nodes=1, ppn=12))

id <- "bootSizeSim_unif"
reg.size.unif <- makeRegistry(id = id)
# map function and data to jobs and submit
ids  <- batchMap(reg.size.unif, batch_function, starts, more.args=list(verbose=TRUE, dataframe="unif", TTR=FALSE))
done <- submitJobs(reg.size.unif, resources = list(nodes=1, ppn=12))

id <- "bootTTRSim_skew"
reg.ttr.skew <- makeRegistry(id = id)
# map function and data to jobs and submit
ids  <- batchMap(reg.ttr.skew, batch_function, starts, more.args=list(verbose=TRUE, dataframe="skewed", TTR=TRUE))
done <- submitJobs(reg.ttr.skew, resources = list(nodes=1, ppn=12))

id <- "bootTTRSim_unif"
reg.ttr.unif <- makeRegistry(id = id)
# map function and data to jobs and submit
ids  <- batchMap(reg.ttr.unif, batch_function, starts, more.args=list(verbose=TRUE, dataframe="unif", TTR=TRUE))
done <- submitJobs(reg.ttr.unif, resources = list(nodes=1, ppn=12))

showStatus(reg.size.unif); showStatus(reg.size.skew); showStatus(reg.ttr.unif); showStatus(reg.ttr.skew) # checking progress
findDone(reg) # checking progress
# results <- loadResults(reg.size.skew.q)
reg.size.unif
 
size.results <- process_batch_results(id="bootstrapSizeSim", dir="nontexts_SizeSim")

library(tidyr); library(dplyr)
nontext.results <- size.results  %>%
  filter(grepl(x=nontext, pattern="N.utts")) %>%
  mutate(cutoff=85) %>%
  unite(criterion, stat, cutoff, sep="", remove=F) %>%
  gather(measure, value, recall:precision) %>%
  rename(context=nontext) %>%
  extract(col=context, into="size", regex="([[:digit:]]+)" )
nontext.results$size <- as.numeric(nontext.results$size)
# how many iterations make up each dist?
iters <- nrow(filter(nontext.results, stat=="MI", measure=="recall", size==unique(nontext.results$size)[1]))

library(ggplot2)
ggplot(nontext.results, aes(x=as.factor(size), y=value))+
  geom_boxplot() +
  facet_grid(stat~measure, scales="free") +
  theme(text = element_text(size=20), axis.text.x=element_text(size=10, angle =90), axis.ticks = element_blank() ) +
  labs(y=NULL, x="Number of utterances in random corpora") + 
  ggtitle(paste(iters, "iterations in each boxplot"))

####################################################################################
# how does the shape of the distribution affect MI dist, precision, and recall?
####################################################################################
# read in the functions written for this analysis
source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/data_processing_functions.r")

# make a skewed corpus and save to text file
lang.skew <- make_corpus(dist="skewed", N.utts=1000, N.types=24)
corpus.skew <- lang.skew[[1]] # the corpus
dict.skew <- lang.skew[[2]] # the dictionary
# write.table(corpus.skew, file="utt_orth_phon_KEY_SKEW.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
# use that corpus to generate a size sim contexts file
df.skew <- contexts_by_size(df=corpus.skew, N.sizes=25, min.utt=10)
# write.table(df.skew, file="contexts_SizeSimSKEW.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")


# make a uniform corpus and save to text file
lang.unif <- make_corpus(dist="unif", N.utts=1000, N.types=24)
corpus.unif <- lang.unif[[1]] # the corpus
dict.unif <- lang.unif[[2]] # the dictionary
# write.table(corpus.unif, file="utt_orth_phon_KEY_UNIF.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
# use that corpus to generate a size sim contexts file
df.unif <- contexts_by_size(df=corpus.unif, N.sizes=25, min.utt=50)
# write.table(df.skew, file="contexts_SizeSimUNIF.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")

# check that the frequency dist looks correct
plot_corpus_dist(corpus.skew)
plot_corpus_dist(corpus.unif)

unif.res <- par_function(df=df.unif, dict=dict.unif, expand=FALSE, seg.utts=TRUE, TP=FALSE, verbose=TRUE)
skew.res <- par_function(df=df.skew, dict=dict.skew, expand=FALSE, seg.utts=TRUE, TP=FALSE, verbose=TRUE)
names(unif.res[[2]]) # the sizes tried
names(skew.res[[2]]) # the sizes tried
summary(unif.res[[2]][[25]]$MI85$seg.results)
summary(unif.res[[2]]$N.utts992$MI85$seg.results)
summary(unif.res[[2]]$N.utts51$MI85$seg.results)

hist(unif.res[[2]]$N.utts10$unique.phon.pairs$MI, breaks=30); abline(v=quantile(unif.res[[2]]$N.utts10$unique.phon.pairs$MI, .85), lty=2, col="red")
hist(skew.res[[2]]$N.utts10$unique.phon.pairs$MI, breaks=30); abline(v=quantile(skew.res[[2]]$N.utts10$unique.phon.pairs$MI, .85), lty=2, col="red")

hist(unif.res[[2]]$N.utts992$unique.phon.pairs$MI, breaks=30); abline(v=quantile(unif.res[[2]][[25]]$unique.phon.pairs$MI, .85), lty=2, col="red")
hist(skew.res[[2]]$N.utts990$unique.phon.pairs$MI, breaks=30); abline(v=quantile(skew.res[[2]][[25]]$unique.phon.pairs$MI, .85), lty=2, col="red")

#################
library(BatchJobs)

library(dplyr); library(tidyr); library(doParallel); library(devtools)

starts <- 1:30

batch_function <- function(start){
  library(devtools)
  fun.version <- "2400f7eeb4395" # refers to the current commit for data_processing_functions.r
  source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/data_processing_functions.r", 
             sha1=fun.version)
  iter <- 50 # the number of times to generate random samples
  
  library(doParallel)
  registerDoParallel()
  r <- foreach(1:iter, 
               .inorder=FALSE,
               .combine = rbind, 
               .packages=c("dplyr", "tidyr", "devtools", "BBmisc") ) %dopar% par_function(dataframe="unif",
                                                                                          dict=NULL,
                                                                                          expand=FALSE,
                                                                                          seg.utts=TRUE,
                                                                                          TP=FALSE,
                                                                                          MI=TRUE,
                                                                                          verbose=FALSE,
                                                                                          prop=FALSE,
                                                                                          cutoff=.85,
                                                                                          nontext=TRUE,
                                                                                          fun.version=fun.version)
  # par_function args: dataframe, dict, expand, seg.utts=TRUE, TP=TRUE, MI=TRUE, verbose=FALSE, prop=FALSE, cutoff=.85, nontext=TRUE, fun.version
}

# create a registry
id.skew <- "bootstrapSizeSimSKEW"
reg.skew <- makeRegistry(id = id.skew)

# map function and data to jobs and submit
ids  <- batchMap(reg.skew, batch_function, starts)
done <- submitJobs(reg.skew, resources = list(nodes = 20, walltime=21600)) # expected to run for 6 hours (21600 seconds)

# create a registry
id.unif <- "bootstrapSizeSimUNIF"
reg.unif <- makeRegistry(id = id.unif)

# map function and data to jobs and submit
ids  <- batchMap(reg.unif , batch_function, starts)
done <- submitJobs(reg.unif , resources = list(nodes = 20, walltime=21600)) # expected to run for 6 hours (21600 seconds)

showStatus(reg.unif); showStatus(reg.skew)

"bootstrapSizeSimSKEWnew"
skew.results1 <- process_batch_results(id="bootstrapSizeSimSKEW", dir="nontexts_SizeSimSKEW/seguttsFALSE")
skew.results2 <- process_batch_results(id="bootstrapSizeSimSKEW50", dir="nontexts_SizeSimSKEW/seguttsFALSE")
skew.results3 <- process_batch_results(id="bootstrapSizeSimSKEWlong", dir="nontexts_SizeSimSKEW")
skew.results4 <- process_batch_results(id="bootstrapSizeSimSKEW1", dir="nontexts_SizeSimSKEW")
skew.results5 <- process_batch_results(id="bootstrapSizeSimSKEW", dir="nontexts_SizeSimSKEW")
skew.results <- rbind(skew.results5)
skew.results$dist <- "skew"

unif.results1 <- process_batch_results(id="bootstrapSizeSimUNIF", dir="nontexts_SizeSimUNIF/seguttsFALSE")
unif.results2 <- process_batch_results(id="bootstrapSizeSimUNIF50", dir="nontexts_SizeSimUNIF/seguttsFALSE")
unif.results3 <- process_batch_results(id="bootstrapSizeSimUNIFlong", dir="nontexts_SizeSimUNIF")
unif.results4 <- process_batch_results(id="bootstrapSizeSimUNIF1", dir="nontexts_SizeSimUNIF")
unif.results5 <- process_batch_results(id="bootstrapSizeSimUNIF", dir="nontexts_SizeSimUNIF")
unif.results <- rbind(unif.results5)
unif.results$dist <- "unif"

nontext.results <- rbind(skew.results, unif.results)

nontext.results <- nontext.results %>%
  mutate(cutoff=85) %>%
  unite(criterion, stat, cutoff, sep="", remove=F) %>%
  gather(measure, value, recall:precision) %>%
  rename(context=nontext) %>%
  extract(col=context, into="size", regex="([[:digit:]]+)" )
nontext.results$size <- as.numeric(nontext.results$size)
# how many iterations make up each dist?
iters <- summarize(group_by(nontext.results, dist, stat, measure, size), count=n())
min.iters <- min(iters$count)
max.iters <- max(iters$count)
  
library(ggplot2)
p <- ggplot(filter(nontext.results, stat=="MI"), aes(x=as.factor(size), y=value, fill=dist, color=dist)) +
p <- ggplot(filter(nontext.results, stat=="MI"), aes(x=size, y=value, color=dist)) +
  geom_point(alpha=.3, position = position_jitter(width = 5)) +
  xlim(0,600) +
  #geom_boxplot() +
  facet_wrap(~measure, scales="free") +
  theme(text = element_text(size=20), axis.text.x=element_text(size=10, angle =90), axis.ticks = element_blank() ) +
  labs(y=NULL, x="Number of utterances in random corpora") +
if(min.iters == max.iters) p + ggtitle(paste(min.iters, "iterations at each size"))
if(min.iters != max.iters) p + ggtitle(paste(min.iters, "to", max.iters, "iterations in each boxplot"))
  
