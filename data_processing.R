rm(list = ls()) # clear the environment
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(knitr)
library(devtools)

# read in the functions written for this analysis
source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/data_processing_functions.r")
setwd("/Users/TARDIS/Documents/STUDIES/context_word_seg")


######################################################################
# translate orth to phon (this also reads in the dict file):
######################################################################
# to translate CHILDES transcripts to phon approximations using Swingley's dictionary:
# source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/data_processing_orth_to_phon.R")

# after the first time, we can just read in the saved results from the above code
dict <- read.table("dict_all3_updated.txt", header=1, sep="\t", quote="", comment.char ="")
df <- read.table("utt_orth_phon_KEY.txt", header=1, sep="\t", stringsAsFactors=F, quote="", comment.char ="") # this gets used for word-lists contexts, it will get over-written for other contexts

if( length(df$orth[grepl(x=df$orth, pattern="[[:upper:]]")]) > 0 )  df$orth <- tolower(df$orth) # make sure the orth stream is all lower case
if( length(df$phon[grepl(x=df$phon, pattern="-", fixed=T)])  > 0 )  df$phon <- gsub(x=df$phon, pattern="-", replacement=" ", fixed=T) # make sure all word-internal syllable boundaries "-" are represnted just the same as between-word syllable boundaries (space)


# add frequency for each word to dictionary
dict$freq.orth <- NA
search_in <- strsplit(paste(df$orth, collapse=" "), split=" ", fixed=T)[[1]] # look for it in the vector of words (df$orth, collapsed into one paste, and then split into a unit for each word)
for(i in 1:nrow(dict)){
  search_for <- paste("^",dict$word[i],"$", sep="") # look for the ith word in the dictionary
  dict$freq.orth[i] <- length(grep(pattern=search_for, x=search_in)) # how many times does it find the ith dict word in the orth stream?
}

######################################################################



######################################################################
# code contexts in df by word lists
######################################################################
contexts <- read.csv("/Users/TARDIS/Documents/STUDIES/context_word_seg/words by contexts.csv")

# mark seed word bigrams in df$orth
bigrams <- grep(pattern="_", x=unlist(as.list(contexts)), fixed=T, value=T) # all of the context seed words that are bigrams (have an _)
for(b in 1:length(bigrams)){
  search_for <- paste0(gsub(x=bigrams[[b]], pattern="_", replacement=" ")) # look for the bigram, but with a space instead of the _
  replace_with <- bigrams[[b]] # replace it with the bigram
  df$orth <- gsub(pattern=search_for, x=df$orth, replacement=replace_with)
}


######################################################################
# only keep context words that actually occur in the corpus
# (this is only relevant for presenting the list, e.g. in a talk - the code works fine with extra words in there)
######################################################################
contexts.list <- as.list(contexts)
for(i in 1:length(names(contexts.list))){
  this.context <- contexts.list[[i]][contexts.list[[i]] %in% global.data$streams$orth.stream] # the context words that show up in the corpus
  this.context <- c(as.character(this.context), rep("", nrow(contexts)-length(this.context))) # add empty values at the end to make all of the columns the same length
  contexts.list[[i]] <- this.context
}
contexts.occuring <- as.data.frame(contexts.list)
contexts.occuring <- contexts.occuring[-which(apply(contexts.occuring,1,function(x)all(x==""))),] # remove empty rows at the end
kable(contexts.occuring)
write.table(contexts.occuring, file="contexts.occuring.csv", sep=",", row.names=F)
############################
df <- df[ , 1:3] # only keep the first three columns (there should only be three columns anyway, but just to be sure)

temp <- unique(df$orth) # to speed up processing, only code each unique utterance once (then we'll join it back to the full df)
temp.codes <- data.frame(orth=temp)
for(k in 1:length(names(contexts))){
  
  temp.codes[[names(contexts)[k]]] <- 0 # make a new column for this context
  words <- as.character(unique(contexts[,k])) # this word list
  words <- words[words !=""] # drop empty character element in the list
  
  for(w in 1:length(words)){
    # for every orth entry that contains this word, put a 1 in this context's column
    temp.codes[[names(contexts)[k]]][grep(pattern=paste0("\\<", words[w], "\\>"), x=temp.codes$orth )] <- 1 
  }
}
df <- left_join(df, temp.codes) # join temp.codes back to full df

df <- expand_windows(df, context.names=names(contexts)) # extend context codes 2 utterances before and after each hit

write.table(df, file="contexts_WL.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")

############################################################################
# tag contexts by human coder judgments
############################################################################
# run code in context_coding_cleaning.r to retrieve and clean codes
df <- read.table("contexts_HJ.txt", header=1, sep="\t", stringsAsFactors=F, quote="", comment.char ="") # this overwrites the word list context df above

################## run on aciss
library(BatchJobs)

library(dplyr); library(tidyr); library(doParallel); library(devtools)

starts <- 1:20

batch_function <- function(starts){
  library(dplyr)
  library(tidyr)
  library(devtools)
  
  source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/data_processing_functions.r")
  
  # note that df should have the context columns already (from lists, human coding, or topic modeling, etc.)
  # df <- read.table("contexts_HJ_voting.txt", header=1, sep="\t", stringsAsFactors=F, quote="", comment.char ="")
  df <- read.table("contexs_HJ_prop.txt", header=1, sep="\t", stringsAsFactors=F, quote="", comment.char =""); prop=TRUE
  # df <- read.table("contexts_WL.txt", header=1, sep="\t", stringsAsFactors=F, quote="", comment.char =""); prop=FALSE
  if(nrow(df) == 0) stop("df didn't load")
  
  dict <- read.table("dict_all3_updated.txt", sep="\t", quote="", comment.char ="", header=1, stringsAsFactors=F)
  cols <- ncol(dict)
  if(nrow(dict) == 0) stop("dict didn't load")
  
  iter <- 50 # the number of times to generate random samples
  
  library(doParallel)
  registerDoParallel()
  r <- foreach(1:iter, 
               .errorhandling='remove',
               .combine = rbind, 
               .packages=c("dplyr", "tidyr", "devtools") ) %dopar% par_function(df=df,
                                                                                dict=dict,
                                                                                expand=FALSE,
                                                                                seg.utts=TRUE,
                                                                                TP=FALSE,
                                                                                MI=TRUE, 
                                                                                verbose=FALSE, 
                                                                                prop=prop,
                                                                                cutoff=.85,
                                                                                nontext=TRUE)
}


# create a registry
id <- "bootstrapHJpropContext2"
reg.prop.con2 <- makeRegistry(id = id)

# map function and data to jobs and submit
ids  <- batchMap(reg.prop.con2, batch_function, starts)
done <- submitJobs(reg.prop.con2, resources = list(nodes = 12, walltime=28800)) # expected to run for 8 hours (28800 seconds)

showStatus(reg.prop.con2)

# create a registry
id <- "bootstrapHJpropNontext2"
reg.prop.non2 <- makeRegistry(id = id)

# map function and data to jobs and submit
ids  <- batchMap(reg.prop.non2, batch_function, starts)
done <- submitJobs(reg.prop.non2, resources = list(nodes = 12, walltime=28800)) # expected to run for 8 hours (28800 seconds)

showStatus(reg.prop.non2)

HJnontext.results1 <- process_batch_results(id="bootstrapHJpropNontext", dir="nontexts_humanjudgments")
HJnontext.results2 <- process_batch_results(id="bootstrapHJpropNontext2", dir="nontexts_humanjudgments")
HJnontext.results <- HJnontext.results2 
HJnontext.results$analysis <- "nontext"

HJcontext.results1 <- process_batch_results(id="bootstrapHJpropContext", dir="contexts_humanjudgments")
HJcontext.results2 <- process_batch_results(id="bootstrapHJpropContext1", dir="contexts_humanjudgments")
HJcontext.results <- rbind(HJcontext.results1, HJcontext.results2)
HJcontext.results$analysis <- "context"

HJresults <- rbind(HJcontext.results, HJnontext.results)
HJresults.summary <- HJresults %>%
  gather(key=measure, value=value, recall:precision) %>%
  group_by(analysis, nontext, measure) %>%
  summarize(mean=mean(value), sd=sd(value)) %>%
  rename(context=nontext)
HJresults.summary <- as.data.frame(HJresults.summary)

HJresults.plot <- HJresults %>%
  gather(key=measure, value=value, recall:precision) %>%
  rename(context=nontext)

# histograms
ggplot(HJresults.plot, aes(x=value)) +
  geom_histogram(data=filter(HJresults.plot, analysis=="nontext"), alpha=.5) +
  geom_histogram(data=filter(HJresults.plot, analysis=="context"), aes(x=value, fill=context), alpha=.5) +
  facet_wrap(~ measure + context, scales="free", ncol=12) +
  theme(text = element_text(size=20), axis.ticks = element_blank()) +
  labs(x=NULL, y=NULL)

# barplots
ggplot(HJresults.summary, aes(x=context, y=mean, fill=analysis)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean - sd, ymax=mean + sd),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  facet_wrap(~ measure) +
  theme(text = element_text(size=20), axis.ticks = element_blank()) +
  labs(x=NULL) 

############################################################################
# global results
############################################################################
global.data <- list(N.utterances=nrow(df), streams=NULL, phon.pairs=NULL, unique.phon.pairs=NULL) # storage variable

global.data$streams <- make_streams(df)
global.data$unique.phon.pairs <- calc_MI(phon.pairs=global.data$streams$phon.pairs, phon.stream=global.data$streams$phon.stream)


hist(global.data$unique.phon.pairs$MI, main="Global MI")
# hist(global.data$unique.phon.pairs$MI.1, main="Global MI 1 \nred line shows chance"); abline(v=1, lty=2, col="red")
# hist(global.data$unique.phon.pairs$MI.2, main="Global MI 2\nred line shows chance"); abline(v=1, lty=2, col="red")
hist(global.data$unique.phon.pairs$TP, main="Global TP")

############################################################################
# context results
############################################################################
colnames(df)[which(is.na(colnames(df)) )] <- "none" # replace NA with "none"

context.data <- context_results(df=df, seg.utts=TRUE) # calls make_streams() and calc_MI()

# present results
par(mfrow=c(1,2))
for(k in 1:length(colnames(contexts))){
  hist(context.data[[k]]$unique.phon.pairs$MI, main=paste("Mutual Information,\n", colnames(contexts)[k], "context"), xlim=c(min(global.data$unique.phon.pairs$MI), max(global.data$unique.phon.pairs$MI)))
  
  hist(context.data[[k]]$unique.phon.pairs$TP, main=paste("Transitional Probability,\n", colnames(contexts)[k], "context"), xlim=c(0,1))
}


# how many utterances are in each context corpus?
N.utt <- vector("numeric", length=length(colnames(contexts)))
for(k in 1:length(colnames(contexts))){
  N.utt[k] <- context.data[[k]]$N.utterances
}
names(N.utt) <- colnames(contexts)


#####################################
# segment speech
#####################################
global.data$TP85$seg.phon.stream <- segment_speech(cutoff=.85, 
                                                   stat="TP", 
                                                   data=global.data)

global.data$MI85$seg.phon.stream <- segment_speech(cutoff=.85, 
                                                   stat="MI", 
                                                   data=global.data)

for(k in 1:length(names(context.data))){
  
  message(paste("processing ", names(contexts)[k], "...", sep=""))
      
  context.data[[k]]$TP85$seg.phon.stream <- segment_speech(cutoff=.85, 
                                                           stat="TP", 
                                                           data=context.data[[k]])
  
  context.data[[k]]$MI85$seg.phon.stream <- segment_speech(cutoff=.85, 
                                                           stat="MI", 
                                                           data=context.data[[k]])
  
}

###############################
# assess segmentation
###############################
global.data$TP85$seg.results <- assess_seg(seg.phon.stream=global.data$TP85$seg.phon.stream, words=global.data$streams$words, dict=dict)
colMeans(global.data$TP85$seg.results[,3:4], na.rm=T)

global.data$MI85$seg.results <- assess_seg(seg.phon.stream=global.data$MI85$seg.phon.stream, words=global.data$streams$words, dict=dict)
colMeans(global.data$MI85$seg.results[,3:4], na.rm=T)
saveRDS(global.data, file="results/global_results.rds")

for(k in 1:length(names(context.data))){
  
  message(paste("processing ", names(contexts)[k], "...", sep=""))
  message("TPs...")
      
  context.data[[k]]$TP85$seg.results <- assess_seg(seg.phon.stream=context.data[[k]]$TP85$seg.phon.stream, words=context.data[[k]]$streams$words, dict=dict)
  
  print(colMeans(context.data[[k]]$TP85$seg.results[,3:4], na.rm=T))
  
  message("MIs...")
  context.data[[k]]$MI85$seg.results <- assess_seg(seg.phon.stream=context.data[[k]]$MI85$seg.phon.stream, words=context.data[[k]]$streams$words, dict=dict)
  
  print(colMeans(context.data[[k]]$MI85$seg.results[,3:4], na.rm=T))
}
# saveRDS(context.data, file="results/context_results_WL.rds") # save the word list results
# saveRDS(context.data, file="results/context_results_HJ.rds") # save the human judgment context results
# context.data <- readRDS("results/context_results_WL.rds")
# context.data <- readRDS("results/context_results_HL.rds")

#####################################################
# use batchjobs_script.r to run nontext comparison distributions on ACISS (HPC)
# save resulting dataframe as nontext.results
#####################################################
nontext.results <- process_batch_results(id="bootstrapWL1", dir="nontexts_wordlists")


library(tidyr)
nontext.results <- nontext.results %>%
  mutate(cutoff=85) %>%
  unite(criterion, stat, cutoff, sep="", remove=F) %>%
  gather(measure, value, recall:precision) %>%
  rename(context=nontext)

context.results <- data.frame(context=NULL, criterion=NULL, measure=NULL, context.est=NULL)
for (k in 1:length(names(context.data)) ){
  
  criteria <- c("TP85", "MI85")
  
  for (c in 1:length(criteria)){
    context.est <- colMeans(context.data[[k]][[criteria[c]]]$seg.results[,c(3:4)], na.rm=T)
    estimates <- as.data.frame(context.est)
    estimates$measure <- row.names(estimates)
    estimates$context <- names(context.data)[k]
    estimates$criterion <- criteria[c]
    row.names(estimates) <- NULL
    
    context.results <- rbind(context.results, estimates) # add the results from this context and criterion to the rest
  }
}  

full.results <- left_join(nontext.results, context.results)

# add global estimates
global.results <- data.frame(measure=NA, criterion=NA, global.est=NA)
global.results[1,] <- c("recall",    "TP85", colMeans(global.data$TP85$seg.results[, 3:4], na.rm=T)[1])
global.results[2,] <- c("precision", "TP85", colMeans(global.data$TP85$seg.results[, 3:4], na.rm=T)[2])
global.results[3,] <- c("recall",    "MI85", colMeans(global.data$MI85$seg.results[, 3:4], na.rm=T)[1])
global.results[4,] <- c("precision", "MI85", colMeans(global.data$MI85$seg.results[, 3:4], na.rm=T)[2])
global.results$global.est <- as.numeric(global.results$global.est)

full.results <- left_join(full.results, global.results)
#####################################################
# THE PLOT
#####################################################
ggplot(filter(full.results, criterion=="MI85"), aes(x=context, y=value))+
  # geom_boxplot() +
  geom_boxplot(color=NA, fill=NA, outlier.colour =NA) +
  facet_wrap(~measure) +
  # geom_point(aes(x=context, y=context.est, color=context), size=4, show_guide=F) + 
  # geom_hline(aes(yintercept=global.est), linetype = 2) + 
  theme(text = element_text(size=30), axis.ticks = element_blank(), axis.text.x = element_blank()) +
  labs(x=NULL, y=NULL)

# with histograms instead of boxplots, to examine the nontext distributions (for shape, skew, etc.)
ggplot(filter(full.results, criterion=="MI85"), aes(x=value))+
  geom_histogram() +
  geom_vline(aes(color=context, xintercept=context.est), size=2) + 
  facet_wrap(~ measure + context, scales="free", ncol=7) +
  theme(text = element_text(size=20), axis.ticks = element_blank()) +
  labs(x=NULL, y=NULL)

# bootstrap p-values
full.results$above.est <- ifelse(full.results$value > full.results$context.est, 1, 0)
tests <- full.results %>%
  group_by(context, criterion, measure) %>%
  summarize(p.val = mean(above.est))
tests$stars <- ifelse(tests$p.val < .001, "***",
                      ifelse(tests$p.val < .01, "**",
                             ifelse(tests$p.val < .05, "*", 
                                    ifelse(tests$p.val < .1, "+", ""))))

kable(filter(tests, criterion=="MI85")[,-2], digits=3)

#####################################################

##################################################
# descriptives
descriptives <- results_descriptives(data=global.data, 
                                     context="global",
                                     criterion="MI85")
for(k in 1:length(colnames(contexts))){
  descriptives <- rbind(descriptives, results_descriptives(data=context.data[[k]], 
                                                           context=colnames(contexts[k]), 
                                                           criterion="MI85") )
}

# histograms of MI for each context
for(k in 0:length(names(contexts))){
  
  if(k==0) data <- global.data else data <- context.data[[k]]
  title <- ifelse(k==0, "global", names(contexts)[k])
  
  p <- ggplot(data$unique.phon.pairs, aes(x=MI)) +
    geom_histogram() +
    geom_vline(aes(color="red", xintercept=quantile(data$unique.phon.pairs$MI, .85)[[1]]), size=2) + 
    theme(text = element_text(size=20), axis.ticks = element_blank()) +
    ggtitle(title) + 
    labs(x="mutual information (MI)", y=NULL) +
    xlim(c(-5,15))
  
  ggsave(filename=paste0("plots/descriptives/MIhist_", title, ".png"),
         plot=p, 
         units="in", 
         width=4, 
         height=4)
}



##################################################
# NOTE: Swingley 2005 only considers a "word" segmented if it has BOTH high within-unit MI and also high frequency as a unit.

plot.data.con <- data.frame(context=names(context.data), TP85recall.mean=NA, TP85precision.mean=NA, MI85recall.mean=NA, MI85precision.mean=NA, TP85recall.sd=NA, TP85precision.sd=NA, MI85recall.sd=NA, MI85precision.sd=NA, N.utt=NA, group="context")
for(k in 1:length(names(contexts))){
  plot.data.con[k, 2:3] <- colMeans(context.data[[k]]$TP85$seg.results[,4:5], na.rm=T)
  plot.data.con[k, 4:5] <- colMeans(context.data[[k]]$MI85$seg.results[,4:5], na.rm=T)
  plot.data.con$N.utt[k] <- context.data[[k]]$N.utterances
}


plot.data.non <- bootstrap.summary
plot.data.non$N.utt <- plot.data.con$N.utt
plot.data.non$group <- "nontext"

plot.data <- rbind(plot.data.con, plot.data.non)
plot.data$group <- as.factor(plot.data$group)

plot.data <- plot.data %>%
  tidyr::gather(variable, value, TP85recall.mean:MI85precision.sd) %>%
  tidyr::extract(col=variable, into=c("criterion", "variable", "stat"), regex="([A-Z]{2}[0-9]{2})([a-z]+)[.]([a-z]+)") %>%
  tidyr::spread(stat, value)
plot.data$criterion <- as.factor(plot.data$criterion)
plot.data$variable <- as.factor(plot.data$variable)

#####################################################
# Is accuracy just driven by number of utterances?

ggplot(plot.data, aes(x=N.utt, y=mean, group=group)) +
  geom_point(aes(color=context, shape=group), size=4 )+
  geom_line(stat="smooth", method="lm", aes(linetype=group)) +
  facet_wrap(~variable + criterion)

# exclude routines
ggplot(subset(plot.data, context != "routines"), aes(x=N.utt, y=mean, group=group)) +
  geom_point(aes(color=context, shape=group), size=4 )+
  geom_line(stat="smooth", method="lm", aes(linetype=group)) +
  facet_wrap(~variable + criterion) +
  ggtitle("Excluding routines context and corresponding nontext")

summary(lm(mean ~ group*N.utt*criterion, data=plot.data))

#####################################################
# Do context vs. nontexts differ in accuracy?

ggplot(plot.data, aes(x=group, y=mean, fill=context)) +
  geom_bar(aes(), stat="identity", position="dodge")+
  geom_errorbar(aes(ymax = mean + sd, ymin=mean - sd), width=.3, position=position_dodge(.9)) +
  facet_wrap(~ variable + criterion)

ggplot(plot.data, aes(x=context, y=mean, fill=group)) +
  geom_bar(aes(), stat="identity", position="dodge")+
  geom_errorbar(aes(ymax = mean + sd, ymin=mean - sd), width=.3, position=position_dodge(.9)) +
  facet_wrap(~ variable + criterion) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Error bars are +-SD", x=NULL, y=NULL)

ggplot(plot.data, aes(x=criterion:variable, y=mean, fill=group)) +
  geom_bar(aes(), stat="identity", position="dodge")+
  geom_errorbar(aes(ymax = mean + sd, ymin=mean - sd), width=.3, position=position_dodge(.9)) +
  facet_wrap(~ context, ncol=4) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Error bars are +-SD", x=NULL, y=NULL)


#####################################################
# Segmentation sucess by frequency
plot_seg_results(global.data$TP85$seg.results, "global TP85")
plot_seg_results(global.data$MI85$seg.results, "global MI85")

for(k in 1:length(colnames(contexts))){
  TPplot <- plot_seg_results(context.data[[k]]$TP85$seg.results, title=paste(colnames(contexts)[k], "TP85"))
  MIplot <- plot_seg_results(context.data[[k]]$MI85$seg.results, title=paste(colnames(contexts)[k], "MI85"))
  #print(TPplot)
  print(MIplot)
}



#####################################################
# is it segmenting seed words correctly?
global.data$TP85$check_seed_words <- check_seed_words(global.data$TP85$seg.results)
summary(global.data$TP85$check_seed_words$seg.result)
plot_seg_results(seg.results=global.data$TP85$check_seed_words, title="Segmentation accuracy for seed words\nTP85", boxplot=TRUE, scatterplot=TRUE, by="contexts")
plot_seg_results(seg.results=global.data$TP85$check_seed_words, title="Segmentation accuracy for seed words\nTP85", boxplot=TRUE, scatterplot=TRUE, by="syl")

global.data$MI85$check_seed_words <- check_seed_words(global.data$MI85$seg.results)
summary(global.data$MI85$check_seed_words$seg.result)
plot_seg_results(seg.results=global.data$MI85$check_seed_words, title="Segmentation accuracy for seed words\nMI85", boxplot=TRUE, scatterplot=TRUE, by="contexts")
plot_seg_results(seg.results=global.data$MI85$check_seed_words, title="Segmentation accuracy for seed words\nMI85", boxplot=TRUE, scatterplot=TRUE, by="syl")

for(k in 1:length(colnames(contexts))){
  context.data[[k]]$TP85$check_seed_words <- check_seed_words(context.data[[k]]$TP85$seg.results)
  summary(context.data[[k]]$TP85$check_seed_words$seg.result)
  TPplot<- plot_seg_results(seg.results=context.data[[k]]$TP85$check_seed_words, title=paste("Segmentation accuracy for seed words\nTP85", colnames(contexts)[k]), boxplot=TRUE, scatterplot=TRUE, by="contexts")
  
  context.data[[k]]$MI85$check_seed_words <- check_seed_words(context.data[[k]]$MI85$seg.results)
  summary(context.data[[k]]$MI85$check_seed_words$seg.result)
  MIplot <- plot_seg_results(seg.results=context.data[[k]]$MI85$check_seed_words, title=paste("Segmentation accuracy for seed words\n", colnames(contexts)[k], "context"), boxplot=TRUE, scatterplot=TRUE, by="contexts")
  #print(TPplot)
  print(MIplot)
}


gmodels::CrossTable(global.data$MI85$seg.results[,6], global.data$MI85$seg.results$syl.bins, prop.r=T, prop.c=F, prop.chisq=F)
seg.by.syl <- table(global.data$MI85$seg.results[,c(6, 10)])
syl.by.seg <- table(global.data$MI85$seg.results[,c(10, 6)])
plot(table(global.data$MI85$seg.results[,c(6, 10)]))
barplot(seg.by.syl, beside=T, legend=rownames(seg.by.syl), xlab="syllables")
barplot(seg.by.syl, beside=F, legend=rownames(seg.by.syl), xlab="syllables")
barplot(syl.by.seg, beside=T, legend=rownames(syl.by.seg), xlab=NULL)
barplot(syl.by.seg, beside=F, legend=rownames(syl.by.seg), xlab=NULL)

combined.seg.results <- context.data[[1]]$MI85$seg.results
combined.seg.results$context.corpus <- colnames(contexts)[1]
for(k in 2:length(colnames(contexts))){
  seg.results <- context.data[[k]]$MI85$seg.results
  seg.results$context.corpus <- colnames(contexts)[k]
  combined.seg.results <- rbind(combined.seg.results,seg.results)
}

combined.seg.results$context.corpus <- as.factor(combined.seg.results$context.corpus)

bottoms <- grepl("bottom", df$orth)
bottom.corpus <- df[bottoms,]
bottom.corpus[,c(1,6,7)]

bellys <- grepl("belly", df$orth)
belly.corpus <- df[bellys,]
belly.corpus[, c(1, 3:9)]
belly.seg <- grepl("'bE-lI", combined.seg.results$phon, fixed=TRUE)
belly.seg <- combined.seg.results[belly.seg,]

big.kiss <- df[grepl("'bIg 'kIs", df$phon, fixed=T),]; nrow(big.kiss)
big <- df[grepl("'bIg", df$phon, fixed=T),]; nrow(big)
kiss <- df[grepl("'kIs", df$phon, fixed=T),]; nrow(kiss)

arrange(filter(context.data$body.touch$unique.phon.pairs, syl1=="'bIg"), -MI)
arrange(filter(global.data$unique.phon.pairs, syl1=="'bIg"), -MI)
arrange(filter(context.data$body.touch$unique.phon.pairs, syl2=="'f{t"), -MI)
arrange(filter(global.data$unique.phon.pairs, syl2=="'f{t"), -MI)
arrange(filter(context.data$body.touch$unique.phon.pairs, syl1=="'tV"), -MI)
arrange(filter(context.data$body.touch$unique.phon.pairs, syl2=="mI"), -MI)
arrange(filter(global.data$unique.phon.pairs, syl1=="'tV"), -MI)
arrange(filter(global.data$unique.phon.pairs, syl2=="mI"), -MI)
head(arrange(context.data$body.touch$unique.phon.pairs, -freq), 40)

length(which(global.data$streams$phon.pairs$syl1=="'tV"))
length(which(context.data$body.touch$streams$phon.pairs$syl1=="'tV"))

filter(context.data$body.touch$unique.phon.pairs, syl2=="'kIs")
filter(global.data$unique.phon.pairs, syl2=="'kIs")

combined.seg.results[grepl("'kIs", combined.seg.results$phon),] # kiss

combined.seg.results[grepl("'tI-kP", combined.seg.results$phon),c(1,2,7,13)] # tickle
combined.seg.results[grepl("kP", combined.seg.results$phon),c(1,2,7,9,13)] # 'ckle (end of "tickle", "chuckle", "typical")
global.data$MI85$seg.results[grepl("'tI-kP", global.data$MI85$seg.results$phon, fixed=T),] # tickle

combined.seg.results[grepl("'f{t", combined.seg.results$phon, fixed=T),] # fat
combined.seg.results[grepl("'bIg", combined.seg.results$phon, fixed=T),] # big
combined.seg.results[grepl("'bIg-'f{t", combined.seg.results$phon, fixed=T),] # big fat
global.data$MI85$seg.results[grepl("'f{t", global.data$MI85$seg.results$phon, fixed=T),] # fat
global.data$MI85$seg.results[grepl("'bIg", global.data$MI85$seg.results$phon, fixed=T),] # big
global.data$MI85$seg.results[grepl("'tV", global.data$MI85$seg.results$phon, fixed=T),] # tu- (mmy)
df[grepl("'f{t", df$phon, fixed=T),c(1,7,9)] # fat
df[grepl("'bIg 'f{t 'tV mI", df$phon, fixed=T),c(1,7,9)] # big fat tummy
df[grepl("'bIg 'f{t", df$phon, fixed=T),c(1,7,9)] # big fat
df[grepl("'bIg 'kIs", df$phon, fixed=T),c(1,7,9)]# big kiss
df[grepl("'bIg", df$phon, fixed=T),c(1,7,9)] # big
df[grepl("'kIs", df$phon, fixed=T),c(1,7,9)]# kiss
global.data$MI85$seg.results[grepl("^@$", global.data$MI85$seg.results$phon),] # a
combined.seg.results[grepl("^@$", combined.seg.results$phon),] # a

# global.data$MI85$seg.results[grepl("'fV", global.data$MI85$seg.results$phon, fixed=T),] # fun (-nny)
# combined.seg.results[grepl("'h{-pI", combined.seg.results$phon, fixed=T),] # happy
# combined.seg.results[grepl("'kr2-IN", combined.seg.results$phon, fixed=T),] # crying
# combined.seg.results[grepl("'bQ-tP", combined.seg.results$phon, fixed=T),] # bottle
# combined.seg.results[grepl("'sI-lI", combined.seg.results$phon, fixed=T),]# 'sI-lI
# combined.seg.results[grepl("'bE-lI", combined.seg.results$phon, fixed=T),] # belly
# combined.seg.results[grepl("'bju-t@-fUl", combined.seg.results$phon, fixed=T),] # beautiful
# combined.seg.results[grepl("'wIn-dI", combined.seg.results$phon, fixed=T),] # windy
# combined.seg.results[grepl("'mV-mI", combined.seg.results$phon, fixed=T),] # mummy
# combined.seg.results[grepl("'s5-pI", combined.seg.results$phon, fixed=T),] # splashing
combined.seg.results[grepl("'spl{-SIN", combined.seg.results$phon, fixed=T),] # splashing
combined.seg.results[grepl("'spl{", combined.seg.results$phon, fixed=T),] # spla-
global.data$MI85$seg.results[grepl("'spl{-SIN", global.data$MI85$seg.results$phon, fixed=T),] # splashing
global.data$MI85$seg.results[grepl("'spl{", global.data$MI85$seg.results$phon, fixed=T),] # spla-
global.data$MI85$seg.results[grepl("SIN", global.data$MI85$seg.results$phon, fixed=T),] # -shing
global.data$unique.phon.pairs[grepl("'spl{", global.data$unique.phon.pairs$syl1, fixed=T) ,] # spla-
global.data$unique.phon.pairs[grepl("SIN", global.data$unique.phon.pairs$syl2, fixed=T) ,] # -shing

head(arrange(filter(global.data$MI85$seg.results, seg.result=="miss"), -freq), 100)
head(arrange(filter(global.data$MI85$seg.results, seg.result=="miss" & freq>2), freq), 100)

high.freq.fails <- arrange(filter(combined.seg.results, seg.result!="hit"), -freq) # high freq fails
df[grepl("'mV mI", df$phon, fixed=T),1:2] # mummy
df[grepl("'lI", df$phon, fixed=T),1:2] # li' (e.g., li-cking, little, li-sten)
df[grepl("'kV ", df$phon, fixed=T),1:2] # kuh (cu-ddle, co-ming, cou-ple, )
df[grepl("'I ", df$phon, fixed=T),1:2] # i (i-sn't, i-t'll, )
df[grepl("'d7 ", df$phon, fixed=T),1:2] # dear
filter(global.data$unique.phon.pairs, syl1=="'lI") # global data
filter(context.data$mealtime$unique.phon.pairs, syl1=="'lI") # global data

unique(global.data$streams$orth.stream[-which(global.data$streams$orth.stream %in% dict$word)])

library(igraph)
df.big <- df[grepl("'bIg", df$phon, fixed=T),]
streams.big <- make_streams(df.big)
plot(graph.edgelist(as.matrix(streams.big$phon.pairs), directed=F))

df.big.sample <- df.big[sample(1:nrow(df.big), 15),]
streams.big.sample <- make_streams(df.big.sample)
plot(graph.edgelist(as.matrix(streams.big.sample$phon.pairs), directed=F), main="overall\nsample with big")

df.big.body.touch <- df[grepl("'bIg", df$phon, fixed=T) & df$body.touch>0,]
df.big.body.touch.sample <- df.big.body.touch[sample(1:nrow(df.big.body.touch), 15),]
streams.big.body.touch.sample <- make_streams(df.big.body.touch.sample)
plot(graph.edgelist(as.matrix(streams.big.body.touch.sample$phon.pairs), directed=F), main="body.touch\nsample with big")

df.body.touch <- df[df$body.touch>0,]
df.body.touch.sample <- df.body.touch[sample(1:nrow(df.body.touch), 22),]
streams.df.body.touch.sample <- make_streams(df.body.touch.sample)
plot(graph.edgelist(as.matrix(streams.df.body.touch.sample$phon.pairs), directed=F), main="body.touch\nrandom sample")

df.sample <- df[sample(1:nrow(df), 22),]
streams.sample <- make_streams(df.sample)
plot(graph.edgelist(as.matrix(streams.sample$phon.pairs), directed=F), main="overall\nrandom sample")

body.touch.edge.list <- context.data$body.touch$streams$phon.pairs[grepl("'bIg", context.data$body.touch$streams$phon.pairs[,1], fixed=T),]
plot(graph.edgelist(as.matrix(body.touch.edge.list), directed=F))

overall.edge.list <- global.data$streams$phon.pairs[grepl("'bIg", global.data$streams$phon.pairs[,1], fixed=T),]
plot(graph.edgelist(as.matrix(overall.edge.list)))

routines.edge.list <- context.data$routines$streams$phon.pairs[grepl("'bIg", context.data$routines$streams$phon.pairs[,1], fixed=T),]
plot(graph.edgelist(as.matrix(routines.edge.list)))

network_plot(global.data, "global.data")
network_plot(context.data$body.touch, "body.touch")

library(GGally)
global.network <- network::network(as.matrix(global.data$streams$phon.pairs))
ggnet(global.network,  title="overall\nwhole corpus", weight.method="degree")

body.touch.network <- network::network(as.matrix(context.data$body.touch$streams$phon.pairs))
ggnet(body.touch.network, main="body.touch\nwhole corpus", weight.method="degree")


el <- graph.data.frame(global.data$unique.phon.pairs)
plot(el,layout=layout.fruchterman.reingold, edge.width=E(el)$width/2)
# line node size should be syllable freq, weights of edges should be 

ggplot(combined.seg.results, aes(x=seg.result, y=freq.segd)) +
  facet_wrap(~context.corpus, ncol=7) +
  geom_boxplot() +
  scale_y_log10() + labs(y="Log10(frequency)", x=NULL, title=NULL) + 
  scale_x_discrete(limits=c("false alarm", "hit")) +
  coord_flip() +
  theme(text = element_text(size=30))
ggplot(combined.seg.results, aes(x=seg.result, y=freq.orth)) +
  facet_wrap(~context.corpus, ncol=7) +
  geom_boxplot() +
  scale_y_log10() + labs(y="Log10(frequency)", x=NULL, title=NULL) + 
  scale_x_discrete(limits=c("miss", "hit")) +
  coord_flip() +
  theme(text = element_text(size=30))

combined.check_seed_words <- context.data[[1]]$MI85$check_seed_words
combined.check_seed_words$context.corpus <- colnames(contexts)[1]
for(k in 2:length(colnames(contexts))){
  check_seed_words <- context.data[[k]]$MI85$check_seed_words
  check_seed_words$context.corpus <- colnames(contexts)[k]
  combined.check_seed_words <- rbind(combined.check_seed_words,check_seed_words)
}

combined.check_seed_words$context.corpus <- as.factor(combined.check_seed_words$context.corpus)

ggplot(combined.check_seed_words, aes(x=seg.result, y=freq)) +
  facet_wrap(~context.corpus, ncol=4) +
  geom_boxplot() +
  geom_point(aes(color=context), alpha=.7, size=4, position = position_jitter(w = .3, h = 0)) +
  scale_y_log10() + labs(y="Log10(frequency)", x=NULL, title=title) + 
  coord_flip()

library(knitr)
N.utt
tests <- vector("list", length(names(contexts))) # storage variable
names(tests) <- colnames(contexts)

for(k in 1:length(names(contexts))){
  message(paste("processing ", names(contexts)[k], "...", sep=""))
  
  tests[[k]]$context.pairs.N <- nrow(context.data[[k]]$unique.phon.pairs)
  tests[[k]]$nontext.pairs.N <- nrow(nontext.data[[k]]$unique.phon.pairs)
  
  tests[[k]]$test.MI <- ks.test(context.data[[k]]$unique.phon.pairs$MI, nontext.data[[k]]$unique.phon.pairs$MI)
  tests[[k]]$test.TP <-ks.test(context.data[[k]]$unique.phon.pairs$TP, nontext.data[[k]]$unique.phon.pairs$TP)
}

tests

# # reformat for printing as table
tests.table <- data.frame(N.utt=N.utt , TP.KSstat = NA, TP.KSpval=NA, MI.KSstat=NA, MI.KSpval=NA)
  for(k in 1:length(names(context.data))){
    tests.table[k,2:5] <- round(c(tests[[k]]$test.TP$statistic, tests[[k]]$test.TP$p.value, tests[[k]]$test.MI$statistic, tests[[k]]$test.MI$p.value), 3)
  }
tests.table$TP.KSpval <- ifelse(tests.table$TP.KSpval==0, ">.001", tests.table$TP.KSpval)
tests.table$MI.KSpval <- ifelse(tests.table$MI.KSpval==0, ">.001", tests.table$MI.KSpval)
knitr::kable(tests.table)
x


# reformat data for plotting
plot.data.wide <- data.frame(context=NULL, sample=NULL, MI=NULL, TP=NULL)
for(k in 1:length(colnames(contexts))){
  con <- data.frame(context=colnames(contexts)[k], sample="context", MI=context.data[[k]]$unique.phon.pairs$MI, TP=context.data[[k]]$unique.phon.pairs$TP)
  non <- data.frame(context=colnames(contexts)[k], sample="nontext", MI=nontext.data[[k]]$unique.phon.pairs$MI, TP=nontext.data[[k]]$unique.phon.pairs$TP)
  
 plot.data.wide <- rbind(plot.data.wide, con, non)
 
}
plot.data <- melt(plot.data.wide)

ggplot(subset(plot.data, variable=="MI"), aes(x=value, fill=sample)) + 
  geom_histogram(aes(y = ..density..), position="identity", alpha=.5) +
  facet_wrap( ~ context , scales="free") +
  theme_bw() +
  labs(title="Mutual Information")

ggplot(subset(plot.data, variable=="TP"), aes(x=value, fill=sample)) + 
  geom_histogram(aes(y = ..density..), position="identity", alpha=.5) +
  facet_wrap( ~ context , scales="free") +
  theme_bw() +
  labs(title="Transitional Probabilities")

# dist of syllables
par(mfrow=c(1,1))
for(k in 1:length(colnames(contexts))){
  syl.freq <- table(context.data[[k]]$streams$phon.stream)
  hist(syl.freq, main=paste(colnames(contexts)[k], "syllable freq"))
}

for(k in 1:length(names(nontext.data))){
  syl.freq <- table(nontext.data[[k]]$streams$phon.stream)
  hist(syl.freq, main=paste(names(nontext.data)[k], " syllable freq (", colnames(contexts)[k], ")", sep=""))
}



freq.bigrams <- summarise(group_by(global.data$streams$phon.pairs, syl1, syl2), count=n()) # frequency of bigrams
freq.words <- table(global.data$streams$orth.stream) # frequency of words
freq.syl <- table(global.data$streams$phon.stream) # frequency of syllables

# freq plots
barplot(sort(freq.words, decreasing=TRUE)) 
barplot(sort(freq.words, decreasing=TRUE)[1:100]) # top 100 words only 
barplot(sort(freq.syl, decreasing=TRUE))
barplot(sort(freq.syl, decreasing=TRUE)[1:100]) # top 100 syllables only 

freq.words.df <- as.data.frame(freq.words)

# merge frequency with dictionary information (number of syllables, etc.)
colnames(freq.words.df)[1] <- "word"
freq.words.df <- merge(x=freq.words.df, y=dict, all.x=TRUE)

# present results

plot(Freq ~ jitter(N.syl), data = freq.words.df, main="Frequency of words \nby number of syllables")
plot(Freq ~ jitter(N.syl), data = filter(freq.words.df, Freq<2000), main="Frequency of words \nby number of syllables \n(outlier removed)") # remove outlier

ggplot(freq.words.df, aes(y=Freq, x=jitter(N.syl))) +
  geom_point(aes(alpha=.5)) +
  facet_wrap(~context)

freq.summary <- summarise(group_by(freq.words.df, Syllables=as.factor(N.syl)), Mean.Freq=mean(Freq), sd=sd(Freq), n=n(), se=sd/sqrt(n))
ggplot(freq.summary, aes(x=Syllables, y=Mean.Freq))+
  geom_bar(stat="identity", position=position_dodge(.9)) +
  geom_errorbar(aes(ymax = Mean.Freq + se, ymin=Mean.Freq - se), width=.3, position=position_dodge(.9)) +
  labs(title="Frequency by number of syllables\n(Error bars +-SE)")


# contexts and ambiguity?
context.syl.tokens <- list(NULL)
# number of syllables per utterance in contexts vs. nontexts?
for(k in 1:length(names(context.data))){
  context.syl.tokens[k] <- length(context.data[[k]]$streams$phon.stream)
}
nontext.syl.tokens <- list(NULL)
# number of syllables per utterance in contexts vs. nontexts?
for(k in 1:length(names(nontext.data))){
  nontext.syl.tokens[k] <- length(nontext.data[[k]]$streams$phon.stream)
}

names(context.syl.tokens) <- names(context.data)
names(nontext.syl.tokens) <- names(context.data)

N.utt

df$utterance <- row.names(df)
df$y <- 1

par(mfrow=c(1,1))

fifth <- floor(nrow(df)/5)
plot.data <- df[1:100, ]
ggplot(plot.data, aes(x=utterance, y=y, color=context)) + 
  theme_bw() +
  geom_point()


filter(context.data$mealtime$unique.phon.pairs, syl1=="'mIlk") 
filter(nontext.data[[1]]$unique.phon.pairs, syl1=="'mIlk") 
