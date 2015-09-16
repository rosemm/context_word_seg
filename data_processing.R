rm(list = ls()) # clear the environment
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(knitr)

# read in the functions written for this analysis
source("/Users/TARDIS/Documents/STUDIES/TPs/data_processing_functions.r")
setwd("/Users/TARDIS/Documents/STUDIES/TPs")

contexts <- read.csv("/Users/TARDIS/Documents/STUDIES/TPs/words by contexts.csv")

orth <- readLines("eng_korman_from_swingley2005/allEd11")
phon <- readLines("eng_korman_from_swingley2005/syl_ko_6b") # note that "dear" is incorrectly entered as "'d7" here, and it should be "'d7R" according to the dict file.


df <- data.frame(orth=orth, phon=phon, stringsAsFactors=F)

# clean up
df$orth <- sapply(strsplit(df$orth, "\t"), '[', 2) # split it at the tab and only keep the second part 
df$orth <- substr(df$orth, start=1, stop=nchar(df$orth)-2) # remove the last two characters (space and punctuation)
df$phon <- sapply(strsplit(df$phon, ": "), '[', 2) # split it at ": " and only keep the second part


#####################
# tidy bigrams in orthographic stream
#####################
df$orth <- gsub(pattern="oh dear", replacement="oh_dear", df$orth, fixed=TRUE)
df$orth <- gsub(pattern="all gone", replacement="all_gone", df$orth, fixed=TRUE)
df$orth <- gsub(pattern="thank you", replacement="thank_you", df$orth, fixed=TRUE)
df$orth <- gsub(pattern="uh oh", replacement="uh_oh", df$orth, fixed=TRUE)
df$orth <- gsub(pattern="patty cake", replacement="patty_cake", df$orth, fixed=TRUE)

#####################
# correct "dear" mistake
#####################
grep("dear", df$orth, value=T)
df$phon <- gsub(pattern="'d7$", replacement="'d7R", df$phon) # when 'd7 occurs utterance-final, replace it with 'd7R (that's definitely "dear" and not "dearie" or "dearier")
df$phon <- gsub(pattern="'d7 ([^r])", replacement="'d7R \\1", df$phon) # when 'd7 occurs and is NOT followed by an r after a space, replace it with 'd7R
df$phon <- gsub(pattern="'d7 ([^r])", replacement="'d7R \\1", df$phon) # need to run this twice because of overlapping hits with "dear dear"

unique(df[grep("dear", df$orth),]) # check all utterances with "dear" in them

dict <- read.table("/Users/TARDIS/Documents/STUDIES/TPs/eng_korman_from_swingley2005/dict_all3.txt", sep=" ", quote="", comment.char ="")
colnames(dict) <- c("word", "phon")

grep("dear", dict$word, value=T)
dict[grep("dear", dict$word),]
dict$phon <- as.character(dict$phon)
dict[which(dict$word=="dear"),]$phon <- "'d7" # doesn't work

# add number of syllables for each word
dict$N.syl <- rep(NA, nrow(dict))
for(i in 1:nrow(dict)){
  dict$N.syl[i] <- length(strsplit(as.character(dict$phon[i]), split="-", fixed=TRUE)[[1]])
  }

# add frequency for each word
dict$freq.orth <- rep(NA, nrow(dict))
for(i in 1:nrow(dict)){
  dict$freq.orth[i] <- length(grep(paste("^",dict$word[i],"$", sep=""), x=strsplit(paste(df$orth, collapse=" "), split=" ", fixed=T)[[1]]))
  }


# add defined context for each word
cols <- ncol(dict)
for(i in 1:length(colnames(contexts))){
  
  dict <- cbind(dict, as.numeric(rep(NA, nrow(dict)))) # add a column of NAs to dict
  colnames(dict)[cols+i] <- colnames(contexts)[i] # name that column after the current context
  
  key.words <- as.vector(contexts[,i][contexts[,i] != ""]) # drop empty cells from context columns to save just a vector of key words for this context
  
  dict[,cols+i] <- ifelse(dict$word %in% key.words, 1, 0) # if word occurs in the key.words, then mark 1 for this column, otherwise 0
  }

# classify dictionary words by context
context.columns <- (cols+1):(cols+length(colnames(contexts)))
dict$sum <- rowSums(dict[,context.columns]) # identify words that are key words from more than one context (ambiguous) - there should be none of these for mutually exclusive lists.

dict$context <- ifelse(dict$sum == 1, colnames(contexts)[apply(dict[,context.columns], 1, which.max)], 
                     ifelse(dict$sum > 1, "ambiguous", 
                            ifelse(dict$sum == 0, "none", NA)))
dict$context <- as.factor(dict$context)
summary(dict$context)


# code context for each utterance
p <- progress_estimated(n=length(colnames(contexts))) # print progress bar while working
message("coding context for each utterance")
for(k in 1:length(colnames(contexts))){
  df[[colnames(contexts)[k]]] <- NA # add a column of NAs to df, name that column after the current context
  
# check each utterance for each context
  key.words <- as.vector(contexts[,k][contexts[,k] != ""]) # drop empty cells from context columns to save just a vector of key words for this context

  for(j in 1:nrow(df)){
     df[[colnames(contexts)[k]]][j] <- ifelse(any(strsplit(df$orth[j], " ")[[1]] %in% key.words), 1, 0)
  }
print(p$tick()) # advance progress bar
}


#########################################################
# expand context windows to utterance with key word +-2 utterances
#########################################################
df <- expand_windows(df) 

# classify utterances by context (ignore nontext columns)
context.columns <- (3):(2+length(colnames(contexts)))
df$sum <- rowSums(df[,context.columns]) # identify utterances with key words from more than one context (ambiguous)

df$context <- ifelse(df$sum > 1.5, "ambiguous", 
                     ifelse(df$sum > 0, colnames(contexts)[apply(df[,context.columns], 1, which.max)], 
                            ifelse(df$sum == 0, "none", NA)))
df$context <- as.factor(df$context)
summary(df$context)

ambiguous.utterances <- filter(df,context=="ambiguous")
# View(ambiguous.utterances)

############################################################################
# global results
############################################################################
global.data <- list(N.utterances=nrow(df), streams=NULL, phon.pairs=NULL, unique.phon.pairs=NULL) # storage variable

global.data$streams <- make_streams(df)
global.data$unique.phon.pairs <- calc_MI(phon.pairs=global.data$streams$phon.pairs, phon.stream=global.data$streams$phon.stream)

hist(global.data$unique.phon.pairs$MI, xlim=c(-5,15), main="Global MI")
hist(global.data$unique.phon.pairs$TP, main="Global TP")

############################################################################
# context results
############################################################################
# get context results
context.data <- context_results(contexts=contexts, df=df) # calls make_streams() and calc_MI()

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

# 
# # present results
# par(mfrow=c(1,2))
# for(k in 1:length(names(nontext.data))){
#   
#   hist(nontext.data[[k]]$unique.phon.pairs$MI, main=paste("Mutual Information,\n", names(nontext.data)[k], "nontext"), xlim=c(min(global.data$unique.phon.pairs$MI), max(global.data$unique.phon.pairs$MI)))
#   
#   hist(nontext.data[[k]]$unique.phon.pairs$TP, main=paste("Transitional Probability,\n", names(nontext.data)[k], "nontext"), xlim=c(0,1))
# }


#####################
# only keep context words that actually occur in the corpus
#####################
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


#####################################
# segment speech
#####################################
global.data$TP85$seg.phon.stream <- segment_speech(cutoff=.85, stat="TP", global.data$unique.phon.pairs, global.data$streams$phon.stream)

global.data$MI85$seg.phon.stream <- segment_speech(cutoff=.85, stat="MI", global.data$unique.phon.pairs, global.data$streams$phon.stream)

for(k in 1:length(names(context.data))){
  
  message(paste("processing ", names(contexts)[k], "...", sep=""))
      
  context.data[[k]]$TP85$seg.phon.stream <- segment_speech(cutoff=.85, stat="TP", context.data[[k]]$unique.phon.pairs, context.data[[k]]$streams$phon.stream)
  
  context.data[[k]]$MI85$seg.phon.stream <- segment_speech(cutoff=.85, stat="MI", context.data[[k]]$unique.phon.pairs, context.data[[k]]$streams$phon.stream)
  
}


###############################
# assess segmentation
###############################
global.data$TP85$seg.results <- assess_seg(seg.phon.stream=global.data$TP85$seg.phon.stream, words=global.data$streams$words, dict=dict)
colMeans(global.data$TP85$seg.results[,4:5], na.rm=T)

global.data$MI85$seg.results <- assess_seg(seg.phon.stream=global.data$MI85$seg.phon.stream, words=global.data$streams$words, dict=dict)
colMeans(global.data$MI85$seg.results[,4:5], na.rm=T)


for(k in 1:length(names(context.data))){
  
  message(paste("processing ", names(contexts)[k], "...", sep=""))
  message("TPs...")
      
  context.data[[k]]$TP85$seg.results <- assess_seg(seg.phon.stream=context.data[[k]]$TP85$seg.phon.stream, words=context.data[[k]]$streams$words, dict=dict)
  
  print(colMeans(context.data[[k]]$TP85$seg.results[,4:5], na.rm=T))
  
  message("MIs...")
  context.data[[k]]$MI85$seg.results <- assess_seg(seg.phon.stream=context.data[[k]]$MI85$seg.phon.stream, words=context.data[[k]]$streams$words, dict=dict)
  
  print(colMeans(context.data[[k]]$MI85$seg.results[,4:5], na.rm=T))
}


go <- Sys.time()

iter <- 30 # the number of times to generate random nontexts

# storage variable
bootstrap.results <- vector("list", length(colnames(contexts)))  
for(k in 1:length(colnames(contexts))){
  bootstrap.results[[k]] <- matrix(ncol=iter, nrow=4, dimnames=list(c("TP85recall", "TP85precision", "MI85recall", "MI85precision")))
}
names(bootstrap.results) <- colnames(contexts)

for(i in 1:iter){
  message(paste("*********************\nIteration", i, "...\n*********************"))  
  
  # pick nontexts
  results <- nontext_cols(df=df, context_names=colnames(contexts)) # add the nontext col
  non <- results[[1]]
  nontexts <- results[[2]]
  
  # add nontext columns to dataframe
  df.non <- cbind(df, non)
  names(nontexts) <- paste("non.", colnames(contexts), sep="")
  
  # expand windows to + - 2 utterances before and after
  df.non <- expand_windows(df.non)
  
  # calculate MIs and TPs
  nontext.data <- context_results(contexts=nontexts, df=df.non) # calls make_streams() and calc_MI()
  
  # segment speech
  for(k in 1:length(names(nontext.data))){
    
    message(paste("Segment speech! Processing ", names(nontexts)[k], "...", sep=""))
    
    nontext.data[[k]]$TP85$seg.phon.stream <- segment_speech(cutoff=.85, stat="TP", nontext.data[[k]]$unique.phon.pairs, nontext.data[[k]]$streams$phon.stream)
    
    nontext.data[[k]]$MI85$seg.phon.stream <- segment_speech(cutoff=.85, stat="MI", nontext.data[[k]]$unique.phon.pairs, nontext.data[[k]]$streams$phon.stream)
  }
  
  # assess segmentation
  for(k in 1:length(names(nontext.data))){
    
    message(paste("processing ", names(nontexts)[k], "...", sep=""))
    message("TPs...")
    
    nontext.data[[k]]$TP85$seg.results <- assess_seg(seg.phon.stream=nontext.data[[k]]$TP85$seg.phon.stream, words=nontext.data[[k]]$streams$words, dict=dict)
    
    TPresults <- colMeans(nontext.data[[k]]$TP85$seg.results[,4:5], na.rm=T)
    
    message("MIs...")
    nontext.data[[k]]$MI85$seg.results <- assess_seg(seg.phon.stream=nontext.data[[k]]$MI85$seg.phon.stream, words=nontext.data[[k]]$streams$words, dict=dict)
    
    MIresults <- colMeans(nontext.data[[k]]$MI85$seg.results[,4:5], na.rm=T)
    
    
    bootstrap.results[[k]][1,i] <- TPresults[1]
    bootstrap.results[[k]][2,i] <- TPresults[2]
    bootstrap.results[[k]][3,i] <- MIresults[1]
    bootstrap.results[[k]][4,i] <- MIresults[2]
  }
}

record_bootstrap_results(bootstrap.results, wd="./bootstrap_output/") # saves date-stamped files with the bootstrapping results, so I don't lose them :)

message("How long did that take?")
Sys.time() - go

#bootstrap.results <- read_bootstrap_results(iter=60, contexts=contexts, wd="./bootstrap_output/")


bootstrap.summary <- data.frame(context=rep(NA, length(colnames(contexts))), TP85recall.mean=NA, TP85precision.mean=NA, TP85recall.sd=NA, TP85precision.sd=NA, MI85recall.mean=NA, MI85precision.mean=NA, MI85recall.sd=NA, MI85precision.sd=NA) # storage variable

for(k in 1:length(colnames(contexts))){
  bootstrap.summary$context[k] <- names(bootstrap.results)[[k]]
  bootstrap.summary$TP85recall.mean[k] <- rowMeans(bootstrap.results[[k]])[1]
  bootstrap.summary$TP85precision.mean[k] <- rowMeans(bootstrap.results[[k]])[2]
  bootstrap.summary$MI85recall.mean[k] <- rowMeans(bootstrap.results[[k]])[3]
  bootstrap.summary$MI85precision.mean[k] <- rowMeans(bootstrap.results[[k]])[4]
  bootstrap.summary$TP85recall.sd[k] <- sd(bootstrap.results[[k]][1,])
  bootstrap.summary$TP85precision.sd[k] <- sd(bootstrap.results[[k]][2,])
  bootstrap.summary$MI85recall.sd[k] <- sd(bootstrap.results[[k]][3,])
  bootstrap.summary$MI85precision.sd[k] <- sd(bootstrap.results[[k]][4,])

}
bootstrap.summary$context <- as.factor(bootstrap.summary$context)

bootstrap.plot <- bootstrap.summary %>%
  gather(key="key", value="value", -context) %>%
  tidyr::extract(col=key, into=c("criterion", "measure", "stat"), regex="([A-Z]{2}[0-9]{2})([a-z]+).([a-z]+)") %>%
  tidyr::spread(stat, value)
bootstrap.plot$criterion <- as.factor(bootstrap.plot$criterion)
bootstrap.plot$measure <- as.factor(bootstrap.plot$measure)


ggplot(bootstrap.plot, aes(x=context, y=mean)) +
  facet_wrap(~measure+criterion)+
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymax = mean + sd, ymin=mean - sd), width=.3, position=position_dodge(.9)) +
  labs(title="Nontext accuracy (error bars are SD)", x=NULL, y=NULL) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#####################################################
# one-sample t-tests comparing each nontext dist to its context estimate
#####################################################
bootstrap.plot$context.est <- NA
for(k in 1:length(colnames(contexts))){
  context.resultsTP <- colMeans(context.data[[k]]$TP85$seg.results[,4:5], na.rm=T) 
  context.resultsMI <- colMeans(context.data[[k]]$MI85$seg.results[,4:5], na.rm=T)
  bootstrap.plot$context.est <- with(bootstrap.plot, ifelse(context==colnames(contexts)[k],
                                ifelse(criterion=="TP85" & measure=="recall", context.resultsTP[which(attr(context.resultsTP, "names")=="recall")],
                                  ifelse(criterion=="TP85" & measure=="precision", context.resultsTP[which(attr(context.resultsTP, "names")=="precision")],
                                         ifelse(criterion=="MI85" & measure=="recall", context.resultsMI[which(attr(context.resultsTP, "names")=="recall")],
                                                ifelse(criterion=="MI85" & measure=="precision", context.resultsMI[which(attr(context.resultsTP, "names")=="precision")],
                                                       NA)))), context.est))
}
 
bootstrap.plot$t <- with(bootstrap.plot, (context.est - mean)/(sd/sqrt(iter)))
bootstrap.plot$d <- with(bootstrap.plot, (context.est - mean)/sd)
bootstrap.plot$pval <- 2*pt(abs(bootstrap.plot$t), df=iter-1, lower.tail=F)
bootstrap.plot$stars <- ifelse(bootstrap.plot$pval < .001, "***",
                               ifelse(bootstrap.plot$pval < .01, "**",
                                      ifelse(bootstrap.plot$pval < .05, "*",
                                             "")))
kable(bootstrap.plot)
kable(filter(bootstrap.plot, criterion=="MI85"))


# average effect size
mean(filter(bootstrap.plot, criterion=="MI85" & measure=="recall")$d)
mean(filter(bootstrap.plot, criterion=="MI85" & measure=="precision")$d)


#############
#############
#############
combined.bootstrap.results <- as.data.frame(t(bootstrap.results[[1]]))
combined.bootstrap.results$iter <- 1:nrow(combined.bootstrap.results)
combined.bootstrap.results$context.corpus <- names(bootstrap.results)[1]
for(k in 2:length(names(bootstrap.results))){
  results <- as.data.frame(t(bootstrap.results[[k]]))
  results$iter <- 1:nrow(results)
  results$context.corpus <- names(bootstrap.results)[k]
  combined.bootstrap.results <- rbind(combined.bootstrap.results,results)
}
combined.bootstrap.results$context.corpus <- as.factor(combined.bootstrap.results$context.corpus)
combined.bootstrap.results <- combined.bootstrap.results %>%
  gather(key="key", value="value", 1:4) %>%
  extract(col=key, into=c("criterion", "measure"), regex="([A-Z]{2}[0-9]{2})([a-z]+)" ) 
combined.bootstrap.results$criterion <- as.factor(combined.bootstrap.results$criterion)
combined.bootstrap.results$measure <- as.factor(combined.bootstrap.results$measure)
#############
ggplot(filter(combined.bootstrap.results, criterion=="MI85"), aes(x=context.corpus, y=value))+
  geom_boxplot() +
  facet_wrap(~measure) +
  geom_point(data=filter(bootstrap.plot, criterion=="MI85"), aes(x=context, y=context.est, color=context), size=4) + 
  # geom_hline(data=global.plot, aes(yintercept=mean), linetype = 2) + 
  theme(text = element_text(size=30), axis.ticks = element_blank(), axis.text.x = element_blank()) +
  labs(x=NULL, y=NULL)

global.acc <- global.acc <- colMeans(global.data$MI85$seg.results[,4:5], na.rm=T)
global.plot <- data.frame(context="global", criterion="MI85", measure=names(global.acc), mean=global.acc, sd=NA, context.est=NA, t=NA, pval=NA, stars=NA, d=NA)

##################################################
# descriptives
descriptives <- results_descriptives(data=global.data, criteria="MI85")
for(k in 1:length(colnames(contexts))){
  descriptives <- rbind(descriptives, results_descriptives(context.data[[k]], context=colnames(contexts[k])))
  #descriptives <- rbind(results_descriptives(context.data[[k]], context=colnames(contexts[k]), criteria="MI85"), descriptives)
}

##################################################
# NOTE: Swingley only considers a "word" segmented if it has BOTH high within-unit MI and also high frequency as a unit.

plot.data.con <- data.frame(context=names(context.data), TP85recall.mean=NA, TP85precision.mean=NA, MI85recall.mean=NA, MI85precision.mean=NA, TP85recall.sd=NA, TP85precision.sd=NA, MI85recall.sd=NA, MI85precision.sd=NA, N.utt=NA, group="context")
for(k in 1:length(names(contexts))){
  plot.data.con[k, 2:3] <- colMeans(context.data[[k]]$TP85$seg.results[,4:5], na.rm=T)
  plot.data.con[k, 4:5] <- colMeans(context.data[[k]]$MI85$seg.results[,4:5], na.rm=T)
  plot.data.con$N.utt[k] <- context.data[[k]]$N.utterances
}

# plot.data.non <- data.frame(context=names(context.data), TP85recall=NA, TP85precision=NA, MI85recall=NA, MI85precision=NA, N.utt=NA)
# for(k in 1:length(names(contexts))){
#   plot.data.non[k, 2:3] <- colMeans(nontext.data[[k]]$TP85$seg.results[,4:5], na.rm=T)
#   plot.data.non[k, 4:5] <- colMeans(nontext.data[[k]]$MI85$seg.results[,4:5], na.rm=T)
#   plot.data.non$N.utt[k] <- nontext.data[[k]]$N.utterances
# }
# plot.data.non$group <- "nontext"

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
# Note, this should perhaps be a DST, not IST
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

summary(lm(mean ~ group*variable*criterion, data=plot.data))
summary(lm(mean ~ group*context*variable + N.utt, data=plot.data)) # controlling for the number of utterances

summary(lm(mean ~ group, data=subset(plot.data, variable=="recall" & criterion=="TP85")))
summary(lm(mean ~ group, data=subset(plot.data, variable=="recall" & criterion=="MI85")))
summary(lm(mean ~ group, data=subset(plot.data, variable=="precision" & criterion=="TP85")))
summary(lm(mean ~ group, data=subset(plot.data, variable=="precision" & criterion=="MI85")))




#####################################################
# how many words are there to find? check how many words were available in each group.

#####################################################
# display context info
# what are the overlaps? Venn diagram showing size of corpora and overlap
# how are contexts related over time (sequence plot)?

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

# source("/Users/TARDIS/Documents/STUDIES/TPs/data_processing_functions.r")

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
# 
# # reformat for printing as table
tests.table <- data.frame(N.utt=N.utt , TP.KSstat = NA, TP.KSpval=NA, MI.KSstat=NA, MI.KSpval=NA)
  for(k in 1:length(names(context.data))){
    tests.table[k,2:5] <- round(c(tests[[k]]$test.TP$statistic, tests[[k]]$test.TP$p.value, tests[[k]]$test.MI$statistic, tests[[k]]$test.MI$p.value), 3)
  }
tests.table$TP.KSpval <- ifelse(tests.table$TP.KSpval==0, ">.001", tests.table$TP.KSpval)
tests.table$MI.KSpval <- ifelse(tests.table$MI.KSpval==0, ">.001", tests.table$MI.KSpval)
knitr::kable(tests.table)
x
# # test nontexts against each other, to check
# nontext.tests <- vector("list", length(colnames(contexts))) # storage variable
# names(tests) <- colnames(contexts)
# for(k in 1:length(colnames(contexts))){
#   message(paste("processing ", colnames(contexts)[k], "...", sep=""))
#   
#   nontext.tests[[k]]$nontext.pairs2.N <- nrow(nontext.data2[[k]]$phon.pairs)
#   nontext.tests[[k]]$nontext.pairs.N <- nrow(nontext.data[[k]]$phon.pairs)
#   
#   nontext.tests[[k]]$test.MI <- ks.test(nontext.data2[[k]]$phon.pairs$MI, nontext.data[[k]]$phon.pairs$MI)
#   nontext.tests[[k]]$test.TP <-ks.test(nontext.data2[[k]]$phon.pairs$TP, nontext.data[[k]]$phon.pairs$TP)
# }


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
# barplot(sort(freq.bigrams, decreasing=TRUE)) # not working
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

# frequency counts by context 
# View(summarise(group_by(freq.words.df, context), types=n(), tokens=sum(Freq)))
# View(arrange(filter(freq.words.df, context=="routines"), -Freq))
# View(arrange(filter(freq.words.df, context=="mealtime"), -Freq))

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

# # presentation of results
# 
# # context-free results
# TP <- read.csv("./full results/korman_TP.csv", row.names=1)
# MI <- read.csv("./full results/korman_MI.csv", row.names=1)
# 
# # 100 syllables (100^2 entries), about 3 minutes, 500 (500^2 entries) syllables about 1 hour, whole set about 11 hours
# hist(as.matrix(MI), breaks=30); abline(v=0, col="red")
# length(which(is.na(MI)))/length(MI) # 99% of syllable pairs never occur
# hist(as.matrix(TP), breaks=30)
# length(which(is.na(TP)))/length(TP)


# # curiosity re log tranformations
# TP_log <- log2(TP)
# hist(TP_log, breaks=30)
# MI_raw <- 2^MI
# hist(MI_raw, breaks=30) # almost everything is close to zero but the range is enormous
# hist(MI_raw, breaks=300000, xlim=c(0,5)); abline(v=1, col="red") #zooming in

```
What if kids don't actually get 100,000 syllables? So TPs don't stabilize. And that's part of the system.

How many syllables do kids hear in a day? Fernald's words per day times rate of monosyllabic words. Tokens.

Chance for TPs? Shuffle corpus and recalculate.

Get frequency dists for each syllable, get frequency dists for each word



df$utterance <- row.names(df)
df$y <- 1

par(mfrow=c(1,1))

fifth <- floor(nrow(df)/5)
plot.data <- df[1:100, ]
ggplot(plot.data, aes(x=utterance, y=y, color=context)) + 
  theme_bw() +
  # geom_point(shape=124)
  geom_point()


# ####################################################
# # sequence plots (adapted from code by CF)
# ####################################################
# library(TraMineR)
# # data have to to be organized as one sequence per row. 
# seq <- t(as.matrix(data.frame(context=df$context)))
# 
# #set up info for sequence
# seqstatl(seq)    #check the values in seq.  this is like unique() over all your rows
# 
# #have to define these three things in order for seqdef() to work below. it's just every unique value in your sequences
# seq.alphabet = as.vector(unique(df$context))
# seq.labels = as.vector(unique(df$context))
# seq.scodes = as.vector(unique(df$context))
# 
# #create the sequence object. 
# seq.seq = seqdef(seq, alphabet = seq.alphabet, states = seq.scodes, labels = seq.labels)
# 
# #plot the sequence
# seqplot(seq.seq, cpal = c('green3', 'red2', 'gray87', 'blue3'), xtlab = '', type = 'I', ylab = '', withlegend = T, axes = F, yaxis = F)


# 
# context.data <- vector("list", length(colnames(contexts))) # storage variable
# names(context.data) <- colnames(contexts)
# 
# for(k in 1:length(colnames(contexts))){
# #  df.context <- filter(df, context==colnames(contexts)[k])
#   df.context <- filter(df, df[ , which(colnames(df)==colnames(contexts)[k])] > 0)
#   #View(df.context[,c(1,3:9)])
#   
#   context.data[[k]]$N.utterances <- nrow(df.context)
#   # collapse phonological utterances into one continuous stream
#   context.data[[k]]$phon.stream <- unlist(strsplit(df.context$phon, " "))
# 
#   # how many unique syllables are there?
#   context.data[[k]]$syllables <- unique(context.data[[k]]$phon.stream)
#   # context.data[[k]]$syllables <- sample(context.data[[k]]$syllables, 20) # for testing, just use some random syllables
#   context.data[[k]]$N.syl <- length(context.data[[k]]$syllables)
# 
#   # make phone stream into a list of all of the bisyllable pairs that occur
#   context.data[[k]]$phon.pairs <- data.frame(syl1=context.data[[k]]$phon.stream[1:length(context.data[[k]]$phon.stream)-1], syl2=context.data[[k]]$phon.stream[2:length(context.data[[k]]$phon.stream)])
# 
#   # collapse orthographic utterances into one stream
#   context.data[[k]]$orth.stream <- unlist(strsplit(df.context$orth, " "))
# 
#   # how many unique words are there?
#   context.data[[k]]$words <- unique(context.data[[k]]$orth.stream)
#   context.data[[k]]$N.words <- length(context.data[[k]]$words)
# 
#   message(paste("processing ", colnames(contexts)[k], "...", sep=""))
#   
# #   # mutual information, and transitional probabilty. See Swingley (2005) p97
# #   context.data[[k]]$MI <- matrix(nrow=context.data[[k]]$N.syl, ncol=context.data[[k]]$N.syl, dimnames=list(context.data[[k]]$syllables, context.data[[k]]$syllables))
# #   context.data[[k]]$TP <- matrix(nrow=context.data[[k]]$N.syl, ncol=context.data[[k]]$N.syl, dimnames=list(context.data[[k]]$syllables, context.data[[k]]$syllables))
# # 
# #   p <- progress_estimated(n=context.data[[k]]$N.syl) # print progress bar while working
# #   for(i in 1:context.data[[k]]$N.syl){
# #     for(j in 1:context.data[[k]]$N.syl){
# #       # AB <- filter(context.data[[k]]$phon.pairs, syl1==context.data[[k]]$syllables[i] & syl2==context.data[[k]]$syllables[j])
# #       AB <- subset(context.data[[k]]$phon.pairs, syl1==context.data[[k]]$syllables[i] & syl2==context.data[[k]]$syllables[j])
# #       p.AB <- nrow(AB)/nrow(context.data[[k]]$phon.pairs)
# #       p.A <- length(which(context.data[[k]]$phon.stream == context.data[[k]]$syllables[i]))/length(context.data[[k]]$phon.stream)
# #       p.B <- length(which(context.data[[k]]$phon.stream == context.data[[k]]$syllables[j]))/length(context.data[[k]]$phon.stream)
# #       
# #       context.data[[k]]$MI[i,j] <- ifelse(p.AB==0, NA, log2(p.AB/(p.A * p.B))) # if AB never occurs, enter NA, otherwise calculate MI
# #       context.data[[k]]$TP[i,j] <- ifelse(p.AB==0, NA, p.AB/(p.A)) # if AB never occurs, enter NA, otherwise calculate TP
# #     }
# #     print(p$tick()) # advance progress bar
# #   }
# 
# ############################
# # OPTION 2
# ############################
#     p <- progress_estimated(n=nrow(context.data[[k]]$phon.pairs)) # print progress bar while working
#     for(i in 1:nrow(context.data[[k]]$phon.pairs)){
#       AB <- filter(context.data[[k]]$phon.pairs, syl1==syl1[i] & syl2==syl2[i])
#       p.AB <- nrow(AB)/nrow(context.data[[k]]$phon.pairs)
#       p.A <- length(which(context.data[[k]]$phon.stream == context.data[[k]]$phon.pairs$syl1[i]))/length(context.data[[k]]$phon.stream)
#       p.B <- length(which(context.data[[k]]$phon.stream == context.data[[k]]$phon.pairs$syl2[i]))/length(context.data[[k]]$phon.stream)
#       
#       context.data[[k]]$phon.pairs$MI[i] <- ifelse(p.AB==0, NA, log2(p.AB/(p.A * p.B))) # if AB never occurs, enter NA, otherwise calculate MI
#       context.data[[k]]$phon.pairs$TP[i] <- ifelse(p.AB==0, NA, p.AB/(p.A)) # if AB never occurs, enter NA, otherwise calculate TP
#       print(p$tick()) # advance progress bar
#     }
# 
# context.data[[k]]$freq.bigrams <- summarise(group_by(phon.pairs, syl1, syl2), count=n()) # frequency of bigrams
# context.data[[k]]$freq.words <- table(orth.stream) # frequency of words
# context.data[[k]]$freq.syl <- table(phon.stream)
# }

# presenting the results
# 
# par(mfrow=c(1,2))
# for(k in 1:length(colnames(contexts))){
#   hist(context.data[[k]]$phon.pairs$MI, main=paste("Mutual Information,\n", colnames(contexts)[k], "context"), xlim=c(min(phon.pairs$MI), max(phon.pairs$MI)))
#   
#   hist(context.data[[k]]$phon.pairs$TP, main=paste("Transitional Probability,\n", colnames(contexts)[k], "context"), xlim=c(0,1))
# }
# # 
# hist(context.data$mealtime$MI, xlab="", main="Mutual Information,\nmealtime context", xlim=c(min(as.matrix(MI), na.rm=T), max(as.matrix(MI), na.rm=T)))
# hist(context.data$mealtime$TP, xlab="", main="Transitional Probabilities,\nmealtime context")
# 
# hist(context.data$people$MI, xlab="", main="Mutual Information,\npeople context", xlim=c(min(as.matrix(MI), na.rm=T), max(as.matrix(MI), na.rm=T)))
# hist(context.data$people$TP, xlab="", main="Transitional Probabilities,\npeople context")
# 
# hist(context.data$routines$MI, xlab="", main="Mutual Information,\nroutines context", xlim=c(min(as.matrix(MI), na.rm=T), max(as.matrix(MI), na.rm=T)))
# hist(context.data$routines$TP, xlab="", main="Transitional Probabilities,\nroutines context")
# 
# hist(context.data$bathtime$MI, xlab="", main="Mutual Information,\nbathtime context", xlim=c(min(as.matrix(MI), na.rm=T), max(as.matrix(MI), na.rm=T)))
# hist(context.data$bathtime$TP, xlab="", main="Transitional Probabilities,\nbathtime context")
# 
# hist(context.data$bedtime$MI, xlab="", main="Mutual Information,\nbedtime context", xlim=c(min(as.matrix(MI), na.rm=T), max(as.matrix(MI), na.rm=T)))
# hist(context.data$bedtime$TP, xlab="", main="Transitional Probabilities,\nbedtime context")
# 
# # context-free results
# TP <- read.csv("./full results/korman_TP.csv", row.names=1)
# MI <- read.csv("./full results/korman_MI.csv", row.names=1)
# 
# hist(as.matrix(MI), xlab="", main="Mutual Information,\ncontext-free (global)", xlim=c(min(as.matrix(MI), na.rm=T), max(as.matrix(MI), na.rm=T)))
# hist(as.matrix(TP), xlab="", main="Transitional Probabilities,\ncontext-free (global)")


# # how many utterances are in the average context corpus?
# N.utt <- vector("numeric", length=length(colnames(contexts)))
# names(N.utt) <- colnames(contexts)
# for(k in 1:length(colnames(contexts))){
#   N.utt[k] <- context.data[[k]]$N.utterances
# }
# ave.utts <- floor(mean(N.utt))

 
# # construct a corpus with the same number of utterances as a context corpus, but no key words seeding it
# rows <- sample.int(ave.utts)
# rows <- sample.int(N.utt[6])
# length(rows)
# sample <- df[rows,]
# df <- sample
# # overwrite df with sample, i.e. df <- sample, and then re-run the make_streams chunk and the calc_global_MI chunk

filter(context.data$mealtime$unique.phon.pairs, syl1=="'mIlk") 
filter(nontext.data[[1]]$unique.phon.pairs, syl1=="'mIlk") 


# gotta have the window in the nontexts sampling


# may also want to shuffle nontexts and see what happens

# pull out all syllables that occur >1, see which contexts they show up in
