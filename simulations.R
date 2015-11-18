####################################################################################
# how does corpus size affect precision and recall?
####################################################################################

# make a set of "contexts" from 100 utterances to (nearly) the full size of the corpus
df <- contexts_by_size(N.sizes=30)

# write.table(df, file="contexts_SizeSim.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
# send the above text file to ACISS and use it as the contexts file for a bootstrap analysis
 
size.results1 <- process_batch_results(id="bootstrapSizeSim", dir="nontexts_SizeSim")
size.results2 <- process_batch_results(id="bootstrapSizeSim6", dir="nontexts_SizeSim")
size.results3 <- process_batch_results(id="bootstrapSizeSim7", dir="nontexts_SizeSim")
size.results <- rbind(size.results1, size.results2)

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
df.unif <- contexts_by_size(df=corpus.unif, N.sizes=25, min.utt=10)
# write.table(df.skew, file="contexts_SizeSimUNIF.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")

# check that the frequency dist looks correct
plot_corpus_dist(corpus.skew)
plot_corpus_dist(corpus.unif)

unif.res <- par_function2(df=df.unif, dict=dict.unif, expand=FALSE, seg.utts=TRUE, TP=FALSE)
skew.res <- par_function2(df=df.skew, dict=dict.skew, expand=FALSE, seg.utts=TRUE, TP=FALSE)
names(unif.res[[2]]) # the sizes tried
summary(unif.res[[2]][[25]]$MI85$seg.results)
hist(unif.res[[2]][[25]]$unique.phon.pairs$MI, breaks=30); abline(v=quantile(unif.res[[2]][[25]]$unique.phon.pairs$MI, .85), lty=2, col="red")
hist(skew.res[[2]][[25]]$unique.phon.pairs$MI, breaks=30); abline(v=quantile(skew.res[[2]][[25]]$unique.phon.pairs$MI, .85), lty=2, col="red")

library(BatchJobs)

library(dplyr); library(tidyr); library(doParallel); library(devtools)

starts <- 1:12

batch_function <- function(starts){
  library(dplyr)
  library(tidyr)
  library(devtools)
  
  source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/data_processing_functions.r")
  
  dist <- "skewed"
  # dist <- "unif"
  
  lang <- make_corpus(dist=dist, N.utts=1000, N.types=24)
  corpus <- lang[[1]] # the corpus
  art.dict <- lang[[2]] # the dictionary
  # use that corpus to generate a size sim contexts file
  df <- contexts_by_size(df=corpus, N.sizes=25, min.utt=10)

  if(nrow(df) == 0) stop("df didn't load")
  if(nrow(art.dict) == 0) stop("dict didn't load")
  
  iter <- 50 # the number of times to generate random samples
  
  library(doParallel)
  registerDoParallel()
  r <- foreach(1:iter, 
               .combine = rbind, 
               .packages=c("dplyr", "tidyr", "devtools") ) %dopar% par_function(df=df,
                                                                                dict=art.dict,
                                                                                expand=FALSE,
                                                                                seg.utts=TRUE,
                                                                                TP=FALSE)
}

# create a registry
id.skew <- "bootstrapSizeSimSKEW2"
reg.skew <- makeRegistry(id = id.skew)

# map function and data to jobs and submit
ids  <- batchMap(reg.skew, batch_function, starts)
done <- submitJobs(reg.skew, resources = list(nodes = 12, walltime=21600)) # expected to run for 6 hours (21600 seconds)

# create a registry
id.unif <- "bootstrapSizeSimUNIF"
reg.unif <- makeRegistry(id = id.unif)

# map function and data to jobs and submit
ids  <- batchMap(reg.unif, batch_function, starts)
done <- submitJobs(reg.unif, resources = list(nodes = 12, walltime=21600)) # expected to run for 6 hours (21600 seconds)

showStatus(reg.unif); showStatus(reg.skew)


skew.results1 <- process_batch_results(id="bootstrapSizeSimSKEW", dir="nontexts_SizeSimSKEW")
skew.results2 <- process_batch_results(id="bootstrapSizeSimSKEW50", dir="nontexts_SizeSimSKEW")
skew.results3 <- process_batch_results(id="bootstrapSizeSimSKEWlong", dir="nontexts_SizeSimSKEW")
skew.results <- rbind(skew.results1, skew.results2)
skew.results$dist <- "skew"

unif.results1 <- process_batch_results(id="bootstrapSizeSimUNIF", dir="nontexts_SizeSimUNIF")
unif.results2 <- process_batch_results(id="bootstrapSizeSimUNIF50", dir="nontexts_SizeSimUNIF")
unif.results3 <- process_batch_results(id="bootstrapSizeSimUNIFlong", dir="nontexts_SizeSimUNIF")
unif.results <- rbind(unif.results1, unif.results2)
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
p <- ggplot(filter(nontext.results, stat=="MI"), aes(x=as.factor(size), y=value)) +
  geom_boxplot() +
  facet_grid(measure ~ dist, scales="free") +
  theme(text = element_text(size=20), axis.text.x=element_text(size=10, angle =90), axis.ticks = element_blank() ) +
  labs(y=NULL, x="Number of utterances in random corpora")
if(min.iters == max.iters) p + ggtitle(paste(min.iters, "iterations in each boxplot"))
if(min.iters != max.iters) p + ggtitle(paste(min.iters, "to", max.iters, "iterations in each boxplot"))
  
