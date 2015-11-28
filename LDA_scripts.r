# take CHAT transcripts from Korman corpus and break them into "documents" for LDA.
# each document should be within one "episode" (@NewEpisode tag marks where the recorder was turned off and on)

## Roy, Frank & Roy (RFR) stemmed, normalizing word forms to a common root using the Porter stemmer (Porter 1980)
## RFR only accepted words that occured in at least 5 documents
## RFR used a 10-minute window, sliding by 10 minutes across the corpus from 9mos to 24mos.
dir <- "/Users/TARDIS/Documents/STUDIES/TPs/CHILDES_Korman/transcripts/"
files <- list.files(path=dir, pattern="\\.cha")

children <- unique(substr(files, 1,2))

window <- 30 # the number of utterances to use as one "document", when more than that are in an episode

documents <- list()

for(c in 1:length(children)){
  child.files <- grep(pattern=children[c], x=files, value=T)
  Nfiles <- length(child.files)
  file.episodes <- list()
  for(f in 1:Nfiles){
    this.file <- child.files[f]
    this.transcript <- readLines(paste(dir, this.file, sep=""))
    # remove "unintelligible" (it was written into the speaker tier when the transcriber couldn't hear what was said)
    this.transcript <- gsub(pattern="(unintelligible)", replacement="", x=this.transcript, fixed=T)
    this.transcript <- gsub(pattern=" \\[: [[:alpha:]]+\\]", replacement="", x=this.transcript) # remove translations (e.g. from slang to standard english)
    
    clean.transcript <- grep(pattern="^(\\*[A-Z]+|@New)", x=this.transcript, value=TRUE) # keep only speaker tier and @New Episode markers
    clean.transcript <- gsub(pattern="(\\*[A-Z]{3}:\t)(.*)", replacement="\\2", x=clean.transcript, perl=T) # only keep the utterance portion (drop the speaker label, e.g. "*ATT:   ")
    
    # split by @New Episode markers
    episode_breaks <- c(1, grep(pattern="@New", x=clean.transcript, value=FALSE), length(clean.transcript))
    Nepisodes <- length(episode_breaks)-1  # the number of episodes
  
    episodes <- list(as.list(vector(length=Nepisodes)))
    
    for(e in 1:Nepisodes){
      this.document <- clean.transcript[episode_breaks[e]:episode_breaks[e+1]]
      this.document <- grep(pattern="[^(@New Episode)]", x=this.document, value=T) # drop the @New Episode lines
      
      if(length(this.document) <= window){
        episodes[[e]] <- paste(this.document , collapse=" ") # if this episode is not longer than the window, just use it
      } else { 
        Nwindows <- ceiling(length(this.document)/window) # if the episode is longer than one window, then figure out how many windows it should be broken into
        windows <- list()
        for(w in 1:Nwindows){
          start <- (w-1)*window + 1
          end <- min(c(window*w, length(this.document)))
          windows[[w]] <- paste(this.document[start:end], collapse=" ")
        } # end of window loop
        episodes[[e]] <- windows
      } # end of if else
    } # end of episode loop
    file.episodes[[f]] <- episodes
  } # end of file loop
  documents[[c]] <- unlist(file.episodes)
} # end of child loop
names(documents) <- children  


### print it for MALLET
document.vector <- unlist(documents) # collapsing across all children
for(d in 1:length(document.vector)){
  write.table(document.vector[d], file=paste("/Users/TARDIS/Downloads/mallet-2.0.7/TP-data/",names(document.vector[d]), ".txt", sep=""), quote=F, col.names=F, row.names=F, append=F, sep="\t")
}

  

library(RTextTools); library(topicmodels); library(tm)
document.vector <- documents[[c]] # one child's material
document.vector <- unlist(documents) # collapsing across all children

corpus <- VCorpus(VectorSource(document.vector)) 
corpus <- tm_map(corpus, content_transformer(tolower)) # make it all lower case
f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
corpus <- tm_map(corpus, f, "[[:punct:]]+") # remove punctuation
corpus <- tm_map(corpus, f, "[[:digit:]]+") # remove digits
#corpus <- tm_map(corpus, content_transformer(FUN=function(x)wordStem(strsplit(x, " ")[[1]]) ) ) # uses Porter's stemming algorithm
matrix <- DocumentTermMatrix(corpus)
findFreqTerms(matrix, 5) # words that occur at least 5 times
# show the correlations above .3 for some words... 
findAssocs(matrix, "bed", 0.3); findAssocs(matrix, "milk", 0.3); findAssocs(matrix, "sleepy", 0.3); findAssocs(matrix, "read", 0.3); findAssocs(matrix, "tummy", 0.3); findAssocs(matrix, "hungry", 0.3)

matrix <- removeSparseTerms(matrix, sparse=1-(5/length(corpus)))  # only keep terms that occur accross at least 5 documents

  
#matrix <- create_matrix(document.vector, language="english", removeNumbers=TRUE, removePunctuation=TRUE, stemWords=TRUE)
k <- 25 # this is the number RFR found was good
lda <- LDA(matrix, k)
terms(lda)
topics(lda)


#### example from http://www.rtexttools.com/blog/getting-started-with-latent-dirichlet-allocation-using-rtexttools-topicmodels
install.packages(c("RTextTools","topicmodels"))
library(RTextTools)
library(topicmodels)
data(NYTimes)
data <- NYTimes[sample(1:3100,size=1000,replace=FALSE),]
matrix <- create_matrix(cbind(as.vector(data$Title),as.vector(data$Subject)), language="english", removeNumbers=TRUE, stemWords=TRUE, weighting=weightTf)
k <- length(unique(data$Topic.Code))
lda <- LDA(matrix, k)
terms(lda)
topics(lda)

#### example from http://www.r-bloggers.com/text-mining/
k = 2;
SEED = 1234;
my_TM =
  list(VEM = LDA(myDtm, k = k, control = list(seed = SEED)),
       VEM_fixed = LDA(myDtm, k = k,
                       control = list(estimate.alpha = FALSE, seed = SEED)),
       Gibbs = LDA(myDtm, k = k, method = "Gibbs",
                   control = list(seed = SEED, burnin = 1000,
                                  thin = 100, iter = 1000)),
       CTM = CTM(myDtm, k = k,
                 control = list(seed = SEED,
                                var = list(tol = 10^-4), em = list(tol = 10^-3))));

Topic = topics(my_TM[["VEM"]], 1);

#top 5 terms for each topic in LDA
Terms = terms(my_TM[["VEM"]], 5);
Terms;

(my_topics =
   topics(my_TM[["VEM"]]));

most_frequent = which.max(tabulate(my_topics));

terms(my_TM[["VEM"]], 10)[, most_frequent];



# example from demo(lda)
result <- lda.collapsed.gibbs.sampler(cora.documents,
                                      k,  ## Num clusters
                                      terms,
                                      25,  ## Num iterations
                                      0.1,
                                      0.1,
                                      compute.log.likelihood=TRUE) 
top.words <- top.topic.words(result$topics, 5, by.score=TRUE) # get the top words in the cluster
N <- 10 # the number of documents to display
topic.proportions <- t(result$document_sums) / colSums(result$document_sums)
topic.proportions <- topic.proportions[sample(1:dim(topic.proportions)[1], N),]
topic.proportions[is.na(topic.proportions)] <-  1 / K

colnames(topic.proportions) <- apply(top.words, 2, paste, collapse=" ")

topic.proportions.df <- reshape2::melt(cbind(data.frame(topic.proportions),
                                             document=factor(1:N)),
                                       variable.name="topic",
                                       id.vars = "document") 

ggplot(aes(topic, value, fill=document), data=topic.proportions.df) + 
  geom_bar(stat="identity") +
  coord_flip() +
  facet_wrap(~ document, ncol=5)
