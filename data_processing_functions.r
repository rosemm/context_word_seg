
nontext_cols <- function(df, context_names, prop=FALSE){
  nontexts <- vector("list", length(context_names)) # storage variable
  non <- NULL
  
  if(!prop){
    for(k in 1:length(context_names)){  
      N.hits <- length(which(df[[context_names[k]]] == 1)) # the number of utterances that contain a keyword for that context
      nontexts[[k]] <- sample(x=as.numeric(row.names(df)), size=N.hits) # take a random sample of utterances
      col <- rep(0, nrow(df)) # make a column of zeros
      col[nontexts[[k]]] <- 1 # change those zeros to 1's for every random utterance selected
      non <- cbind(non, col) 
      colnames(non)[k] <- paste("non.", context_names[k], sep="") 
    }
  } 
  if(prop){
    non <- df[ , 4:ncol(df)]
    # shuffle the context columns
    for(k in 1:length(context_names)){
      non[[context_names[k]]] <- base::sample(non[[context_names[k]]], 
                                             size=length(non[[context_names[k]]]), 
                                             replace=FALSE)
      colnames(non)[k] <- paste("non.", context_names[k], sep="") 
    }
    # change probabilities into 1 or 0 probabalistically based on their values
    non.probs <- non
    for(r in 1:nrow(non)){
      for(c in 1:ncol(non)){
        non[r,c] <- sample(x=c(0,1), 
                           size=1, 
                           prob=c(1-non.probs[r,c], non.probs[r,c]))
      }
    }
  }
  return(list(non, nontexts))
}

expand_windows <- function(df, context.names){
  p <- progress_estimated(n=length(3:(nrow(df)-2))) # print progress bar while working
  for(i in 3:(nrow(df)-2)){
    for(j in which(colnames(df) == context.names[1]):ncol(df)){
      df[i,j] <- ifelse(df[i,j]==1, df[i,j], # if it is already marked 1, leave it
                        ifelse(df[(i-2),j]==1, 1.5, # if the utterance 2 before it is marked 1, mark 1.5
                               ifelse(df[(i-1),j]==1, 1.5, # if the utterance 1 before it is marked 1, mark 1.5
                                      ifelse(df[(i+1),j]==1, 1.5, # if the utterance 1 after it is marked 1, mark 1.5
                                             ifelse(df[(i+2),j]==1, 1.5, 0))))) # if the utterance 2 after it is marked 1, mark 1.5. Otherwise mark 0.
      # note that utterances classified as a context based on their proximity to an utterance with a key word must be marked with something other than 1 to prevent them being used as key utterances in the next row
      
    }
    print(p$tick()) # advance progress bar
  }
  # the above code skips lines 1 and 2, so go ahead and fill those in now
  for(j in which(colnames(df) == context.names[1]):ncol(df)){
    i <- 1
    df[1,j] <- ifelse(df[i,j]==1, df[i,j], # if it is already marked 1, leave it
                      ifelse(df[(i+1),j]==1, 1.5, # if the utterance 1 after it is marked 1, mark 1.5
                             ifelse(df[(i+2),j]==1, 1.5, 0))) # if the utterance 2 after it is marked 1, mark 1.5. Otherwise mark 0.
    i <- 2
    df[2,j] <- ifelse(df[i,j]==1, df[i,j], # if it is already marked 1, leave it
                      ifelse(df[(i-1),j]==1, 1.5, # if the utterance 1 before it is marked 1, mark 1.5
                             ifelse(df[(i+1),j]==1, 1.5, # if the utterance 1 after it is marked 1, mark 1.5
                                    ifelse(df[(i+2),j]==1, 1.5, 0)))) # if the utterance 2 after it is marked 1, mark 1.5. Otherwise mark 0.
  }
  return(df)
}

calc_MI = function(phon.pairs, phon.stream, MI=TRUE, TP=TRUE){
  # mutual information, and transitional probabilty. See Swingley (2005) p97
  
  library(tidyr)  
    # calculate the number of times each syllable occurs in first position in a bigram
    A.at1 <- phon.pairs %>%
      group_by(syl1) %>%
      summarize(A.freq1=n())
    B.at2 <- phon.pairs %>%
      group_by(syl2) %>%
      summarize(B.freq2=n())
    # NOTE: Since syllable transitions spanning an utterance boundary have been removed, this only counts freq of syl1 / all non-utterance-final syllables
    # This more closely approximates the sense of TP as conditional probability (see e.g. Saffran et al. 1996). 
    # If all occurences of a syllable are counted (including utterance-final occurences for syl1), then the TP can (and does) go far above 1 since there are many more units of syllables than bi-syllable pairs
    phon.pairs <- left_join(phon.pairs, A.at1)
    phon.pairs <- left_join(phon.pairs, B.at2)

    syls <- data.frame(syl=phon.stream[phon.stream != "###"]) %>%
      count(syl) %>%
      mutate(p=n/length(unique(phon.stream[phon.stream != "###"])))
    # merge in sylable info for syl1
    colnames(syls) <- c("syl1", "A.freq.tot", "p.A")
    phon.pairs <- left_join(phon.pairs, syls)
    # merge in sylable info for syl2
    colnames(syls) <- c("syl2", "B.freq.tot", "p.B")
    phon.pairs <- left_join(phon.pairs, syls)
  
  # add a column for the syllable pair (helps for joining this table with pairs.count later)
  phon.pairs <- phon.pairs %>%
    unite(pair, syl1, syl2, sep="-", remove=F)
  
  # calculate the number of times this syllable pair occurs
  pairs.count <- phon.pairs %>%
    group_by(pair) %>%
    summarize(AB.freq=n())
  phon.pairs <- left_join(phon.pairs, pairs.count) # add the AB frequency counts to the phon.pairs table
  
  # calculate MI and TP
  
  phon.pairs <- phon.pairs %>%
    mutate(p.AB=AB.freq/nrow(phon.pairs),
           p.A.at1=A.freq1/nrow(phon.pairs),
           p.B.at2=B.freq2/nrow(phon.pairs) ) # for both TP and MI p(AB) is freq of AB / # bigrams
  
  if(TP){
    phon.pairs <- phon.pairs %>%
      mutate(TP=p.AB/p.A.at1 ) %>%
      arrange(desc(TP))
    # get rank order
    TPrank <- unique(phon.pairs)
    TPrank$TP.rank <- 1:nrow(TPrank)
    phon.pairs <- left_join(phon.pairs, select(TPrank, pair, TP.rank))
  } 
  if(MI){
    phon.pairs <- phon.pairs %>% 
      mutate(MI=log2(p.AB/(p.A.at1 * p.B.at2))) %>%
      arrange(desc(MI))
    # get rank order for MI 
    MIrank <- unique(phon.pairs)
    MIrank$MI.rank <- 1:nrow(MIrank)
    phon.pairs <- left_join(phon.pairs, select(MIrank, pair, MI.rank)) 
  } 
  
  output <- unique(phon.pairs) # Only keep one instance of each syllable pair
  return(output)
}

make_streams = function(df, seg.utts=TRUE){
  if(seg.utts) phon.utts <- paste(df$phon, "###") # add utterance boundary marker to the end of every utterance
  if(!seg.utts)  phon.utts <- df$phon
  
  # replace word-internal syllable boundaries "-" with space, the same as between-word boundaries
  phon.utts <- gsub(pattern="-", replacement=" ", x=phon.utts, fixed=T) 
  # collapse phonological utterances into one continuous stream
  phon.stream <- unlist(strsplit(phon.utts, " "))
  # delete "syllables" that are just empty space
  phon.stream <- phon.stream[ !grepl(pattern="^$", x=phon.stream) ]
  
  # how many unique syllables are there?
  syllables <- unique(phon.stream)
  # syllables <- sample(syllables, 200) # for testing, just use some random syllables
  N.syl <- length(syllables)
  
  # make phon stream into a list of all of the bisyllable pairs that occur
  phon.pairs <- data.frame(syl1=phon.stream[1:length(phon.stream)-1], syl2=phon.stream[2:length(phon.stream)])
  # delete rows that code for utterance boundary (the result is that syllable pairs across utterance boundaries are simply unattested)
  if(seg.utts) phon.pairs <- dplyr::filter(phon.pairs, syl1 !="###" & syl2 !="###")
  
  # collapse orthographic utterances into one stream
  orth.stream <- unlist(strsplit(as.character(df$orth), " "))
  orth.stream <- orth.stream[!grepl(pattern="^[[:punct:]]+$", x=orth.stream)]
  
  # how many unique words are there?
  words <- unique(orth.stream)
  N.words <- length(words)
  
  output <- list(phon.stream=phon.stream, syllables=syllables, N.syl=N.syl, phon.pairs=phon.pairs, orth.stream=orth.stream, words=words, N.words=N.words)
  return(output)
}

context_results <- function(context.names, df, seg.utts=TRUE){
  context.data <- vector("list", length(context.names)) # storage variable
  names(context.data) <- context.names
  
  for(k in 1:length(context.names)){
    
    message(paste("processing ", context.names[k], "...", sep=""))
    
    df.context <- dplyr::filter(df, df[ , which(colnames(df)==context.names[k])] > 0) # select cases that have any value greater than 0 in the column that matches the name of context k
    
    context.data[[k]]$N.hits <- nrow(filter(df, df[ , which(colnames(df)==context.names[k])] == 1)) # the number of utterances that contained a key word (does not include utterances before and after)
    
    context.data[[k]]$N.utterances <- nrow(df.context)
    
    context.data[[k]]$streams <- make_streams(df.context, seg.utts=seg.utts)
    context.data[[k]]$unique.phon.pairs <- calc_MI(phon.pairs=context.data[[k]]$streams$phon.pairs, phon.stream=context.data[[k]]$streams$phon.stream)
    
    context.data[[k]]$freq.bigrams <- dplyr::summarise(group_by(context.data[[k]]$streams$phon.pairs, syl1, syl2), count=n()) # frequency of bigrams
    context.data[[k]]$freq.words <- table(context.data[[k]]$streams$orth.stream) # frequency of words
    phon.stream <- context.data[[k]]$streams$phon.stream[context.data[[k]]$streams$phon.stream != "###"]
    context.data[[k]]$freq.syl <- table(phon.stream) # frequency of syllables
  }
  return(context.data)
}

segment_speech <- function(cutoff, stat, unique.phon.pairs, phon.stream, consider.freq=FALSE, seg.utts=TRUE){
  
  if(stat=="TP") {
    TP.cutoff <- quantile(unique.phon.pairs$TP, cutoff)
    message(paste("...TP cutoff is", round(TP.cutoff, 3)))
    unique.phon.pairs$TPseg <- ifelse(unique.phon.pairs$TP < TP.cutoff, 1, 0)
  } else if(stat=="MI") {
    MI.cutoff <- quantile(unique.phon.pairs$MI, cutoff)
    message(paste("...MI cutoff is", round(MI.cutoff, 3)))
    unique.phon.pairs$MIseg <- ifelse(unique.phon.pairs$MI < MI.cutoff, 1, 0)
  } else {stop("ERROR: Enter stat='TP' or stat='MI' only")}
  
  # to consider frequency as well, only segment units that are above freqency threshold as well as above TP/MI threshold
  if(consider.freq){
    freq.cutoff <- quantile(unique.phon.pairs$AB.freq, cutoff)
    message(paste("...frequency cutoff is", round(freq.cutoff, 3)))
    unique.phon.pairs$seg <- ifelse(is.na(unique.phon.pairs$AB.freq), NA,
                                    ifelse(unique.phon.pairs$AB.freq < freq.cutoff, 0, 
                                           ifelse(unique.phon.pairs$MIseg==1 | unique.phon.pairs$TPseg==1, 1, 0)))
  } else if(stat=="TP") {
    unique.phon.pairs$seg <- unique.phon.pairs$TPseg
  } else if(stat=="MI") {
    unique.phon.pairs$seg <- unique.phon.pairs$MIseg
  } else {stop("ERROR: Enter stat='TP' or stat='MI' only")}
  
  seg.phon.stream <- phon.stream
  
  p <- progress_estimated(n=length(phon.stream)-1) # print progress bar while working
  for(i in 2:length(phon.stream)){
    
    # decide whether to place a boundary between this syllable (i) and the one before it
    if(seg.utts){
      seg <- ifelse(phon.stream[i]=="###" | phon.stream[i-1]=="###", 1, # utterance boundaries are given as word boundaries
                    dplyr::filter(unique.phon.pairs, syl1==phon.stream[i-1] & syl2==phon.stream[i])$seg)
    } else {
      seg <- dplyr::filter(unique.phon.pairs, syl1==phon.stream[i-1] & syl2==phon.stream[i])$seg
    }
    
    if(length(seg) > 1) stop(paste("ERROR at ", i, "th element of phon.stream: more than one entry for seg", sep=""))
    
    seg.phon.stream[i]<- ifelse(seg==1, 
                                paste0(",", phon.stream[i]), 
                                phon.stream[i]) # if seg=1 for this phon pair, then insert a comma before the second syllable
    print(p$tick()) # advance progress bar
  }
  
  # drop utterance boundary markers (the segmentation is still coded on the syllable after the utt boundary)
  seg.phon.stream <- seg.phon.stream[ seg.phon.stream != ",###"]
  
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
  this.dict <- dplyr::filter(dict, word %in% words)[,c("word", "phon")] 
  
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
  
  results$N.segd.units <- length(units)  # how many "words" were found in this corpus?
  
  return(results)
}

# for bootstrapping nontexts:
par_function <- function(df, dict, expand, seg.utts=TRUE, TP=TRUE, MI=TRUE, verbose=FALSE, prop=FALSE, cutoff=.85){ # this is the function that should be done in parallel on the 12 cores of each node
  if(expand & prop) stop("Cannot have both expand and prop TRUE.")
  
  context.names <- colnames(df[4:ncol(df)])
  
  # pick nontexts
  results <- nontext_cols(df=df, context_names=context.names, prop=prop) # add the nontext col
  non <- results[[1]]
  
  # add nontext columns to dataframe
  colnames(non) <- context.names
  df.non <- cbind(df[ , 1:3], non)
  
  if(expand){
    # expand windows to + - 2 utterances before and after
    df.non <- expand_windows(df.non, context.names=context.names)
  }
  
  # calculate MIs and TPs
  nontext.data <- context_results(context.names, df=df.non, seg.utts=seg.utts) # calls make_streams() and calc_MI()
  
  # segment speech
  for(k in 1:length(names(nontext.data))){
    message(paste0("processing ", names(nontext.data)[k], "..."))
    if(TP){
      nontext.data[[k]]$TP85$seg.phon.stream <- segment_speech(cutoff=cutoff, 
                                                               stat="TP", 
                                                               nontext.data[[k]]$unique.phon.pairs, 
                                                               nontext.data[[k]]$streams$phon.stream, 
                                                               seg.utts=seg.utts)
    }
    if(MI){
      nontext.data[[k]]$MI85$seg.phon.stream <- segment_speech(cutoff=cutoff, 
                                                               stat="MI", 
                                                               nontext.data[[k]]$unique.phon.pairs, 
                                                               nontext.data[[k]]$streams$phon.stream, 
                                                               seg.utts=seg.utts)
    }
  }
  
  # assess segmentation
  stat.results <- data.frame(recall=NULL, precision=NULL, stat=NULL, nontext=NULL)
  for(k in 1:length(names(nontext.data))){
    
    if(TP){
      nontext.data[[k]]$TP85$seg.results <- assess_seg(seg.phon.stream=nontext.data[[k]]$TP85$seg.phon.stream, words=nontext.data[[k]]$streams$words, dict=dict)
      TPresults <- colMeans(nontext.data[[k]]$TP85$seg.results[,3:4], na.rm=T)
      TPresults$stat <- "TP"
    }
    if(MI){
      nontext.data[[k]]$MI85$seg.results <- assess_seg(seg.phon.stream=nontext.data[[k]]$MI85$seg.phon.stream, words=nontext.data[[k]]$streams$words, dict=dict)
      MIresults <- colMeans(nontext.data[[k]]$MI85$seg.results[,3:4], na.rm=T)
      MIresults$stat <- "MI"
    }
    if(TP & MI){ 
      this.result <- as.data.frame(rbind(TPresults, MIresults))
    } else if(TP){
      this.result <- as.data.frame(rbind(TPresults))
    } else if(MI){
      this.result <- as.data.frame(rbind(MIresults))
    } else stop("At least one of MI and TP must be true.")
    
    row.names(this.result) <- NULL
    this.result$stat <- as.factor(as.character(this.result$stat))
    this.result$nontext <- names(nontext.data)[[k]]
    stat.results <- rbind(stat.results,this.result)
  } 
  stat.results$nontext <- as.factor(as.character(stat.results$nontext))
  stat.results$recall <- as.numeric(stat.results$recall)
  stat.results$precision <- as.numeric(stat.results$precision)
  
  if(verbose){
    return(list(stat.results, nontext.data) )
  } else {
    return(stat.results)
  }
}


# make an artificial language
make_corpus <- function(dist=c("unif", "skewed"), N.utts=1000, N.types=1800, smallest.most.freq=FALSE, monosyl=FALSE){ 
  N.type <- round(N.types/3, 0)
  if(!monosyl){
    message(paste0("\nGenerating ", N.type, " types each for 2, 3, and 4-syllables words...\n") )
  } else {
    message(paste0("\nGenerating ", N.type, " types each for 1, 2, and 3-syllables words...\n") )
  }
  
  # generate all possible syllables from these consonants and these vowels
  Cs <- c("b", "c", "ch", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "r", "s", "sh", "t", "v", "w", "z")
  Vs <- c("a", "e", "i", "o", "u", "A", "E", "I", "O", "U")
  syls <- NULL
  for(a in 1:length(Cs)){
    for(b in 1:length(Vs)){
      for(c in 1:length(Cs)){
        syl <- paste0(Cs[a], Vs[b], Cs[c]) # syllables have a CVC structure
        syls <- c(syls, syl)
      }
    }
  }
  syls <- base::sample(syls, size=length(syls)) # shuffle the syllables randomly
  
  words <- NULL
  if( !monosyl ){
    for(j in seq(from=1, by=9, to=9*N.type) ){
      # generating one 2-syl word, one 3-syl word, and one 4-syl word takes 9 syllables
      # don't want to re-use any syllables (as per Kurumada, Meylan & Frank, 2013), so this steps through the syls object 9 at a time
      word2 <- paste(syls[(j + 0):(j + 1)], collapse="-")
      word3 <- paste(syls[(j + 2):(j + 4)], collapse="-")
      word4 <- paste(syls[(j + 5):(j + 8)], collapse="-")  
      
      words <- c(words, word2, word3, word4) # add these words to the list
    }
  } else {
    for(j in seq(from=1, by=6, to=6*N.type) ){
      # generating one 1-syl, 2-syl word, and one 3-syl word word takes 6 syllables
      # don't want to re-use any syllables (as per Kurumada, Meylan & Frank, 2013), so this steps through the syls object 6 at a time
      word1 <- paste(syls[(j + 0)], collapse="-")
      word2 <- paste(syls[(j + 1):(j + 2)], collapse="-")
      word3 <- paste(syls[(j + 3):(j + 5)], collapse="-") 
      
      words <- c(words, word1, word2, word3) # add these words to the list
    }
  }
  
  # check to make sure there are no missing vlaues in the words
  if( any(grepl(x=words, pattern="NA")) ) stop(paste("Not enough unique syllables to make", N.types, "word types."))
  
  if( !smallest.most.freq ){
    words <- base::sample(words, size=length(words), replace=FALSE) # shuffle the order of the words (the first word here will end up being the most frequent in the skewed dist) 
  } 
  
  dict <- data.frame(word=gsub(x=words, pattern="-", replacement=""), phon=words)
  
  size <- N.utts*4 # at an average of 4 words per utterance (as per Kurumada, Meylan & Frank, 2013), there will be size tokens in the corpus
  
  if(dist=="unif"){
    reps <- round(size/length(words), 0) # the number of repetitions of each word should be based on the total size of the corpus  
    corpus.words <- c(rep(words, reps) )
    
  } else if(dist=="skewed"){
    # frequency should be proportional to 1/rank (Zipfian dist)
    reps <- NULL
    for(i in 1:length(words)){
      rep <- 1/i
      reps <- c(reps, rep)
    }
    constant <- 1/sum(reps)*size # the constant we need to multiply reps by to get the correct total number of tokens
    reps <- round(reps * constant, 0)
    sum(reps)
    
    corpus.words <- NULL
    for(i in 1:length(words)){
      word.reps <- rep(words[i], reps[i])
      corpus.words <- c(corpus.words, word.reps)
    }
    
  } else stop("dist must be either uniform or skewed")
  
  corpus <- base::sample(corpus.words, length(corpus.words) ) # shuffle the words randomly 
  
  # break it into utterances with 4 words each
  library(BBmisc)
  corpus.list <- chunk(corpus, chunk.size=4)
  corpus <- unlist(lapply(corpus.list, FUN=paste, collapse=" "))
  
  # make text files with columns for utt, orth and phon
  orth <- gsub(x=corpus, pattern="-", replacement="")
  phon <- corpus
  utt <- paste0("utt", seq(from=1, to=length(corpus)))
  
  df <- data.frame(utt=utt, orth=orth, phon=phon)
  
  return( list(df, dict) )
}

plot_corpus_dist <- function(corpus){
  x <- corpus %>%
    separate(col=orth, into=c("word1", "word2", "word3","word4"), sep=" ", extra="drop") %>%
    gather(key="key", value="value", starts_with("word")) %>%
    select(word=value)
  plot(sort(table(x$word), decreasing = TRUE), ylab="word freq", xlab="word rank")
}

contexts_by_size <- function(df=read.table("utt_orth_phon_KEY.txt", header=1, sep="\t", stringsAsFactors=F, quote="", comment.char ="") , N.sizes, min.utt=100){
  start.columns <- ncol(df)
  
  # Add columns for each "context", with increasing number of utterances. 
  # It doesn't matter which utterances are selected for each column since they'll all get shuffled anyway during the bootstrapping process
  max.utt <- nrow(df) - min.utt # a sample that includes all of the utterances in the corpus won't have any variability from sample to sample
  sizes <- round(seq(from=min.utt, to=max.utt, length.out=N.sizes), 0)
  # add a column to the dataframe for each  corpus size to try
  for(s in 1:N.sizes){
    df[[paste0("N.utts", sizes[s])]] <- c( rep(1, sizes[s]), rep(0, nrow(df)-sizes[s]) )
  }
  return(df)
}

process_batch_results <- function(id, dir){
  results <- data.frame(V1=NULL)
  id <- paste0(dir, "/", id)

  nodes <- list.files(paste0(id, "-files/jobs"))
  if(length(nodes)==0) stop("No completed jobs available. Check dir and id to make sure they're correct.")
  
  for(i in 1:length(nodes)){
    load(paste0(id, "-files/jobs/", nodes[i], "/", as.numeric(nodes[i]), "-result.RData"))
    results <- rbind(results, result)
  }
  return(results)
  # saveRDS(results, file=paste0("batchresults_WL.rds") )
}

check_seed_words <- function(seg.results){
  results <- dplyr::filter(seg.results, context!="none" & !is.na(context))
  return(results)
}

plot_seg_results <- function(seg.results, title=NULL, boxplot=TRUE, scatterplot=FALSE, by=c("syl", "contexts")){
  # add break by N.syl option (check whether 1, 2, + syllable words are getting segmented correctly)
  plot <- ggplot(seg.results, aes(x=seg.result, y=freq.segd)) 
  if(boxplot) plot <- plot + geom_boxplot()
  if(scatterplot & by=="syl") plot <- plot + geom_point(aes(color=syl.bins), alpha=.7, size=4, position = position_jitter(w = .3, h = 0)) 
  if(scatterplot & by=="contexts") plot <- plot + geom_point(aes(color=context), alpha=.7, size=4, position = position_jitter(w = .3, h = 0)) 
  plot <- plot + 
    scale_y_log10() + labs(y="Log10(frequency)", x=NULL, title=title) + 
    scale_x_discrete(limits=c("miss", "hit")) + 
    coord_flip() +
    theme(text = element_text(size=30))
  return(plot)
}

results_descriptives <- function(data, criteria=c("MI85", "TP85"), context="global"){
  N.syl <- data$streams$N.syl
  words.tokens <- length(data$streams$orth.stream)
  words.types <- data$streams$N.words
  N.utt <- data$N.utterances
  N.seed.words <- ifelse(context=="global", NA, length(grep(context, data$TP85$seg.results$context, fixed=TRUE)))
  
  descriptives <- data.frame(context=context, 
                             N.seed.words=N.seed.words, 
                             N.utt=N.utt,
                             N.unique.words=words.types,
                             criterion=criteria,
                             N.hits=NA,
                             N.misses=NA,
                             N.false.alarms=NA)
  descriptives$N.hits <- ifelse(descriptives$criterion=="TP85", summary(data$TP85$seg.results$seg.result)[2],
                                ifelse(descriptives$criterion=="MI85", summary(data$MI85$seg.results$seg.result)[2], NA))
  descriptives$N.misses <- ifelse(descriptives$criterion=="TP85", summary(data$TP85$seg.results$seg.result)[3],
                                ifelse(descriptives$criterion=="MI85", summary(data$MI85$seg.results$seg.result)[3], NA))
  descriptives$N.false.alarms <- ifelse(descriptives$criterion=="TP85", summary(data$TP85$seg.results$seg.result)[1],
                                ifelse(descriptives$criterion=="MI85", summary(data$MI85$seg.results$seg.result)[1], NA))
return(descriptives)
}


# combine_results <- function(data){
#   combined.results <- data[[1]]$MI85$seg.results
#   combined.results$context.corpus <- colnames(contexts)[1]
#   for(k in 2:length(colnames(contexts))){
#     seg.results <- context.data[[k]]$MI85$seg.results
#     seg.results$context.corpus <- colnames(contexts)[k]
#     combined.results <- rbind(combined.results,seg.results)
#   }
#   
#   combined.results$context.corpus <- as.factor(combined.results$context.corpus)
# }


network_plot <- function(data, title=""){
  unique.phon.pairs <- data$unique.phon.pairs
  bsk.network<-graph.data.frame(unique.phon.pairs, directed=T) # edge list, with additional attributes
  bad.edges <- E(bsk.network)[E(bsk.network)$MI < quantile(unique.phon.pairs$MI, .85)] #identify those edges with MI below the cutoff for segmentation
  bsk.network<-delete.edges(bsk.network, bad.edges) #exclude them from the graph
  
  
  
  V(bsk.network)$size <- table(data$streams$phon.stream) # size nodes by frequency 
  message(paste("max freq", max(V(bsk.network)$size)))
  V(bsk.network)$size <- V(bsk.network)$size/(.03*max(V(bsk.network)$size)) # size nodes by frequency 
  bad.vs <- V(bsk.network)[V(bsk.network)$size < .5] #identify those nodes with freq below 5
  bsk.network<-delete.vertices(bsk.network, bad.vs) #exclude them from the graph
  
  # For example we can separate some vertices (people) by color:
  V(bsk.network)$color<-ifelse(V(bsk.network)$name=="'bIg", 'darkgreen', 'lightgrey') #useful for highlighting certain people. Works by matching the name attribute of the vertex to the one specified in the 'ifelse' expression
  E(bsk.network)$width <- E(bsk.network)$MI/5
  E(bsk.network)$arrow.width <- .3
  
  
  plot(bsk.network, vertex.label=ifelse(V(bsk.network)$name=="'tV", "'tV", ""))
  return(plot.igraph(bsk.network, vertex.label="", main=title))
}

