
nontext_cols <- function(df){
  non <- dplyr::select(df, -utt, -orth, -phon)
  message(paste("nontext_cols using all but the following columns:", paste(colnames(df)[!colnames(df) %in% colnames(non)], collapse=", ")))
  context.names <- colnames(non)
  
  # shuffle the context columns
  for(k in 1:length(context.names)){
    non[[context.names[k]]] <- base::sample(non[[context.names[k]]], 
                                            size=nrow(df), 
                                            replace=FALSE)
    colnames(non)[k] <- context.names[k]
  }
  return(non)
}  

sample_probs <- function(df){
  sample <- dplyr::select(df, -utt, -orth, -phon)
  # change probabilities into 1 or 0 probabalistically based on their values
  message(paste("sample_probs using all but the following columns:", paste(colnames(df)[!colnames(df) %in% colnames(sample)], collapse=", ")))
  
  for(r in 1:nrow(sample)){
    for(c in 1:ncol(sample)){
      sample[r,c] <- sample(x=c(0,1), 
                         size=1, 
                         prob=c(1-sample[r,c], sample[r,c]))
    }
  }
  return(sample)
}
  
expand_windows <- function(df, context.names){
  contextcols <- which(colnames(df) %in% context.names)
  stopifnot(require(dplyr), require(tidyr))
  stopifnot(length(contextcols) + 3 == ncol(df))
  
  # clean out any codes that aren't 1 or 0
  df <- df %>%
    gather(key=key, value=value, contextcols) %>%
    mutate(value=ifelse(value==1, 1, 0)) %>% # if it's a 1 leave it, otherwise 0
    spread(key=key, value=value) # note that this step re-orders the columns alphabetically
  
  contextcols <- which(colnames(df) %in% context.names) # re-check to make sure this is still accurate
  
  p <- progress_estimated(n=length(3:(nrow(df)-2))) # print progress bar while working
  for(i in 3:(nrow(df)-2)){
    for(j in contextcols){
      df[i,j] <- ifelse(df[i,j]==1, df[i,j], # if it is already marked 1, leave it
                        ifelse(df[(i-2),j]==1, 1.5, # if the utterance 2 before it is marked 1, mark 1.5
                               ifelse(df[(i-1),j]==1, 1.5, # if the utterance 1 before it is marked 1, mark 1.5
                                      ifelse(df[(i+1),j]==1, 1.5, # if the utterance 1 after it is marked 1, mark 1.5
                                             ifelse(df[(i+2),j]==1, 1.5, 0))))) # if the utterance 2 after it is marked 1, mark 1.5. Otherwise mark 0.
      # note that utterances classified as a context based on their proximity to an utterance with a key word must be marked with something other than 1 to prevent them being used as key utterances in the next row
      
    }
    print(p$tick()) # advance progress bar
  }
  # the above code skips the first 2 lines and last 2 lines, so go ahead and fill those in now
  for(j in contextcols){
    i <- 1
    df[i,j] <- ifelse(df[i,j]==1, df[i,j], # if it is already marked 1, leave it
                      ifelse(df[(i+1),j]==1, 1.5, # if the utterance 1 after it is marked 1, mark 1.5
                             ifelse(df[(i+2),j]==1, 1.5, 0))) # if the utterance 2 after it is marked 1, mark 1.5. Otherwise mark 0.
    i <- 2
    df[i,j] <- ifelse(df[i,j]==1, df[i,j], # if it is already marked 1, leave it
                      ifelse(df[(i-1),j]==1, 1.5, # if the utterance 1 before it is marked 1, mark 1.5
                             ifelse(df[(i+1),j]==1, 1.5, # if the utterance 1 after it is marked 1, mark 1.5
                                    ifelse(df[(i+2),j]==1, 1.5, 0)))) # if the utterance 2 after it is marked 1, mark 1.5. Otherwise mark 0.
    i <- nrow(df)-1
    df[i,j] <- ifelse(df[i,j]==1, df[i,j], # if it is already marked 1, leave it
                      ifelse(df[(i-2),j]==1, 1.5, # if the utterance 2 before it is marked 1, mark 1.5
                         ifelse(df[(i-1),j]==1, 1.5, # if the utterance 1 before it is marked 1, mark 1.5
                             ifelse(df[(i+1),j]==1, 1.5, 0)))) # if the utterance 1 after it is marked 1, mark 1.5. Otherwise mark 0.
                                  
    i <- nrow(df)
    df[i,j] <- ifelse(df[i,j]==1, df[i,j], # if it is already marked 1, leave it
                      ifelse(df[(i-2),j]==1, 1.5, # if the utterance 2 before it is marked 1, mark 1.5
                             ifelse(df[(i-1),j]==1, 1.5, 0))) # if the utterance 1 before it is marked 1, mark 1.5. Otherwise mark 0.
                                    
  }
  
  return(df)
}

calc_MI = function(phon.pairs, phon.stream, MI=TRUE, TP=TRUE){
  # mutual information, and transitional probabilty. See Swingley (2005) p97

  stopifnot(require(tidyr))
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
    phon.pairs <- left_join(phon.pairs, A.at1, by="syl1")
    phon.pairs <- left_join(phon.pairs, B.at2, by="syl2")

    syls <- data.frame(syl=phon.stream[phon.stream != "###"], stringsAsFactors=FALSE) %>%
      count(syl) %>%
      mutate(p=n/length(unique(phon.stream[phon.stream != "###"])))
    # merge in sylable info for syl1
    colnames(syls) <- c("syl1", "A.freq.tot", "p.A")
    phon.pairs <- left_join(phon.pairs, syls, by="syl1")
    # merge in sylable info for syl2
    colnames(syls) <- c("syl2", "B.freq.tot", "p.B")
    phon.pairs <- left_join(phon.pairs, syls, by="syl2")
  
  # add a column for the syllable pair (helps for joining this table with pairs.count later)
  phon.pairs <- phon.pairs %>%
    unite(pair, syl1, syl2, sep="-", remove=F)
  
  # calculate the number of times this syllable pair occurs
  pairs.count <- phon.pairs %>%
    group_by(pair) %>%
    summarize(AB.freq=n())
  phon.pairs <- left_join(phon.pairs, pairs.count, by="pair") # add the AB frequency counts to the phon.pairs table
  
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
    phon.pairs <- left_join(phon.pairs, select(TPrank, pair, TP.rank), by="pair")
  } 
  if(MI){
    phon.pairs <- phon.pairs %>% 
      mutate(MI=log2(p.AB/(p.A.at1 * p.B.at2))) %>%
      arrange(desc(MI))
    # get rank order for MI 
    MIrank <- unique(phon.pairs)
    MIrank$MI.rank <- 1:nrow(MIrank)
    phon.pairs <- left_join(phon.pairs, select(MIrank, pair, MI.rank), by="pair") 
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
  phon.stream <- phon.stream[ !grepl(pattern="^[[:space:]]*$", x=phon.stream) ]
  
  # make phon stream into a list of all of the bisyllable pairs that occur
  phon.pairs <- data.frame(syl1=phon.stream[1:length(phon.stream)-1], syl2=phon.stream[2:length(phon.stream)], stringsAsFactors=FALSE)
  # delete rows that code for utterance boundary (the result is that syllable pairs across utterance boundaries are simply unattested)
  if(seg.utts) phon.pairs <- dplyr::filter(phon.pairs, syl1 !="###" & syl2 !="###")
  
  # collapse orthographic utterances into one stream
  orth.stream <- unlist(strsplit(as.character(df$orth), " "))
  orth.stream <- orth.stream[!grepl(pattern="^[[:punct:]]+$", x=orth.stream)] # remove any "words" that are only punctuation marks
  orth.stream <- orth.stream[!grepl(pattern="^[[:space:]]*$", x=orth.stream)] # remove any "words" that are only blank space
  
  # STATS TO COLLECT
  # how many syllables are there?
  syllables <- unique(phon.stream[ !grepl(pattern="###", x=phon.stream) ])
  # syllables <- sample(syllables, 200) # for testing, just use some random syllables
  N.syl.types <- length(syllables)
  N.syl.tokens <- length(phon.stream[ !grepl(pattern="###", x=phon.stream) ])
  
  # how many unique words are there?
  words <- unique(orth.stream)
  N.types <- length(words)
  N.tokens <- length(orth.stream)
  
  # how long are the utterances?
  orth.stats <- gsub(x=df$orth, pattern="_", replacement=" ", fixed=TRUE) # removing underscores
  words.in.utts <- sapply(orth.stats, strsplit, split=" ")
  words.per.utt <- sapply(words.in.utts, length)
  
  syls.in.utts <- sapply(df$phon, strsplit, split=" ")
  syls.per.utt <- sapply(syls.in.utts, length)
  
  output <- list(phon.stream=phon.stream, syllables=syllables, N.syl.types=N.syl.types, N.syl.tokens=N.syl.tokens, phon.pairs=phon.pairs, orth.stream=orth.stream, words=words, N.types=N.types, N.tokens=N.tokens, words.per.utt=words.per.utt, syls.per.utt=syls.per.utt)
  return(output)
}

context_results <- function(df, seg.utts=TRUE){
  if(ncol(df) > 3){
    message(paste("\ncontext_results using all but the following columns:", paste(colnames(df)[1:3], collapse=", ")))
    # retains contexts as they were used for column names, regardless of where they came from (works for any context defining method)
    context.names <- colnames(df)[4:ncol(df)]
  } else {
    message("\nNo context columns in df, so analyzing as global.")
    df$global <- 1 # add a column of 1's, to include everything
    # if there are no context columns, analyze as global
    context.names <- as.vector("global")
  }
  context.data <- vector("list", length(context.names)) # storage variable
  names(context.data) <- context.names
  
  for(k in 1:length(context.names)){
    
    message(paste0("make_streams and calc_MI on ", context.names[k], "..."))
    
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

seg <- function(phon.stream, unique.phon.pairs, seg.utts=TRUE, quiet=TRUE){
  if(!quiet) message("leave utterance boundaries? ", seg.utts)
  
  if( length(phon.stream) == 1 ) { # if phon.stream isn't already vectorized syllables, make it so
    phon.stream <- gsub(x=phon.stream, pattern=" ", replacement="-")
    phon.stream <- strsplit(phon.stream, split="-")[[1]]
  }
  
  message("...segmenting phon stream...")
  seg.phon.stream <- phon.stream
  if( length(phon.stream) > 1 ){
    for(i in 2:length(phon.stream)){
      
      this.pair <- dplyr::filter(unique.phon.pairs, syl1==phon.stream[i-1] & syl2==phon.stream[i])
      # decide whether to place a boundary between this syllable (i) and the one before it
      if(seg.utts){
        seg <- ifelse(phon.stream[i]=="###" | phon.stream[i-1]=="###", 1, # utterance boundaries are given as word boundaries
                      this.pair$seg)
      } else {
        seg <- this.pair$seg
      }
      if(!quiet) message("position ", i, " in phon.stream\n", "sy1: ", phon.stream[i-1], "\nsyl2: ", phon.stream[i], "\nseg: ", seg)

      if(length(seg) != 1) stop(paste("ERROR at ", i, "th element of phon.stream: more or less than one entry for seg", sep=""))
      
      seg.phon.stream[i]<- ifelse(seg==1, 
                                  paste0(",", phon.stream[i]), 
                                  phon.stream[i]) # if seg=1 for this phon pair, then insert a comma before the second syllable
    } # end for loop
  } # end if statement
  
  # drop utterance boundary markers (the segmentation is still coded on the syllable after the utt boundary)
  seg.phon.stream <- seg.phon.stream[ seg.phon.stream != ",###"]
  
  return(seg.phon.stream)
}

segment_speech <- function(cutoff, stat, data, consider.freq=FALSE, seg.utts=TRUE, quiet=TRUE){
  unique.phon.pairs <- as.tbl(data$unique.phon.pairs) # making it a tbl for speed
  phon.stream <- data$streams$phon.stream
  
  if(stat=="TP") {
    TP.cutoff <- quantile(unique.phon.pairs$TP, cutoff)
    message(paste("...TP cutoff is", round(TP.cutoff, 3)))
    unique.phon.pairs$TPseg <- ifelse(unique.phon.pairs$TP < TP.cutoff, 1,
                                      ifelse(unique.phon.pairs$TP >= TP.cutoff, 0, NA))
  } else if(stat=="MI") {
    MI.cutoff <- quantile(unique.phon.pairs$MI, cutoff)
    message(paste("...MI cutoff is", round(MI.cutoff, 3)))
    unique.phon.pairs$MIseg <- ifelse(unique.phon.pairs$MI < MI.cutoff, 1,
                                        ifelse(unique.phon.pairs$MI >= MI.cutoff, 0, NA))
  } else {stop("ERROR: Enter stat='TP' or stat='MI' only")}
  
  # to consider frequency as well, only segment units that are above freqency threshold as well as above TP/MI threshold
  freq.cutoff <- NA
  if(consider.freq){
    # From Swingley2005: "To use a consistent frequency metric, frequency for units of all lengths was relativized to monosyllable frequency; thus, a bisyllable was considered frequent if it was as common as (e.g.) 85% of all monosyllables. This use of a single frequency metric seemed more reasonable than assuming that whether sequence feels frequent to a child depends upon how long it is."
    syl.freqs <- table(phon.stream[ !grepl(pattern="###", x=phon.stream) ])
    freq.cutoff <- quantile(syl.freqs, cutoff)
    message(paste("...frequency cutoff is", round(freq.cutoff, 3)))
    unique.phon.pairs$freqseg <- ifelse(unique.phon.pairs$AB.freq < freq.cutoff, 1,
                                    ifelse(unique.phon.pairs$AB.freq >= freq.cutoff, 0, NA))
    
    if(stat=="TP") unique.phon.pairs$seg <- ifelse(unique.phon.pairs$TPseg==1 | unique.phon.pairs$freqseg==1, 1, 0)
    if(stat=="MI") unique.phon.pairs$seg <- ifelse(unique.phon.pairs$MIseg==1 | unique.phon.pairs$freqseg==1, 1, 0)
    
  } else if(stat=="TP") {
    unique.phon.pairs$seg <- unique.phon.pairs$TPseg
  } else if(stat=="MI") {
    unique.phon.pairs$seg <- unique.phon.pairs$MIseg
  } else stop("ERROR: Enter stat='TP' or stat='MI' only")
  
#   # faster version # http://blogs.uoregon.edu/rclub/2015/11/03/using-dplyr-to-batch-analyses/
#   # requires df
#   unique.utts <- data.frame(phon=unique(df$phon)) %>%
#     group_by(phon)
#   segd.utts <- unique.utts %>%
#     do({
#       seg.utt <- seg(.$phon, unique.phon.pairs, seg.utts=seg.utts)
#       seg.utt <- data.frame(phon=paste0(seg.utt, collapse = "-"))
#     })
#   ##################
  
  seg.phon.stream <- seg(phon.stream, unique.phon.pairs, seg.utts=seg.utts, quiet=quiet)
  
  return(list(seg.phon.stream=seg.phon.stream, freq.cutoff=freq.cutoff))
}

assess_seg <- function(seg.phon.stream, streams, dict, freq.cutoff=NULL, embedding.rule=FALSE, trisyl.limit=FALSE){
  words <- streams$words
  # extract units from segmented stream
  collapsed.temp <- paste(seg.phon.stream, collapse="-")
  collapsed <- paste(collapsed.temp, "-", sep="") # add a - to the very end, so every unit will have - as the last character
  collapsed.unseg <- gsub(",", "", collapsed, fixed=T) # make a version of the collapsed stream with commas removed, for frequency counts
  units.temp <- strsplit(collapsed, ",", fixed=T)[[1]]
  units <- gsub('.{1}$', "", units.temp) # remove the trailing - on each segmented unit
  unique.units <- data.frame(phon=unique(units), stringsAsFactors=FALSE )
  
  # compare extracted units to dictionary words
  this.dict <- dplyr::filter(dict, word %in% words)[,c("word", "phon", "N.syl")] 
  
  # add N.syl for units that aren't in the dictionary
  unique.units$N.syl <- NA
  for(i in 1:nrow(unique.units)){
    unique.units$N.syl[i] <- length(strsplit(as.character(unique.units$phon[i]), split="-", fixed=TRUE)[[1]])
  }
  unique.units <- filter(unique.units, N.syl > 0) # this removes a line that's a blank phon 
  
  units.freq <- as.data.frame(table(units), stringsAsFactors=FALSE)
  unique.units <- left_join(unique.units, units.freq, by=c("phon" = "units")) %>% 
    rename(unit.freq=Freq)
  
  if(!is.na(freq.cutoff)){
    # from Swingley2005: "Monosyllables were considered wordlike if they exceeded the criterial frequency percentile."
    syls.freq <- as.data.frame(table(streams$phon.stream[ !grepl(pattern="###", x=streams$phon.stream) ]), stringsAsFactors=FALSE)
    unique.units <- left_join(unique.units, syls.freq, by=c("phon" = "Var1"))
    
    # if it is monosyllabic and less than freq.cutoff, then remove
    unique.units$remove <- ifelse(unique.units$N.syl > 1, 0,
                                  ifelse(unique.units$Freq >= freq.cutoff, 0,
                                         ifelse(unique.units$Freq < freq.cutoff, 1, NA)))
    message(paste("filtering out", sum(unique.units$remove, na.rm=T), "monosyllabic words because they do not meet frequency cutoff."))
    unique.units <- unique.units %>% 
      filter(remove != 1) %>% 
      select(-remove, -Freq)
    
    # note that bisyllabic units are taken care of during segment_speech()
    
    # trisyllabic units need to meet threshold as well
    
    # if it is trisyllabic and less than freq.cutoff, then remove
    unique.units$remove <- ifelse(unique.units$N.syl < 3, 0, # mono- and bi-syllabic units already taken care of
                                  ifelse(unique.units$unit.freq >= freq.cutoff, 0,
                                         ifelse(unique.units$unit.freq < freq.cutoff, 1, NA)))
    message(paste("filtering out", sum(unique.units$remove, na.rm=T), "trisyllabic words because they do not meet frequency cutoff."))
    unique.units <- unique.units %>% 
      filter(remove != 1) %>% 
      select(-remove)
    
    message(paste(nrow(unique.units), "unqiue units segmented"))
  }
  
  if(trisyl.limit){
    # from Swingley2005: "Sequences longer than three syllables were not evaluated for wordhood, on the grounds that (1) almost no 4-syllable sequences would exceed the mutual information criteria, and (2) over 96% of Dutch and English word types have fewer than 4 syllables, and the longer ones are nearly always rare. Thus, modeling the acquisition of 1-, 2-, and 3-syllable words provides good coverage of the infants lexical environment."
    message(paste0("Removing ", sum(unique.units$N.syl >=4) , " units (", 100*round(sum(unique.units$N.syl >=4)/nrow(unique.units), 3), "% of all seg'd units) because they are longer than 3 syllables."))
    unique.units <- filter(unique.units, N.syl < 4) # this removes all units longer than 3 syllables
  }
  
  if(embedding.rule){
    # From Swingley2005: "Postulated words embedded in other postulated words were excluded. For example, suppose that the bisyllable lephone met the bigram criteria, and the trisyllable telephone met the trigram criteria. Only telephone would be considered a word. This exclusion was motivated by the finding that infants extracting a bisyllabic word (e.g., kingdom) do not appear to treat the stressed syllable of that word (e.g., king) as familiar (Jusczyk et al., 1999)."
    unique.units$remove <- NA
    unique.units$phon <- as.character(unique.units$phon)
    temp.unique.units <- data.frame(NULL)
    for(s in sort(unique(unique.units$N.syl)) ){
      # for each unit length (number of syllables) observed, remove any smaller units that contain its syllables
      this.length <- filter(unique.units, N.syl==s)
      # print(s)
      for(i in 1:nrow(this.length)){
        this.phon <- this.length$phon[i]
        # surround each character with [] so it will be treated literally for regex
        test.phon <- paste(paste0("[", strsplit(this.phon, split=NULL)[[1]], "]"), collapse="")
        
        remove.beg <- grep(pattern=paste0("^", test.phon, "-"), x=filter(unique.units, N.syl > s)$phon)
        remove.mid <- grep(pattern=paste0("-", test.phon, "-"), x=filter(unique.units, N.syl > s)$phon)
        remove.end <- grep(pattern=paste0("-", test.phon, "$"), x=filter(unique.units, N.syl > s)$phon)
        remove <- unique(c(remove.beg, remove.mid, remove.end))
        
        this.length$remove[i] <- ifelse(length(remove) > 0, 1, 0) # if there is at least one word that contains this syllable/these syllables, remove=1, otherwise remove=0
        # print(this.phon)
        # print(filter(unique.units, N.syl > s)[remove, ])
      } # end phon unit for loop
      temp.unique.units <- rbind(temp.unique.units, this.length)
    } # end syllable for loop
    message(paste0("Removing ", sum(temp.unique.units$remove), " units (", 100*round(sum(temp.unique.units$remove)/nrow(unique.units), 3), "% of all seg'd units) because they occur within larger units (Swingley2005 embedding constraint)."))
    unique.units <- temp.unique.units %>% 
      filter(remove != 1) %>% 
      select(-remove)
  } 
  
  # number of hits and false alarms
  unique.units$precision <- ifelse(unique.units$phon %in% this.dict$phon, 1, 0) # if this segmented unit is in the dict it's a hit, if it's not in the dictionary it's a false alarm
  # number of hits and misses
  this.dict$recall <- ifelse(this.dict$phon %in% unique.units$phon, 1, 0) # if this dictionary entry is in the segmented units, it's a hit. Otherwise it's a miss. 
  
  results <- dplyr::full_join(this.dict, unique.units, by=c("phon", "N.syl"))
  
  # segmentation result
  results$seg.result <- as.factor(ifelse(results$recall==1 & results$precision==1, "hit", 
                               ifelse(results$recall==0 & is.na(results$precision), "miss",
                                      ifelse(is.na(results$recall) & results$precision==0, "false alarm",
                                        NA))))
  print(summary(results$seg.result))

  # add frequency for each word
  word.freq <- as.data.frame(table(streams$orth.stream), stringsAsFactors=FALSE)
  results <- left_join(results, word.freq, by=c("word"="Var1"))
  results$freq <- ifelse(results$seg.result=="miss", results$Freq, results$unit.freq)
  results <- select(results, word, phon, recall, precision, seg.result, N.syl, freq, unit.freq) %>% 
    rename(freq.segd=unit.freq)
  
  print(summary(results))

  results$N.segd.units <- length(units)  # how many "words" were found in this corpus?
  
  message(paste(length(units), "unique units kept after exclusions and everything."))
  
  return(results)
}

# for bootstrapping nontexts:
par_function <- function(x, dict=NULL, consider.freq=FALSE, embedding.rule=FALSE, trisyl.limit=FALSE, N.types=NULL, N.utts=NULL, by.size=TRUE, expand=FALSE, seg.utts=TRUE, TP=TRUE, MI=TRUE, verbose=FALSE, prop=FALSE, cutoff=.85, nontext=TRUE, fun.version=NULL, quiet=TRUE){ # this is the function that should be done in parallel on the 12 cores of each node
  message("************************\nbeginning par_function\n************************")
  # accept arguments as a list
  if (is.list(x)) {
    if (all(c("dataframe", "dict") %in% names(x))) {
      dataframe <- x[["dataframe"]]
      dict <- x[["dict"]]
      if("consider.freq" %in% names(x)) consider.freq <- x[["consider.freq"]]
      if("embedding.rule" %in% names(x)) embedding.rule <- x[["embedding.rule"]] 
      if("trisyl.limit" %in% names(x)) trisyl.limit <- x[["trisyl.limit"]] 
      if("N.types" %in% names(x)) N.types <- x[["N.types"]]
      if("N.utts" %in% names(x)) N.utts <- x[["N.utts"]]
      if("by.size" %in% names(x)) by.size <- x[["by.size"]]
      if("expand" %in% names(x)) expand <- x[["expand"]]
      if("seg.utts" %in% names(x)) seg.utts <- x[["seg.utts"]]
      if("TP" %in% names(x)) TP <- x[["TP"]]
      if("MI" %in% names(x)) MI <- x[["MI"]]
      if("verbose" %in% names(x)) verbose <- x[["verbose"]]
      if("prop" %in% names(x)) prop <- x[["prop"]]
      if("cutoff" %in% names(x)) cutoff <- x[["cutoff"]]
      if("nontext" %in% names(x)) nontext <- x[["nontext"]]
      if("quiet" %in% names(x)) quiet <- x[["quiet"]]
      if("fun.version" %in% names(x)) fun.version <- x[["fun.version"]]
    } else stop("arguments are a list, but does not have components 'dataframe' and 'dict'")
  } else dataframe <- x
  
  if(expand & prop) stop("Cannot have both expand and prop TRUE - expand does not work with prop.")
  if(!any(MI, TP)) stop("At least one of MI and TP must be true.")
  if(!is.character(dataframe) & !is.null(N.types)) message("NOTE: Cannot specify N.types when providing a real dataframe.")

  if(is.character(dataframe)){
    # if a corpus isn't given, generate an artificial one
    if(is.null(N.types)) N.types <- 1800 # default value of 1800 types
    if(is.null(N.utts)) N.utts <- 1000 # default value of 1000 utterances
    
    if(dataframe=="skewed"){
      lang <- make_corpus(dist="skewed", N.utts=N.utts, N.types=N.types, smallest.most.freq=FALSE, monosyl=TRUE)
      corpus <- lang[[1]] # the corpus
      dict <- lang[[2]] # the dictionary
    } else if(dataframe=="unif"){
      lang <- make_corpus(dist="unif", N.utts=N.utts, N.types=N.types, smallest.most.freq=FALSE, monosyl=TRUE)
      corpus <- lang[[1]] # the corpus
      dict <- lang[[2]] # the dictionary
    } else stop("Only recognized dists are 'skewed' or 'unif'.")
    
    df <- corpus
    
    if(!by.size){
      df$N.uttAll <- 1 # make a column of ones for the whole corpus (i.e. use everything)
    }
  } 
  if(is.data.frame(dataframe)) df <- dataframe # if the given dataframe is a data.frame, then use that
  
  if(by.size){
    df <- dplyr::select(df, utt, orth, phon) # only keep the utt, orth, and phon columns
    # use that corpus to generate a size sim contexts file
    df <- contexts_by_size(df=df, N.sizes=20, min.utt=100)
  }
    
  if(nrow(df) == 0) stop("df didn't load")
  if(nrow(dict) == 0) stop("dict didn't load")
  if(ncol(df) > 3){
    context.names <- colnames(df[4:ncol(df)])
  } else {
    context.names <- as.vector("global")
  }
  
  
  if(prop){
    # change probabilities into 1 or 0 probabalistically based on their values
    sample <- sample_probs(df=df) 
    # replace probabalistic columns with binary ones
    df <- cbind(df[ , 1:3], sample)
  }
  
  if(nontext & ncol(df)>4){
    # pick nontext utterances
    non <- nontext_cols(df=df) 
    # add nontext columns to dataframe
    df <- cbind(df[ , 1:3], non)
  }
  
  if(expand){
    # expand windows to + - 2 utterances before and after
    df <- expand_windows(df, context.names=context.names)
  }
  
  # calculate MIs and TPs
  data <- context_results(df=df, seg.utts=seg.utts) # calls make_streams() and calc_MI()
  addl.info <- vector("list", length(names(data))); names(addl.info) <- names(data) # empty storage variable
  
  # segment speech and assess segmentation
  # note that this loop is slow
  freq.cutoff <- list(TP85=NULL, MI85=NULL)
  for(k in 1:length(names(data))){ 
    message(paste0("segmenting speech for ", names(data)[k], "..."))
    if(TP){
      seg_speech_output <- segment_speech(cutoff=cutoff,
                                          stat = "TP", 
                                          data = data[[k]],
                                          seg.utts = seg.utts,
                                          consider.freq = consider.freq,
                                          quiet=quiet)
      data[[k]]$TP85$seg.phon.stream <- seg_speech_output$seg.phon.stream
      if(consider.freq) {
        freq.cutoff$TP85 <- seg_speech_output$freq.cutoff}
      else {
        freq.cutoff$TP85 <- NA
      }
    }
    if(MI){
      seg_speech_output <- segment_speech(cutoff = cutoff,
                                          stat = "MI", 
                                          data = data[[k]],
                                          seg.utts = seg.utts,
                                          consider.freq = consider.freq,
                                          quiet=quiet)
      data[[k]]$MI85$seg.phon.stream <- seg_speech_output$seg.phon.stream
      if(consider.freq) {
        freq.cutoff$MI85 <- seg_speech_output$freq.cutoff}
      else {
        freq.cutoff$MI85 <- NA
      }
    }
  
  # assess segmentation
  
    message(paste0("assessing segmentation for ", names(data)[k], "..."))
    
    if(TP){
      data[[k]]$TP85$seg.results <- assess_seg(seg.phon.stream=data[[k]]$TP85$seg.phon.stream, 
                                               streams=data[[k]]$streams, 
                                               dict=dict, 
                                               embedding.rule=embedding.rule, 
                                               trisyl.limit=trisyl.limit, 
                                               freq.cutoff=freq.cutoff$TP85)
      TPresults <- colMeans(select(data[[k]]$TP85$seg.results, recall, precision), na.rm=T)
      TPresults <- as.data.frame(t(TPresults)) # make it a data.frame
      TPresults$stat <- "TP"
    }
    if(MI){
      data[[k]]$MI85$seg.results <- assess_seg(seg.phon.stream=data[[k]]$MI85$seg.phon.stream, 
                                               streams=data[[k]]$streams, 
                                               dict=dict, 
                                               embedding.rule=embedding.rule, 
                                               trisyl.limit=trisyl.limit, 
                                               freq.cutoff=freq.cutoff$MI85)
      MIresults <- colMeans(select(data[[k]]$MI85$seg.results, recall, precision), na.rm=T)
      MIresults <- as.data.frame(t(MIresults)) # make it a data.frame
      MIresults$stat <- "MI"
    }
    
    # save these results
    if(MI & TP){
      message("here comes rbind at line 589 (if MI & TP)")
      this.result <- as.data.frame(rbind(TPresults, MIresults))
      this.result$TPN.segd.units <- median(data[[k]]$TP85$seg.results$N.segd.units)
      this.result$MIN.segd.units <- median(data[[k]]$MI85$seg.results$N.segd.units)
    } else if(MI){
      this.result <- MIresults
      this.result$MIN.segd.units <- median(data[[k]]$MI85$seg.results$N.segd.units)
    } else if(TP){
      this.result <- TPresults
      this.result$TPN.segd.units <- median(data[[k]]$TP85$seg.results$N.segd.units)
    }

    row.names(this.result) <- NULL
    this.result$stat <- as.factor(as.character(this.result$stat))
    this.result$nontext <- names(data)[[k]]
    this.result$cutoff <- cutoff
    # stats to save for anlaysis and comparison
    this.result$N.utts <- data[[k]]$N.utterances
    this.result$N.syl.tokens <- data[[k]]$streams$N.syl.tokens
    this.result$N.syl.types <- data[[k]]$streams$N.syl.types
    this.result$N.tokens <- data[[k]]$streams$N.tokens
    this.result$N.types <- data[[k]]$streams$N.types
    this.result$TTR <- data[[k]]$streams$N.types/data[[k]]$streams$N.tokens
    highest.freq <- max(max(data[[k]]$unique.phon.pairs$A.freq.tot), max(data[[k]]$unique.phon.pairs$B.freq.tot))
    this.result$highest.freq.syl <- highest.freq 
    this.result$prop.most.freq <- highest.freq/data[[k]]$streams$N.syl.tokens
    this.result$mean.MI <- ifelse(MI, mean(data[[k]]$unique.phon.pairs$MI), NA)
    this.result$mean.TP <- ifelse(TP, mean(data[[k]]$unique.phon.pairs$TP), NA)
    this.result$mean.words.per.utt <- mean(data[[k]]$streams$words.per.utt)
    this.result$mean.syls.per.utt <- mean(data[[k]]$streams$syls.per.utt)
    this.result$prop.one.word.utt <- sum(data[[k]]$streams$words.per.utt==1)/length(data[[k]]$streams$words.per.utt)
    this.result$prop.one.syl.utt <- sum(data[[k]]$streams$syls.per.utt==1)/length(data[[k]]$streams$syls.per.utt)
    
    this.unique.phon.pairs <- data[[k]]$unique.phon.pairs
    this.unique.phon.pairs$context <- names(data)[k]
    
    this.TP85.seg.results <- data[[k]]$TP85$seg.results
    this.TP85.seg.results$context <- names(data)[k]
    
    this.MI85.seg.results <- data[[k]]$MI85$seg.results
    this.MI85.seg.results$context <- names(data)[k]
    
    if(k==1) {
      # on the first run through the for loop
      stat.results <- this.result 
      unique.phon.pairs <- this.unique.phon.pairs
      TP85.seg.results <- this.TP85.seg.results
      MI85.seg.results <- this.MI85.seg.results
    } else {
      # all subsequent runs
      message(paste("here comes rbind at line 638, with stat.results", ncol(stat.results), "and this.result", ncol(this.result)))
      
      stat.results <- rbind(stat.results, this.result) 
      unique.phon.pairs <- rbind(unique.phon.pairs, this.unique.phon.pairs)
      TP85.seg.results <- rbind(TP85.seg.results, this.TP85.seg.results)
      MI85.seg.results <- rbind(MI85.seg.results, data[[k]]$MI85$seg.results)
    }
    message(paste0("done segmenting speech and assessing segmentation for ", names(data)[k], "..."))
  } # end of for loop
  
  message("saving results...")
  if(!quiet) message("here are the results: \n")
  if(!quiet) print(stat.results)
  stat.results$nontext <- as.factor(as.character(stat.results$nontext))
  stat.results$recall <- as.numeric(stat.results$recall)
  stat.results$precision <- as.numeric(stat.results$precision)
  stat.results$SHA1 <- fun.version
  
  if(verbose){
    return( list(stat.results=stat.results, unique.phon.pairs=unique.phon.pairs, TP85.seg.results=TP85.seg.results, MI85.seg.results=MI85.seg.results) )
  } else {
    return(stat.results)
  }
}

# a quick version for testing (doesn't actually run the analyses)
par_function_test <- function(dataframe, verbose){
  
  data <- dataframe[,4:ncol(dataframe)]
  
  stat.results <- data.frame(recall=NULL, precision=NULL, stat=NULL, nontext=NULL) # empty storage variable
  MIs <- vector("list", length(names(data))); names(MIs) <- names(data) # empty storage variable
  TPs <- vector("list", length(names(data))); names(TPs) <- names(data) # empty storage variable
  for(k in 1:length(names(data))){
    this.result <- data.frame(recall=runif(1), 
                              precision=runif(1), 
                              stat="stat", 
                              nontext=names(data)[k])
   
    stat.results <- rbind(stat.results, this.result )
    
    MI <- rnorm(nrow(df))
    MIs[[k]] <- MI
    TP <- rbeta(nrow(df), .5, .75)
    TPs[[k]] <- TP
  }
  
  if(verbose){
    return( list(stat.results=stat.results, MIs=MIs, TPs=TPs) )
  } else {
    return(stat.results)
  }
}

aciss_function <- function(fun.version, id, starts, iter, par_function_args, walltime=9600){
  # fun.version refers to the current commit for data_processing_functions.r
  
  batch_function <- function(start, par_function_args){
    library(devtools)
    source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/R/data_processing_functions.r", 
               sha1=fun.version)
    iter <- iter # the number of times to generate random samples
    
    library(doParallel)
    registerDoParallel()
    r <- foreach(1:iter, 
                 .inorder=FALSE,
                 .combine = rbind,
                 .verbose=TRUE,
                 .packages=c("dplyr", "tidyr", "devtools", "BBmisc") ) %dopar% par_function(par_function_args)
  }
  
  # create a registry
  reg <- makeRegistry(id = id)
  
  # map function and data to jobs and submit
  ids  <- batchMap(reg, batch_function, starts, more.args=list(par_function_args=par_function_args))
  done <- submitJobs(reg, resources = list(nodes = 1, ppn=1)) 
}

# make an artificial language
make_corpus <- function(dist=c("unif", "skewed"), N.utts=1000, N.types=1800, smallest.most.freq=FALSE, monosyl=FALSE){ 
  N.type <- round(N.types/3, 0)
  if(!monosyl){
    message(paste0("\nGenerating ", N.type, " types each for 2, 3, and 4-syllables words from a ", dist, " distribution...\n") )
  } else {
    message(paste0("\nGenerating ", N.type, " types each for 1, 2, and 3-syllables words...\n") )
  }
  
  # generate all possible syllables from these consonants and these vowels
  Cs <- c("b", "c", "ch", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "r", "s", "sh", "t", "v", "w", "z")
  Vs <- c("a", "e", "i", "o", "u", "a1", "e1", "i1", "o1", "u1")
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
  
  # check to make sure there are no missing values in the words
  if( any(grepl(x=words, pattern="NA")) ) stop(paste("Not enough unique syllables to make", N.types, "word types."))
  
  if( !smallest.most.freq ){
    words <- base::sample(words, size=length(words), replace=FALSE) # shuffle the order of the words (the first word here will end up being the most frequent in the skewed dist) 
  } 
  
  dict <- data.frame(word=gsub(x=words, pattern="-", replacement=""), phon=words, stringsAsFactors=FALSE)
  
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
  
  df <- data.frame(utt=utt, orth=orth, phon=phon, stringsAsFactors=FALSE)
  
  return( list(df, dict) )
}

contexts_by_size <- function(df, N.sizes, min.utt=100){
  start.columns <- ncol(df)
  
  # Add columns for each "context", with increasing number of utterances. 
  # It doesn't matter which utterances are selected for each column since they'll all get shuffled anyway during the bootstrapping process
  max.utt <- nrow(df) - min.utt # a sample that includes all of the utterances in the corpus won't have any variability from sample to sample
  sizes <- round(seq(from=min.utt, to=max.utt, length.out=N.sizes), 0)
  # add a column to the dataframe for each  corpus size to try
  for(s in 1:N.sizes){
    df[[paste0("N.utts", sizes[s])]] <- c( rep(1, sizes[s]), rep(0, nrow(df)-sizes[s]) )
    df[[paste0("N.utts", sizes[s])]] <- base::sample(df[[paste0("N.utts", sizes[s])]], nrow(df), replace=F) # shuffle
  }
  return(df)
}

get_from_https <- function (url, ..., sha1 = NULL) {
  # based on code for devtools::source_url
  stopifnot(is.character(url), length(url) == 1)
  temp_file <- tempfile()
  on.exit(unlink(temp_file))
  request <- httr::GET(url)
  httr::stop_for_status(request)
  writeBin(httr::content(request, type = "raw"), temp_file)
  file_sha1 <- digest::digest(file = temp_file, algo = "sha1")
  if (is.null(sha1)) {
    message("SHA-1 hash of file is ", file_sha1)
  }
  else {
    if (nchar(sha1) < 6) {
      stop("Supplied SHA-1 hash is too short (must be at least 6 characters)")
    }
    file_sha1 <- substr(file_sha1, 1, nchar(sha1))
    if (!identical(file_sha1, sha1)) {
      stop("SHA-1 hash of downloaded file (", file_sha1, 
           ")\n  does not match expected value (", sha1, 
           ")", call. = FALSE)
    }
  }
  read.table(temp_file, header=1, sep="\t", stringsAsFactors=F, quote="", comment.char ="")
  # return(temp_file)
}
