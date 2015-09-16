nontext_cols <- function(df, context_names){
  nontexts<- vector("list", length(colnames(contexts))) # storage variable
  non <- NULL
  
  for(k in 1:length(context_names)){  
    N.hits <- sum(df[[context_names[k]]])# the number of utterances that contain a keyword for that context
    nontexts[[k]] <- sample(x=as.numeric(row.names(df)), size=N.hits)
    col <- rep(0, nrow(df))
    col[nontexts[[k]]] <- 1
    non <- cbind(non, col)
    colnames(non)[k] <- paste("non.", context_names[k], sep="") 
  }
  return(list(non, nontexts))
}

expand_windows <- function(df){
  p <- progress_estimated(n=length(3:(nrow(df)-2))) # print progress bar while working
  for(i in 3:(nrow(df)-2)){
    for(j in 3:ncol(df)){
      df[i,j] <- ifelse(df[i,j]==1, df[i,j], # if it is already marked 1, leave it
                        ifelse(df[(i-2),j]==1, 1.5, # if the utterance 2 before it is marked 1, mark 1.5
                               ifelse(df[(i-1),j]==1, 1.5, # if the utterance 1 before it is marked 1, mark 1.5
                                      ifelse(df[(i+1),j]==1, 1.5, # if the utterance 1 after it is marked 1, mark 1.5
                                             ifelse(df[(i+2),j]==1, 1.5, 0))))) # if the utterance 2 after it is marked 1, mark 1.5. Otherwise mark 0.
      # note that utterances classified as a context based on their proximity to an utterance with a key word must be marked with something other than 1 to prevent them being used as key utterances in the next row
      
    }
    print(p$tick()) # advance progress bar
  }
  return(df)
}

calc_MI = function(phon.pairs, phon.stream){
  # mutual information, and transitional probabilty. See Swingley (2005) p97
  p <- progress_estimated(n=nrow(phon.pairs)) # print progress bar while working
  for(i in 1:nrow(phon.pairs)){
    AB <- filter(phon.pairs, syl1==syl1[i] & syl2==syl2[i])
    p.AB <- nrow(AB)/nrow(phon.pairs)
    p.A <- length(which(phon.stream == phon.pairs$syl1[i]))/length(phon.stream)
    p.B <- length(which(phon.stream == phon.pairs$syl2[i]))/length(phon.stream)
    
    phon.pairs$MI[i] <- ifelse(p.AB==0, NA, log2(p.AB/(p.A * p.B))) # if AB never occurs, enter NA, otherwise calculate MI
    phon.pairs$TP[i] <- ifelse(p.AB==0, NA, p.AB/(p.A)) # if AB never occurs, enter NA, otherwise calculate TP
    phon.pairs$freq[i] <- ifelse(p.AB==0, NA, nrow(AB)) # if AB never occurs, enter NA, otherwise enter freq
    print(p$tick()) # advance progress bar
  }
  output <- unique(phon.pairs) # Only keep one instance of each syllable pair
  return(output)
}

make_streams = function(df){
  # collapse phonological utterances into one continuous stream
  phon.stream <- unlist(strsplit(df$phon, " "))
  
  # how many unique syllables are there?
  syllables <- unique(phon.stream)
  # syllables <- sample(syllables, 200) # for testing, just use some random syllables
  N.syl <- length(syllables)
  
  # make phone stream into a list of all of the bisyllable pairs that occur
  phon.pairs <- data.frame(syl1=phon.stream[1:length(phon.stream)-1], syl2=phon.stream[2:length(phon.stream)])
  
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
    seg <- dplyr::filter(unique.phon.pairs, syl1==phon.stream[i-1] & syl2==phon.stream[i])$seg
    
    if(length(seg) > 1) stop(paste("ERROR at ", i, "th element of phon.stream: more than one entry for seg", sep=""))
    
    seg.phon.stream[i]<- ifelse(seg==1, paste(",", phon.stream[i], sep="" ), phon.stream[i]) # if seg=1 for this phon pair, then insert a comma before the second syllable
  }
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
  this.dict <- filter(dict, word %in% words)[,c("word", "phon", "context")] 
  
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

record_bootstrap_results <- function(bootstrap.results, wd="./bootstrap_output/"){
 for(k in 1:length(names(bootstrap.results))){
   write.table(bootstrap.results[[k]], file=paste(wd, names(bootstrap.results)[k], Sys.Date(),".txt", sep=""), sep="\t", col.names=F, row.names=row.names(bootstrap.results[[2]])) 
  }
}

read_bootstrap_results <- function(iter, contexts, wd="./bootstrap_output/"){
  bootstrap.results <- vector("list", length(colnames(contexts)))  
  for(k in 1:length(colnames(contexts))){
    bootstrap.results[[k]] <- matrix(ncol=iter, nrow=4, dimnames=list(c("TP85recall", "TP85precision", "MI85recall", "MI85precision")))
  }
  files <- list.files(path=wd, ".txt")
  
  if(length(files) != length(colnames(contexts))) stop("ERROR: The number of contexts in this folder does not match the number of contexts in the contexts argument.")
  
  for(f in 1:length(files)){
   bootstrap.results[[f]] <- read.table(file=paste(wd, files[f], sep=""), sep="\t", header=F, row.names=1) 
   names(bootstrap.results)[f] <- sub(pattern="^([[:alpha:]]+\\.?[[:alpha:]]+).*", replacement="\\1", x=files[f])
  }
  
  return(bootstrap.results)
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

