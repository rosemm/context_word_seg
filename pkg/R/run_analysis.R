#' @export
# for bootstrapping nontexts:
run_analysis <- function(x, dict=NULL, consider.freq=FALSE, embedding.rule=FALSE, trisyl.limit=FALSE, N.types=NULL, N.utts=NULL, by.size=TRUE, expand=FALSE, seg.utts=TRUE, TP=TRUE, MI=TRUE, verbose=FALSE, prop=FALSE, cutoff=.85, nontext=TRUE, date=NULL, quiet=TRUE){ # this is the function that should be done in parallel on the 12 cores of each node
  message("************************\nbeginning run_analysis\n************************")
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
      if("date" %in% names(x)) date <- x[["date"]]
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
    continuity.measures <- data.frame(context="global", "continuity"=NA) 
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
  
  if(ncol(df)>3){
    continuity.measures <- measure_continuity(df=df, verbose=verbose)
  }
  
  # calculate MIs and TPs
  if(ncol(df) > 3){
    message(paste("\ncontext results using all but the following columns:", paste(colnames(df)[1:3], collapse=", ")))
    # retains contexts as they were used for column names, regardless of where they came from (works for any context defining method)
    context.names <- colnames(df)[4:ncol(df)]
  } else {
    message("\nNo context columns in df, so analyzing as global.")
    df$global <- 1 # add a column of 1's, to include everything
    # if there are no context columns, analyze as global
    context.names <- as.vector("global")
  }
  data <- vector("list", length(context.names)) # storage variable
  names(data) <- context.names
  
  for(k in 1:length(names(data))){
    
    message(paste0("make_streams and calc_MI on ", context.names[k], "..."))
    
    df.context <- dplyr::filter(df, df[ , which(colnames(df)==context.names[k])] > 0) # select cases that have any value greater than 0 in the column that matches the name of context k
    
    data[[k]]$N.hits <- nrow(filter(df, df[ , which(colnames(df)==context.names[k])] == 1)) # the number of utterances that contained a key word (does not include utterances before and after)
    
    data[[k]]$N.utterances <- nrow(df.context)
    
    data[[k]]$streams <- make_streams(df.context, seg.utts=seg.utts)
    data[[k]]$unique.phon.pairs <- calc_MI(phon.pairs=data[[k]]$streams$phon.pairs, phon.stream=data[[k]]$streams$phon.stream)
    
    data[[k]]$freq.bigrams <- dplyr::summarise(group_by(data[[k]]$streams$phon.pairs, syl1, syl2), count=n()) # frequency of bigrams
    data[[k]]$freq.words <- table(data[[k]]$streams$orth.stream) # frequency of words
    phon.stream <- data[[k]]$streams$phon.stream[data[[k]]$streams$phon.stream != "###"]
    data[[k]]$freq.syl <- table(phon.stream) # frequency of syllables
  }
  
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
      TPresults <- colMeans(dplyr::select(data[[k]]$TP85$seg.results, recall, precision), na.rm=T)
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
      MIresults <- colMeans(dplyr::select(data[[k]]$MI85$seg.results, recall, precision), na.rm=T)
      MIresults <- as.data.frame(t(MIresults)) # make it a data.frame
      MIresults$stat <- "MI"
    }
    
    # save these results
    if(MI & TP){
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
      message(paste("added results for ", names(data)[k]))
    } else {
      # all subsequent runs
      stat.results <- rbind(stat.results, this.result) 
      unique.phon.pairs <- rbind(unique.phon.pairs, this.unique.phon.pairs)
      TP85.seg.results <- rbind(TP85.seg.results, this.TP85.seg.results)
      MI85.seg.results <- rbind(MI85.seg.results, this.MI85.seg.results)
      message(paste("added results for ", names(data)[k]))
    }
    message(paste0("done segmenting speech and assessing segmentation for ", names(data)[k], "..."))
  } # end of for loop
  
  message("saving results...")
  if(!quiet) message("here are the results: \n")
  if(!quiet) print(stat.results)
  stat.results$nontext <- as.factor(as.character(stat.results$nontext))
  stat.results$recall <- as.numeric(stat.results$recall)
  stat.results$precision <- as.numeric(stat.results$precision)
  stat.results$date <- date
  
  
  if(verbose){
    # add continuity measures
    stat.results <- left_join(stat.results, continuity.measures$continuity.stats, by=c("nontext"="context"))
    return( list(stat.results=stat.results, unique.phon.pairs=unique.phon.pairs, TP85.seg.results=TP85.seg.results, MI85.seg.results=MI85.seg.results, continuity.measures=continuity.measures) )
  } else {
    # add continuity measures
    stat.results <- left_join(stat.results, continuity.measures, by=c("nontext"="context"))
    return(stat.results)
  }
}
