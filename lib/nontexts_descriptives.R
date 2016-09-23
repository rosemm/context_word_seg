nontexts_descriptives <- function(x){
  if (all(c("dataframe", "dict") %in% names(x))) {
    df <- x[["dataframe"]]
    dict <- x[["dict"]]
    if("seg.utts" %in% names(x)) seg.utts <- x[["seg.utts"]]
    if("verbose" %in% names(x)) verbose <- x[["verbose"]]
    if("nontext" %in% names(x)) nontext <- x[["nontext"]]
    if("quiet" %in% names(x)) quiet <- x[["quiet"]]
    if("date" %in% names(x)) date <- x[["date"]]
  } else stop("arguments are a list, but does not have components 'dataframe' and 'dict'")
  
  stopifnot(is.data.frame(df))
  if(nrow(df) == 0) stop("df didn't load")
  if(nrow(dict) == 0) stop("dict didn't load")  
  
  if(ncol(df) > 3){
    context.names <- colnames(dplyr::select(df, -utt, -orth, -phon))
    continuity.measures <- measure_continuity(df=df, verbose=verbose)
  } else {
    context.names <- as.vector("global")
    continuity.measures <- data.frame(context="global", "continuity"=NA) 
  }
  
  if( nontext & ncol(df) > 4 ){
    # pick nontext utterances
    non <- nontext_cols(df=df) 
    # add nontext columns to dataframe
    df <- cbind(dplyr::select(df, utt, orth, phon), non)
  }
  
  if( ncol(df) > 3 ){
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
    
    message(paste0("make_streams on ", context.names[k], "..."))
    
    df.context <- dplyr::filter(df, df[ , which(colnames(df)==context.names[k])] > 0) # select cases that have any value greater than 0 in the column that matches the name of context k
    
    data[[k]]$N.hits <- nrow(filter(df, df[ , which(colnames(df)==context.names[k])] == 1)) # the number of utterances that contained a key word (does not include utterances before and after)
    
    data[[k]]$N.utterances <- nrow(df.context)
    
    data[[k]]$streams <- make_streams(df.context, seg.utts=TRUE)

    data[[k]]$freq.bigrams <- dplyr::summarise(group_by(data[[k]]$streams$phon.pairs, syl1, syl2), count=n()) # frequency of bigrams
    data[[k]]$freq.words <- table(data[[k]]$streams$orth.stream) # frequency of words
    phon.stream <- data[[k]]$streams$phon.stream[data[[k]]$streams$phon.stream != "###"]
    data[[k]]$freq.syl <- table(phon.stream) # frequency of syllables
  }
  
  addl.info <- vector("list", length(names(data))); names(addl.info) <- names(data) # empty storage variable
  
  for(k in 1:length(names(data))){ 
    nontext <- names(data)[[k]]
    # stats to save for anlaysis and comparison
    N.utts <- data[[k]]$N.utterances
    N.syl.tokens <- data[[k]]$streams$N.syl.tokens
    N.syl.types <- data[[k]]$streams$N.syl.types
    N.tokens <- data[[k]]$streams$N.tokens
    N.types <- data[[k]]$streams$N.types
    TTR <- data[[k]]$streams$N.types/data[[k]]$streams$N.tokens
    highest.freq <- max(data[[k]]$freq.syl)
    highest.freq.syl <- names(which.max(data[[k]]$freq.syl))
    prop.most.freq <- highest.freq/data[[k]]$streams$N.syl.tokens
    mean.words.per.utt <- mean(data[[k]]$streams$words.per.utt)
    mean.syls.per.utt <- mean(data[[k]]$streams$syls.per.utt)
    prop.one.word.utt <- sum(data[[k]]$streams$words.per.utt==1)/length(data[[k]]$streams$words.per.utt)
    prop.one.syl.utt <- sum(data[[k]]$streams$syls.per.utt==1)/length(data[[k]]$streams$syls.per.utt)
    
    this.result <- data.frame(nontext = nontext,
                              N.utts = N.utts,
                              N.syl.tokens = N.syl.tokens,
                              N.syl.types = N.syl.types,
                              N.tokens = N.tokens,
                              N.types = N.types,
                              TTR = TTR,
                              highest.freq = highest.freq,
                              highest.freq.syl = highest.freq.syl,
                              prop.most.freq = prop.most.freq,
                              mean.words.per.utt = mean.words.per.utt,
                              mean.syls.per.utt = mean.syls.per.utt,
                              prop.one.word.utt = prop.one.word.utt,
                              prop.one.syl.utt = prop.one.syl.utt)
    
    if(k==1) {
      # on the first run through the for loop
      stat.results <- this.result 
      message(paste("added results for ", names(data)[k]))
    } else {
      # all subsequent runs
      stat.results <- rbind(stat.results, this.result) 
      message(paste("added results for ", names(data)[k]))
    }
    message(paste0("done getting stats for ", names(data)[k], "..."))
  } # end of for loop

  message("saving results...")

  if(verbose){
    
    stat.results <- left_join(stat.results, continuity.measures$continuity.stats, by=c("nontext"="context"))
    
    return( list(stat.results=stat.results, continuity.measures=continuity.measures) )
  } else {
    
    stat.results <- left_join(stat.results, continuity.measures, by=c("nontext"="context"))
    
    return(stat.results)
  }
}