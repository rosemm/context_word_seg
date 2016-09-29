#' @export
code_contexts <- function(this_pass=1, window_size=30, slide_by=3){
  stopifnot(require(devtools))
  source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/R/utils.R")
  check_packages(list.of.packages=c("dplyr", "car", "tidyr", "pbkrtest"))
  
  # first, clean all of the transcripts
  message("Prepping the transcripts...")
  transcripts <- CleanTranscripts()
  
  encouragements <- c("Woo-hoo!", "Yay! :D", "Great!", "Woo-hoo!", "You're on a roll!", "Gorgeous!", "Awesome!", "That's the ticket!", "I just love coding, don't you?", "Hooray!") # just a bit of fun, inspired by swirl()
  
  # load the coding doc
  coding_doc <- read.table("coding_doc.txt", header=1, sep="\t", stringsAsFactors=F)
  
  # which files still have NAs for contexts?
  still_to_code <- dplyr::filter(coding_doc, pass==this_pass & is.na(context))
  while(nrow(still_to_code)==0) {
    this_pass <- this_pass + 1
    still_to_code <- dplyr::filter(coding_doc, pass==this_pass & is.na(context))
  }
  
  # we will break each transcript into windows of [window_size] utterances each, for example 20, sliding (for example) every 2 utterances (so the first would be lines 1-20, and the next would be lines 2-22, etc.)
  starts <- seq(from=1, to=window_size-1, by=slide_by) # each starting point will set up a different set of windows
  start_at <- sample(starts, 1) # the utterance to begin counting the windows from (randomly selected)
  if(is.na(start_at)) stop("Error in this_pass value. To allow more passes, use a smaller slide_by value.")
  
  message("\nHello, and welcome to coding. :)\n")
  
  coder_response <- ""
  while(grepl(pattern="^$", x=coder_response)){
    coder_response <- readline("Enter your initials, please: ")
    if(grepl(pattern="^$", x=coder_response)) message("You need to enter your initials. Please enter them now.")
  }
  
  date <- Sys.time()
  
  all_done <- FALSE 
  Ncoded <- 0
  
  while(all_done==FALSE){
    # whenever coding is done, stop and save.
    if( nrow(dplyr::filter(coding_doc, is.na(context)))==0 ) {
      message("\nHoly moley! The coding is DONE!! Wow, thank you!\n<3\n")
      all_done <- TRUE
    }
    
    # select transcript
    message("Selecting transcript...")
    
    this.file <- sample(unique(still_to_code$file), 1) # pick one of the transcripts that isn't done yet, at random
    
    # select utterances 
    message("Selecting utterances...")
    
    # if there are more than 30 utterances left to code still in this document, pick a window
    if(nrow(dplyr::filter(still_to_code, file==this.file & pass==this_pass)) > 30){
      # the total number of utterances in this transcript
      all.utts <- dplyr::filter(coding_doc, file==this.file & pass==this_pass)$UttNum       
      Nutts <- max(all.utts)
      # the vector of starting values for coding windows
      start_vals <- seq(from=start_at, to=Nutts, by=window_size) 
      start_vals <- start_vals[start_vals %in% all.utts] # only keep start_vals that are actually available in this file
      # select a starting utterance at random from the starting values
      start.num <- sample(1:length(start_vals), 1) 
      start <- start_vals[start.num]
      
      # if that section has already been fully coded, select new starting values until you get some stuff that hasn't been coded yet
      wait <- Sys.time() # only keep looping for a max of 5 seconds, then just go with whatever's there
      while( Sys.time() - wait < as.difftime(5, units = "secs") & !anyNA(dplyr::filter(coding_doc, file==this.file & start-1 < UttNum & UttNum < start+window_size & pass==this_pass)$context ) ){
        # select a new starting utterance from the starting values
        # loop through the list of starting values until we find one that hasn't been fully coded
        start.num <- start.num + 1 
        if(start.num > length(start_vals)) start.num <- 1 # if we're at the end of the list, start over
        start <- start_vals[start.num]
      }
    } else {
      start <- dplyr::filter(still_to_code, file==this.file, pass==this_pass)$UttNum[1] # if there are fewer than 30 utterances left, just start at the beginning
    }
    
    this.transcript <- transcripts[[this.file]]
    
    # print participants
    message(paste0("The participants in this recording are: " , dplyr::filter(this.transcript, speaker=="@Par")$utterance ))
    
    # print window
    window <- dplyr::filter(this.transcript, start-1 < UttNum & UttNum < start+window_size)
    print(as.matrix(dplyr::select(window, speaker, utterance)), quote=F)
    
    message("Read the transcript, then enter the context(s) for what is happening. \nFor multiple contexts, enter them with a semi-colon between each (for example, 'bath time ; playing').\nIf you can't enter any contexts at all (you can't tell what's happening), enter 'none'.\n")
    
    confirm <- FALSE
    context_response <- ""
    confirm_response <- ""
    while(confirm==FALSE){
      context_response <- readline("Context(s): ")
      
      if(grepl(pattern="[,/:]", x=context_response)){
        confirm_response <- "n"
        message("Only use semi colons (;) to separate contexts, no other punctuation.")
      } else if(nchar(context_response) < 3) {
        confirm_response <- "n"
        message("It looks like that's not a context. If you can't code a context for this window, enter 'none'.")
      } else confirm_response <- readline("Confirm? (Y/N) ")
      
      confirm <- grepl("y", confirm_response, ignore.case=T) # if they enter a y, then change confirm to TRUE (if they enter anything else, it will remain FALSE)
      if(!grepl("y", confirm_response, ignore.case=T)) message("Please re-enter the context(s) to correct the error.\n")
    }
    
    # save context info
    this_doc <- data.frame(LineNum=NA,
                           UttNum=window$UttNum, 
                           file=this.file, 
                           coder=coder_response, 
                           date=as.integer(Sys.time()), 
                           context=context_response, 
                           pass=NA)
    this_doc$file <- as.character(this_doc$file)
    this_doc$coder <- as.character(this_doc$coder)
    this_doc$context <- as.character(this_doc$context)
    still_to_code_this_doc <- dplyr::filter(coding_doc, 
                                            file==this.file & 
                                              start-1 < UttNum & 
                                              UttNum < start+window_size &  
                                              is.na(coding_doc$context))
    
    # if a particular utterance has been coded previously, then set the pass to the next value that's still uncoded
    for(i in 1:nrow(this_doc)){
      # is this an utterance in the coding doc?
      if( nrow(dplyr::filter(coding_doc, UttNum==this_doc$UttNum[i] & file==this.file)) > 0 ){
        
        # for this utterance, which passes still haven't been coded?
        still_to_code_this_doc_passes <- dplyr::filter(still_to_code_this_doc, UttNum==this_doc$UttNum[i])$pass
        
        max_pass <- max(dplyr::filter(coding_doc, file==this.file & UttNum==this_doc$UttNum[i])$pass) # the highest pass value available for this Utterance in the coding doc
        if( !is.integer(max_pass) ) max_pass <- max(coding_doc$pass) # in case there's an error above
        
        this_doc$LineNum[i] <- median(dplyr::filter(coding_doc, file==this.file & UttNum==this_doc$UttNum[i])$LineNum)
        
        
        if (length(still_to_code_this_doc_passes) > 0) {
          this_doc$pass[i] <- min(still_to_code_this_doc_passes) # set pass to the lowest pass value that shows up in still_to_code_this_doc for this utterance
          # update coding_doc
          coding_doc[coding_doc$UttNum==this_doc$UttNum[i] & 
                       coding_doc$file==this.file & 
                       coding_doc$pass==this_doc$pass[i],]$coder <- this_doc$coder[i]
          coding_doc[coding_doc$UttNum==this_doc$UttNum[i] & 
                       coding_doc$file==this.file & 
                       coding_doc$pass==this_doc$pass[i],]$date <- this_doc$date[i]
          coding_doc[coding_doc$UttNum==this_doc$UttNum[i] & 
                       coding_doc$file==this.file & 
                       coding_doc$pass==this_doc$pass[i],]$context <- this_doc$context[i]
        } else if (length(still_to_code_this_doc_passes)==0) {
          this_doc$pass[i] <- max_pass + 1 # if this utterance has already been fully coded, add a new pass for it
          # update coding_doc
          this_doc$LineNum[i] <- unique(filter(coding_doc, file==this.file & this_doc$UttNum[i])$LineNum)
          coding_doc <- rbind(coding_doc, this_doc[i,]) 
        }
      } # if this utterance isn't in the coding doc (i.e. it's not on the list to be coded), don't save anything for it
    }
    
    
    Ncoded <- Ncoded + 1 # bump up the coding counter
    if(Ncoded %% 10 == 0) { # the number of coded sections is divisible by 10
      message(paste("\nAwesome! You've coded ", Ncoded, " windows! I'll just save your progress real quick."))
      # write updated coding_doc to file
      write.table(coding_doc, file="coding_doc.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
    }
    
    # check whether to keep coding or quit
    code_another <- readline("\nCode another one? (Y/N) ")
    if (grepl("n", code_another, ignore.case=T )) {
      all_done=TRUE
      message("\nOkay, sounds good. I'll save your work.\n")
    } else { 
      message(paste("\n", sample(encouragements,1), "\n")) 
      Sys.sleep(1) # pause before loading the next transcript (this just makes it feel nicer)
    }
    
  }
  
  # write updated coding_doc to file
  write.table(coding_doc, file="coding_doc.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
  
  time <- Sys.time() - date
  message(paste("You coded ", Ncoded, " windows in ", round(time[[1]],1), " ", attributes(time)$units, "! Well done! :)\nThank you, and bye!", sep=""))
}
