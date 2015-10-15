# need to run transcript cleaning script first
# this requires all of the raw .cha transcripts in a folder called "transcripts".
CleanTranscripts <- function(wd="./transcripts/"){
  
  files <- list.files(path=wd)
  Nfiles <- length(files)
  
  transcripts <- list()
  
  for(f in 1:Nfiles){
    this.file <- files[f]
    this.transcript <- readLines(paste(wd, this.file, sep=""))
    
    # The split between speaker id and utterance is :\t (colon, then tab), but somtimes that pattern also occurs later in the utterance, especially on the %pho tier where : is used for vowel length.
    # To restrict the :\t split to only the one that divides speaker ID and utterance, only look for it within the first 7 characters of each line.
    # @Comment:\t is used occassionally to indicate activity happening (Korman corpus). To make sure the reocrded comment is correctly retained, substitue "@Comment" with "@Com" throughout, so the :\t will fall within the first 7 characters of the line.
    this.transcript <- gsub(pattern="@Comment", replacement="@Com", x=this.transcript)
    this.transcript <- gsub(pattern="@Participants", replacement="@Par", x=this.transcript)
    this.transcript <- gsub(pattern="@Situation", replacement="@Sit", x=this.transcript)
    this.transcript <- gsub(pattern="@Activities", replacement="@Act", x=this.transcript)
    
    speaker.str <- substr(this.transcript, start=1, stop=7) # take the first 7 characters of each row
    split <- strsplit(speaker.str, ":\t", fixed=T)
    speaker <- sapply(split, FUN='[', 1) # takes the first item in each split
    
    utterance <- substr(this.transcript, start=7, stop=100000000) # take characters 7 through the end of each row 
    
    data <- data.frame(speaker=speaker, utterance=utterance)
    data$LineNum <- as.numeric(row.names(data))
    
    # Add utterance numbers
    temp <- dplyr::filter(data, grepl("*", speaker, fixed=TRUE))
    temp$UttNum <- 1:nrow(temp)
    data <- merge(data, temp, by=c("LineNum", "speaker", "utterance"), all=T, sort=T)
    #data$UttNum <- na.locf(data$UttNum, na.rm = FALSE)  # fill in utterance number column
    
    # print(sort(unique(data$speaker))) # check
    
    # drop unneeded rows
    # keep only rows that contain a * (all speaker lines, i.e. all of the utterances), or the following list of annotations
    data <- dplyr::filter(data, grepl("*", speaker, fixed=TRUE) | 
                            speaker == "@Com" |
                            speaker == "@Sit" |
                            speaker == "@Act" |
                            speaker == "@New Ep" |  
                            speaker == "%com" |
                            speaker == "@Par"  )
    
    data$speaker <- factor(data$speaker) # removes the empty levels from speaker
    data$utterance <- as.character(data$utterance)
    
    data$speaker <- car::recode(data$speaker, recodes="
                                '@Com'='(comment)'; 
                                '@Sit'='(situation)';
                                '@Act'='(activities)';
                                '@New Ep'='(New Episode)';
                                '%com'='(comment)' ")
    
    data$utterance <-  ifelse(data$speaker=="(New Episode)", "...",  data$utterance )
    
    transcripts[[f]] <- data
  }
  
  names(transcripts) <- files
  
  return(transcripts)
}


CodeContexts <- function(this_pass=1, window_size=30, slide_by=10){
  # check whether packages need to be installed
  list.of.packages <- c("dplyr", "car")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  # first, clean all of the transcripts
  message("Prepping the transcripts...")
  transcripts <- CleanTranscripts()
  
  encouragements <- c("Woo-hoo!", "Yay! :D", "Great!", "Woo-hoo!", "You're on a roll!", "Gorgeous!", "Awesome!", "That's the ticket!", "I just love coding, don't you?", "Hooray!") # just a bit of fun, inspired by swirl()
  
  # load the coding doc
  coding_doc <- read.table("coding_doc.txt", header=1, sep="\t", stringsAsFactors=F)
  
  # we will break each transcript into windows of [window_size] utterances each, for example 20, sliding (for example) every 2 utterances (so the first would be lines 1-20, and the next would be lines 2-22, etc.)
  starts <- seq(from=1, to=window_size-1, by=slide_by) # each starting point will set up a different set of windows
  start_at <- starts[this_pass] # the utterance to begin counting the windows from
  if(is.na(start_at)) stop("Error in this_pass value. To allow more passes, use a smaller slide_by value.")
  
  message("\nHello, and welcome to coding. :)")
  
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
      message("Holy moley! The coding is DONE!! Wow, thank you!\n<3")
      all_done <- TRUE
    }
    
    # select transcript
    message("Selecting transcript...")
    # which files still have NAs for contexts?
    still_to_code <- dplyr::filter(coding_doc, pass==this_pass & is.na(context))
    while(nrow(still_to_code)==0) {
      this_pass=this_pass+1
      still_to_code <- dplyr::filter(coding_doc, pass==this_pass & is.na(context))
    }
    this.file <- sample(unique(still_to_code$file), 1) # pick one of the transcripts that isn't done yet, at random
    
    # select utterances 
    message("Selecting utterances...")
    Nutts <- max(dplyr::filter(coding_doc, file==this.file)$UttNum) # the total number of utterances in this transcript
    start_vals <- seq(from=start_at, to=Nutts, by=window_size) # the vector of starting values for coding windows
    start <- sample(start_vals, 1) # select a starting utterance at random from the starting values
    
    # if that section has already been fully coded, select new starting values until you get some stuff that hasn't been coded yet
    while( !anyNA(dplyr::filter(coding_doc, file==this.file & start-1 < UttNum & UttNum < start+window_size & pass==this_pass)$context ) ){
      start <- sample(start_vals, 1) # select a new starting utterance at random from the starting values
    }
    
    this.transcript <- transcripts[[this.file]]
    
    # print participants
    message(paste("The participants in this recording are: " , dplyr::filter(this.transcript, speaker=="@Par")$utterance ))
    
    # print window
    window <- dplyr::filter(this.transcript, start-1 < UttNum & UttNum < start+window_size)
    print(as.matrix(dplyr::select(window, speaker, utterance)), quote=F)
    
    message("Read the transcript, then enter the context(s) for what is happening. \nFor multiple contexts, enter them with a semi-colon between each (for example, 'bath time ; playing').\nIf you can't enter any contexts at all (you can't tell what's happening), enter 'none'.\n")
    
    confirm <- FALSE
    context_response <- ""
    confirm_response <- ""
    while(confirm==FALSE){
      context_response <- readline("Context(s): ")
      confirm_response <- readline("Confirm? (Y/N) ")
      confirm <- grepl("y", confirm_response, ignore.case=T) # if they enter a y, then change confirm to TRUE (if they enter anything else, it will remain FALSE)
      if(  !grepl("y", confirm_response, ignore.case=T) ) message("Please re-enter the context(s), and then confirm that it's correct.\n")
      if(  grepl("n", confirm_response, ignore.case=T) ) message("Please re-enter the context(s) to correct the error.\n")
    }
    
    # save context info
    this_doc <- data.frame(LineNum=NA,
                           UttNum=start:(start+window_size-1), 
                           file=this.file, 
                           coder=coder_response, 
                           date=date, 
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
      still_to_code_this_doc_passes <- dplyr::filter(still_to_code_this_doc, UttNum==this_doc$UttNum[i])$pass
      
      max_pass <- max(dplyr::filter(coding_doc, file==this_doc$file[i] & UttNum==this_doc$UttNum[i])$pass) # the highest pass value available for this Utterance in the coding doc
      if( !is.integer(max_pass) ) max_pass <- max(coding_doc$pass) # in case there's an error above
        
      this_doc$LineNum[i] <- median(dplyr::filter(coding_doc, file==this_doc$file[i] & UttNum==this_doc$UttNum[i])$LineNum)
        
      if (length(still_to_code_this_doc_passes)>0) {
        this_doc$pass[i] <- min(still_to_code_this_doc_passes) # set pass to the lowest pass value that shows up in still_to_code_this_doc for this utterance
        # update coding_doc
        coding_doc[coding_doc$UttNum==this_doc[i,]$UttNum & 
                     coding_doc$file==this_doc[i,]$file & 
                     coding_doc$pass==this_doc[i,]$pass,]$coder <- this_doc[i,]$coder
        coding_doc[coding_doc$UttNum==this_doc[i,]$UttNum & 
                     coding_doc$file==this_doc[i,]$file & 
                     coding_doc$pass==this_doc[i,]$pass,]$date <- this_doc[i,]$date
        coding_doc[coding_doc$UttNum==this_doc[i,]$UttNum & 
                     coding_doc$file==this_doc[i,]$file & 
                     coding_doc$pass==this_doc[i,]$pass,]$context <- this_doc[i,]$context
        }
      if (length(still_to_code_this_doc_passes)==0) {
        this_doc$pass[i] <- max_pass + 1 # if this utterance has already been fully coded, add a new pass for it
        # update coding_doc
        coding_doc <- rbind(coding_doc, this_doc[i,])
      }
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
      Sys.sleep(.75) # pause before loading the next transcript (this just makes it feel nicer)
    }
    
  }
  
  # write updated coding_doc to file
  write.table(coding_doc, file="coding_doc.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
  
  time <- Sys.time() - date
  message(paste("You coded ", Ncoded, " windows in ", round(time[[1]],1), " ", attributes(time)$units, "! Well done! :)\nThank you, and bye!", sep=""))
}