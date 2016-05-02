# need to run transcript cleaning script first
# this requires all of the raw .cha transcripts in a folder called "transcripts".
CleanTranscripts <- function(wd="./transcripts/"){
  
  files <- list.files(path=wd)
  Nfiles <- length(files)
  if(Nfiles==0) stop("No transcripts found. Check the working directory.")
  
  transcripts <- list(NULL)
  
  for(f in 1:Nfiles){
    this.file <- files[f]
    if(is.na(this.file)) stop(paste(this.file, "not found."))
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
    head(data)
    
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

BlankDoc <- function(wd="./transcripts/", for.coding=TRUE){
  # write blank coding_doc.txt for coders to code from
  if(!require(dplyr)) install.packages("dplyr")
  
  coding_doc <- data.frame(LineNum=NULL, UttNum=NULL, file=NULL, utterance=NULL)
  
  transcripts <- CleanTranscripts()
  
  files <- list.files(path=wd)
  Nfiles <- length(files)
  
  for(f in 1:Nfiles){
    this_coding_doc <- data.frame(dplyr::filter(dplyr::select(transcripts[[f]], LineNum, UttNum, utterance), !is.na(UttNum) ) )
    this_coding_doc$file <- files[f]
    coding_doc <- rbind(coding_doc, this_coding_doc)
  }
  
  if(for.coding){
    coding_doc$coder <- NA
    coding_doc$date <- NA
    coding_doc$context <- NA
    # each utterance should be coded multiple times if there is overlap between windows. 
    # duplicate the coding rows for each time an utterance should be coded
    coding_doc1 <- coding_doc
    coding_doc1$pass <- 1
    coding_doc2 <- coding_doc
    coding_doc2$pass <- 2
    coding_doc3 <- coding_doc
    coding_doc3$pass <- 3
    
    coding_doc <- rbind(coding_doc1, coding_doc2, coding_doc3)
    coding_doc$utterance <- NULL # drop this column for coding purposes
  }
  
  return(coding_doc)
}
# write.table(blank_doc, file="blank_coding_doc.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")

UpdateDoc <- function(doc, criterion){
  # update an existing, partially coded document, to focus coders on the thin parts
  
  stopifnot(require(dplyr), require(tidyr))
  
  below.crit <- doc %>%
    unite(utt, file, UttNum) %>% 
    count(utt) %>% 
    filter(n < criterion) # only keep utterances that have not been coded at least criterion times
 
  # the line numbers, utterance number, and file name for everything in doc
  blank_doc <- BlankDoc() %>% 
    select(LineNum, UttNum, file) %>% 
    unite(utt, file, UttNum, remove=FALSE)
  nums <- blank_doc %>% 
    unique() %>% 
    as.tbl()
  
  # make a new coding doc with just the utterances below criterion number of codes
  update_doc <- below.crit %>% 
    left_join(nums, by="utt") %>% 
    select(LineNum, UttNum, file) %>% 
    na.omit() %>% 
    arrange(file, LineNum) 

message(paste0(nrow(update_doc), " utterances still have fewer than ", criterion, " codes (", 100*round(nrow(below.crit)/length(unique(blank_doc$utt)), 2),"% of total). \nWriting a new coding_doc with just those utterances..."))
  
  update_doc$coder <- NA
  update_doc$date <- NA
  update_doc$context <- NA
  # each utterance should be coded multiple times if there is overlap between windows. 
  # duplicate the coding rows for each time an utterance should be coded
  update_doc1 <- update_doc
  update_doc1$pass <- 1
  update_doc2 <- update_doc
  update_doc2$pass <- 2
  update_doc3 <- update_doc
  update_doc3$pass <- 3
  
  update_doc <- rbind(update_doc1, update_doc2, update_doc3)
}

CodeContexts <- function(this_pass=1, window_size=30, slide_by=3){
  # check whether packages need to be installed
  list.of.packages <- c("dplyr", "car", "tidyr", "pbkrtest")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  for(p in list.of.packages) {
    library(p, character.only=TRUE)
  }
  
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

doc_doctor <- function(recursive=T){
  if(!require(dplyr) ) install.packages("dplyr")
  docs <- dir(pattern="coding_doc.txt", recursive=recursive)
  message("Including the following coding documents:\n", paste(docs, collapse="\n"), "\n")
  
  for(i in 1:length(docs)){
    # load the coding doc
    message("Checking ", docs[i], "...\n")
    coding_doc <- read.table(docs[i], header=1, sep="\t", stringsAsFactors=F)
    
    # make all coder initials upper case
    coding_doc$coder <- toupper(coding_doc$coder)
    # make all contexts lower case
    coding_doc$context <- tolower(coding_doc$context)
    
    library(dplyr)
    bads <- filter(coding_doc, ( coder=="RM" | coder=="TEST" | coder=="" & !is.na(date) | nchar(context) < 3 & !is.na(date) ) | pass < 1 ) # likely issues in the coding
    goods <- filter(coding_doc, !( coder=="RM" | coder=="TEST" | coder=="" & !is.na(date) | nchar(context) < 3 & !is.na(date) ) | is.na(coder) & pass > 0) # the opposite of those issues
    if(nrow(bads)>0){
      message(nrow(bads), " bad cases found.")
      print <- FALSE
      print <- grepl(readline("Print bad cases for review now? (y/n) "), "y")
      if(print){
        print(as.matrix(bads))
      }
      
      correct_errors <- FALSE
      correct_errors <- grepl(readline("Do you wish to correct these cases now? \nDoing so will overwrite your old coding file with the new clean one. \n(y/n): "), "y")
      
      if(correct_errors){
        bads$coder <- NA
        bads$date <- NA
        bads$context <- NA
        
        if ( nrow(coding_doc) != nrow(goods) + nrow(bads) )  stop("oh no!")
        coding_doc <- rbind(goods, bads)
        
        message(nrow(filter(coding_doc, coder=="RM" | coder=="TEST" | coder=="" & !is.na(date))), " bad cases left. :)")
        
        # write updated coding_doc to file
        message("Writing the squeaky clean doc...")
        write.table(coding_doc, file=docs[i], quote=F, col.names=T, row.names=F, append=F, sep="\t")
      }
    }
  }
  message("All done!\n")
}

collect_codes <- function(){
  
  docs <- dir(pattern="coding_doc.txt", recursive=TRUE)
  message("Including the following coding documents:\n", paste(docs, collapse="\n"))
  
  # pull out the coded portion from each doc
  for(i in 1:length(docs)){
    this.doc <- read.table(docs[i], header=1, sep="\t", stringsAsFactors=F)
    coded <- dplyr::filter(this.doc, !is.na(context))
    stopifnot( nrow(dplyr::filter(coded, coder=="RM" | coder=="TEST" | coder=="" & !is.na(date))) == 0 )
    if(i==1) {
        doc <- coded 
      } else {
        doc <- base::rbind(doc, coded)
    }
  }
  # clean up some bad punctuation
  doc$context <- gsub(pattern=",", x=doc$context, replacement=";", fixed=T)
  doc$context <- gsub(pattern="/", x=doc$context, replacement=";", fixed=T)
  doc$context <- tolower(doc$context)
  
  message(" Removing ", nrow(doc) - nrow(unique(doc)), " duplicate rows.")
  doc <- unique(doc) # there are duplicate rows because of copying the coding docs when I was originally starting the RAs on coding
  
  return(doc)
}

new_codes <- function( raw_codes, cols=c("raw", "clean"), key_file ){
  message(paste0("Checking to see if we need to update the cleaning key ", key_file, "...")) 

          key <- read.table(key_file, header=1, sep="\t", stringsAsFactors=F)
          
          message("(Note that the first column of the key needs to correspond to raw codes)\n")
          new_raw_codes <- raw_codes[!raw_codes %in% key[,1]]
          
          message(length(new_raw_codes), " new raw codes (not already in the key).")
          if(length(new_raw_codes) > 0) {
            add_codes <- FALSE
            add_codes <- grepl(readline("Do you wish to add these codes now? (y/n): "), "y")
            
            if(add_codes){
              new_code_pairs <- data.frame(raw=new_raw_codes, clean=NA, stringsAsFactors=F)
              colnames(new_code_pairs) <- cols
              
              message("\nEnter the clean code for each raw code \n")
              for(i in 1:nrow(new_code_pairs)){
                
                change <- readline(paste0("Change '", new_code_pairs[i,1], "'? (y/n) "))
                
                if( change == "n" ) {
                  new_code_pairs[i,2] <- as.character(new_code_pairs[i,1]) # make clean code the same as raw code if change is "no" 
                } else if( change == "y" ){
                  
                  replace_confirm <- FALSE
                  while(!replace_confirm){
                    clean_code <- readline("Replace with: ")
                    replace_confirm <- readline("Confirm? (y/n) ") =="y"
                  }
                  new_code_pairs[i,2] <- clean_code
                } else stop("Error. Change must be y or n")
              }
              key <- rbind(key, new_code_pairs)
              key <- key[ order(key[,1]) , ] # re-sort the key based on the first column (raw codes)
              write.table(key, key_file, quote=F, col.names=T, row.names=F, append=F, sep="\t")
              
              message("New contexts added! :) \n")
            }
          } 
}

process_codes <- function(doc, min.codes=10, max.codes=10){
  if(!require(tidyr)) install.packages("tidyr"); library(tidyr)
  if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
  if(!require(psych)) install.packages("psych"); library(psych)
  if(!require(GPArotation)) install.packages("GPArotation"); library(GPArotation)
  
  doc <- doc %>% 
    dplyr::filter( !grepl("^[[:blank:]]*$",doc$context)) %>% # cleaning out empty codes
    tidyr::unite(utt, file, UttNum) %>% 
    as.tbl() # for speed
  
  doc$coder <- toupper(doc$coder)
  
  RA_info <- doc %>%
    dplyr::select(utt, coder, context, pass) %>%
    group_by(coder) %>%
    dplyr::summarize(n_utts_codes=n())
  
  message("\nRAs have coded this many utterances:\n") ; print(as.data.frame(RA_info))
  
  counts <- doc %>%
    filter(!is.na(context)) %>% 
    count(utt)
  
  message("\nUtterances have been coded this many times across coders:\n") ; print(summary(counts$n))
  
  # only keep utterances that have been coded at least [min.codes] times and no more than [max.codes] times
  in.range <- counts %>% 
    dplyr::filter(n >= min.codes) %>% 
    dplyr::filter(n <= max.codes) %>% 
    select(utt) %>% # drop the n column
    left_join(doc, by="utt")
  
  # for utterances with more than [max.codes], randomly select [max.codes] of them
  above.max.sample <- counts %>% 
    dplyr::filter(n > max.codes) %>% # only keep utterances with more than [max.codes]
    select(utt) %>% # drop the n column
    left_join(doc, by="utt") %>% # re-expand it back to doc, but only for the selected utts
    group_by(utt) %>% 
    sample_n(max.codes) # group_by() and then sample_n() takes random samples from each group
  
  doc_keep <- rbind(in.range, above.max.sample)  
  
  message("Removing ", length(unique(doc$utt)) - length(unique(doc_keep$utt)) , " utterances because they have been coded fewer than ", min.codes, " times across all coders.\n")    
  message( 100*round( length(unique(doc_keep$utt))/length(unique(doc$utt)), 4), "% of total utterances are included in analyses.\n" )
  
  doc_keep$context <- as.factor(doc_keep$context)
  
  maxcontexts <- 10 # the maximum number of contexts that can be read for one window
  
  doc_keep <- doc_keep %>% 
    tidyr::separate(col=context, into=paste("context", 1:maxcontexts, sep="."), sep="[[:blank:]]*;[[:blank:]]*", extra="drop") %>% 
    tidyr::gather(key="contextnum", value="context", starts_with("context."), na.rm=TRUE)

  return(doc_keep)
}

clean_contexts <- function(doc, key_file="context_cleaning_keys.txt" ){
  # check if any codes in the coding doc are missing from the cleaning key, and if so add them
  raw_codes <- sort(unique(doc$context))
  new_codes(raw_codes, cols=c("context_raw", "context_clean"), key_file)
  # read in the key again, to get any updates
  cleaning_keys <- read.table(key_file, header=1, sep="\t", stringsAsFactors=F)
  
  doc <- doc %>% 
    left_join(cleaning_keys, by=c("context"="context_raw")) %>% 
    select( -context ) %>% 
    rename(context = context_clean)
  
  doc$context <- as.factor(doc$context)
  summary(doc$context)
  
  doc <- dplyr::filter(doc, context !="TEST") # cleaning
  doc <- dplyr::filter(doc, context !="") # cleaning
  
  return(doc)
}

clean_categories <- function(doc, key_file="categories_cleaning_keys.txt" ){
  # check if any context codes are missing from the categories key, and if so add them
  new_codes(raw_codes=unique(doc$context), cols=c("context_clean", "category"), key_file)
  # read in the key again, to get any updates
  categories_keys <- read.table(key_file, header=1, sep="\t", stringsAsFactors=F)
  
  # print a nice (.md) version of the categories_cleaning_keys.txt, to link to online
  tb <- categories_keys
  colnames(tb) <- c("context (raw)", "context category (clean)")
  tb <- knitr::kable(tb)
  writeLines(tb, "categories_keys.md")
  
  # add categories to doc
  doc <- left_join(doc, categories_keys, by=c("context" = "context_clean"))

  # stopifnot( length(unique(doc$category)) == length(unique(categories_keys$category)) )
  
  summary(as.factor(doc$category)) 
  
  # check to make sure that one coder isn't contributing 2 hits on the same category for the same utterance
  # e.g. if a coder tagged an utterance as "eating ; mealtime" then it would result in "mealtime" and "mealtime" as the categories
  cat.check <- doc %>%
    count(utt, coder, date, category) 
  table(cat.check$n) # see how many times a single utterance is coded by the same coder multiple times with the same category
  
  # only keep one of the same category code per coder per day (i.e. if there's more than one hit from the same coder on the same day for the same category, collapse it)
  clean_doc <- cat.check %>% 
    select( utt, coder, date, category ) # drop the extra columns
  
  return(clean_doc)
}
