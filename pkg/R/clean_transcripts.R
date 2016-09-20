
clean_transcripts <- function(wd="./transcripts/"){
  
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
