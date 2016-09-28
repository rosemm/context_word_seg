#' @export
clean_transcripts <- function(wd="./transcripts/"){
  
  files <- list.files(path=wd)
  Nfiles <- length(files)
  if(Nfiles==0) stop("No transcripts found. Check the working directory.")
  
  transcripts <- list(NULL)
  
  for(f in 1:Nfiles){
    this.file <- files[f]
    if(is.na(this.file)) stop(paste(this.file, "not found."))
    this.transcript <- readLines(file.path(wd, this.file))
    
    # The split between speaker id and utterance is :\t (colon, then tab), e.g. *MOT: Is that your train?
    # But somtimes that pattern also occurs later in the utterance, 
    # especially on the %pho tier where : is used for vowel length.
    # To restrict the :\t split to only the one that divides speaker ID and utterance, 
    # only look for the first time it happens.
    # @Comment:\t is used occassionally to indicate activity happening (Korman corpus). 
    # For length consistency, substitue "@Comment" with "@Com" throughout.
    this.transcript <- gsub(pattern="@Comment", replacement="@Com", x=this.transcript)
    this.transcript <- gsub(pattern="@Participants", replacement="@Par", x=this.transcript)
    this.transcript <- gsub(pattern="@Situation", replacement="@Sit", x=this.transcript)
    this.transcript <- gsub(pattern="@Activities", replacement="@Act", x=this.transcript)
    
    transcript.split <- stringr::str_split_fixed(this.transcript, pattern = ":\t", n=2)
    speaker <- transcript.split[, 1] # the content before the :\t in each line (e.g. *MOT)
    utterance <- transcript.split[, 2] # the content after the :\t in each line (e.g. Is that your train?)
    
    data <- data.frame(speaker=speaker, utterance=utterance)
    data$LineNum <- as.numeric(row.names(data))
    
    # Add utterance numbers
    # create a temp dataframe that's just the lines with utterances (speaker has an * in it)
    temp <- dplyr::filter(data, grepl("*", speaker, fixed=TRUE))
    # number the utterances 1 to the end of the data frame
    temp$UttNum <- 1:nrow(temp)
    # merge the temp dataframe back in with the rest of it, to line up the UttNums with the LineNums in the full transcript
    # note that any lines that don't have an utterance (such as comments and header material) will have NA for UttNum
    data <- merge(data, temp, by=c("LineNum", "speaker", "utterance"), all=T, sort=T)
    
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
    
    # cleaning up the CHAT annotations, to make them a little more readable
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
