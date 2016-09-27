#' @export
blank_doc <- function(wd="./transcripts/", for.coding=TRUE){
  # write blank coding_doc.txt for coders to code from
  stopifnot(require(dplyr))
  
  # make an empty coding_doc dataframe, which will be filled in by the for loop below
  coding_doc <- data.frame(LineNum=NULL, UttNum=NULL, file=NULL, utterance=NULL)
  
  transcripts <- clean_transcripts(wd)
  
  files <- list.files(path=wd)
  Nfiles <- length(files)
  
  for(f in 1:Nfiles){
    this_coding_doc <- transcripts[[f]] %>% 
      # take just the LineNum, UttNum, and utterance from each transcript row
      dplyr::select(LineNum, UttNum, utterance) %>% 
      # only keep utterances (not comments, etc. which will have NA UttNum)
      dplyr::filter(!is.na(UttNum)) 
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
