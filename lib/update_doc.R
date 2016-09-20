#' @export
update_doc <- function(doc, criterion, unique.coders){
  # update an existing, partially coded document, to focus coders on the thin parts
  
  stopifnot(require(dplyr), require(tidyr), require(lubridate))
  
  doc$day <- as.POSIXct(doc$date, origin = '1970-01-01 00:00:00')
  
  if(!unique.coders){
    counts <- doc %>%
      unite(utt, file, UttNum) %>% 
      count(utt)
  }
  if(unique.coders) {
    counts <- doc %>%
      unite(utt, file, UttNum) %>%
      count(utt, coder) %>% 
      count(utt) # running count twice makes it only max one hit per coder per utterance
  }
  # only keep utterances that have not been coded at least criterion times  
  below.crit <- counts %>%   
    filter(n < criterion) 
  
  
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
  
  message(paste0(nrow(update_doc), " utterances still have fewer than ", criterion, " codes (", 100*round(nrow(update_doc)/length(unique(blank_doc$utt)), 2),"% of total). \nWriting a new coding_doc with just those utterances..."))
  
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
