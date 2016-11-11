#' @export
update_doc <- function(doc, criterion, unique.coders){
  # update an existing, partially coded document, to focus coders on the thin parts
  
  stopifnot(require(dplyr), require(tidyr), require(lubridate))
  
  doc$day <- as.POSIXct(doc$date, origin = '1970-01-01 00:00:00')
  
  if(!unique.coders){
    counts <- doc %>%
      tidyr::unite(utt, file, UttNum) %>% 
      dplyr::count(utt)
  }
  if(unique.coders) {
    counts <- doc %>%
      tidyr::unite(utt, file, UttNum) %>%
      dplyr::count(utt, coder) %>% 
      dplyr::summarize(n=n()) # running count twice makes it only max one hit per coder per utterance
  }
  # only keep utterances that have not been coded at least criterion times  
  below.crit <- counts %>%   
    dplyr::filter(n < criterion) %>% 
    dplyr::select(-n) %>% 
    dplyr::mutate(below.crit = 1)
  
  # the line numbers, utterance number, and file name for everything in doc
  blank_doc <- blank_doc() %>% 
    dplyr::select(LineNum, UttNum, file) %>% 
    tidyr::unite(utt, file, UttNum, remove=FALSE)
  nums <- blank_doc %>% 
    unique() %>% 
    as.tbl()
  
  # make a new coding doc with just the utterances below criterion number of codes,
  # and 30 utterances before and after (so each can still appear in its 30-utterance windows)
  update_doc <- nums %>% 
    dplyr::left_join(below.crit, by="utt") %>% 
    dplyr::mutate(include = within_k(below.crit, k=30, type = "either"),
                  include = rowSums(cbind(below.crit, include), na.rm = TRUE) > 0) %>% 
    dplyr::filter(include) %>% 
    dplyr::select(LineNum, UttNum, file) %>% 
    na.omit() %>% 
    dplyr::arrange(file, LineNum) 
  
  message(paste0(nrow(below.crit), " utterances still have fewer than ", criterion, " codes (", 100*round(nrow(below.crit)/length(unique(blank_doc$utt)), 2),"% of total). \nWriting a new coding_doc with just those utterances and the ones around them..."))
  
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
