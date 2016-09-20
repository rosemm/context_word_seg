clean_contexts <- function(doc, dir, key_file="context_cleaning_keys.txt", interactive=TRUE ){
  # check if any codes in the coding doc are missing from the cleaning key, and if so add them
  raw_codes <- sort(unique(doc$context))
  new_codes(raw_codes, cols=c("context_raw", "context_clean"), file.path(dir, key_file), interactive)
  # read in the key again, to get any updates
  cleaning_keys <- read.table(file.path(dir, key_file), header=1, sep="\t", stringsAsFactors=F)
  
  doc <- doc %>% 
    left_join(cleaning_keys, by=c("context"="context_raw")) %>% 
    dplyr::select( -context ) %>% 
    rename(context = context_clean)
  
  doc <- dplyr::filter(doc, context !="TEST") # cleaning
  
  doc$context <- as.factor(doc$context)
  summary(doc$context)
  
  return(doc)
}
