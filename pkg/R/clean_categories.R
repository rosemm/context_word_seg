
clean_categories <- function(doc, dir, key_file="categories_cleaning_keys.txt", interactive = TRUE ){
  # check if any context codes are missing from the categories key, and if so add them
  new_codes(raw_codes=unique(doc$context), cols=c("context_clean", "category"), file.path(dir, key_file), interactive)
  # read in the key again, to get any updates
  categories_keys <- read.table(file.path(dir, key_file), header=1, sep="\t", stringsAsFactors=F)
  
  # print a nice (.md) version of the categories_cleaning_keys.txt, to link to online
  tb <- arrange(categories_keys, category)
  colnames(tb) <- c("context (raw)", "context category (clean)")
  tb <- knitr::kable(tb)
  writeLines(tb, file.path(dir, "categories_keys.md") )
  
  # add categories to doc
  doc$context <- as.character(doc$context)
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
    dplyr::select( utt, coder, date, category ) # drop the extra columns
  
  return(list(doc=doc, clean_doc=clean_doc))
}
