#' @export
process_codes <- function(doc, min.codes=10, max.codes=10, unique.coders){
  if(!require(tidyr)) install.packages("tidyr"); library(tidyr)
  if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
  if(!require(psych)) install.packages("psych"); library(psych)
  if(!require(GPArotation)) install.packages("GPArotation"); library(GPArotation)
  
  doc <- doc %>% 
    dplyr::filter( !grepl(pattern="^[[:blank:]]*$", x=doc$context)) %>% # cleaning out empty codes
    tidyr::unite(utt, file, UttNum) %>% 
    as.tbl() # for speed
  
  doc$coder <- toupper(doc$coder)
  
  RA_info <- doc %>%
    dplyr::select(utt, coder, context, pass) %>%
    count(coder) %>%
    arrange(n)
  
  message("\nRAs have coded this many utterances (raw coding):\n") ; print(as.data.frame(RA_info))
  
  if(unique.coders){
    # for utterances with more than 1 coding event from the same coder, randonly select 1 of them
    doc.unique <- doc %>% 
      group_by(utt, coder) %>% 
      sample_n(1) %>% # group_by() and then sample_n() takes random samples from each group (utt, coder)
      ungroup()
    
    message("Removing ", nrow(doc) - nrow(doc.unique), " coding events (", 100*round(nrow(doc.unique)/nrow(doc), 2) ,"%) because the same utterance has been coded more than one time by the same coder.\n")   
    doc <- doc.unique
  }
  
  counts <- doc %>%
    dplyr::filter(!is.na(context)) %>% 
    count(utt)
  
  message("NOTE: Only allow each coder to code each utterance once? ", unique.coders)
  message("Utterances have been coded this many times across coders:\n") ; print(summary(counts$n))
  
  # only keep utterances that have been coded at least [min.codes] times and no more than [max.codes] times
  in.range <- counts %>% 
    dplyr::filter(n >= min.codes) %>% 
    dplyr::filter(n <= max.codes) %>% 
    dplyr::select(utt) %>% # drop the n column
    left_join(doc, by="utt") %>% 
    ungroup()
  
  # for utterances with more than [max.codes], randomly select [max.codes] of them
  above.max.sample <- counts %>% 
    dplyr::filter(n > max.codes) %>% # only keep utterances with more than [max.codes]
    dplyr::select(utt) %>% # drop the n column
    left_join(doc, by="utt") %>% # re-expand it back to doc, but only for the selected utts
    group_by(utt) %>% 
    sample_n(max.codes) %>% # group_by() and then sample_n() takes random samples from each group (utt)
    ungroup()
  
  doc_keep <- rbind(in.range, above.max.sample)  
  
  message("Removing ", length(unique(doc$utt)) - length(unique(doc_keep$utt)) , " utterances because they have been coded fewer than ", min.codes, " times across all coders.\n")    
  message( 100*round( length(unique(doc_keep$utt))/length(unique(doc$utt)), 4), "% of total utterances are included in analyses.\n" )
  
  RA_update <- doc_keep %>% 
    count(coder) %>% 
    arrange(n)
  
  message("\nRAs have coded this many utterances (after all of the cleaning and exclusions):\n") ; print(as.data.frame(RA_update))
  doc_keep$context <- as.factor(doc_keep$context)
  
  maxcontexts <- 10 # the maximum number of contexts that can be read for one window
  
  doc_keep <- doc_keep %>% 
    tidyr::separate(col=context, into=paste("context", 1:maxcontexts, sep="."), sep="[[:blank:]]*;[[:blank:]]*", extra="drop") %>% 
    tidyr::gather(key="contextnum", value="context", starts_with("context."), na.rm=TRUE) %>% 
    dplyr::filter(!grepl(pattern="^[[:blank:]]*$", x=context))
  
  return(doc_keep)
}
