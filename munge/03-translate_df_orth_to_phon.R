####################################################################
# translate orthographic utterances to phon approximations using Swingley's dictionary
# generates the data frame "df", which gets used in several other scripts
####################################################################

# since we need to apply this repeatedly in this script, save it as a function
translate_phon <- function(dict, orth, verbose = FALSE){ # translate orth to phon based on dict
  
  phon <- orth # start by copying it 
  
  # then swap in the translated forms
  
  for(i in 1:nrow(dict)){
    # for each entry in the "dictionary", find that orth word and replace it with the phon translation
    this.word <- as.character(dict$word[i])
    this.phon <- as.character(dict$phon[i])
    if( verbose ) message(this.word)
    
    # search for this.word in every position in which it could occur, and replace with this.phon
    # utterance-medial
    phon <- gsub(pattern=paste("[[:space:]]", this.word, "[[:space:]]", sep=""), replacement=paste0(" ", this.phon, " "), x=phon)
    # utterance-initial
    phon <- gsub(pattern=paste("^", this.word, "[[:space:]]", sep=""), replacement=paste0(this.phon, " "), x=phon)
    # utterance-final
    phon <- gsub(pattern=paste("[[:space:]]", this.word, "$", sep=""), replacement=paste0(" ", this.phon), x=phon)
    # whole utterance (occurs in isolation)
    phon <- gsub(pattern=paste("^", this.word, "$", sep=""), replacement=this.phon, x=phon)
  }
  return( phon )
}

# run translate_phon() the first time
df$phon <- translate_phon(dict, df$orth, verbose = FALSE)

# note that words repeated immediately (e.g. "dear dear") will get missed by the gsub commands because of the way gsub() searches. 
# Running it again fixes it.

# rather than running the whole dictionary every time (computationally expensive and unnecessary),
# only run the dict entries for words that have so far been missed in the translation
missed.dict <- dict # to begin with, use the whole dict
while( nrow(missed.dict) > 0 ){
  # run through translation again for the missed words
  # this loop will continue until there are no rows left in missed.dict
  
  df$phon <- translate_phon(missed.dict, df$phon)  # run it again, using df$phon as the input
  
  # to see which words have been missed, get a list of all "words" (characters surrounded by spaces) in df$phon
  phon.stream <- strsplit(paste(df$phon, collapse=" "), " ", fixed=T)[[1]]
  phon.stream <- grep(pattern="[[:alpha:]]+", x=phon.stream, value=T) # only keep ones that have at least one letter in them (drops punctuation-only items)
  
  # take the list of "words" in df$phon and pull out ones that can't match any entries in dict$phon, so match() will return NA
  # these will be orthographic forms that have not yet been translated
  missed.phon.counts <- phon.stream[is.na(match(phon.stream, as.character(dict$phon)))]
  missed.phon <- unique(missed.phon.counts)
  
  # the list of missed words, then, will be all "words" from missed.phon that match an entry in dict$word
  missed.word <- missed.phon[missed.phon %in% dict$word] 
  
  # update missed.dict to only include entries from missed.word
  missed.dict <- dplyr::filter(dict, word %in% missed.word)
  if( nrow(missed.dict) > 0 ) message("********************************************\n Missed words remain in dict. Run it again.\n********************************************")
}

# which words didn't translate? (there was no phon available for that word)
sort(table(missed.phon.counts))
# how many utterances are affected by each missed phon?
sum(table(missed.phon.counts)) 

# delete utterances that contain untranslateable sounds
original.nrow <- nrow(df)
message("\n", original.nrow, " utterances in df... \n")

for(i in 1:length(missed.phon) ){
  # search for the items in missed.phon in each of the positions in which they could occur
  # for each search, redefine df as all of df EXCEPT rows that match the search
  # i.e. delete any utterances containing missed.phon
  
  # utterance-medial
  df <- df[!grepl(pattern=paste0("[[:space:]]", missed.phon[i], "[[:space:]]"), x=df$phon), ]
  # utterance-initial
  df <- df[!grepl(pattern=paste0("^", missed.phon[i], "[[:space:]]"), x=df$phon), ]
  # utterance-final
  df <- df[!grepl(pattern=paste0("[[:space:]]", missed.phon[i], "$"), x=df$phon), ]
  # whole utterance (occurs in isolation)
  df <- df[!grepl(pattern=paste0("^", missed.phon[i], "$"), x=df$phon), ]
}
message("Deleting utterances with untranscribable material.\n...now ", nrow(df), " utterances in df. ", 100*round(nrow(df)/original.nrow, 4), "% utterances remain.\n")

if( length(df$orth[grepl(x=df$orth, pattern="[[:upper:]]")]) > 0 )  df$orth <- tolower(df$orth) # make sure the orth stream is all lower case

write.table(df, file="utt_orth_phon_KEY.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")

# Finalize df for anlaysis
df$orth <- tolower(df$orth) # make sure the orth stream is all lower case
df$phon <- gsub(x=df$phon, pattern="-", replacement=" ", fixed=TRUE) # make sure all word-internal syllable boundaries "-" are represnted just the same as between-word syllable boundaries (space)

df <- filter(df, !grepl(x=utt, pattern= "hi.*")) # remove this child, since the transcripts are so short

cache('df')
