####################################################################
# translate orthographic utterances to phon approximations using Swingley's dictionary
# generates utt_orth_phon_KEY.txt
####################################################################

# since we need to apply this twice, save it as a function
translate_phon <- function(dict, orth, verbose = FALSE){ # translate orth to phon based on dict
  
  phon <- orth # start by copying it 
  
  # then swap in the translated forms
  
  for(i in 1:nrow(dict)){
    # for each entry in the "dictionary", find that orth word and replace it with the phon translation
    this.word <- as.character(dict$word[i])
    this.phon <- as.character(dict$phon[i])
    if( verbose ) message(this.word)
    
    phon <- gsub(pattern=paste("[[:space:]]", this.word, "[[:space:]]", sep=""), replacement=paste0(" ", this.phon, " "), x=phon)
    phon <- gsub(pattern=paste("^", this.word, "[[:space:]]", sep=""), replacement=paste0(this.phon, " "), x=phon)
    phon <- gsub(pattern=paste("[[:space:]]", this.word, "$", sep=""), replacement=paste0(" ", this.phon), x=phon)
    phon <- gsub(pattern=paste("^", this.word, "$", sep=""), replacement=this.phon, x=phon)
  }
  return( phon )
}

df$phon <- translate_phon(dict, df$orth, verbose = TRUE)

# note that words repeated immediately (e.g. "dear dear") will get missed by the gsub commands for some reason. Running it twice fixes it.
missed.dict <- dict
while( nrow(missed.dict) > 0 ){
  # run through translation again for the missed words
  df$phon <- translate_phon(missed.dict, df$phon)  
  
  phon.stream <- strsplit(paste(df$phon, collapse=" "), " ", fixed=T)[[1]]
  phon.stream <- grep(pattern="[[:alpha:]]+", x=phon.stream, value=T) # only keep ones that have at least one letter in them (drops punctuation-only items)
  
  missed.phon.counts <- phon.stream[is.na(match(phon.stream, as.character(dict$phon)))]
  missed.phon <- unique(missed.phon.counts)
  missed.word <- missed.phon[missed.phon %in% dict$word] 
  missed.dict <- dplyr::filter(dict, word %in% missed.word)
  if( nrow(missed.dict) > 0 ) message("********************************************\n Missed words remain in dict. Run it again.\n********************************************")
}

# which words didn't translate? (there was no phon available for that word)
sort(table(missed.phon.counts)); sum(table(missed.phon.counts)) # how many utterances are affected by each missed phon?

# delete utterances that contain untranslateable sounds
original.nrow <- nrow(df)
message("\n", original.nrow, " utterances in df... \n")

for(i in 1:length(missed.phon) ){
  df <- df[!grepl(pattern=paste0("[[:space:]]", missed.phon[i], "[[:space:]]"), x=df$phon), ]
  df <- df[!grepl(pattern=paste0("^", missed.phon[i], "[[:space:]]"), x=df$phon), ]
  df <- df[!grepl(pattern=paste0("[[:space:]]", missed.phon[i], "$"), x=df$phon), ]
  df <- df[!grepl(pattern=paste0("^", missed.phon[i], "$"), x=df$phon), ]
}
message("Deleting utterances with untranscribable material.\n...now ", nrow(df), " utterances in df. ", 100*round(nrow(df)/original.nrow, 4), "% utterances remain.\n")

if( length(df$orth[grepl(x=df$orth, pattern="[[:upper:]]")]) > 0 )  df$orth <- tolower(df$orth) # make sure the orth stream is all lower case

write.table(df, file="utt_orth_phon_KEY.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")

# Finalize df for anlaysis
df$orth <- tolower(df$orth) # make sure the orth stream is all lower case
df$phon <- gsub(x=df$phon, pattern="-", replacement=" ", fixed=TRUE) # make sure all word-internal syllable boundaries "-" are represnted just the same as between-word syllable boundaries (space)

cache('df')
