####################################################################
# to translate CHILDES transcripts to phon approximations using Swingley's dictionary
####################################################################
# generates utt_orth_phon_KEY.txt

# use blank_doc to turn all of the transcripts into one neat dataframe (but omit the coder columns)
coding_doc <- blank_doc(wd = "corpora/transcripts/", for.coding = FALSE) %>%
  unite(utt, file, UttNum) %>%
  select(utt, orth=utterance)

# delete the 7 utterances with "unintelligible" content
# (note that that this corpus does not adhere to all modern CHAT guidelines for indicating, for example, untranscribable speech)
coding_doc <- coding_doc[!grepl(pattern="unintelligible", x=coding_doc$orth), ] 

# tidying the utterances to match conventions in the dict file
orth <- coding_doc$orth # to make the following commands a little easier to read
orth <- gsub(pattern="@[[:alpha:]]+", replacement="", x=orth) # delete @ tags at the end of words (used to denote, for examle, word play or singing)
orth <- gsub(pattern="[?]", replacement="", x=orth) # delete question marks
orth <- gsub(pattern="[.]", replacement="", x=orth) # delete periods
orth <- gsub(pattern="[!]", replacement="", x=orth) # delete exclamation points
orth <- gsub(pattern="[;]", replacement="", x=orth) # delete semicolons
orth <- gsub(pattern="mummmy", replacement="mummy", x=orth) #typo
orth <- gsub(pattern="nosy", replacement="nosey", x=orth) #typo
orth <- gsub(pattern="loulou", replacement="lou-lou", x=orth)
orth <- gsub(pattern="tata's", replacement="ta-ta's", x=orth)
orth <- gsub(pattern="cmon", replacement="c'mon", x=orth)
orth <- gsub(pattern="uhuh", replacement="uh-huh", x=orth)
orth <- gsub(pattern="tumtum", replacement="tum tum", x=orth)
orth <- gsub(pattern="haha", replacement="ha ha", x=orth)
orth <- gsub(pattern="ahhah", replacement="aah ha", x=orth)
orth <- gsub(pattern="[[].*[]]", replacement="", x=orth) # delete bracket patterns from the utterances (used to provide notes or translations of unconventional speech)
orth <- gsub(pattern="bath+room", replacement="bathroom", x=orth, fixed=T)
orth <- gsub(pattern="play+mate", replacement="playmate", x=orth, fixed=T)
orth <- gsub(pattern="t+shirt", replacement="t-shirt", x=orth, fixed=T)
orth <- gsub(pattern="sack+cloth", replacement="sackcloth", x=orth, fixed=T)
orth <- gsub(pattern="+", replacement=" ", x=orth, fixed=T) # replace remaining + with a space (used to join words, e.g. "patty+cake")
orth <- gsub(pattern="_", replacement=" ", x=orth, fixed=T) # also used to join words (e.g. "all_gone")
orth <- gsub(pattern="[(]i)t", replacement="it", x=orth)
orth <- gsub(pattern="y[(]ou[)]", replacement="you", x=orth)
orth <- gsub(pattern="[(]i[)]f", replacement="if", x=orth)
orth <- gsub(pattern="d[(]o[)]", replacement="do", x=orth)
orth <- gsub(pattern="t[(]o[)]", replacement="to", x=orth)
orth <- gsub(pattern="[(]h[)]ave", replacement="'ave", x=orth)
orth <- gsub(pattern="[(]h[)]ere", replacement="'ere", x=orth)
orth <- gsub(pattern="[(].*[)]", replacement="", x=orth) # delete remaining () patterns from the utterances (used for shortened speech, e.g. "(be)cause")
orth <- gsub(pattern="[:]", replacement="", x=orth) # delete remaining colons (used to indicate drawn-out words, e.g. "oooooh" as "o:h")
coding_doc$orth <- tolower(orth) # copy it back over to the dataframe

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

coding_doc$phon <- translate_phon(dict, coding_doc$orth, verbose = TRUE)

# note that words repeated immediately (e.g. "dear dear") will get missed by the gsub commands for some reason. Running it twice fixes it.
missed.dict <- dict
while( nrow(missed.dict) > 0 ){
  # run through translation again for the missed words
  coding_doc$phon <- translate_phon(missed.dict, coding_doc$phon)  
  
  phon.stream <- strsplit(paste(coding_doc$phon, collapse=" "), " ", fixed=T)[[1]]
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
message("\n", nrow(coding_doc), " utterances in coding_doc... \n")
df <- coding_doc 
for(i in 1:length(missed.phon) ){
  df <- df[!grepl(pattern=paste0("[[:space:]]", missed.phon[i], "[[:space:]]"), x=df$phon), ]
  df <- df[!grepl(pattern=paste0("^", missed.phon[i], "[[:space:]]"), x=df$phon), ]
  df <- df[!grepl(pattern=paste0("[[:space:]]", missed.phon[i], "$"), x=df$phon), ]
  df <- df[!grepl(pattern=paste0("^", missed.phon[i], "$"), x=df$phon), ]
}
message("\n...now ", nrow(df), " utterances in df. ", 100*round(nrow(df)/nrow(coding_doc), 4), "% utterances remain\n")

if( length(df$orth[grepl(x=df$orth, pattern="[[:upper:]]")]) > 0 )  df$orth <- tolower(df$orth) # make sure the orth stream is all lower case

write.table(df, file="utt_orth_phon_KEY.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
