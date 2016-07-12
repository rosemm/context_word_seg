# to translate CHILDES transcripts to phon approximations using Swingley's dictionary
# generates utt_orth_phon_KEY.txt

source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/R/coding_functions.R")

# use BlankDoc to turn all of the transcripts into one neat dataframe (but omit the coder columns)
coding_doc <- BlankDoc(for.coding=F)


coding_doc <- coding_doc %>%
  unite(utt, file, UttNum) %>%
  select(utt, orth=utterance)

write.table(coding_doc, file="utt_orth_KEY.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
# coding_doc <- read.table("utt_orth_KEY.txt", header=1, sep="\t", stringsAsFactors=F, quote="", comment.char ="")


####################################################################
# translate coding doc utterances
####################################################################
coding_doc <- coding_doc[!grepl(pattern="unintelligible", x=coding_doc$orth), ] # delete utterances with unintelligible content

translate_phon <- function(dict, orth, first=TRUE){ # translate orth to phon based on dict
  
  # tidying the utterances to match conventions in the dict file
    if(first){
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
      phon <- tolower(orth)
    } 
    if(!first) phon <- orth 
    
  for(i in 1:nrow(dict)){
    # for each entry in the "dictionary", find that orth word and replace it with the phon translation
    this.word <- as.character(dict$word[i])
    this.phon <- as.character(dict$phon[i])
    
    phon <- gsub(pattern=paste("[[:space:]]", this.word, "[[:space:]]", sep=""), replacement=paste(" ",this.phon, " ", sep=""), x=phon)
    phon <- gsub(pattern=paste("^", this.word, "[[:space:]]", sep=""), replacement=paste(this.phon, " ", sep=""), x=phon)
    phon <- gsub(pattern=paste("[[:space:]]", this.word, "$", sep=""), replacement=paste(" ",this.phon, sep=""), x=phon)
    phon <- gsub(pattern=paste("^", this.word, "$", sep=""), replacement=this.phon, x=phon)
  }
  if(first) return( list(orth, phon) )
  if(!first) return(phon)
}

translated <- translate_phon(dict, orth=coding_doc$orth)
coding_doc$orth <- translated[[1]]
coding_doc$phon <- translated[[2]]

# note that words repeated immediately (e.g. "dear dear") will get missed by the gsub commands for some reason. Running it twice fixes it.
missed.dict <- dict[1,]
while(nrow(missed.dict) > 0){
  coding_doc$phon <- translate_phon(missed.dict, orth=coding_doc$phon, first=FALSE) # run through translation again for the missed words 
  
  phon.stream <- strsplit(paste(coding_doc$phon, collapse=" "), " ", fixed=T)[[1]]
  phon.stream <- grep(pattern=".*[[:alpha:]].*", x=phon.stream, value=T) # only keep ones that have at least one letter in them (drops punctuation-only items)
  
  missed.phon.counts <- phon.stream[is.na(match(phon.stream, as.character(dict$phon)))]
  missed.phon <- unique(missed.phon.counts)
  missed.word <- missed.phon[missed.phon %in% dict$word] 
  missed.dict <- dplyr::filter(dict, word %in% missed.word)
}

# which words didn't translate? (there was no phon available for that word)
table(missed.phon.counts) # how many utterances are affected by each missed phon?

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
