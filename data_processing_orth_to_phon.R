source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/coding_scripts.r")
coding_doc <- BlankDoc(for.coding=F)
coding_doc <- coding_doc %>%
  unite(utt, file, UttNum) %>%
  select(utt, orth=utterance)

# write.table(coding_doc, file="utt_orth_KEY.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
# coding_doc <- read.table("utt_orth_KEY.txt", header=1, sep="\t", stringsAsFactors=F, quote="", comment.char ="")

#################################
# translate orth to phon
#################################

dict <- read.table("eng_korman_from_swingley2005/dict_all3.txt", sep=" ", quote="", comment.char ="")
colnames(dict) <- c("word", "phon")

# add some entries to the dict file
dict.add <- data.frame(word=    c("mummie", "mummie's", "mummie'll", "tellie", "handie", "pottie", "bathie", "ruskie", "headie", "chinnie", "hankie", "nappie", "drinkie", "rolly", "rollie", "polly", "pollie", "deary",  "hiccough", "hiccoughs", "poo",  "lew", "&hmm", "&er", "&eh", "&ha", "&mm", "&ah", "&uh", "&hah", "whee", "treas",  "hee", "oo"), 
                       old.word=c("mummy",  "mummy's",  "mummy'll",  "telly",  "handy",  "potty",  "bathy",  "rusky",  "heady",  "chinny",  "hanky",  "nappy",  "drinkee", "roley", "roley",  "poley", "poley",  "dearie", "hiccup",   "hiccups",   "pooh", "lu",  "hmm",  "er",  "eh",  "ha",  "mm",  "aah", "uh",  "ha",   "wee",  "treazh", "he",  "ooh"),
                       phon=NA)
for(d in 1:nrow(dict.add)){
 dict.add[d,]$phon <- as.character(dict[as.character(dict$word)==dict.add[d,]$old.word , ]$phon) 
}
dict.add$old.word <- NULL
dict <- rbind(dict, dict.add)

coding_doc <- coding_doc[!grepl(pattern="unintelligible", x=coding_doc$orth), ] # delete utterances with unintelligible content

translate_phon <- function(dict, orth, first=TRUE){ # translate orth to phon based on dict
  
  # tidying the utterances to match conventions in the dict file
    phon <- orth
    if(first){
      phon <- tolower(phon)
      phon <- gsub(pattern="[?]", replacement="", x=phon) # delete question marks
      phon <- gsub(pattern="[.]", replacement="", x=phon) # delete periods
      phon <- gsub(pattern="[!]", replacement="", x=phon) # delete exclamation points
      phon <- gsub(pattern="[;]", replacement="", x=phon) # delete semicolons
      phon <- gsub(pattern="mummmy", replacement="mummy", x=phon) #typo
      phon <- gsub(pattern="nosy", replacement="nosey", x=phon) #typo
      phon <- gsub(pattern="loulou", replacement="lou-lou", x=phon)
      phon <- gsub(pattern="tata's", replacement="ta-ta's", x=phon)
      phon <- gsub(pattern="cmon", replacement="c'mon", x=phon)
      phon <- gsub(pattern="uhuh", replacement="uh-huh", x=phon)
      phon <- gsub(pattern="tumtum", replacement="tum tum", x=phon)
      phon <- gsub(pattern="haha", replacement="ha ha", x=phon)
      phon <- gsub(pattern="ahhah", replacement="aah ha", x=phon)
      phon <- gsub(pattern="[[].*[]]", replacement="", x=phon) # delete bracket patterns from the utterances (used to provide notes or translations of unconventional speech)
      phon <- gsub(pattern="bath+room", replacement="bathroom", x=phon, fixed=T)
      phon <- gsub(pattern="play+mate", replacement="playmate", x=phon, fixed=T)
      phon <- gsub(pattern="t+shirt", replacement="t-shirt", x=phon, fixed=T)
      phon <- gsub(pattern="sack+cloth", replacement="sackcloth", x=phon, fixed=T)
      phon <- gsub(pattern="+", replacement=" ", x=phon, fixed=T) # replace remaining + with a space (used to join words, e.g. "patty+cake")
      phon <- gsub(pattern="_", replacement=" ", x=phon, fixed=T) # also used to join words (e.g. "all_gone")
      phon <- gsub(pattern="[(]i)t", replacement="it", x=phon)
      phon <- gsub(pattern="y[(]ou[)]", replacement="you", x=phon)
      phon <- gsub(pattern="[(]i[)]f", replacement="if", x=phon)
      phon <- gsub(pattern="d[(]o[)]", replacement="do", x=phon)
      phon <- gsub(pattern="t[(]o[)]", replacement="to", x=phon)
      phon <- gsub(pattern="[(]h[)]ave", replacement="'ave", x=phon)
      phon <- gsub(pattern="[(]h[)]ere", replacement="'ere", x=phon)
      phon <- gsub(pattern="[(].*[)]", replacement="", x=phon) # delete remaining () patterns from the utterances (used for shortened speech, e.g. "(be)cause")
      phon <- gsub(pattern="[:]", replacement="", x=phon) # delete remaining colons (used to indicate drawn-out words, e.g. "oooooh" as "o:h")
    } 
      
    
  
  for(i in 1:nrow(dict)){
    this.word <- as.character(dict$word[i])
    this.phon <- as.character(dict$phon[i])
    
    phon <- gsub(pattern=paste("[[:space:]]", this.word, "[[:space:]]", sep=""), replacement=paste(" ",this.phon, " ", sep=""), x=phon)
    phon <- gsub(pattern=paste("^", this.word, "[[:space:]]", sep=""), replacement=paste(this.phon, " ", sep=""), x=phon)
    phon <- gsub(pattern=paste("[[:space:]]", this.word, "$", sep=""), replacement=paste(" ",this.phon, sep=""), x=phon)
    phon <- gsub(pattern=paste("^", this.word, "$", sep=""), replacement=this.phon, x=phon)
  }
  return(phon)
}

coding_doc$phon <- translate_phon(dict, orth=coding_doc$orth)

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
coding_doc_clean <- coding_doc
for(i in 1:length(missed.phon) ){
  coding_doc_clean <- coding_doc_clean[!grepl(pattern=paste0("[[:space:]]", missed.phon[i], "[[:space:]]"), x=coding_doc_clean$phon), ]
  coding_doc_clean <- coding_doc_clean[!grepl(pattern=paste0("^", missed.phon[i], "[[:space:]]"), x=coding_doc_clean$phon), ]
  coding_doc_clean <- coding_doc_clean[!grepl(pattern=paste0("[[:space:]]", missed.phon[i], "$"), x=coding_doc_clean$phon), ]
  coding_doc_clean <- coding_doc_clean[!grepl(pattern=paste0("^", missed.phon[i], "$"), x=coding_doc_clean$phon), ]
}
message("\n...now ", nrow(coding_doc_clean), " utterances in coding_doc_clean. ", 100*round(nrow(coding_doc_clean)/nrow(coding_doc), 4), "% utterances remain\n")

# write.table(coding_doc_clean, file="utt_orth_phon_KEY.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
# key <- read.table("utt_orth_phon_KEY.txt", header=1, sep="\t", stringsAsFactors=F, quote="", comment.char ="")
