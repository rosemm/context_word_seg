#################################
# read in dict
#################################

dict <- read.table("CORPUS_korman_from_swingley2005/dict_all3.txt", sep=" ", quote="", comment.char ="")
colnames(dict) <- c("word", "phon")

# add some entries to the dict file
dict.add <- data.frame(word=    c("shh", "ssh", "mummie", "mummie's", "mummmy's", "mummie'll", "tellie", "handie", "pottie", "bathie", "ruskie", "headie", "chinnie", "hankie", "nappie", "drinkie", "rolly", "rollie", "polly", "pollie", "deary",  "hiccough", "hiccoughs", "poo",  "lew", "&hmm", "&er", "&eh", "&ha", "&mm", "&ah", "&uh", "&hah", "whee", "treas",  "hee", "oo"), 
                       old.word=c("sh",  "sh",  "mummy",  "mummy's",  "mummy's",  "mummy'll",  "telly",  "handy",  "potty",  "bathy",  "rusky",  "heady",  "chinny",  "hanky",  "nappy",  "drinkee", "roley", "roley",  "poley", "poley",  "dearie", "hiccup",   "hiccups",   "pooh", "lu",  "hmm",  "er",  "eh",  "ha",  "mm",  "aah", "uh",  "ha",   "wee",  "treazh", "he",  "ooh"),
                       phon=NA)
for(d in 1:nrow(dict.add)){
  # for each new entry, use the phon for its homophone's entry
  dict.add[d,]$phon <- as.character(dict[as.character(dict$word)==dict.add[d,]$old.word , ]$phon) 
}
dict.add$old.word <- NULL # drop the old word column, so this is just orth and phon, like the rest of the dict entries
dict <- rbind(dict, dict.add) # add it to the dict


# add number of syllables for each word to dictionary
dict$N.syl <- NA
for(i in 1:nrow(dict)){
  dict$N.syl[i] <- length(strsplit(as.character(dict$phon[i]), split="-", fixed=TRUE)[[1]])
}


write.table(dict, file="dict_all3_updated.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")