#################################
# read in dict and update it
#################################

dict <- read.table("corpora/dict_all3.txt", sep=" ", quote="", comment.char ="", stringsAsFactors = FALSE)
colnames(dict) <- c("word", "phon")

# add some alternate spelling entries to the dict file
dict.add <- data.frame(word=    c("shh", "ssh", "mummie", "mummie's", "mummmy's", "mummie'll", "tellie", "handie", "pottie", "bathie", "ruskie", "headie", "chinnie", "hankie", "nappie", "drinkie", "rolly", "rollie", "polly", "pollie", "deary",  "hiccough", "hiccoughs", "poo",  "lew", "&hmm", "&er", "&eh", "&ha", "&mm", "&ah", "&uh", "&hah", "whee", "treas",  "hee", "oo"), 
                       old.word=c("sh",  "sh",  "mummy",  "mummy's",  "mummy's",  "mummy'll",  "telly",  "handy",  "potty",  "bathy",  "rusky",  "heady",  "chinny",  "hanky",  "nappy",  "drinkee", "roley", "roley",  "poley", "poley",  "dearie", "hiccup",   "hiccups",   "pooh", "lu",  "hmm",  "er",  "eh",  "ha",  "mm",  "aah", "uh",  "ha",   "wee",  "treazh", "he",  "ooh"),
                       stringsAsFactors = FALSE)
dict.add <- left_join(dict.add, dict, by=c("old.word"="word")) %>% 
  select(- old.word)

dict <- full_join(dict, dict.add, by = c("word", "phon"))

# add number of syllables for each word to dictionary
dict$N.syl <-sapply(X = strsplit(as.character(dict$phon), split="-", fixed=TRUE), length)

write.table(dict, file="corpora/dict_all3_updated.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")