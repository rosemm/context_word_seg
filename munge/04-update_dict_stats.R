
# add number of syllables for each word to dictionary
dict$N.syl <-sapply(X = strsplit(as.character(dict$phon), split="-", fixed=TRUE), length)

# add frequency for each word to dictionary
orth_words <- strsplit(paste(df$orth, collapse=" "), split=" ", fixed=T)[[1]] # the vector of words (df$orth, collapsed into one paste, and then split into a unit for each word)

# drop empty elements
freq_orth <- data.frame(word=orth_words[orth_words != ""], stringsAsFactors = FALSE) %>% 
  count(word) # get count of each word

dict <- left_join(dict, freq_orth, by="word") %>% 
  rename(freq.orth = n)

cache('dict')
