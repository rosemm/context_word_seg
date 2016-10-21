
df.wn.count <- df %>%
  select(-phon) %>%
  extract(col=utt, into=c("child", "age_weeks"), regex="^([[:alpha:]]{2})([[:digit:]]{2})", remove=FALSE) %>%
  extract(col=utt, into=c("file", "utt.num"), regex="^([[:alpha:]]{2}[[:digit:]]{2})[.]cha_([[:digit:]]+)", remove=FALSE) %>%
  filter(child != "hi") # remove this child, since the transcripts are so short

wn <- 30 # number of utterances in each window
# add wn blank lines at the end of each file, so we can break it into wn-utterance windows without spanning recordings
df.wn.count <- group_by(df.wn.count, file) %>%
  do({
    max.utt.num <- as.numeric(max(as.numeric(.$utt.num)))
    new.rows <- data.frame(utt=NA, 
                           file=.$file[1], 
                           utt.num=seq(from=(max.utt.num+1), to=(max.utt.num+wn)),
                           child=NA,
                           age_weeks=NA,
                           orth=NA,
                           stringsAsFactors=FALSE)
    file.data <- rbind(., new.rows)
    n <- ceiling(nrow(file.data)/wn) # how many levels will be needed in the counter?
    file.data$wn.count <- gl(n=n, k=wn, labels=paste0(paste(sample(letters, 20, replace=T), collapse=""), 1:n))[1:nrow(file.data)] 
    # add a counter that goes 1 to wn, so we can split the resulting data frame using that counter
    return(file.data)
  })

df.wn.count <- na.omit(df.wn.count) # delete the extra empty rows made as a buffer between recordings

stopifnot(wn >= max(table(df.wn.count$wn.count))) # if this is greater than wn, error!

# collapse utterances from within each wn-utterance window
doc.data <- group_by(df.wn.count, wn.count) %>%
  dplyr::select(child, age_weeks, orth, wn.count) %>%
  do({
    doc.data <- data.frame(child=.$child[1],
                           age_weeks=.$age_weeks[1],
                           # up until this point, each utterance has been one row in the df
                           # now collapse so each row is one window with up to wn utterances (bag of words)
                           documents=paste(.$orth, collapse=" "),
                           stringsAsFactors=FALSE)
  }) 
doc.data$child <- as.factor(doc.data$child)

TM_doc.data <- doc.data
cache('TM_doc.data')

# adding the phon column back in
df.wn.count <- df.wn.count %>%
  ungroup() %>%
  left_join(df, by=c("utt", "orth")) 

cache('df.wn.count')

# prep
processed <- stm::textProcessor(doc.data$documents, 
                           metadata = doc.data, 
                           removestopwords=TRUE, 
                           wordLengths=c(1, Inf))
out <- stm::prepDocuments(processed$documents, 
                          processed$vocab, 
                          processed$meta,
                          lower.thresh = 2) 
# removes infrequent terms depending on user-set parameter lower.thresh (the minimum number of documents a word needs to appear in order for the word to be kept within the vocabulary)

docs <- out$documents
vocab <- out$vocab
meta <-out$meta

TM_doc_prep_out <- out
cache('TM_doc_prep_out')
