rm(list = ls()) # clear the environment

# load required packages (and install them if they're not already installed)
list.of.packages <- c("dplyr", "tidyr" , "ggplot2", "reshape2", "knitr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


##########################################
# read in materials
##########################################
# read in the contexts
contexts <- read.csv("words by contexts.csv")

# read in the corpus
orth <- readLines("eng_korman_from_swingley2005/allEd11")
phon <- readLines("eng_korman_from_swingley2005/syl_ko_6b") 
message("\nNote that the word 'dear' is incorrectly entered as 'd7 in this corpus, and it should be 'd7R according to the dictionary file.\nThis is corrected in this analysis by substituting 'd7R for 'd7 in the phonetic speech stream.")
df <- data.frame(orth=orth, phon=phon, stringsAsFactors=F)

# read in the dictionary
dict <- read.table("/Users/TARDIS/Documents/STUDIES/TPs/eng_korman_from_swingley2005/dict_all3.txt", sep=" ", quote="", comment.char ="")
colnames(dict) <- c("word", "phon")


##############################################
# clean up contexts (only keep context words that actually occur in the corpus)
##############################################
# note that this is technically unneccesary - if you leave in extra words, it will not affect the results
contexts.list <- as.list(contexts)
for(i in 1:length(names(contexts.list))){
  this.context <- contexts.list[[i]][contexts.list[[i]] %in% global.data$streams$orth.stream] # the context words that show up in the corpus
  this.context <- c(as.character(this.context), rep("", nrow(contexts)-length(this.context))) # add empty values at the end to make all of the columns the same length
  contexts.list[[i]] <- this.context
}
contexts.occuring <- as.data.frame(contexts.list)
contexts.occuring <- contexts.occuring[-which(apply(contexts.occuring,1,function(x)all(x==""))),] # remove empty rows at the end
write.table(contexts.occuring, file="contexts.occuring.csv", sep=",", row.names=F)
contexts <- contexts.occuring # overwrite the contexts object with just the list of key words that actually occur in the corpus

##########################################
# clean up corpus
##########################################
df$orth <- sapply(strsplit(df$orth, "\t"), '[', 2) # split it at the tab and only keep the second part 
df$orth <- substr(df$orth, start=1, stop=nchar(df$orth)-2) # remove the last two characters (space and punctuation)
df$phon <- sapply(strsplit(df$phon, ": "), '[', 2) # split it at ": " and only keep the second part

# tidy bigrams in orthographic stream (so they can be treated as one lexical item)
df$orth <- gsub(pattern="oh dear", replacement="oh_dear", df$orth, fixed=TRUE)
df$orth <- gsub(pattern="all gone", replacement="all_gone", df$orth, fixed=TRUE)
df$orth <- gsub(pattern="thank you", replacement="thank_you", df$orth, fixed=TRUE)
df$orth <- gsub(pattern="uh oh", replacement="uh_oh", df$orth, fixed=TRUE)
df$orth <- gsub(pattern="patty cake", replacement="patty_cake", df$orth, fixed=TRUE)

# correct "dear" mistake
df$phon <- gsub(pattern="'d7$", replacement="'d7R", df$phon) # when 'd7 occurs utterance-final, replace it with 'd7R (that's definitely "dear" and not "dearie" or "dearier")
df$phon <- gsub(pattern="'d7 ([^r])", replacement="'d7R \\1", df$phon) # when 'd7 occurs and is NOT followed by an r after a space, replace it with 'd7R
df$phon <- gsub(pattern="'d7 ([^r])", replacement="'d7R \\1", df$phon) # need to run this twice because of overlapping hits with "dear dear"
# check all utterances with "dear" in them:
# unique(df[grep("dear", df$orth),]) 

##########################################
# clean up dictionary
##########################################
# add number of syllables for each word
dict$N.syl <- rep(NA, nrow(dict))
for(i in 1:nrow(dict)){
  dict$N.syl[i] <- length(strsplit(as.character(dict$phon[i]), split="-", fixed=TRUE)[[1]])
  }

# add defined context for each word
cols <- ncol(dict)
for(i in 1:length(colnames(contexts))){
  
  dict <- cbind(dict, as.numeric(rep(NA, nrow(dict)))) # add a column of NAs to dict
  colnames(dict)[cols+i] <- colnames(contexts)[i] # name that column after the current context
  key.words <- as.vector(contexts[,i][contexts[,i] != ""]) # drop empty cells from context columns to save just a vector of key words for this context
  dict[,cols+i] <- ifelse(dict$word %in% key.words, 1, 0) # if word occurs in the key.words, then mark 1 for this column, otherwise 0
  }

# classify dictionary words by context
context.columns <- (cols+1):(cols+length(colnames(contexts)))
dict$sum <- rowSums(dict[,context.columns]) # identify words that are key words from more than one context (ambiguous) - there should be none of these for mutually exclusive lists.

dict$context <- ifelse(dict$sum == 1, colnames(contexts)[apply(dict[,context.columns], 1, which.max)], 
                     ifelse(dict$sum > 1, "ambiguous", 
                            ifelse(dict$sum == 0, "none", NA)))
dict$context <- as.factor(dict$context)