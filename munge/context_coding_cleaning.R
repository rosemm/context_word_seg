library(devtools)
source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/R/coding_functions.R")
# set working directory to transcript coding folder on server
# setwd("/Volumes/cas-fs2/baldwinlab/Maier/Transcript Coding")
# setwd("/Volumes/cas-fs2/Learninglab/6_ContextCoding_Maier")
doc_doctor()

master_doc <- collect_codes()

setwd("/Users/TARDIS/Documents/STUDIES/context_word_seg")
write.table(master_doc, file="master_doc.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
# master_doc <- read.table("master_doc.txt", header=1, sep="\t", stringsAsFactors=F)

# to update coding doc leaving only utterances that are below criterion on number of codes, to fill in the thin places
update_doc <- UpdateDoc(master_doc, criterion = 10)
write.table(update_doc, file="coding_doc.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")

master_doc_contexts <- process_codes(master_doc, key_file="context_cleaning_keys.txt")

master_doc_clean <- process_categories(master_doc_contexts, key_file="categories_cleaning_keys.txt")

####################################
summary(as.factor(master_doc_clean$category) ) # how many utterances per category
hist(summary(as.factor(master_doc_clean$category)), breaks=1000) # how many utterances per category
hist(summary(as.factor(master_doc_clean$category)), breaks=1000, xlim=c(0,1000)) # how many utterances per category
barplot(sort(summary(as.factor(master_doc_clean$category)), decreasing=T))


###################################

# calculate the number of times each context category is coded for each utterance
master_doc_calc <- master_doc_clean %>%
  select(utt, category) %>%
  mutate(hit=1) %>%
  group_by(utt, category ) %>%
  summarize(hits = sum(hit)) %>%
  spread(key=category, value=hits, fill = 0) %>%
  separate(col=utt, into=c("file", "UttNum"), sep="_" , remove=F) %>%
  arrange(file, as.numeric(UttNum) )

contextcols <- 4:ncol(master_doc_calc) # the column numbers for all columns identifying contexts
master_doc_calc$total <- rowSums(x=master_doc_calc[ ,contextcols], na.rm=TRUE)  # total number of codes for each utterace


# make "burping" part of "mealtime"?
burp.no.meal <- sum(table(master_doc_calc$burping, master_doc_calc$mealtime)[-1,1]) # the number of utterances tagged as burping but not also mealtime
burp.tot <- nrow(filter(master_doc_calc, burping > 0)) # total number of utterances tagged as burping
burp.no.meal/burp.tot

View(master_doc_calc)

master_doc_prop <- master_doc_calc %>%
  mutate_each(funs(./total),  contextcols) # making each hit the proportion of all hits for that utterance rather than count

View(master_doc_prop)

#########################################################################
# lump little categories into "misc"
cats <- master_doc_calc %>%
  # count how many utterances are tagged with a particular code (regardless of how many coders identified it for each utterace)
  mutate_each(funs(ifelse(. > 0, 1, 0)),  contextcols) %>%
  gather(key="category", value="hit", contextcols) %>%
  group_by(category) %>%
  summarize(hits=sum(hit)) %>%
  arrange(desc(hits))

barplot(cats$hits, names.arg=cats$category, las=2)
cat_med <- median(cats$hits)

main_cats <- filter(cats, hits > cat_med) # the categories with more than 100 utterances
misc_cats <- filter(cats, hits < (cat_med+1)) # the categories with less than 100 utterances
# check that I'm not losing any categories...
nrow(main_cats) + nrow(misc_cats) == nrow(cats)

barplot(main_cats$hits, names.arg=main_cats$category, las=2)

summary(as.factor(master_doc_clean$category))
for(i in 1:nrow(misc_cats)){
  master_doc_clean$category <- gsub(pattern=paste0("^", as.character(misc_cats[i,]$category), "$"), x=master_doc_clean$category, replacement="misc")
}
summary(as.factor(master_doc_clean$category))
#########################################################################
# THEN RE-RUN CODE TO GENERATE master_doc_calc AND master_doc_prop

master_doc_prop$UttNum <- as.numeric(master_doc_prop$UttNum)
master_doc_seq <- gather(master_doc_prop, key=context, value=value, -total, -file, -UttNum, -utt)


#########################################################################
# define context by utterance
#########################################################################
#################################################
# calc contexts by voting threshold
include <- master_doc_clean %>%
  group_by(utt, category) %>%
  summarize(votes=n()) %>%
  separate(utt, into=c("file", "UttNum"), sep="_", remove=F) %>%
  arrange(file, as.numeric(UttNum))
include$yes <- ifelse(include$votes > 2, 1, 0)
print(mean(include$yes)) # the % of codes that have agreement of at least 2 coders
# only keep the codes for each utterance that have at least 2 votes
include <- include %>%
  filter( yes==1 ) %>%
  select(utt, category)
summary(as.factor(include$category))

# note that we are probably missing utterances from the include dataframe (if there was no code that had 2 coders agree for a particular utt)
length(unique(include$utt))
# to restore the full corpus, merge with the utterances from utt_orth_phon_KEY.txt
temp <- read.table("utt_orth_phon_KEY.txt", header=1, sep="\t", stringsAsFactors=F, quote="", comment.char ="")

contexts_from_coding <- left_join(temp, include, by="utt") # note that this step drops utterances excluded during orth to phon translation (i.e. more utterances are coded for context than are included in the final analyses)
contexts_from_coding$category <- ifelse(is.na(contexts_from_coding$category), "none", contexts_from_coding$category )

# translate this into a binary 1/0 for each context for each utt
contexts_from_coding <- contexts_from_coding %>%
  mutate(hit=1) %>%
  spread(category, hit, fill=0)

# the number of utterances per context
N.utts <- colSums(contexts_from_coding[ , 4:ncol(contexts_from_coding)])
print(N.utts)

# the number of utterances that have 0, 1, 2, 3, and 4 contexts identified (to get a sense of the overlap)
just_real_contexts <- table(rowSums(contexts_from_coding[ , which(colnames(contexts_from_coding) %in% names(N.utts)[!grepl(pattern="none|misc", x=names(N.utts))])]))
round(just_real_contexts/nrow(contexts_from_coding), 3) # as percents of the whole corpus

# save these contexts in a file we can send to ACISS for bootstrap nontexts
key <- na.omit(contexts_from_coding)
write.table(key, file="contexts_HJ_voting.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")


#################################################
# assign utterances to contexts probabalistically (e.g. a code with 80% mealtime category endorsement would be in the mealtime corpus 80% of the time)

# how many codes are available per utterance? (how much information do we have per utt?)
summary(master_doc_calc$total)
hist(master_doc_calc$total, main="number of codes per utterance", xlab=NULL)

contexts_from_coding <- select(master_doc_prop, -file, -UttNum, -total) %>% # drop extra columns
  select(-misc, -none) # drop context columns that don't actually code for a context

# save these contexts in a file we can send to ACISS for bootstrap nontexts
key <- read.table("utt_orth_phon_KEY.txt", header=1, sep="\t", stringsAsFactors=F, quote="", comment.char ="")
key <- left_join(key, contexts_from_coding, by="utt") # note that this step drops utterances excluded during orth to phon translation (i.e. more utterances are coded for context than are included in the final analyses)
key <- na.omit(key)
write.table(key, file="contexts_HJ_prop.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
