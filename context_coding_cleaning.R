source_url("https://raw.githubusercontent.com/rosemm/context_word_seg/master/coding_scripts.R")
# set working directory to transcript coding folder on server
# setwd("/Volumes/cas-fs2/baldwinlab/Maier/Transcript Coding")
doc_doctor()

master_doc <- collect_codes()

setwd("/Users/TARDIS/Documents/STUDIES/context_word_seg")
write.table(master_doc, file="master_doc.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
# master_doc <- read.table("master_doc.txt", header=1, sep="\t", stringsAsFactors=F)

master_doc_contexts <- process_codes(master_doc, key_file="context_cleaning_keys.txt")

master_doc_keep <- process_categories(master_doc_contexts, key_file="categories_cleaning_keys.txt")

####################################
# lump little categories into "misc"
summary(as.factor(master_doc_keep$category) ) # how many utterances per category
hist(summary(as.factor(master_doc_keep$category)), breaks=1000, xlim=c(0,1000)) # how many utterances per category
barplot(sort(summary(as.factor(master_doc_keep$category)), decreasing=T))

main_cats <- summary(as.factor(master_doc_keep$category) )[summary(as.factor(master_doc_keep$category) ) > 500] # the categories with more than 100 utterances
misc_cats <- summary(as.factor(master_doc_keep$category) )[summary(as.factor(master_doc_keep$category) ) < 501] # the categories with less than 100 utterances
# check that I'm not losing any categories...
length(main_cats) + length(misc_cats) == length(summary(as.factor(master_doc_keep$category) ))

for(i in 1:length(misc_cats)){
  master_doc_keep$category <- gsub(pattern=paste0("^", names(misc_cats)[i], "$"), x=master_doc_keep$category, replacement="misc")
}


###################################

# calculate the number of times each context category is coded for each utterance
master_doc_calc <- master_doc_keep %>%
  select(utt, category) %>%
  mutate(hit=1) %>%
  group_by(utt, category ) %>%
  summarize(hits = sum(hit)) %>%
  spread(key=category, value=hits, fill = 0) %>%
  separate(col=utt, into=c("file", "UttNum"), sep="_" , remove=F) %>%
  arrange(file, as.numeric(UttNum) )

# make "burping" part of "mealtime"?
burp.no.meal <- sum(table(master_doc_calc$burping, master_doc_calc$mealtime)[-1,1]) # the number of utterances tagged as burping but not also mealtime
burp.tot <- nrow(filter(master_doc_calc, burping > 0)) # total number of utterances tagged as burping
burp.no.meal/burp.tot

contextcols <- 4:ncol(master_doc_calc) # the column numbers for all columns identifying contexts
master_doc_calc$total <- rowSums(x=master_doc_calc[ ,contextcols], na.rm=TRUE)  # total number of codes for each utterace

View(master_doc_calc)

master_doc_prop <- master_doc_calc %>%
  mutate_each(funs(./total),  contextcols) # making each hit the proportion of all hits for that utterance rather than count

View(master_doc_prop)


#########################################################################
# sequence plots
#########################################################################
master_doc_prop$UttNum <- as.numeric(master_doc_prop$UttNum)
master_doc_seq <- gather(master_doc_prop, key=context, value=value, -total, -file, -UttNum, -utt)

# or read in the contexts as they're used in the analysis (for both contexts and nontexts)
context_seq <- read.table("contexts_WL.txt", header=1, sep="\t", stringsAsFactors=F, quote="", comment.char ="") # this file is generated later on in this script
context_seq <- read.table("contexts_HJ_voting.txt", header=1, sep="\t", stringsAsFactors=F, quote="", comment.char ="") # this file is generated later on in this script
context_seq <- read.table("contexts_HJ_prop.txt", header=1, sep="\t", stringsAsFactors=F, quote="", comment.char ="") # this file is generated later on in this script
master_doc_seq <- gather(context_seq, key=context, value=value, -orth, -phon, -utt) %>%
  separate(col=utt, into=c("file", "UttNum"), sep="_", remove=FALSE) %>%
  # for word list analysis only, make all contexts either 1 or 0 (smoothing over 1.5's from expand_windows)
  mutate(value=ifelse(value>0, 1, 0)) 

master_doc_seq$context <- as.factor(master_doc_seq$context)
master_doc_seq$utt <- as.factor(master_doc_seq$utt)
master_doc_seq$file <- as.factor(master_doc_seq$file)
master_doc_seq$UttNum <- as.numeric(master_doc_seq$UttNum)


# sequence plots for all of the files
ggplot(filter(master_doc_seq, context != "misc", context !="none"), aes(x=UttNum, y=context, color=context, alpha=value) ) +
  geom_point()+
  facet_wrap(~file, scales="free_x") +
  scale_alpha(guide = 'none')

# sequence plots for each file separately
context_codes <- "WL"
context_codes <- "HJ_voting"
context_codes <- "HJ_prop"

files <- unique(master_doc_seq$file)
for (f in 1:length(files)){
  data <- filter(master_doc_seq, file==files[f], context != "misc", context !="none")
  ggplot(data ) +
    geom_point(aes(x=UttNum, y=context, color=context, alpha=value, size=4, shape="|" ), na.rm = TRUE, show_guide=F) + 
    scale_shape_identity() +
    scale_alpha(guide = 'none') + 
    scale_size(guide = 'none')
  ggsave( paste0("plots/seqplot-",context_codes, files[f], ".png"), width=9, height=3, units="in" )
}

#########################################################################
# define context by utterance
#########################################################################
#################################################
# calc contexts by voting threshold
include <- master_doc_keep %>%
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
