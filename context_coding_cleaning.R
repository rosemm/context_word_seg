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
hist(summary(as.factor(master_doc_keep$category) ), breaks=1000, xlim=c(0,1000)) # how many utterances per category

main_cats <- summary(as.factor(master_doc_keep$category) )[summary(as.factor(master_doc_keep$category) ) > 300] # the categories with more than 100 utterances
misc_cats <- summary(as.factor(master_doc_keep$category) )[summary(as.factor(master_doc_keep$category) ) < 301] # the categories with less than 100 utterances
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
master_doc_seq$context <- as.factor(master_doc_seq$context)
master_doc_seq$utt <- as.factor(master_doc_seq$utt)
master_doc_seq$file <- as.factor(master_doc_seq$file)

# sequence plots for all of the files
ggplot(master_doc_seq, aes(x=UttNum, y=value, color=context, alpha=value) ) +
  geom_point()+
  facet_wrap(~file, scales="free")

# sequence plots for each file separately
files <- unique(master_doc_seq$file)
for (f in 1:length(files)){
  data <- filter(master_doc_seq, file==files[f], context != "misc", context !="none")
  ggplot(data, aes(x=UttNum, y=context, color=context, alpha=value, size=2*(1+value) ) ) +
    geom_point(na.rm = TRUE) + 
    scale_alpha(guide = 'none') + 
    scale_size(guide = 'none')
  ggsave( paste0("plots/seqplot2-", files[f], ".png") )
}

#########################################################################
# define context by utterance
#########################################################################
# calc most endorsed context for each utterance (mutually exclusive context coding)
master_doc_prop$max <- apply(master_doc_prop[contextcols], 1, function(x) max(x, na.rm=T))
master_doc_prop$maxes <-  apply(master_doc_prop[c(contextcols, which(colnames(master_doc_prop)=="max"))], 1, function(x) length(which(x[1:(length(x)-1)]==x[length(x)]))) # how many contexts have the same value as the max context?
master_doc_prop$which <- apply(master_doc_prop[contextcols], 1, which.max) + 3 # which.max() finds which of the context columns has the highest value, add 3 to get which column from the dataframe (since there are three other columns at the beginning before the context columns start)
master_doc_prop$which <- ifelse(master_doc_prop$maxes>1, NA, master_doc_prop$which) # don't pick a context for utterances where there's a tie
master_doc_prop$which <- ifelse(master_doc_prop$max < .66 , NA, master_doc_prop$which) # don't pick a context if the max context is less than 66%
master_doc_prop$context <- colnames(master_doc_prop)[master_doc_prop$which ]
summary(as.factor(master_doc_prop$context) )


# calc contexts by voting threshold
include <- master_doc_keep %>%
  group_by(utt, category) %>%
  summarize(votes=n())
include$yes <- ifelse(include$votes > 2, 1, 0)
print(mean(include$yes)) # the % of codes that have agreement of at least 2 coders
# only keep the codes for each utterance that have at least 2 votes
include <- include %>%
  filter( yes==1 ) %>%
  select(utt, category)

# note that we are probably missing utterances from the include dataframe (if there was no code that had 2 coders agree for a particular utt)
# to restore the full corpus, merge with the utterances from master_doc_keep
temp <- data.frame(utt=unique(master_doc_keep$utt))
  
contexts_from_coding <- left_join( temp , include) 
contexts_from_coding$category <- ifelse(is.na(contexts_from_coding$category), "misc", contexts_from_coding$category )

# translate this into a binary 1/0 for each context for each utt
contexts_from_coding <- unique(contexts_from_coding) %>%
  mutate(hit=1) %>%
  spread(category, hit, fill=0)

# the number of utterances per context
N.utts <- colSums(contexts_from_coding[ , 2:ncol(contexts_from_coding)])
print(N.utts)

# the number of utterances that have 1, 2, 3, and 4 contexts identified (to get a sense of the overlap)
table(rowSums(contexts_from_coding[ , 2:ncol(contexts_from_coding)]))


# save these contexts in a file we can send to ACISS for bootstrap nontexts
key <- read.table("utt_orth_phon_KEY.txt", header=1, sep="\t", stringsAsFactors=F, quote="", comment.char ="")
key <- left_join(key, contexts_from_coding) # note that this step drops utterances excluded during orth to phon translation (i.e. more utterances are coded for context than are included in the final analyses)
write.table(key, file="contexs_HJ.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
