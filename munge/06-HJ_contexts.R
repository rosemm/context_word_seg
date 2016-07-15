###############################
# NOT RUN (Done on server)
# collect coder judgments
###############################
# # set working directory to transcript coding folder on server
# # setwd("/Volumes/cas-fs2/baldwinlab/Maier/Transcript Coding")
# # setwd("/Volumes/cas-fs2/Learninglab/6_ContextCoding_Maier")
# doc_doctor()
# 
# master_doc <- collect_codes()
# setwd("/Users/TARDIS/Documents/STUDIES/context_word_seg")
# write.table(master_doc, file="context_codes/human_judgments/master_doc.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")

master_doc <- read.table("context_codes/human_judgments/master_doc.txt", header=1, sep="\t", stringsAsFactors=F)

contexts_clean <- master_doc %>% 
  process_codes(min.codes=5, max.codes=5, unique.coders=TRUE) %>% 
  clean_contexts(dir="context_codes/human_judgments", key_file="context_cleaning_keys.txt", interactive = TRUE)

HJ_contexts <- contexts_clean %>% 
  clean_categories(dir="context_codes/human_judgments", key_file="categories_cleaning_keys.txt", interactive = TRUE) 
# note that running clean_categories also updates the .md file, which pushes to here https://github.com/rosemm/context_word_seg/blob/master/categories_keys.md

cache('HJ_contexts')
