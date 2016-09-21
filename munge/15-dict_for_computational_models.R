coling_dict <- read.table("doc/coling_phoneSet.txt")
colnames(coling_dict) <- c("output", "input")

# dict_syls <- unique(strsplit(paste(dict$phon, collapse = "-"), split = "-")[[1]])

# have to remove stress information to look at it by phoneme (stress doesn't apply to phons)
dict_phons <- gsub(x=dict$phon, pattern = "'", replacement = "", fixed = TRUE)
dict_phons <- gsub(x=dict_phons, pattern = "\"", replacement = "", fixed = FALSE)
dict_phons <- gsub(x=dict_phons, pattern = "-", replacement = "", fixed = TRUE)  
dict_phons <- unique(strsplit(paste(dict_phons, collapse = ""), split = "")[[1]])

okay <- dict_phons[dict_phons %in% coling_dict$input]
unknown <- dict_phons[!dict_phons %in% coling_dict$input]
unused <- coling_dict[!coling_dict$input %in% dict_phons, ]
use <- coling_dict[coling_dict$input %in% dict_phons, ]

add <- read.table("doc/coling_phoneSet_add.txt", comment.char ="", quote = "")
colnames(add) <- c("output", "input")

coling_dict <- rbind(use, add) %>% 
  dplyr::arrange(output)

length(dict_phons[!dict_phons %in% coling_dict$input]) == 0 # there should be no forms unaccounted for

write.table(coling_dict, "phoneSet_updated.txt", sep = "\t")
cache('coling_dict')

# Word boundaries marked by tab in phon_cm, phones separated by space

# in this file, phon has words separated by space and syllables within words separated by -
phon_cm_d <- read.table("utt_orth_phon_KEY.txt", sep="\t", quote="", comment.char ="", header=1, stringsAsFactors=F)%>% 
  dplyr::filter(!grepl(x=.$utt, pattern = "hi"))
phon_cm <- phon_cm_d$phon
phon_cm <- gsub(x=phon_cm, pattern = " ", replacement = "\t") # mark word boundaries with \t
phon_cm <- gsub(x=phon_cm, pattern = "-", replacement = "") # remove syllable markers (will separate by phone)
# Translate phon from df into cm_phon using coling dict
for(p in 1:nrow(coling_dict)){
  phon_cm <- gsub(x=phon_cm, pattern = coling_dict$input[p], replacement = paste0(coling_dict$output[p], " "), fixed = TRUE)
}
phon_cm <- gsub(x=phon_cm, pattern = "xx ", replacement = "") # drop stress markers

df_all_cm <- df_all %>% 
  mutate(phon_cm = phon_cm) %>% 
  dplyr::select(utt, orth, phon, phon_cm, starts_with("WL"), starts_with("STM"), starts_with("HJ")) %>% 
  mutate_if(is.factor, funs(as.numeric(as.character(.))))
# make the variable names easy to parse later for the cm scripts
# first 2-3 characters are uppercase letters indicating method, after that is lowercase letters and digits indicating context
cols <- str_split_fixed(string = colnames(df_all_cm), pattern = "_", n = 2) %>% 
  as.data.frame() %>% 
  mutate(V2=tolower(V2)) %>% 
  unite(col="cols", V1, V2, sep = "") %>% 
  mutate(cols=gsub(x = cols, pattern = "_", replacement = "")) 
colnames(df_all_cm) <- c("utt", "orth", "phon", "phon_cm", cols$cols[5:length(cols$cols)])
cache('df_all_cm')
