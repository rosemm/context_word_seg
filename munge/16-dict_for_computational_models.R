####################################################################################
# The adaptor grammar software expects different formatting of the phon stream than dpseg.
# The original files here use Swingley's (2005) dictionary, based on CELEX pronunciations,
# but py-cfg (the adaptor grammar) expects DARPABET pronunciations.
# Rather than using potentially different pronunciations read from the DARPABET coding, 
# the existing phonetic approximations are simply reformatted to match DARPABET formatting.
# This means the "phonemes" that are the basis for the analysis for the adaptor grammar
# exactly correspond the ones that are used for the dpseg model. 
# It also means that the pronunciations depicted here may not line up with standard DARPABET
# coding (i.e. one could not rely on these pronunciations and the DARPABET dictionary to 
# regenerate the original orth stream).
####################################################################################

# the phoneSet file provided in the Boerschinger (2012) materials for implementing apdaptor grammars
# converts single character phonemes into the characters available for py-cfg
coling_dict <- read.table("doc/coling_phoneSet.txt")
colnames(coling_dict) <- c("output", "input")

# have to remove stress information to look at it by phoneme (stress doesn't apply to phons)
dict_phons <- gsub(x=dict$phon, pattern = "'", replacement = "", fixed = TRUE)
dict_phons <- gsub(x=dict_phons, pattern = "\"", replacement = "", fixed = FALSE)
dict_phons <- gsub(x=dict_phons, pattern = "-", replacement = "", fixed = TRUE)  
dict_phons <- unique(strsplit(paste(dict_phons, collapse = ""), split = "")[[1]]) # all phonemes used in this corpus

# the current phonemes that have translations ready
okay <- dict_phons[dict_phons %in% coling_dict$input]
# the current phonemes that do NOT have translations ready
unknown <- dict_phons[!dict_phons %in% coling_dict$input]
# the translations that are NOT going to be used
unused <- coling_dict[!coling_dict$input %in% dict_phons, ]
# the translations that ARE going to be used
use <- coling_dict[coling_dict$input %in% dict_phons, ]

# a set of additional translations that do not conflict with the existing ones
# to cover the unknown phonemes in this corpus
add <- read.table("doc/coling_phoneSet_add.txt", comment.char ="", quote = "")
colnames(add) <- c("output", "input")

# combined translations (will cover all phonemes in corpus now)
coling_dict <- rbind(use, add) %>% 
  dplyr::arrange(output)

length(dict_phons[!dict_phons %in% coling_dict$input]) == 0 # there should be no forms unaccounted for

write.table(coling_dict, "phoneSet_updated.txt", sep = "\t")
cache('coling_dict')

# for py-cfg: word boundaries marked by tab, phones separated by space

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
  dplyr::mutate(phon_cm = phon_cm) %>% 
  dplyr::select(utt, orth, phon, phon_cm, dplyr::starts_with("WL"), dplyr::starts_with("STM"), dplyr::starts_with("HJ")) 
# make the variable names easy to parse later for the cm scripts
# first 2-3 characters are uppercase letters indicating method, after that is lowercase letters and digits indicating context
cols <- stringr::str_split_fixed(string = colnames(df_all_cm), pattern = "_", n = 2) %>% 
  as.data.frame() %>% 
  dplyr::mutate(V2=tolower(V2)) %>% 
  tidyr::unite(col="cols", V1, V2, sep = "") %>% 
  dplyr::mutate(cols=gsub(x = cols, pattern = "_", replacement = "")) %>% 
  dplyr::mutate(cols=gsub(x = cols, pattern = " ", replacement = "")) %>% 
  dplyr::mutate(cols=gsub(x = cols, pattern = "(STMtopic[[:digit:]]+)", replacement = "\\1t"))
colnames(df_all_cm) <- c("utt", "orth", "phon", "phon_cm", cols$cols[5:length(cols$cols)])
cache('df_all_cm')
