library(ProjectTemplate)
load.project()

# exclude contexts with fewer than 100 utterances
keep <- df_all %>% 
  dplyr::select(-utt, -orth, -phon) %>% 
  colSums(na.rm=TRUE)
keep <- keep[keep >= 100] %>% names() # the contexts with at least 100 utterances
df_all_keep <- df_all %>% 
  dplyr::select(utt, orth, phon, one_of(keep)) 

WL.bin <- df_all_keep %>% 
  dplyr::select(utt, orth, phon, starts_with("WL"))
STM.bin <- df_all_keep %>% 
  dplyr::select(utt, orth, phon, starts_with("STM"))
HJ.bin <- df_all_keep %>% 
  dplyr::select(utt, orth, phon, starts_with("HJ"))

LCA.bin <- df_LCA_bin

# ----------------------------------------------
# descriptives
# number of utterances per context sub-corpus
df_all_keep %>% 
  # keep the context columns
  dplyr::select(starts_with("WL"), starts_with("STM"), starts_with("HJ")) %>% 
  # reformat to long
  tidyr::gather(key="context", value="value") %>% 
  dplyr::group_by(context) %>% 
  # count number of 1's for each context (total number of utterances in that sub-corpus)
  dplyr::summarize(n=sum(value, na.rm=TRUE)) %>% 
  # remove underscores in context names, but not between method and context (e.g. only the second "_" in HJ_bath_time)
  dplyr::mutate(context=gsub(x=context, pattern="([[:lower:]])_", replacement = "\\1")) %>% 
  # use underscores between method and context to split the column
  tidyr::separate(context, c("method", "context"), sep="_") %>% 
  # change HJ to CJ for consistency with other plots
  dplyr::mutate(method = factor(method, levels=c("WL", "STM", "HJ"), labels=c("WL", "STM", "CJ"))) %>% 
  # plot
  ggplot(aes(y=n, x=reorder(as.factor(context), n), fill=method)) + 
  geom_bar(stat = 'identity', show.legend = FALSE) + 
  facet_wrap(~method, scales = "free", ncol=1) + 
  theme_classic() + 
  scale_fill_manual(values=colors, name="Approach to\ndefining context") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x=NULL, y=NULL) 
ggsave(filename ="utts_per_context.png" , path = file.path("graphs","descriptives"), width = 5, height = 14, units="in")

# check that contexts are distributed across families and transcripts
contexts_by_transcript <- df_all %>% 
  tidyr::gather(key = "key", value = "value", -utt, -orth, -phon) %>% 
  tidyr::extract(col = key, into = c("method", "context"), regex = "(^[[:upper:]]{2,3})_(.*)$", remove = FALSE) %>% 
  tidyr::extract(col = utt, into = c("child", "age", "utt"), regex = "(^[[:alpha:]]{2})([[:digit:]]+)[.]cha_([[:digit:]]+)$", convert = TRUE) %>% 
  dplyr::mutate(child = paste0("Child: ", toupper(child))) %>% 
  dplyr::group_by(child, age, method, context) %>% 
  dplyr::summarize(N.utts = sum(value, na.rm = TRUE),
                   occurs = N.utts > 0) 

context_occurs_transcript <- contexts_by_transcript %>% 
  dplyr::group_by(method, context) %>% 
  dplyr::summarise(n=sum(occurs), max.possible=n()) 
# contexts that occur in only one transcript
context_occurs_transcript %>% 
  dplyr::filter(n == 1)
# contexts that occur in at least half of the transcripts
context_occurs_transcript %>% 
  dplyr::filter(n >= max.possible/2)
# contexts that occur in every transcript
context_occurs_transcript %>% 
  dplyr::filter(n == max.possible)

context_occurs_family <- contexts_by_transcript %>% 
  dplyr::group_by(child, method, context) %>% 
  dplyr::summarize(N.utts = sum(N.utts, na.rm = TRUE),
                   occurs = N.utts > 0) %>% 
  dplyr::group_by(method, context) %>% 
  dplyr::summarise(n=sum(occurs), max.possible=n()) 
# contexts that occur in only one family
context_occurs_family %>% 
  dplyr::filter(n == 1)

# http://www.people.vcu.edu/~pdattalo/702SuppRead/MeasAssoc/NominalAssoc.html
# Cramer's V is the most popular of the chi-square-based measures of nominal association because it gives good norming from 0 to 1 regardless of table size, when row marginals equal column marginals. 
# V equals the square root of chi-square divided by sample size, n, times m, which is the smaller of (rows - 1) or (columns - 1): V = SQRT(X2/nm).
# V may be viewed as the association between two variables as a percentage of their maximum possible variation.
# V2 is the mean square canonical correlation between the variables. For 2-by-2 tables, V = phi (hence some packages like Systat print V only for larger tables).
# http://uregina.ca/~gingrich/ch11a.pdf
cat_WL_STM <- cat_agreement(cat.codes=list(WL.bin=WL.bin, 
                             STM.bin=STM.bin), adj=TRUE)
cache('cat_WL_STM') 

cat_WL_HJ <- cat_agreement(cat.codes=list(WL.bin=WL.bin, 
                             HJ.bin=HJ.bin), adj=TRUE)
cache('cat_WL_HJ')

cat_HJ_STM <- cat_agreement(cat.codes=list(HJ.bin=HJ.bin, 
                             STM.bin=STM.bin), adj=TRUE)
cache('cat_HJ_STM')

# agreement with LCA results
cat_LCA_WL <- cat_agreement(cat.codes=list(LCA.bin=remove_ambig(LCA.bin), WL=remove_ambig(WL)))
assocplot(cat_LCA_WL[[1]])
cache('cat_LCA_WL')
cat_LCA_HJ <- cat_agreement(cat.codes=list(LCA.bin=remove_ambig(LCA.bin), HJ=remove_ambig(HJ.bin)))
assocplot(cat_LCA_HJ[[1]])
cache('cat_LCA_HJ')
cat_LCA_STM <- cat_agreement(cat.codes=list(LCA.bin=remove_ambig(LCA.bin), STM=remove_ambig(STM.bin)))
assocplot(cat_LCA_STM[[1]])
cache('cat_LCA_STM')

# change colnames to match other naming conventions
colnames(lca_class_var_probs) <- gsub(x=colnames(lca_class_var_probs), pattern = "HJ_", replacement = "CJ_")

lca_class_var_probs %>% 
  # reformat to long
  tidyr::gather(key="context", value="value", -class) %>%  
  # remove underscores in context names
  dplyr::mutate(context=gsub(x=context, pattern="_", replacement = " ")) %>%
  tidyr::extract(col=context, into="topic.n", regex="([[:digit:]]+)", remove=FALSE, convert = TRUE) %>%
  
  dplyr::mutate(topic.n = ifelse(is.na(topic.n), "", 
                                 ifelse(topic.n < 10, paste0("0", topic.n), 
                                        ifelse(topic.n >= 10, paste0(topic.n), NA))),
                context = gsub(x=context, pattern = " [[:digit:]]+", replacement = "")) %>%
  tidyr::unite(col=context, context, topic.n, sep="") %>% 
  ggplot(aes(x=class, y=context)) + 
  geom_tile(aes(fill = value)) +
  scale_fill_gradient(low = "white",  high = "darkblue", name="Class-conditional\nProbability") + 
  labs(x=NULL, y=NULL) + 
  theme_classic() + 
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("LCA_classcondvars_heatmap.png", path=file.path("graphs", "LCA"), width = 14, height = 20, units = "in")
  

p <- lca_class_var_probs %>% 
  gather(key=var, value=prob, -class) %>% 
  extract(col=var, into="method", regex="^([[:upper:]]+)_", remove=FALSE)

ggplot(filter(p, method=="WL"), aes(y=prob, x=class, color=var, fill=var)) + 
  geom_bar(stat="identity", position = "dodge") + 
  facet_wrap(~class, scale = "free_x")  +
  theme(text = element_text(size=20)) + 
  theme_classic()
ggsave("LCA_varsWL.png", path=file.path("graphs", "LCA"), width = 8, height = 6, units = "in")

ggplot(filter(p, method=="CJ"), aes(y=prob, x=class, color=var, fill=var)) + 
  geom_bar(stat="identity", position = "dodge") + 
  facet_wrap(~class, scale = "free_x")  +
  theme(text = element_text(size=20))  + 
  theme_classic()
ggsave("LCA_varsCJ.png", path=file.path("graphs", "LCA"), width = 8, height = 6, units = "in")

ggplot(filter(p, method=="STM"), aes(y=prob, x=class, color=var, fill=var)) + 
  geom_bar(stat="identity", position = "dodge") + 
  facet_wrap(~class, scale = "free_x")  +
  theme(text = element_text(size=20))  + 
  theme_classic()
ggsave("LCA_varsSTM.png", path=file.path("graphs", "LCA"), width = 8, height = 6, units = "in")

