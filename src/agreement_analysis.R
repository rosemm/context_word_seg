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
  gather(key=var, value=value, -class) %>% 
  ggplot(aes(class, var, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white",  high = "darkblue", guide = guide_legend(title="Class-conditional\nProbability") ) + 
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

