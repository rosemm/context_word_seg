library(ProjectTemplate)
load.project()

# remove underscores from WL orth column
WL <- df_WL_bin
WL$orth <- gsub(x=WL$orth, pattern="_", replacement=" ")
STM.bin <- df_STM_bin
HJ.bin <- df_HJ_bin

LCA.bin <- df_LCA_bin


# http://www.people.vcu.edu/~pdattalo/702SuppRead/MeasAssoc/NominalAssoc.html
# Cramer's V is the most popular of the chi-square-based measures of nominal association because it gives good norming from 0 to 1 regardless of table size, when row marginals equal column marginals. 
# V equals the square root of chi-square divided by sample size, n, times m, which is the smaller of (rows - 1) or (columns - 1): V = SQRT(X2/nm).
# V may be viewed as the association between two variables as a percentage of their maximum possible variation.
# V2 is the mean square canonical correlation between the variables. For 2-by-2 tables, V = phi (hence some packages like Systat print V only for larger tables).
# http://uregina.ca/~gingrich/ch11a.pdf
cat_WL_STM <- cat_agreement(cat.codes=list(WL=WL, 
                             STM.bin=STM.bin))
cache('cat_WL_STM') 
cat_WL_HJ <- cat_agreement(cat.codes=list(WL=WL, 
                             HJ.bin=HJ.bin))
cache('cat_WL_HJ')
cat_HJ_STM <- cat_agreement(cat.codes=list(HJ.bin=HJ.bin, 
                             STM.bin=STM.bin))
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
