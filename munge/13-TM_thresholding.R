##############################################################
# creates the dataframe df_STM_bin
# which has columns utt, orth, phon, and all STM context columns
# by converting df_STM_prop using a threshold of that maximizes
# one or two contexts per utterance
# 
# creates the dataframe df_LDA_bin
# which has columns utt, orth, phon, and all LDA context columns
# by converting df_LDA_prop using a threshold of that maximizes
# one or two contexts per utterance
##############################################################

# ------------------------------------------------------------------
# LDA

# the function threshold_plots() generates plots showing a range of thresholds
# the output of the functio is the threshold corresponding to the most utterances with 1 or 2 contexts
# here that value gets saved as "LDA_threshold"
LDA.threshold <- threshold_plots(df_LDA_prop, 
                                 thresholds=seq(0,.75,.05), # what thresholds to try, 
                                 method="LDA", # for labeling plots
                                 save.to=file.path("graphs", "LDA"),
                                 misc.cutoff="none") 
# any additional values to be saved in the plot name can be included as extra arguments

# apply threshold
df_LDA_bin <- apply_threshold(df_LDA_prop, 
                              LDA.threshold, # threshold determined above
                              plot=TRUE, 
                              method="LDA", # for labeling plots
                              save.to=file.path("graphs", "LDA"))

write.table(df_LDA_bin, file=file.path("context_codes", "human_judgments", "contexts_LDA_bin.txt"), quote=F, col.names=T, row.names=F, append=F, sep="\t")

# remove spaces from column names
colnames(df_LDA_bin) <- gsub(x=colnames(df_LDA_bin), pattern = " ", replacement = "")

cache('df_LDA_bin')

# ------------------------------------------------------------------
# STM

# determine threshold
STM.threshold <- threshold_plots(df_STM_prop, 
                                 thresholds=seq(0,.75,.05), # what thresholds to try
                                 method="STM",
                                 save.to=file.path("graphs", "STM"),
                                 misc.cutoff="none") 

# apply threshold
df_STM_bin <- apply_threshold(df_STM_prop, 
                              STM.threshold, 
                              plot=TRUE, 
                              method="STM", 
                              save.to=file.path("graphs", "STM"))

write.table(df_STM_bin, file=file.path("context_codes", "human_judgments", "contexts_STM_bin.txt"), quote=F, col.names=T, row.names=F, append=F, sep="\t")

colnames(df_STM_bin) <- gsub(x=colnames(df_STM_bin), pattern = " ", replacement = "")

cache('df_STM_bin')
