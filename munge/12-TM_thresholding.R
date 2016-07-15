
# any additional values to be saved in the plot name can be included as extra arguments
LDA.threshold <- threshold_plots(df_LDA_prop, 
                                 thresholds=seq(0,.75,.05), # what thresholds to try, 
                                 method="LDA",
                                 save.to=file.path("graphs", "LDA"),
                                 misc.cutoff="none") 

# apply threshold
df_LDA_bin <- apply_threshold(df_LDA_prop, LDA.threshold, 
                              plot=T, method="LDA", save.to=file.path("graphs", "LDA"))

write.table(df_LDA_bin, file=file.path("context_codes", "human_judgments", "contexts_LDA_bin.txt"), quote=F, col.names=T, row.names=F, append=F, sep="\t")
cache('df_LDA_bin')

# any additional values to be saved in the plot name can be included as extra arguments
STM.threshold <- threshold_plots(df_STM_prop, 
                thresholds=seq(0,.75,.05), # what thresholds to try, 
                method="STM",
                save.to=file.path("graphs", "STM"),
                misc.cutoff="none") 

# apply threshold
df_STM_bin <- apply_threshold(df_STM_prop, STM.threshold, 
                             plot=T, method="STM", save.to=file.path("graphs", "STM"))

write.table(df_STM_bin, file=file.path("context_codes", "human_judgments", "contexts_STM_bin.txt"), quote=F, col.names=T, row.names=F, append=F, sep="\t")
cache('df_STM_bin')
