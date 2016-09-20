
# any additional values to be saved in the plot name can be included as extra arguments
hj.threshold <- threshold_plots(df_HJ_prop, 
                thresholds=seq(0,.85,.1), # what thresholds to try, 
                method="HJ",
                save.to=file.path("graphs", "HJ"),
                misc.cutoff="none") 

# apply threshold
# .6 means 3/5 coders must agree
df_HJ_bin <- apply_threshold(df_HJ_prop, .6, 
                             plot=T, method="HJ", save.to=file.path("graphs", "HJ"))

write.table(df_HJ_bin, file=file.path("context_codes", "human_judgments", "contexts_HJ_bin.txt"), quote=F, col.names=T, row.names=F, append=F, sep="\t")
cache('df_HJ_bin')
