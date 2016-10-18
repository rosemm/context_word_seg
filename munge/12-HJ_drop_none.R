# drop "none" codes from HJ contexts
df_HJ_bin <- dplyr::select(df_HJ_bin, -none)

write.table(df_HJ_bin, file=file.path("context_codes", "human_judgments", "contexts_HJ_bin.txt"), quote=F, col.names=T, row.names=F, append=F, sep="\t")

cache('df_HJ_bin')
