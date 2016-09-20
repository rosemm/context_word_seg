# prep files for computational models

# WORD LIST


# STM

# CODER JUDGMENTS
methods <- c("WL", "STM", "HJ")
for(m in methods){
  contexts <- dplyr::select(df_all_cm, starts_with(m))
  for( k in 1:ncol(contexts) ){
    phon <- data.frame(phon=df_all_cm$phon_cm, context=contexts[[k]]) %>% 
      dplyr::filter(context == 1) %>% 
      dplyr::select(phon)
    write.table(phon, 
                file = file.path("computational_models", paste0(colnames(contexts)[k], "_0to1m.phon")),
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
  }
}

# MAKE shell script to run Makefiles
commands <- paste0("make NAME=", colnames(df_all_cm)[4:ncol(df_all_cm)])
write(commands, file = file.path("computational_models", "cm_contexts.sh"))
