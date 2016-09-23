#' @export
# prep files for computational models
print_for_cm <- function(df, nontext, dir = getwd(), save.to="computational_models", r=0){
  methods <- c("WL", "STM", "HJ")
  
  if(nontext){
    # pick nontext utterances
    non <- nontext_cols(df=df) 
    # add nontext columns to dataframe
    df <- cbind(dplyr::select(df, utt, orth, phon), non)
  }
  if( !save.to %in% list.files(dir) ){
    dir.create(file.path(dir, save.to))
  }
  for(m in methods){
    contexts <- dplyr::select(df, starts_with(m))
    for( k in 1:ncol(contexts) ){
      phon <- data.frame(phon=df$phon, context=contexts[[k]]) %>% 
        dplyr::filter(context == 1) %>% 
        dplyr::select(phon)
      write.table(phon, 
                  file = file.path(dir, save.to, paste0(colnames(contexts)[k], r, "_0to1m.phon")),
                  quote = FALSE,
                  row.names = FALSE,
                  col.names = FALSE)
    }
  }
  
  # MAKE shell script to run Makefiles
  commands <- paste0("make NAME=", colnames(df)[4:ncol(df)], r, " OUTPUTPREFIX=r", r)
  write(c("cd colingFinal", commands, "cd .."), file = file.path(dir, "cm_contexts.sh"))
}
