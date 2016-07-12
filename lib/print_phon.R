# prints a text file with utterances represented in their phonological approximations, for use by computational models
print_phon <- function(df, filename="input", dir=getwd()){
  stopifnot(!is.null(df$phon))
  write.table(df$phon, 
              file = paste0(dir, "/", filename, ".txt"),
              quote = FALSE,
              row.names = FALSE,
              col.names = FALSE) 
}