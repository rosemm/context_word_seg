# Copy functions to the pkg directory, so they're available as an R package on github
update_R_package <- function(code.dir = "lib", pkg = "pkg", ver = NULL, notes = NULL){
  stopifnot(require(dplyr), require(tidyr))
  
  # copy over all files from code.dir
  files <- list.files(code.dir)
  for (file in files){
    file.copy(from=file.path(code.dir, file), 
              to=file.path(pkg, "R"), 
              overwrite=TRUE, recursive=FALSE, copy.mode=FALSE, copy.date=TRUE)
  }
  
  # update DESCRIPTION
  if( any(grepl(pattern="DESCRIPTION", x=list.files(pkg))) ) {
    DESC <- read.table(file.path(pkg, "DESCRIPTION"), sep = "\t", stringsAsFactors = FALSE) %>% 
      tidyr::extract(col = V1, into = "V1", regex = "([[:alpha:]]+):") %>% 
      t_df() %>% 
      mutate_all(as.character)
    
  } else {
    DESC <- data.frame(Package = "context_word_seg",
                       Version = "0.1",
                       Date = date(),
                       Title = "Functions for dissertation on context and word segmentation",
                       Description = "",
                       Author = "Rose M Hartman <rosem@uoregon.edu>",
                       License = "")
  }
  if( !is.null(ver) ) DESC$Version <- ver; DESC$Date <- date()
  if( !is.null(notes) ) DESC$Description <- paste(DESC$Description, notes, sep = "\n")
  
  # prep for printing
  DESC <- t(DESC)
  DESC <- cbind(row.names(DESC), DESC)
  row.names(DESC) <- NULL
  DESC[,1] <- paste0(DESC[,1], ":")
  
  write.table(DESC, file.path(pkg, "DESCRIPTION"), 
              sep = "\t",
              col.names = FALSE,
              row.names = FALSE,
              quote = FALSE)
}


