#' @export
# Copy functions to the pkg directory, so they're available as an R package on github
update_R_package <- function(dir = getwd(), code = "lib", pkg = "pkg", ver = NULL, notes = NULL){
  stopifnot(require(dplyr), require(tidyr))
  
  # copy over all files from code
  files <- list.files(file.path(dir, code))
  for (file in files){
    file.copy(from=file.path(dir, code, file), 
              to=file.path(pkg, "R"), 
              overwrite=TRUE, recursive=FALSE, copy.mode=FALSE, copy.date=TRUE)
  }

  # Update NAMSPACE, or, if there's no NAMESPACE file, create one.
  # update DESCRIPTION
  roxygen2::roxygenize(package.dir = file.path(dir, pkg))
  
  if( !any(grepl(pattern="DESCRIPTION", x=list.files(file.path(dir, pkg)))) ) {
    DESC <- data.frame(Package = "contextwordseg",
                       Version = "0.1",
                       Date = date(),
                       Title = "Functions for dissertation on context and word segmentation",
                       Description = "",
                       Author = "Rose M Hartman <rosem@uoregon.edu>",
                       License = "",
                       Imports = "")
  
  if( !is.null(ver) ) DESC$Version <- ver; DESC$Date <- date()
  if( !is.null(notes) ) DESC$Description <- paste(DESC$Description, notes, sep = "\n")
  
  DESC$Imports <- as.character(read.table(file.path(dir, "config", "global.dcf"), sep = ":", stringsAsFactors = FALSE)$V2[8])
  
  # prep for printing
  DESC <- t(DESC)
  DESC <- cbind(row.names(DESC), DESC)
  row.names(DESC) <- NULL
  DESC[,1] <- paste0(DESC[,1], ":")
  
  write.table(DESC, file.path(dir, pkg, "DESCRIPTION"), 
              sep = "\t",
              col.names = FALSE,
              row.names = FALSE,
              quote = FALSE)
  }
}
