#' @export
doc_doctor <- function(recursive=T){
  if(!require(dplyr) ) install.packages("dplyr")
  docs <- dir(pattern="coding_doc.txt", recursive=recursive)
  message("Including the following coding documents:\n", paste(docs, collapse="\n"), "\n")
  
  for(i in 1:length(docs)){
    # load the coding doc
    message("Checking ", docs[i], "...\n")
    coding_doc <- read.table(docs[i], header=1, sep="\t", stringsAsFactors=F)
    
    # make all coder initials upper case
    coding_doc$coder <- toupper(coding_doc$coder)
    # make all contexts lower case
    coding_doc$context <- tolower(coding_doc$context)
    
    library(dplyr)
    bads <- filter(coding_doc, ( coder=="RM" | coder=="TEST" | coder=="" & !is.na(date) | nchar(context) < 3 & !is.na(date) ) | pass < 1 ) # likely issues in the coding
    goods <- filter(coding_doc, !( coder=="RM" | coder=="TEST" | coder=="" & !is.na(date) | nchar(context) < 3 & !is.na(date) ) | is.na(coder) & pass > 0) # the opposite of those issues
    if(nrow(bads)>0){
      message(nrow(bads), " bad cases found.")
      print <- FALSE
      print <- grepl(readline("Print bad cases for review now? (y/n) "), "y")
      if(print){
        print(as.matrix(bads))
      }
      
      correct_errors <- FALSE
      correct_errors <- grepl(readline("Do you wish to correct these cases now? \nDoing so will overwrite your old coding file with the new clean one. \n(y/n): "), "y")
      
      if(correct_errors){
        bads$coder <- NA
        bads$date <- NA
        bads$context <- NA
        
        if ( nrow(coding_doc) != nrow(goods) + nrow(bads) )  stop("oh no!")
        coding_doc <- rbind(goods, bads)
        
        message(nrow(filter(coding_doc, coder=="RM" | coder=="TEST" | coder=="" & !is.na(date))), " bad cases left. :)")
        
        # write updated coding_doc to file
        message("Writing the squeaky clean doc...")
        write.table(coding_doc, file=docs[i], quote=F, col.names=T, row.names=F, append=F, sep="\t")
      }
    }
  }
  message("All done!\n")
}
