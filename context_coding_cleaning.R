doc_doctor <- function(){
  # load the coding doc
  coding_doc <- read.table("coding_doc.txt", header=1, sep="\t", stringsAsFactors=F)
  
  # make all coder initials upper case
  coding_doc$coder <- toupper(coding_doc$coder)
  # make all contexts lower case
  coding_doc$context <- tolower(coding_doc$context)
  
  library(dplyr)
  bads <- filter(coding_doc, coder=="RM" | coder=="TEST" | coder=="" & !is.na(date))
  goods <- filter(coding_doc, !(coder=="RM" | coder=="TEST" | coder=="" & !is.na(date)) | is.na(coder) )
  message(nrow(bads), " bad cases found.")
  
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
    write.table(coding_doc, file="coding_doc.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
  }
}