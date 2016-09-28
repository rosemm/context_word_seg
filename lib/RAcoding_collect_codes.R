#' @export
collect_codes <- function(){
  
  docs <- dir(pattern="coding_doc.txt", recursive=TRUE)
  message("Including the following coding documents:\n", paste(docs, collapse="\n"))
  
  # pull out the coded portion from each doc
  for(i in 1:length(docs)){
    this.doc <- read.table(docs[i], header=1, sep="\t", stringsAsFactors=F)
    coded <- dplyr::filter(this.doc, !is.na(context))
    stopifnot( nrow(dplyr::filter(coded, coder=="RM" | coder=="TEST" | coder=="" & !is.na(date))) == 0 )
    if(i==1) {
      doc <- coded 
    } else {
      doc <- base::rbind(doc, coded)
    }
  }
  # clean up some bad punctuation
  doc$context <- gsub(pattern=",", x=doc$context, replacement=";", fixed=T)
  doc$context <- gsub(pattern="/", x=doc$context, replacement=";", fixed=T)
  doc$context <- tolower(doc$context)
  
  message(" Removing ", nrow(doc) - nrow(unique(doc)), " duplicate rows.")
  doc <- unique(doc) # there are duplicate rows because of copying the coding docs when I was originally starting the RAs on coding
  
  return(doc)
}
