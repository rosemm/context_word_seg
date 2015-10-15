doc_doctor <- function(recursive=T){
  
  docs <- dir(pattern="coding_doc.txt", recursive=recursive)
  message("Including the following coding documents:\n", paste(docs, collapse="\n"))
  
    for(i in 1:length(docs)){
      # load the coding doc
      message("Checking ", docs[i], "...\n")
      coding_doc <- read.table(docs[i], header=1, sep="\t", stringsAsFactors=F)
      
      # make all coder initials upper case
      coding_doc$coder <- toupper(coding_doc$coder)
      # make all contexts lower case
      coding_doc$context <- tolower(coding_doc$context)
      
      library(dplyr)
      bads <- filter(coding_doc, (coder=="RM" | coder=="TEST" | coder=="" & !is.na(date)) | pass<1 ) 
      goods <- filter(filter(coding_doc, !( coder=="RM" | coder=="TEST" | coder=="" & !is.na(date) ) | is.na(coder) ), pass >0)
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
        message("Writing the squeaky clean doc...")
        write.table(coding_doc, file=docs[i], quote=F, col.names=T, row.names=F, append=F, sep="\t")
    }
  }
}

collect_codes <- function(){
  
  docs <- dir(pattern="coding_doc.txt", recursive=TRUE)
  message("Including the following coding documents:\n", paste(docs, collapse="\n"))
  
  # pull out the coded portion from each doc
  for(i in 1:length(docs)){
    doc <- read.table(docs[i], header=1, sep="\t", stringsAsFactors=F)
    coded <- dplyr::filter(doc, !is.na(context))
    if( nrow(dplyr::filter(coded, coder=="RM" | coder=="TEST" | coder=="" & !is.na(date))) != 0 ) stop("Run doc_doctor first. There are bad cases here.")
    if(i==1) master_doc <- coded else master_doc <- rbind(master_doc, coded)
  }
  return(master_doc)
}

process_codes <- function(master_doc){
  master_doc$context <- as.factor(master_doc$context)
  library(tidyr)
  maxcontexts <- 10
  master_doc <- separate(master_doc, col=context, into=paste("context", 1:maxcontexts, sep="."), sep="[[:blank:]]*;[[:blank:]]*", extra="drop")
  master_doc <- gather(master_doc, key="contextnum", value="context", which(colnames(master_doc)==paste("context",1, sep=".")):which(colnames(master_doc)==paste("context",maxcontexts, sep=".")), na.rm=T)
  contexts <- unique(master_doc$context)
}
