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
  # clean up some bad punctuation
  master_doc$context <- gsub(pattern=",", x=master_doc$context, replacement=";", fixed=T)
  master_doc$context <- gsub(pattern="/", x=master_doc$context, replacement=";", fixed=T)
  master_doc$context <- tolower(master_doc$context)
  
  message(" Removing ", nrow(master_doc) - nrow(unique(master_doc)), " duplicate rows.")
  master_doc <- unique(master_doc) # there are duplicate rows because of copying the coding docs when I was originally starting the RAs on coding
  
  return(master_doc)
}

# setwd("/Users/TARDIS/Documents/STUDIES/context_word_seg")
# write.table(master_doc, file="master_doc.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")

process_codes <- function(master_doc, criterion=3, cleaning_keys=read.table("context_cleaning_keys.txt", header=1, sep="\t", stringsAsFactors=F)){
  if(!require(tidyr)) install.packages("tidyr"); library(tidyr)
  if(!require(dplyr)) install.packages("dplyr"); library(dplyr)
  if(!require(psych)) install.packages("psych"); library(psych)
  if(!require(GPArotation)) install.packages("GPArotation"); library(GPArotation)
  
  
  # master_doc <- read.table("master_doc.txt", header=1, sep="\t", stringsAsFactors=F)
  
  RA_info <- master_doc %>%
    tidyr::unite( utt, file, UttNum) %>%
    dplyr::select(utt, coder, context, pass) %>%
    group_by(coder) %>%
    dplyr::summarize(n_utts_codes=n())

  message("\nRAs have coded this many utterances:\n") ; print(as.data.frame(RA_info))
  
# only keep utterances that have been coded at least [criterion] times
  keep <- master_doc %>%
    tidyr::unite( utt, file, UttNum) %>%
    dplyr::select(utt, coder, context, pass) %>%
    group_by(utt) %>%
    dplyr::summarize(n=n()) %>%
    dplyr::filter(n > (criterion-1)) %>%
    dplyr::select(utt)
  
master_doc_keep <- merge(tidyr::unite(master_doc, utt, file, UttNum), keep, by="utt", all.x=FALSE, all.y=TRUE )
  
message("Removing ", nrow(master_doc)-nrow(master_doc_keep) , " utterances because they have been coded fewer than ", criterion, " times across all coders.\n")    
message( round( nrow(keep)/13350, 4), "% of total utterances are included in analyses.\n" )

  master_doc_keep$context <- as.factor(master_doc_keep$context)
  
  maxcontexts <- 10 # the maximum number of contexts that can be read for one window (30 utterances)
  
  master_doc_keep <- tidyr::separate(master_doc_keep, col=context, into=paste("context", 1:maxcontexts, sep="."), sep="[[:blank:]]*;[[:blank:]]*", extra="drop")
  master_doc_keep <- tidyr::gather(master_doc_keep, key="contextnum", value="context", which(colnames(master_doc_keep)==paste("context",1, sep=".")):which(colnames(master_doc_keep)==paste("context",maxcontexts, sep=".")), na.rm=T)
  
  sort(unique(master_doc_keep$context))
  new_contexts <- unique(master_doc_keep$context)[!unique(master_doc_keep$context) %in% cleaning_keys$context_raw]
  message(length(new_contexts), " new contexts (not already in context_cleaning_keys.txt)")
  if(length(new_contexts) > 0) {
    add_contexts <- FALSE
    add_contexts <- grepl(readline("Do you wish to add these contexts now? (y/n): "), "y")
    
    if(add_contexts){
      new_contexts <- data.frame(context_raw=new_contexts, context_clean=NA)
      message("\nEnter the clean context code for each new context \n")
      for(i in 1:nrow(new_contexts)){
        
        change <- readline(paste("Change '", new_contexts[i,1], "'? (y/n) ", sep=""))
        
        if( change == "n" ) {
          new_contexts[i,2] <- as.character(new_contexts[i,1]) # make clean code the same as raw code if change is "no" 
        } else if( change == "y" ){
          
          replace_confirm <- FALSE
          while(!replace_confirm){
            clean_code <- readline("Replace with: ")
            replace_confirm <- readline("Confirm? (y/n) ") =="y"
          }
          new_contexts[i,2] <- clean_code
        } else stop("Error. Change must be y or n")
      }
      cleaning_keys <- rbind(cleaning_keys, new_contexts)
      cleaning_keys <- dplyr::arrange(cleaning_keys, context_raw)
      write.table(cleaning_keys, "context_cleaning_keys.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")
      message("New contexts added! :) \n")
    } 
  }
  
  # write.table(as.matrix(contexts, ncol=1), "context_cleaning_keys.txt", quote=F, col.names=T, row.names=F, append=F, sep="\t")

  for(i in 1:nrow(cleaning_keys)){
    rows <- grep(pattern=paste("^", cleaning_keys[i,1], "$", sep=""), x=master_doc_keep$context, value=F)
    master_doc_keep[rows,] # just for checking
    master_doc_keep$context <- gsub(pattern=paste("^", cleaning_keys[i,1], "$", sep=""), x=master_doc_keep$context, replacement=cleaning_keys[i,2])
    master_doc_keep[rows,] # just for checking
  }
  master_doc_keep$context <- as.factor(master_doc_keep$context)
  summary(master_doc_keep$context)
  
  # calculate the number of times each context is coded for each utterance
  master_doc_calc <- master_doc_keep %>%
    select(utt, context) %>%
    mutate(hit=1) %>%
    group_by(utt, context) %>%
    summarize(hits = sum(hit)) %>%
    spread(key=context, value=hits, fill = 0) 

contextcols <- 2:ncol(master_doc_calc) # the column numbers for all columns identifying contexts
master_doc_calc$total <- rowSums(x=master_doc_calc[,contextcols], na.rm=TRUE)  # total number of codes for each utterace

return(master_doc_calc)
}


# PCA
master_doc_prop <- master_doc_calc %>%
  mutate_each(funs(./total),  contextcols) # making each hit the proportion of all hits for that utterance rather than count

cor <- cor(master_doc_prop[, contextcols])
heatmap(cor, symm=T, cexRow = 1 + 1/log10(nrow(cor)), cexCol = 1 + 1/log10(nrow(cor)))

pca <- prcomp(x=master_doc_prop[, contextcols] )
screeplot(pca, npcs = 15, type="lines")

nfactors=6 # the number of components to extract (based on screeplot above)
pca_o <- principal(cor, nfactors=nfactors, n.obs=nrow(master_doc_calc), rotate="varimax") # orthogonal components
pca_c <- principal(cor, nfactors=nfactors, n.obs=nrow(master_doc_calc), rotate="promax") # correlated components
print(pca_o$loadings, sort=T)
print(pca_c$loadings, sort=T)
round(pca_c$r.scores, 3)

# calc most endorsed context for each utterance
master_doc_calc$max <- apply(master_doc_calc[contextcols], 1, function(x) max(x, na.rm=T))
master_doc_calc$maxes <-  apply(master_doc_calc[c(contextcols, which(colnames(master_doc_calc)=="max"))], 1, function(x) length(which(x[1:(length(x)-1)]==x[length(x)]))) # how many contexts have the same value as the max context?
master_doc_calc$which <- apply(master_doc_calc[contextcols], 1, which.max)
master_doc_calc$which <- ifelse(master_doc_calc$maxes>1, NA, master_doc_calc$which) # don't pick a context for utterances where there's a tie
master_doc_calc$context <- colnames(master_doc_calc)[master_doc_calc$which + 1 ]
master_doc_calc$context_weight <- master_doc_calc$max/master_doc_calc$total


# how much coding is done?
nrow(master_doc_calc) # total number of utterances included in analyses
nrow(master_doc_calc)/13350 # the percent of all utterances that are currently included in analyses

