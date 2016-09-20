new_codes <- function( raw_codes, cols=c("raw", "clean"), key_file, interactive ){
  message(paste0("Checking to see if we need to update the cleaning key ", key_file, "...")) 
  
  key <- read.table(key_file, header=1, sep="\t", stringsAsFactors=F)
  
  message("(Note that the first column of the key needs to correspond to raw codes)\n")
  new_raw_codes <- raw_codes[!raw_codes %in% key[,1]]
  
  if(interactive){
    message(length(new_raw_codes), " new raw codes (not already in the key).")
    if( length(new_raw_codes) > 0) {
      add_codes <- FALSE
      add_codes <- grepl(readline("Do you wish to add these codes now? (y/n): "), "y")
      
      if(add_codes){
        new_code_pairs <- data.frame(raw=new_raw_codes, clean=NA, stringsAsFactors=F)
        colnames(new_code_pairs) <- cols
        
        message("\nEnter the clean code for each raw code \n")
        for(i in 1:nrow(new_code_pairs)){
          
          change <- readline(paste0("Change '", new_code_pairs[i,1], "'? (y/n) "))
          
          if( change == "n" ) {
            new_code_pairs[i,2] <- as.character(new_code_pairs[i,1]) # make clean code the same as raw code if change is "no" 
          } else if( change == "y" ){
            
            replace_confirm <- FALSE
            while(!replace_confirm){
              clean_code <- readline("Replace with: ")
              replace_confirm <- readline("Confirm? (y/n) ") =="y"
            }
            new_code_pairs[i,2] <- clean_code
          } else stop("Error. Change must be y or n")
        }
        key <- rbind(key, new_code_pairs)
        key <- key[ order(key[,1]) , ] # re-sort the key based on the first column (raw codes)
        write.table(key, key_file, quote=F, col.names=T, row.names=F, append=F, sep="\t")
        
        message("New contexts added! :) \n")
      }
    } 
  } else message(length(new_raw_codes), " skipped raw codes (not already in the key).")
}
