#' @title Print for computational models
#' 
#' @description Prepares (saves to text files) phonetic approximations of transcript utterances for use by computational models.
#' 
#' @details
#' For use with dpseg-1.2.1, corpus should be represented in the following way:
#' \itemize{
#'   \item one utterance per line
#'   \item a space bewteen each word
#'   \item words represented with dictionary pronunciations with one character per phoneme
#' }
#' 
#' For use with py-cfg via colingFinal, corpus should be represented in the following way:
#' \itemize{
#'   \item one utterance per line
#'   \item spaces between each phoneme
#'   \item tabs between each word
#'   \item words represented with dictionary pronunciations using a limited set of possible characters (just alpha, I think). A single phoneme may be represented with more than one character.
#' }
#' 
#' @export
print_for_cm <- function(df, nontext, dir = getwd(), save.to="computational_models", r=0, quiet = TRUE){
  methods <- c("WL", "STM", "HJ")
  
  if(nontext){
    # pick nontext utterances
    non <- nontext_cols(df=df, quiet) 
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
  
  # write shell script to run Makefiles
  
  context.cols <- colnames(df)[4:ncol(df)]
  context.names <- sample(context.cols, length(context.cols)) # shuffle list of contexts
  # Running the cm_contetxs.sh script takes a long time, and when it's interupted, 
  # the contexts listed first are the ones that get completed.
  # By shuffling the context names, the files that get run first will vary iteration to iteration.
  # It won't make any difference to the final output, except if the process is interupted before it completes.
  
  commands <- paste0("make NAME=", context.names, r, " OUTPUTPREFIX=r", r)
  
  write(c("cd colingFinal", commands, "cd .."), file = file.path(dir, "cm_contexts.sh"))
}

#' @inheritParams print_for_cm
#' @export
print_for_coling <- function(df, nontext, dir = getwd(), save.to="computational_models", r=0, quiet = TRUE){
  methods <- c("WL", "STM", "HJ")
  
  if(nontext){
    # pick nontext utterances
    non <- nontext_cols(df=df, quiet) 
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
  
  # write shell script to run Makefiles
  
  context.cols <- colnames(df)[4:ncol(df)]
  context.names <- sample(context.cols, length(context.cols)) # shuffle list of contexts
  # Running the cm_contetxs.sh script takes a long time, and when it's interupted, 
  # the contexts listed first are the ones that get completed.
  # By shuffling the context names, the files that get run first will vary iteration to iteration.
  # It won't make any difference to the final output, except if the process is interupted before it completes.
  
  commands <- paste0("make NAME=", context.names, r, " PYNS=500 OUTPUTPREFIX=r", r)
  
  write(c("cd colingFinal", commands, paste0("rm *", r ,"Tmp/*"), "cd .."), file = file.path(dir, "cm_contexts.sh"))
}


#'
#' @inheritParams print_for_cm
#' @export
print_for_dpseg <- function(df, nontext, dir = getwd(), save.to="computational_models", r=0, quiet = TRUE){
  methods <- c("WL", "STM", "HJ")
  
  if(nontext){
    # pick nontext utterances
    non <- nontext_cols(df=df, quiet) 
    # add nontext columns to dataframe
    df <- cbind(dplyr::select(df, utt, orth, phon), non)
  }
  if( !save.to %in% list.files(dir) ){
    dir.create(file.path(dir, save.to))
  }
  for(m in methods){
    contexts <- dplyr::select(df, starts_with(m))
    for( k in 1:ncol(contexts) ){
      # create a dataframe with the phon for each utterance and the 0/1's for context number k
      phon <- data.frame(phon=df$phon, context=contexts[[k]]) %>% 
        # keep only those utterances where context has a 1 (instead of a 0)
        # this selects all of the utterances tagged for that context
        dplyr::filter(context == 1) %>% 
        # select only the phon column (drop the context 1/0 column)
        dplyr::select(phon)
      
      # write the selected phon utterances to a file, for use by the computational model
      write.table(phon, 
                  file = file.path(dir, save.to, paste0(colnames(contexts)[k], r, ".in")),
                  quote = FALSE,
                  row.names = FALSE,
                  col.names = FALSE)
    }
  }
  
  # write shell script to run the segment command from dpseg-1.2.1 on each input file
  
  context.names <- colnames(df)[4:ncol(df)]
  context.names <- sample(context.names, length(context.names)) # shuffle list of contexts
  # Running the shell script takes a long time, and when it's interupted, 
  # the contexts listed first are the ones that get completed.
  # By shuffling the context names, the files that get run first will vary iteration to iteration.
  # It won't make any difference to the final output, except if the process is interupted before it completes.
  commands <- paste0("segment ", context.names, r, ".in -i5000 -ut > ", context.names, r, ".out")
  
  # save the commands to a file, which can then be called from the command line
  write(c("cd dpseg_input", commands, "cd .."), file = file.path(dir, "dpseg_contexts.sh"))
}
