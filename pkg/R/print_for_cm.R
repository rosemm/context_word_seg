#' @title Print for coling computational model
#' 
#' @description Prepares (saves to text files) phonetic approximations of transcript utterances for use by coling computational models.
#' 
#' @param df A dataframe with the columns utt, orth, phon and all context columns. See details.
#' @param nontext Should utterances be randomly selected for each context column?
#' @param dir The directory containing the folder with all model files (colingFinal). This is also the directory where the .sh file will save. Defaults to the working directory.
#' @param save.to The name of a directory in which to save the input files. 
#' @param r A counter variable. This number will appear in the names of input and output files, making it easy to keep track of multiple parallel processes.
#' @param quiet Turn on additional messages? Useful for debugging.
#' 
#' @details
#' Automates the running of computational models for every context subset defined by a context column in the dataframe df.
#' Note that df must be formatted correctly for this to work. 
#' The columns utt, orth, and phon must contain a unique utterance identifiers, the orthographic transcription of each utterance, and the phonetic approximation of each utterance respectively.
#' All remaining columns in df must be context columns, with a 0 or 1 for each utterance indicating whether that utterance is in that context.
#' Each context column should be named with uppercase letters indicating method (e.g. WL, STM, HJ), and the specific context in lowercase letters and/or numbers only (e.g. bathtime, topic7t). 
#' No spaces or special characters. 
#' The last character of the column name must be a letter (not a number) because the counter r will be appended to the end of the context name in filenames. 
#' 
#' For use with dpseg-1.2.1, corpus input files should be represented in the following way:
#' \itemize{
#'   \item one utterance per line
#'   \item a space bewteen each word
#'   \item words represented with dictionary pronunciations with one character per phoneme
#' }
#' 
#' For use with py-cfg via colingFinal, corpus input files should be represented in the following way:
#' \itemize{
#'   \item one utterance per line
#'   \item spaces between each phoneme
#'   \item tabs between each word
#'   \item words represented with dictionary pronunciations using a limited set of possible characters (just alpha, I think). A single phoneme may be represented with more than one character.
#' }
#' 
#' @return 
#' Prints input files for the model that consist of propperly formatted phonetic approximations of all utterances in each context (or nontext).
#' Also prints a .sh file (bash script) that contains commands to run the model on each of the input files.
#' After running this function on a df, you can then simply run the resulting bash script to run the model over each conetxt (or nontext).
#' This can be easily implemented in parallel processing (highly recommended) if you have a computer with multiple cores, 
#' which is especially useful for running the model on nontexts as the process needs to be repeated so many times.
#' 
#' @export
print_for_coling <- function(df, nontext, dir = getwd(), save.to="computational_models", r=0, quiet = TRUE){
  # extract the methods from column names (indicated by upper case letters)
  methods <- gsub(x=colnames(df), pattern = "([[:upper:]]*).*", replacement="\\1") %>% unique()
  methods <- methods[methods != ""] # drop blanks
  
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
  
  write(c("cd colingFinal", commands, "cd .."), file = file.path(dir, paste0(r, "coling_contexts.sh")))
}


#' @title Print for dpseg computational model
#' 
#' @description Prepares (saves to text files) phonetic approximations of transcript utterances for use by dpseg computational models.
#' 
#' @inheritParams print_for_coling
#' @export
print_for_dpseg <- function(df, nontext, dir = getwd(), save.to="computational_models", r=0, quiet = TRUE){
  # extract the methods from column names (indicated by upper case letters)
  methods <- gsub(x=colnames(df), pattern = "([[:upper:]]*).*", replacement="\\1") %>% unique()
  methods <- methods[methods != ""] # drop blanks
  
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
  write(c("cd dpseg_input", commands, "cd .."), file = file.path(dir, paste0(r, "dpseg_contexts.sh")))
}
