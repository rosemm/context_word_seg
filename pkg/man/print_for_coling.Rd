% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_for_cm.R
\name{print_for_coling}
\alias{print_for_coling}
\title{Print for coling computational model}
\usage{
print_for_coling(df, nontext, dir = getwd(),
  save.to = "computational_models", r = 0, quiet = TRUE, iters = 500)
}
\arguments{
\item{df}{A dataframe with the columns utt, orth, phon and all context columns. See details.}

\item{nontext}{Should utterances be randomly selected for each context column?}

\item{dir}{The directory containing the folder with all model files (colingFinal). This is also the directory where the .sh file will save. Defaults to the working directory.}

\item{save.to}{The name of a directory in which to save the input files.}

\item{r}{A counter variable. This number will appear in the names of input and output files, making it easy to keep track of multiple parallel processes.}

\item{quiet}{Turn on additional messages? Useful for debugging.}
}
\value{
Prints input files for the model that consist of propperly formatted phonetic approximations of all utterances in each context (or nontext).
Also prints a .sh file (bash script) that contains commands to run the model on each of the input files.
After running this function on a df, you can then simply run the resulting bash script to run the model over each conetxt (or nontext).
This can be easily implemented in parallel processing (highly recommended) if you have a computer with multiple cores, 
which is especially useful for running the model on nontexts as the process needs to be repeated so many times.
}
\description{
Prepares (saves to text files) phonetic approximations of transcript utterances for use by coling computational models.
}
\details{
Automates the running of computational models for every context subset defined by a context column in the dataframe df.
Note that df must be formatted correctly for this to work. 
The columns utt, orth, and phon must contain a unique utterance identifiers, the orthographic transcription of each utterance, and the phonetic approximation of each utterance respectively.
All remaining columns in df must be context columns, with a 0 or 1 for each utterance indicating whether that utterance is in that context.
Each context column should be named with uppercase letters indicating method (e.g. WL, STM, HJ), and the specific context in lowercase letters and/or numbers only (e.g. bathtime, topic7t). 
No spaces or special characters. 
The last character of the column name must be a letter (not a number) because the counter r will be appended to the end of the context name in filenames. 

For use with dpseg-1.2.1, corpus input files should be represented in the following way:
\itemize{
  \item one utterance per line
  \item a space bewteen each word
  \item words represented with dictionary pronunciations with one character per phoneme
}

For use with py-cfg via colingFinal, corpus input files should be represented in the following way:
\itemize{
  \item one utterance per line
  \item spaces between each phoneme
  \item tabs between each word
  \item words represented with dictionary pronunciations using a limited set of possible characters (just alpha, I think). A single phoneme may be represented with more than one character.
}
}

