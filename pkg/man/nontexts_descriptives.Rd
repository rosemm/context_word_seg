% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nontexts_descriptives.R
\name{nontexts_descriptives}
\alias{nontexts_descriptives}
\title{Descriptive statistics on sub-corpora}
\usage{
nontexts_descriptives(x)
}
\arguments{
\item{x}{A list of arguments that control how nontext_descriptives runs. See details.}
}
\description{
Gets a number of measures that may relate to segmentability of a language sample. 
Can easily be run in parallel (highly recommended) for different bootstrap iterations.
}
\details{
x must include at minimum "dataframe" (a data frame or tibble with columns utt, orth, phon, and context columns with 1's and 0's indicating whether each utterance is in each context subcorpus)
and "dict" (a data frame or tibble with columns with all words in the corpus and their phon approximations). 
Note that the order of the columns in dataframe is important --- the first three must be utt, orth, phon, and the 4th through last must be context columns.
Additional arguments can be included in x, but if they are omitted nontext_descriptives will just use their defaults:
#' \itemize{
  \item seg.utts (default TRUE): should utterance boundaries be given as word boundaries?
  \item verbose (default FALSE): include extra output?
  \item nontext (defaults to TRUE): shuffle utterances to randomly select for each context?
  \item quiet (default TRUE): supress extra messages to print while it's running (FALSE is good for debugging)?
  \item date (default NULL): today's date, to be added to output for record keeping. Can be supplied with date().
}
Any additional elements in x will be ignored without warning.
}

