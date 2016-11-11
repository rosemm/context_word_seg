#' @import dplyr
#' @import tidyr
#' @export
check_packages <- function(list.of.packages){
  # check whether packages need to be installed
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  for(p in list.of.packages) {
    library(p, character.only=TRUE) # call library() for each package
  }
}

#' @export
get_from_https <- function (url, ..., sha1 = NULL) {
  # based on code for devtools::source_url
  stopifnot(is.character(url), length(url) == 1)
  temp_file <- tempfile()
  on.exit(unlink(temp_file))
  request <- httr::GET(url)
  httr::stop_for_status(request)
  writeBin(httr::content(request, type = "raw"), temp_file)
  file_sha1 <- digest::digest(file = temp_file, algo = "sha1")
  if (is.null(sha1)) {
    message("SHA-1 hash of file is ", file_sha1)
  }
  else {
    if (nchar(sha1) < 6) {
      stop("Supplied SHA-1 hash is too short (must be at least 6 characters)")
    }
    file_sha1 <- substr(file_sha1, 1, nchar(sha1))
    if (!identical(file_sha1, sha1)) {
      stop("SHA-1 hash of downloaded file (", file_sha1, 
           ")\n  does not match expected value (", sha1, 
           ")", call. = FALSE)
    }
  }
  read.table(temp_file, header=1, sep="\t", stringsAsFactors=F, quote="", comment.char ="")
}

#' @export
load_url <- function (url, ..., sha1 = NULL) {
  # based very closely on code for devtools::source_url
  stopifnot(is.character(url), length(url) == 1)
  temp_file <- tempfile()
  on.exit(unlink(temp_file))
  request <- httr::GET(url)
  httr::stop_for_status(request)
  writeBin(httr::content(request, type = "raw"), temp_file)
  file_sha1 <- digest::digest(file = temp_file, algo = "sha1")
  if (is.null(sha1)) {
    message("SHA-1 hash of file is ", file_sha1)
  }
  else {
    if (nchar(sha1) < 6) {
      stop("Supplied SHA-1 hash is too short (must be at least 6 characters)")
    }
    file_sha1 <- substr(file_sha1, 1, nchar(sha1))
    if (!identical(file_sha1, sha1)) {
      stop("SHA-1 hash of downloaded file (", file_sha1, 
           ")\n  does not match expected value (", sha1, 
           ")", call. = FALSE)
    }
  }
  load(temp_file, envir = .GlobalEnv)
}

#' Tests for lag (or lead) within k spaces away
#' 
#' @param x a vector where entries are all 1's or 0's (or NA's)
#' @param k the distance over which to calculate lag or lead
#' @param type should be "lag" or "lead", which call \code{\link[dplyr]{lag}} or
#'   \code{\link[dplyr]{lead}} respectively, or "either", which calls both.
#'   
#' @return A vector of the same length as x, indicating whether there is a 1 in 
#'   x anywhere up to k entries before (for type = "lag"), after (for type = 
#'   "lead") or in either direction (for type ="either"). Will not return NAs.
#'   Note that if x contains numbers other than 0 and 1, it will just treat 1 as
#'   a success and all other values (including NA) as failure.
#'   
#' @examples 
#' # quarters in which presidental approval dropped below 50% 
#' x <- as.numeric(presidents < 50) 
#' within_k(x, k=4, "lag") # quarters that follow quarters with approval below 50% by 1 to 4 quarters 
#' within_k(x, k=4, "either") # quarters that fall within 4 quarters from one with an approval below 50%
#' 
#' @export
within_k <- function(x, k, type){
  stopifnot(require(dplyr))
  x <- as.numeric(x == 1)
  if(type == "lag" | type == "either"){
    lags <- NULL
    for(crit in 1:k){
      lags <- cbind(lags, dplyr::lag(x, crit))
    }
    lag <- as.numeric(rowSums(lags, na.rm = TRUE) > 0)
  }  
  if(type == "lead" | type == "either"){
    leads <- NULL
    for(crit in 1:k){
      leads <- cbind(leads, dplyr::lead(x, crit))
    }
    lead <- as.numeric(rowSums(leads, na.rm = TRUE) > 0)
  } 
  if (type == "either"){
    either <- as.numeric(rowSums(cbind(lead, lag), na.rm = TRUE) > 0)
  }
  
  if (type == "lag") return(lag)
  if (type == "lead") return(lead)
  if (type == "either") return(either)
}

#' @export
add_stars <- function(tests){
  # add stars for significance to a table with p values
  tests$stars <- ifelse(tests$p.val < .001, "***",
                        ifelse(tests$p.val < .01, "**",
                               ifelse(tests$p.val < .05, "*", 
                                      ifelse(tests$p.val < .1, "+", ""))))
  return(tests)
}

#' @export
t_df <- function(df, col.names=1){
  t.df <- t(df[,-col.names])
  colnames(t.df) <- df[[names(df)[col.names]]]
  t.df <- as.data.frame(t.df)
  return(t.df)
}

#' @title Convert to word
#' @description Converts single digit numbers (0-9) into words, for use in manuscripts
#' 
#' @param x An integer 0-9
#' @param cap Should the first letter be capitalized? Default is FALSE.
#' 
#' @return Returns the word form of x (e.g. 0 returns "zero").
#' 
#' @export
convert_to_word <- function(x, cap=FALSE){
  word <- NULL
  if(x == 0) word <- "zero"
  if(x == 1) word <- "one"
  if(x == 2) word <- "two"
  if(x == 3) word <- "three"
  if(x == 4) word <- "four"
  if(x == 5) word <- "five"
  if(x == 6) word <- "six"
  if(x == 7) word <- "seven"
  if(x == 8) word <- "eight"
  if(x == 9) word <- "nine"
  if(cap) {
    word <- gsub(x = word, pattern = "(^[[:alpha:]])", replacement = "\\U\\1", perl=TRUE)
  }
  return(word)
}