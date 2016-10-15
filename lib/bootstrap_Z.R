#' title.
#' 
#' Calculates the Z-scores for observed estimates based on the mean and SD of a bootstrapped null distribution.
#' Requires a dataframe in long format, with bootstrapped estimates in value and observed estimates 
#' (for which Z-scores should be calculated) in est.
#' 
#' @author Rose Hartman
#' 
#' @param df A dataframe
#' @param value The (quoted) name of the column in df with bootstrapped estimates
#' @param est The (quoted) name of the column in df with observed estimates, for which Z-scores should be calculated
#' @param by An option vector of (quoted) column names to group df by. Means and standard deviations for the Z scores will be calculated within these groups. If by=NULL (default), then no grouping is used. 
#' @param Z_est The (quoted) name of the column to create for the Z scores from est
#' @param Z_value The (quoted) name of the column to create for the Z scores from value
#' 
#' @return A dataframe with columns for each grouping variable in by (if used), est, value, mean and sd from value, and Z scores for the est and value columns.
#' 
#' @examples
#' 
#'  
#' @export
bootstrap_Z <- function(df, value, est, by=NULL, Z_est = "Z_est", Z_value = "Z_value"){
  
  # group by the by variable(s)
  if(length(by) > 0){
    for(i in 1:length(by)){
      df <- group_by_(df, by[i], add=TRUE)
    }
  } 
  
  # for these commands to work with variables set in the function arguments, 
  # need to use the standard evaluation (SE) versions of the dplyr commands
  # see https://www.r-bloggers.com/non-standard-evaluation-and-standard-evaluation-in-dplyr/
  
  mean_call <- lazyeval::interp(quote(mean(v, na.rm = TRUE)), v=as.name(value))
  sd_call <- lazyeval::interp(quote(sd(v, na.rm = TRUE)), v=as.name(value))
  summary <- summarize_(df, 
                       mean = mean_call,
                       sd = sd_call)

  mutate_est_call <- lazyeval::interp(~ (x - m)/s, x = as.name(est), m = quote(mean), s=quote(sd))
  mutate_value_call <- lazyeval::interp(~ (x - m)/s, x = as.name(value), m = quote(mean), s=quote(sd))
  
  df_Z <- df %>% 
    # add in means and SDs from summary
    left_join(summary, by=by) %>% 
    # calculate Z scores from est, means, and SDs
    mutate_(.dots = setNames(list(mutate_est_call), Z_est)) %>% 
    mutate_(.dots = setNames(list(mutate_value_call), Z_value))
  
  return(df_Z)
}