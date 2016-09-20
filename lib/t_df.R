#' @export
t_df <- function(df, col.names=1){
  t.df <- t(df[,-col.names])
  colnames(t.df) <- df[[names(df)[col.names]]]
  t.df <- as.data.frame(t.df)
  return(t.df)
}