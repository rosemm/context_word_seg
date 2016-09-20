#' @export
report_chisq <- function(cat){
  df <- cat[[2]]$parameter
  N <- cat[[2]]$n.cases
  cs <- round(cat[[2]]$statistic, 2)
  p <- round(cat[[2]]$p.value, 3)
  v <- round(cat[[3]]$cramer, 2)
  p.boot <- ifelse(length(cat[[4]]) != 1, round(cat[[4]]$boot$p.value.boot, 3), NA)
  
  p <- ifelse(p==0, "p < .001", paste0("p = ", p))
  p.boot <- ifelse(p.boot==0, "p < .001", paste0("p = ", p.boot))
  return(list(df=df, N=N, cs=cs, p=p, v=v, p.boot=p.boot))
}
