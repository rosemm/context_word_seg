# agreement analysis functions
cat_agreement <- function(cat.codes){
  
  stopifnot(length(cat.codes) == 2, require(dplyr), require(tidyr), require(vcd))
  
  message("Assessing agreement between ", paste(c(names(cat.codes)[1],names(cat.codes)[2]), collapse=" and "))
  
  cat1 <- cat.codes[[1]]
  cat2 <- cat.codes[[2]]
  
  cat1.long <- gather(cat1, key="cat1", value="include", -utt, -orth, -phon) %>% 
    filter(include==1) %>% 
    select(-include)
  cat2.long <- gather(cat2, key="cat2", value="include", -utt, -orth, -phon) %>% 
    filter(include==1) %>% 
    select(-include)
  
  freqs <- full_join(cat1.long, cat2.long, by="utt")
  
  freqs <- freqs %>% 
    count(cat1, cat2)
  
  xt <- xtabs(n ~ cat1 + cat2, data=freqs) # cross-tabs
  names(attr(xt, "dimnames")) <- c(names(cat.codes)[1],names(cat.codes)[2])
  
  chi.sq <- summary(xt) # chi-squared test of independence
  v <- assocstats(xt) # cramers v
  
  return(list(xt, chi.sq, v))
}
