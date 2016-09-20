# agreement analysis functions
cat_agreement <- function(cat.codes, adj=TRUE){
  
  stopifnot(length(cat.codes) == 2, require(dplyr), require(tidyr), require(vcd))
  
  message("Assessing agreement between ", paste(c(names(cat.codes)[1],names(cat.codes)[2]), collapse=" and "))
  
  cat1 <- cat.codes[[1]]
  cat2 <- cat.codes[[2]]
  
  if(adj){
    adj.chi.sq <- MI.test(data=cbind(dplyr::select(cat1, -utt, -orth, -phon), dplyr::select(cat2, -utt, -orth, -phon)),
                          I = ncol(cat1)-3, 
                          J = ncol(cat2)-3, 
                          type="boot")
  } else adj.chi.sq <- "Not run"
  
  # remove cases that have ambiguous codes within context approach (more than 1 context tagged)
  cat1 <- remove_ambig(cat1)
  cat2 <- remove_ambig(cat2)
  
  cat1.long <- gather(cat1, key="cat1", value="include", -utt, -orth, -phon) %>% 
    dplyr::filter(include==1) %>% 
    dplyr::select(-include)
  cat2.long <- gather(cat2, key="cat2", value="include", -utt, -orth, -phon) %>% 
    dplyr::filter(include==1) %>% 
    dplyr::select(-include)
  
  freqs <- full_join(cat1.long, cat2.long, by=c("utt", "orth", "phon"))
  
  freqs[is.na(freqs)] <- "none"
  
  freqs <- freqs %>% 
    count(cat1, cat2)
  
  xt <- xtabs(n ~ cat1 + cat2, data=freqs) # cross-tabs
  names(attr(xt, "dimnames")) <- c(names(cat.codes)[1],names(cat.codes)[2])
  
  chi.sq <- summary(xt) # chi-squared test of independence
  v <- assocstats(xt) # cramers v
  
  return(list(xt, chi.sq, v, adj.chi.sq))
}
