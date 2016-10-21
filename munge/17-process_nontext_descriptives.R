
ds_context_est <- read.csv("contexts_descriptives.csv") %>% 
  # remove any spaces in nontext/context names (e.g. diaper change)
  dplyr::mutate(nontext = gsub(x=nontext, pattern = " ", replacement = "")) %>% 
  # drop N.utts, since it will get added back in by ds_results
  dplyr::select(-N.utts) %>% 
  # reformat to long
  tidyr::gather(key="measure", value="context_est", -nontext) %>% 
  dplyr::as.tbl()

ds_results <- read.csv("nontexts_descriptives.csv") %>% 
  # remove any spaces in nontext/context names (e.g. diaper change)
  dplyr::mutate(nontext = gsub(x=nontext, pattern = " ", replacement = "")) %>% 
  # reformat to long
  tidyr::gather(key="measure", value="value", -nontext, -N.utts) %>% 
  dplyr::as.tbl() %>% 
  # add in the context estimates
  dplyr::left_join(ds_context_est, by=c("nontext", "measure")) %>% 
  # make value and context_est columns numeric instead of character
  dplyr::mutate_at(vars(value, context_est), as.numeric) %>% 
  # split nontext column into method (HJ, STM, WL) and context
  tidyr::extract(col=nontext, into=c("method", "context"), regex="([[:upper:]]{2,3})([[:alnum:]]+)") %>% 
  # add Z-scored versions of context_est and bootstrapped values
  bootstrap_Z(value="value", est="context_est", by=c("method", "context", "measure"))

cache('ds_results')

# calculate p vlaues for each context and measure
ds_boot_tests <- ds_results %>% 
  dplyr::mutate(above=ifelse(value >= context_est, 1, ifelse(value < context_est, 0, NA))) %>% 
  dplyr::group_by(method, context, measure, context_est, Z_est, N.utts) %>% 
  dplyr::summarize(iters = n(), 
                   n.above = sum(above),
                   n.below = iters - n.above,
                   p.hi = n.above/iters,
                   p.lo = n.below/iters,
                   p.val = min(p.hi, p.lo),
                   # is p.val significantly different from alpha? Davidson, R., & MacKinnon, J. G. (2000). Bootstrap tests: How many bootstraps?. Econometric Reviews, 19(1), 55-68.
                   p.boot.unsure = ifelse(p.hi < p.lo & p.hi < .025, pbinom(n.above, iters, .025),
                                          ifelse(p.lo < p.hi & p.lo < .025, pbinom(n.below, iters, .025),
                                                 ifelse(p.hi < p.lo & p.hi >= .025, pbinom(n.above, iters, .025, lower.tail = FALSE),
                                                        ifelse(p.lo < p.hi & p.lo >= .025, pbinom(n.below, iters, .025, lower.tail = FALSE), NA))))) %>% 
  add_stars()

cache('ds_boot_tests')

dplyr::filter(ds_boot_tests, p.boot.unsure > .05 & N.utts > 100 & measure %in% c("TTR", "prop.most.freq", "mean.words.per.utt", "prop.one.word.utt")) # these should be run with more bootstrap iterations
