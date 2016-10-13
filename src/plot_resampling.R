# save some plots showing the resampled null distributions
ds_results %>% 
  dplyr::filter(measure %in% c("TTR", "prop.most.freq", "mean.words.per.utt", "prop.one.word.utt")) %>% 
  group_by(method, context, measure) %>% 
  do({
    resampling_convergence(.$value, 
                           n=50,
                           save.to=file.path("graphs", "resampling"),
                           measure=unique(.$measure),
                           note=paste(unique(.$method), unique(.$context), sep = "_"),
                           plot.mean=TRUE,
                           plot.sig.cutoffs=TRUE)
    
  })


cm_results %>% 
  dplyr::filter(measure == "token_f.score") %>% 
  group_by(model, method, context, measure) %>% 
  do({
    resampling_convergence(.$value, 
                           n=50,
                           save.to=file.path("graphs", "resampling"),
                           measure=unique(.$measure),
                           note=paste(unique(.$model), unique(.$method), unique(.$context), sep = "_"),
                           plot.mean=TRUE,
                           plot.sig.cutoffs=TRUE)
    
  })
