library(ProjectTemplate)
load.project()

model <- lca_models$lca6 # which model to investigate
model <- lca_models_STM35$lca7 # which model to investigate

# Classes defined as...

# take highest probability values and "assign" them to that class
class_contexts <- lca_class_var_probs %>% 
  tidyr::gather(key="context", value="prob", -class) %>% 
  # tidyr::spread(key=class, value=prob) %>% 
  tidyr::extract(col=context, into="method", regex = "([[:upper:]]+)_.*", remove = FALSE) %>% 
  dplyr::group_by(class, method) %>% 
  dplyr::mutate(rank=rank(1/prob)) %>% 
  dplyr::ungroup() %>% 
  # the largest probability for each class for each method, as well as any additional probabilities over the 90th percentile
  dplyr::filter(rank == 1 | prob >= quantile(prob, .90)) %>% 
  dplyr::arrange(class) %>% 
  dplyr::select(class, context) %>%
  dplyr::mutate(hit=1) %>% 
  tidyr::spread(key=context, value=hit, fill = 0)

# what percent of ___ match ___, given grouping from LCA?
percent_utts_match(df_all, class_contexts, "WL_diaper_dressing")
