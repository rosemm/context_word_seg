###################################################################################################
# Use context codes from all three approaches (WL, HJ, STM) to conduct a Latent Class Analysis (LCA)
# using poLCA() from poLCA package
###################################################################################################
# NOTE:
# Depending on the starting parameters, this algorithm may only locate a local, rather than global, maximum. 
# This becomes more and more of a problem as nclass increases. 
# It is therefore highly advisable to run poLCA multiple times until you are relatively certain that you have located the global maximum log-likelihood. 
# This can be done auotmatically with the nrep command in poLCA()
# In the code here, nrep increases as needed as the number of latent classes increases
# It will estimate the model nrep times and keep the best one, helping to ensure that it doesn't end up with a local maximum
###################################################################################################

# context columns should be coded as 0 or 1 at this point
method_contexts <- df_all %>% 
  tidyr::gather(key="context", value="value", -utt, -orth, -phon) %>% 
  tidyr::extract(context, into=c("method", "context"), regex="([[:upper:]]+)_(.*)") %>% 
  # only keep positive context codes (1 means context is occuring, 0 means not)
  dplyr::filter(value == 1) 
method_counts <- method_contexts %>% 
  dplyr::count(utt, orth, phon, method) # returns number of occurrences per method per utterance as n
df_by_method <- method_counts %>% 
  # only keeping occurrences where there is exactly one context listed per method
  dplyr::filter(n == 1) %>% 
  dplyr::select(-n) %>% 
  # join the contexts themselves back in
  dplyr::left_join(method_contexts, by=c("utt", "orth", "phon", "method")) %>% 
  dplyr::select(-value) %>% 
  # reformat to wide
  tidyr::spread(key=method, value=context) %>% 
  dplyr::select(utt, orth, phon, HJ, STM, WL) %>% # keep context columns for HJ, STM, and WL
  dplyr::mutate_at(vars(HJ, STM, WL), as.factor) # turn all context columns into factors if they're not already (poLCA needs factors)

# This code takes a very long time to run. 
# I strongly recommend you run it in parallel on a machine that has enough cores to estimate each model separately
# It will still take up to a couple days to estimate the larger models (which also have higher nrep)
library(doParallel)
registerDoParallel() 
lca_models <- foreach(i=3:29, 
                      .inorder=TRUE,
                      .errorhandling="pass",
                      .verbose=TRUE,
                      .packages=c("poLCA", "MASS") ) %dopar% poLCA(cbind(HJ, STM, WL) ~ 1, 
                                                                   df_by_method, 
                                                                   nclass=i, 
                                                                   maxiter = 100000, 
                                                                   nrep=i, # Number of times to estimate the model, using different random starting values.
                                                                   na.rm=FALSE) # for how poLCA handles cases with missing values on the manifest variables. If TRUE, those cases are removed (listwise deleted) before estimating the model. If FALSE, cases with missing values are retained.


HJ_levels <- levels(df_by_method$HJ)
WL_levels <- levels(df_by_method$WL)
STM_levels <- levels(df_by_method$STM)

cache('lca_models')

#---------------------------------------------------------------
# compile the lca model reuslts into a more usable summary data frame:

# create an empty storage data.frame, which will get filled in the for loop below
lca_results <- data.frame(model=rep(NA, length(lca_models)), AIC=NA,
                          BIC=NA, Chisq=NA, llik=NA, llik_mean_top3=NA, llik_diff=NA, npar=NA, resid.df=NA, numiter=NA)
for(m in 1:length(lca_models)){
  # check that the model didn't stop becuase it hit the maxiter
  if(lca_models[[m]]$maxiter == lca_models[[m]]$numiter) {
    warning("Did not converge! Model ", m)
  } else {
    # the name for each model is lca# where # is the number of latent classes
    # lca_models[[m]]$P is the size of each latent class, so length(lca_models[[m]]$P) is the number of classes
    this.name <- paste0("lca", length(lca_models[[m]]$P))
    names(lca_models)[m] <- this.name
    lca_results$model[m] <- this.name
    # save important metrics from this model
    lca_results$AIC[m] <- lca_models[[m]]$aic
    lca_results$BIC[m] <- lca_models[[m]]$bic
    lca_results$Gsq[m] <- lca_models[[m]]$Gsq # Likelihood ratio/deviance statistic.
    lca_results$Chisq[m] <- lca_models[[m]]$Chisq # Pearson Chi-square goodness of fit statistic for fitted vs. observed multiway tables.
    lca_results$llik[m] <- lca_models[[m]]$llik
    lca_results$llik_mean_top3[m] <- mean(tail(sort(lca_models[[m]]$attempts), 4)[1:3]) # the mean of the next best three llik values (not including the first best) from nrep attempts
    lca_results$llik_diff[m] <- lca_results$llik_mean_top3[m] - lca_results$llik[m] # difference between best and next three best models
    lca_results$npar[m] <- lca_models[[m]]$npar  # number of degrees of freedom used by the model (estimated parameters)
    lca_results$resid.df[m] <- lca_models[[m]]$resid.df # number of residual degrees of freedom
    lca_results$numiter[m] <- lca_models[[m]]$numiter
    lca_results$maxiter[m] <- lca_models[[m]]$maxiter
  }
}
lca_results_plot <- lca_results %>% 
  gather(key=measure, value=value, AIC, BIC, Gsq, Chisq, llik, llik_mean_top3) %>% 
  extract(col=model, into="nclass", regex="([[:digit:]]+)") %>% 
  mutate(nclass=as.numeric(nclass))
ggplot(filter(lca_results_plot, measure %in% c("llik", "llik_mean_top3")), aes(y=-2*value, x=nclass, color=measure))+
  geom_line() +
  scale_x_continuous(breaks = seq(from=min(lca_results_plot$nclass, na.rm = TRUE), to=max(lca_results_plot$nclass, na.rm=TRUE), by=2)) + 
  theme_classic()
ggplot(lca_results_plot, aes(y=value, x=nclass, color=measure))+
  geom_line(show.legend=FALSE) +
  facet_wrap(~ measure, scales = "free") +
  labs(x="Numer of classes", y=NULL) + 
  scale_x_continuous(breaks = seq(from=min(lca_results_plot$nclass, na.rm = TRUE), to=max(lca_results_plot$nclass, na.rm=TRUE), by=2)) + 
  theme(text = element_text(size=20)) + 
  theme_classic()
ggsave("LCA_fit_measures.png", path=file.path("graphs", "LCA"), width = 16, height = 10, units = "in")

selected_model <- lca_results_plot %>% 
  dplyr::filter(measure == "BIC") %>% 
  dplyr::mutate(rank=dplyr::min_rank(value)) %>% 
  dplyr::filter(rank == 1) # the model with the lowest BIC
  
ggplot(dplyr::filter(lca_results_plot, measure == "BIC"), aes(y=value, x=nclass))+
  geom_line() +
  labs(x="Numer of latent classes", y="BIC") + 
  scale_x_continuous(breaks = seq(from=min(lca_results_plot$nclass, na.rm = TRUE), to=max(lca_results_plot$nclass, na.rm=TRUE), by=1)) + 
  theme(text = element_text(size=20)) + 
  geom_vline(xintercept = selected_model$nclass, lty=2) + 
  theme_classic()
ggsave("LCA_fit_BIC.png", path=file.path("graphs", "LCA"), width = 5, height = 5, units = "in")


model <- lca_models$lca6 # which model to investigate

# Posterior prob for class assignment. What's the prob each utterance is in each latent class?
df_LCA_prop <- df_by_method %>% 
  dplyr::select(utt, orth, phon) %>% 
  ungroup() %>% 
  cbind(data.frame(model$posterior)) %>% 
  dplyr::mutate(predclass = model$predclass)

# A list of matrices containing the estimated class-conditional outcome probabilities πˆjrk. 
# Each item in the list represents one manifest variable; 
# columns correspond to possible outcomes on each variable, and rows correspond to the latent classes.
for(v in names(model$probs)){
  if(ncol(model$probs[[v]]) == 2){
    # if there are only two outcomes for this variable (dichotomous), only keep the second
    this.probs <- model$probs[[v]][ ,2] # the second column is "yes" for that variable, i.e. the probability of 1 for that context
    names(this.probs) <- v
  } else {
    # if this variable is polytomous, keep all probs
    this.probs <- model$probs[[v]]
    colnames(this.probs) <- paste(v, 1:ncol(this.probs), sep="_")
  }
  if(v == names(model$probs)[1]){
    # first time through the loop
    lca_class_var_probs <- data.frame(this.probs)
  } else {
    # all subsequent times
    lca_class_var_probs <- cbind(lca_class_var_probs, this.probs)
  }
}  
  

# make column names line up with actual levels of context codes in each method
colnames(lca_class_var_probs) <- c(paste("HJ", HJ_levels, sep="_"), 
                                   paste("STM", STM_levels, sep="_"), 
                                   paste("WL", WL_levels, sep="_"))

lca_class_var_probs <- lca_class_var_probs %>% 
  mutate(class=1:6) %>% 
  mutate(class = ifelse(class < 10, paste0("Class 0", class), paste0("Class ", class)))

cache('lca_class_var_probs')


# thresholding
lca.threshold <- threshold_plots(df_LCA_prop, thresholds=seq(.1, .9, .05), method = "LCA", save.to=file.path("graphs", "LCA"))

df_LCA_bin <- apply_threshold(df_LCA_prop, lca.threshold, 
                              plot=TRUE, method="LCA", save.to=file.path("graphs", "LCA"))
cache('df_LCA_bin')

# seqplots for LCA classes
seq_plots(df_LCA_prop, method = "LCA")
