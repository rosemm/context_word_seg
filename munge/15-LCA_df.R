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
# turn all context columns into factors if they're not already (poLCA needs factors)
df_all <- df_all %>% 
  mutate_if(is.numeric, as.factor) 

method_contexts <- df_all %>% 
  tidyr::gather(key="context", value="value", -utt, -orth, -phon) %>% 
  tidyr::extract(context, into=c("method", "context"), regex="([[:upper:]]+)_(.*)") %>% 
  dplyr::filter(value == 1) 
method_counts <- method_contexts %>% 
  dplyr::count(utt, orth, phon, method) 
df_by_method <- method_counts %>% 
  dplyr::filter(n == 1) %>% 
  dplyr::select(-n) %>% 
  left_join(method_contexts, by=c("utt", "orth", "phon", "method")) %>% 
  dplyr::select(-value) %>% 
  tidyr::spread(key=method, value=context) %>% 
  dplyr::select(utt, orth, phon, HJ, STM, WL) %>% # keep context columns for HJ, STM, and WL
  mutate_at(vars(HJ, STM, WL), as.factor)

#---------------------------------------------------------------
# run the LCA models

# f is the formula for us in poLCA commands
# for LCA with no predictors (just estimating the latent classes), the formula is cbind(all outcome vectors) ~ 1
all_outcome_vars <- df_all %>% dplyr::select(-utt, -orth, -phon) %>% colnames() %>% paste(collapse = ", ")
# print all_outcome_vars in the console and then copy it into the formula:
f <- cbind(WL_bath, WL_bed, WL_body_touch, WL_diaper_dressing, WL_fussing, WL_meal, WL_play, HJ_bathtime, HJ_diaperchange, HJ_dressing, HJ_fussing, HJ_housework, HJ_interaction, HJ_mealtime, HJ_playtime, HJ_sleep, LDA_topic_1, LDA_topic_10, LDA_topic_11, LDA_topic_12, LDA_topic_2, LDA_topic_3, LDA_topic_4, LDA_topic_5, LDA_topic_6, LDA_topic_7, LDA_topic_8, LDA_topic_9, STM_topic_1, STM_topic_10, STM_topic_11, STM_topic_12, STM_topic_2, STM_topic_3, STM_topic_4, STM_topic_5, STM_topic_6, STM_topic_7, STM_topic_8, STM_topic_9) ~ 1

library(doParallel)
registerDoParallel()
lca_models <- foreach(i=5:6, 
                      .inorder=TRUE,
                      .verbose=TRUE,
                      .packages=c("poLCA", "MASS") ) %dopar% poLCA(f, df_all, nclass=i, maxiter = 10000, nrep=i, na.rm=FALSE)


# cache('lca_models')

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
  scale_x_continuous(breaks = seq(from=min(lca_results_plot$nclass), to=max(lca_results_plot$nclass), by=2)) 
ggplot(lca_results_plot, aes(y=value, x=nclass, color=measure))+
  geom_line(show.legend=FALSE) +
  facet_wrap(~ measure, scales = "free") +
  scale_x_continuous(breaks = seq(from=min(lca_results_plot$nclass), to=max(lca_results_plot$nclass), by=2)) + 
  theme(text = element_text(size=20))
ggsave("LCA_fit_measures.png", path=file.path("graphs", "LCA"), width = 16, height = 10, units = "in")
ggplot(dplyr::filter(lca_results_plot, measure == "BIC"), aes(y=value, x=nclass))+
  geom_line() +
  scale_x_continuous(breaks = seq(from=min(lca_results_plot$nclass), to=max(lca_results_plot$nclass), by=2)) + 
  labs(y="BIC", x="Number of classes") +
  theme(text = element_text(size=20))
ggsave("LCA_fit_BIC.png", path=file.path("graphs", "LCA"), width = 8, height = 8, units = "in")


model <- lca_models$lca19 # which model to investigate

# Posterior prob for class assignment. What's the prob each utterance is in each latent class?
df_LCA_prop <- cbind(dplyr::select(df_all, utt, orth, phon), as.data.frame(model$posterior)) %>% 
  as.tbl()
df_LCA_prop$predclass <- model$predclass

# A list of matrices containing the estimated class-conditional outcome probabilities πˆjrk. 
# Each item in the list represents one manifest variable; 
# columns correspond to possible outcomes on each variable, and rows correspond to the latent classes.
lca_class_var_probs <- matrix(nrow=nrow(model$probs[[1]]), ncol=length(names(model$probs)) ) %>% 
  as.data.frame()
colnames(lca_class_var_probs) <- names(model$probs)
for(v in names(model$probs)){
  lca_class_var_probs[[v]] <- model$probs[[v]][,2] # the second column is "yes" for that variable, i.e. the probability of 1 for that context
}  
  
lca_class_var_probs <- lca_class_var_probs %>% 
  mutate(class=1:19) %>% 
  mutate(class = ifelse(class < 10, paste0("Class 0", class), paste0("Class ", class)))

# cache('lca_class_var_probs')

lca_class_var_probs %>% 
  gather(key=var, value=value, -class) %>% 
  ggplot(aes(class, var, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white",  high = "darkblue", guide = guide_legend(title="Class-conditional\nProbability") ) + 
  labs(x=NULL, y=NULL) + 
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("LCA_classcondvars_heatmap.png", path=file.path("graphs", "LCA"), width = 14, height = 20, units = "in")


p <- lca_class_var_probs %>% 
  gather(key=var, value=prob, -class) %>% 
  extract(col=var, into="method", regex="^([[:upper:]]+)_", remove=FALSE)

ggplot(filter(p, method=="WL"), aes(y=prob, x=class, color=var, fill=var)) + 
  geom_bar(stat="identity", position = "dodge") + 
  facet_wrap(~class, scale = "free_x")  +
  theme(text = element_text(size=20))
ggsave("LCA_varsWL.png", path=file.path("graphs", "LCA"), width = 16, height = 16, units = "in")

ggplot(filter(p, method=="HJ"), aes(y=prob, x=class, color=var, fill=var)) + 
  geom_bar(stat="identity", position = "dodge") + 
  facet_wrap(~class, scale = "free_x")  +
  theme(text = element_text(size=20))
ggsave("LCA_varsHJ.png", path=file.path("graphs", "LCA"), width = 16, height = 16, units = "in")

ggplot(filter(p, method=="STM"), aes(y=prob, x=class, color=var, fill=var)) + 
  geom_bar(stat="identity", position = "dodge") + 
  facet_wrap(~class, scale = "free_x")  +
  theme(text = element_text(size=20))
ggsave("LCA_varsSTM.png", path=file.path("graphs", "LCA"), width = 16, height = 16, units = "in")

colors <- c("#FFFF33", "#FD8D3C", "#FC4E2A","#238B45",
            brewer.pal(9,"YlGnBu")[c(4,6,8)],
            brewer.pal(9,"PuRd")[c(5,7)],
            brewer.pal(9,"BuPu")[c(5,7,9)],
            brewer.pal(9,"YlOrRd")[c(9)], 
            brewer.pal(9,"Greens")[c(4,9)],
            brewer.pal(11,"RdGy")[c(8,10)],
            brewer.pal(8, "Pastel1"))

ggplot(filter(p, prob > .2), aes(y=prob, x=class, fill=var)) + 
  geom_bar(stat="identity", position = "dodge") + 
  facet_wrap(~class, scale = "free_x") + 
  scale_fill_manual(values=colors) +
  labs(title="Variables that load on LCA classes above .2 prob",
       x=NULL) + 
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank() )  +
  theme(text = element_text(size=20))
ggsave("LCA_classcondvars_bar.png", path=file.path("graphs", "LCA"), width = 16, height = 16, units = "in")

ggplot(filter(p, method=="WL"), aes(y=prob, x=var, fill=class)) + 
  geom_bar(stat="identity", position = "dodge") + 
  facet_wrap(~var, scale = "free") + 
  scale_fill_manual(values=colors) +
  labs(title="Class-conditional probabilities for each word list context",
       x=NULL) + 
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank() )  +
  theme(text = element_text(size=20))
ggsave("LCA_classcondvars_WL_bar.png", path=file.path("graphs", "LCA"), width = 16, height = 16, units = "in")

ggplot(filter(p, method=="HJ"), aes(y=prob, x=var, fill=class)) + 
  geom_bar(stat="identity", position = "dodge") + 
  facet_wrap(~var, scale = "free") + 
  scale_fill_manual(values=colors) +
  labs(title="Class-conditional probabilities for each coder judgment context",
       x=NULL) + 
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank() )  +
  theme(text = element_text(size=20))
ggsave("LCA_classcondvars_HJ_bar.png", path=file.path("graphs", "LCA"), width = 16, height = 16, units = "in")

ggplot(filter(p, method=="STM"), aes(y=prob, x=var, fill=class)) + 
  geom_bar(stat="identity", position = "dodge") + 
  facet_wrap(~var, scale = "free") + 
  scale_fill_manual(values=colors) +
  labs(title="Class-conditional probabilities for each STM context",
       x=NULL) + 
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank() )  +
  theme(text = element_text(size=20))
ggsave("LCA_classcondvars_STM_bar.png", path=file.path("graphs", "LCA"), width = 16, height = 16, units = "in")

ggplot(filter(p, prob > .2), aes(y=prob, x=class)) + 
  geom_point(alpha=.3) +
  geom_text_repel(aes(label = var), segment.color=NA) + 
  facet_wrap(~class, scale = "free_x") + 
  labs(title="Variables that load on LCA classes above .2 prob",
       x=NULL) + 
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank() )   +
  theme(text = element_text(size=20))
ggsave("LCA_classcondvars_labs.png", path=file.path("graphs", "LCA"), width = 16, height = 16, units = "in")

# thresholding
lca.threshold <- threshold_plots(df_LCA_prop, thresholds=seq(.1, .9, .05), method = "LCA", save.to=file.path("graphs", "LCA"))

df_LCA_bin <- apply_threshold(df_LCA_prop, lca.threshold, 
                              plot=T, method="LCA", save.to=file.path("graphs", "LCA"))
# cache('df_LCA_bin')

# seqplots for LCA classes
seq_plots(df_LCA_prop, method = "LCA")
