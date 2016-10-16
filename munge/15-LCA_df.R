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
###################################################################################################

# remove spaces in column names
colnames(df_all) <- gsub(x=colnames(df_all), pattern = " ", replacement = "_")
# context columns should be coded as 0 or 1 at this point
# turn all context columns into factors if they're not already (poLCA needs factors)
df_all <- df_all %>% 
  mutate_if(is.numeric, as.factor)

#---------------------------------------------------------------
# run the LCA models

# f is the formula for us in poLCA commands
# for LCA with no predictors (just estimating the latent classes), the formula is cbind(all outcome vectors) ~ 1
f <- cbind(WL_bath, WL_bed, WL_body_touch, WL_diaper_dressing, WL_fussing, WL_meal, WL_media, WL_play, HJ_bathtime, HJ_diaper_change, HJ_dressing, HJ_fussing, HJ_hiccups, HJ_housework, HJ_interaction, HJ_mealtime, HJ_outside, HJ_playtime, HJ_sleep, HJ_taking_pictures, HJ_touching, HJ_TV, STM_topic_1, STM_topic_10, STM_topic_11, STM_topic_12, STM_topic_2, STM_topic_3, STM_topic_4, STM_topic_5, STM_topic_6, STM_topic_7, STM_topic_8, STM_topic_9) ~ 1

# create an empty storage variable called lca_models
# this will be filled up by each call to poLCA below
lca_models <- list()
lca_models$lca5 <- poLCA(f, df_all, nclass=5, maxiter = 10000, nrep=5, na.rm=FALSE)
lca_models$lca6 <- NULL #poLCA(f, df_all, nclass=6, maxiter = 10000, nrep=5, na.rm=FALSE)
lca_models$lca7 <- NULL #poLCA(f, df_all, nclass=7, maxiter = 10000, nrep=5, na.rm=FALSE)
lca_models$lca8 <- poLCA(f, df_all, nclass=8, maxiter = 10000, nrep=5, na.rm=FALSE)
lca_models$lca9 <- NULL #poLCA(f, df_all, nclass=9, maxiter = 10000, nrep=5, na.rm=FALSE)
lca_models$lca10 <- NULL #poLCA(f, df_all, nclass=10, maxiter = 10000, nrep=10, na.rm=FALSE)
lca_models$lca11 <- poLCA(f, df_all, nclass=11, maxiter = 10000, nrep=10, na.rm=FALSE)
lca_models$lca12 <- NULL #poLCA(f, df_all, nclass=12, maxiter = 10000, nrep=10, na.rm=FALSE)
lca_models$lca13 <- NULL #poLCA(f, df_all, nclass=13, maxiter = 10000, nrep=10, na.rm=FALSE)
lca_models$lca14 <- poLCA(f, df_all, nclass=14, maxiter = 10000, nrep=10, na.rm=FALSE)
lca_models$lca15 <- NULL #poLCA(f, df_all, nclass=15, maxiter = 10000, nrep=10, na.rm=FALSE)
lca_models$lca16 <- NULL #poLCA(f, df_all, nclass=16, maxiter = 10000, nrep=10, na.rm=FALSE)
lca_models$lca17 <- poLCA(f, df_all, nclass=17, maxiter = 10000, nrep=10, na.rm=FALSE)
lca_models$lca18 <- NULL #poLCA(f, df_all, nclass=18, maxiter = 10000, nrep=10, na.rm=FALSE)
lca_models$lca19 <- poLCA(f, df_all, nclass=19, maxiter = 10000, nrep=10, na.rm=FALSE)
lca_models$lca20 <- poLCA(f, df_all, nclass=20, maxiter = 10000, nrep=10, na.rm=FALSE)
lca_models$lca21 <- poLCA(f, df_all, nclass=21, maxiter = 10000, nrep=10, na.rm=FALSE)
lca_models$lca22 <- NULL #poLCA(f, df_all, nclass=22, maxiter = 10000, nrep=10, na.rm=FALSE)
lca_models$lca23 <- poLCA(f, df_all, nclass=23, maxiter = 10000, nrep=10, na.rm=FALSE)
lca_models$lca24 <- NULL #poLCA(f, df_all, nclass=24, maxiter = 10000, nrep=10, na.rm=FALSE)
lca_models$lca25 <- poLCA(f, df_all, nclass=25, maxiter = 10000, nrep=10, na.rm=FALSE)
lca_models$lca26 <- NULL #poLCA(f, df_all, nclass=26, maxiter = 10000, nrep=10, na.rm=FALSE)
lca_models$lca27 <- NULL #poLCA(f, df_all, nclass=27, maxiter = 10000, nrep=10, na.rm=FALSE)
lca_models$lca28 <- poLCA(f, df_all, nclass=28, maxiter = 10000, nrep=10, na.rm=FALSE)
lca_models$lca29 <- NULL #poLCA(f, df_all, nclass=29, maxiter = 10000, nrep=10, na.rm=FALSE)
lca_models$lca30 <- NULL #poLCA(f, df_all, nclass=30, maxiter = 10000, nrep=10, na.rm=FALSE)

# lca_models$lca21$time # how long did it take to run?

cache('lca_models')

#---------------------------------------------------------------
# compile the lca model reuslts into a more usable summary data frame:

# create an empty storage data.frame, which will get filled in the for loop below
lca_results <- data.frame(model=names(lca_models), aic=NA,
                          bic=NA, Chisq=NA, llik=NA, npar=NA, resid.df=NA, numiter=NA)
for(m in 1:length(lca_models)){
  if(lca_models[[m]]$maxiter == lca_models[[m]]$numiter) {
    warning("Did not converge! ", names(lca_models)[m])
  } else {
    lca_results$aic[m] <- lca_models[[m]]$aic
    lca_results$bic[m] <- lca_models[[m]]$bic
    lca_results$Gsq[m] <- lca_models[[m]]$Gsq # Likelihood ratio/deviance statistic.
    lca_results$Chisq[m] <- lca_models[[m]]$Chisq # Pearson Chi-square goodness of fit statistic for fitted vs. observed multiway tables.
    lca_results$llik[m] <- lca_models[[m]]$llik
    lca_results$npar[m] <- lca_models[[m]]$npar  # number of degrees of freedom used by the model (estimated parameters)
    lca_results$resid.df[m] <- lca_models[[m]]$resid.df # number of residual degrees of freedom
    lca_results$numiter[m] <- lca_models[[m]]$numiter
  }
}
lca_results <- lca_results %>% 
  gather(key=measure, value=value, aic, bic, Gsq, Chisq, llik) %>% 
  extract(col=model, into="nclass", regex="([[:digit:]]+)") %>% 
  mutate(nclass=as.numeric(nclass))

ggplot(lca_results, aes(y=value, x=nclass, color=measure))+
  geom_line()+
  facet_wrap(~ measure, scales = "free") +
  scale_x_continuous(breaks = seq(from=min(lca_results$nclass), to=max(lca_results$nclass), by=2)) + 
  theme(text = element_text(size=20))
ggsave("LCA_fit_measures.png", path=file.path("graphs", "LCA"), width = 16, height = 10, units = "in")
ggplot(dplyr::filter(lca_results, measure == "bic"), aes(y=value, x=nclass))+
  geom_line() +
  scale_x_continuous(breaks = seq(from=min(lca_results$nclass), to=max(lca_results$nclass), by=2)) + 
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

cache('lca_class_var_probs')

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
cache('df_LCA_bin')

# seqplots for LCA classes
seq_plots(df_LCA_prop, method = "LCA")
