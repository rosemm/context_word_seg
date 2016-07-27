library(ProjectTemplate)
load.project()

WL <- df_WL %>% 
  mutate_each(funs(ifelse(. > 0, 1, 0)), -utt, -orth, -phon) # for word list analysis only, make all contexts either 1 or 0 (smoothing over 1.5's from expand_windows)
# remove underscores from WL orth column
WL$orth <- gsub(x=WL$orth, pattern="_", replacement=" ")

STM.con <- df_STM_prop
STM.bin <- df_STM_bin
LDA.con <- df_LDA_prop
LDA.bin <- df_LDA_bin
HJ.con <- df_HJ_prop
HJ.bin <- df_HJ_bin

# http://www.people.vcu.edu/~pdattalo/702SuppRead/MeasAssoc/NominalAssoc.html
# Cramer's V is the most popular of the chi-square-based measures of nominal association because it gives good norming from 0 to 1 regardless of table size, when row marginals equal column marginals. V equals the square root of chi-square divided by sample size, n, times m, which is the smaller of (rows - 1) or (columns - 1): V = SQRT(X2/nm).
# V may be viewed as the association between two variables as a percentage of their maximum possible variation.V2 is the mean square canonical correlation between the variables. For 2-by-2 tables, V = phi (hence some packages like Systat print V only for larger tables).
cat_agreement(cat.codes=list(WL=WL, 
                             STM.bin=STM.bin))
cat_agreement(cat.codes=list(WL=WL, 
                             LDA.bin=LDA.bin))
cat_agreement(cat.codes=list(WL=WL, 
                             HJ.bin=HJ.bin))
cat_agreement(cat.codes=list(HJ.bin=HJ.bin, 
                             STM.bin=STM.bin))
cat_agreement(cat.codes=list(HJ.bin=HJ.bin, 
                             LDA.bin=LDA.bin))
# just out of curiosity...
cat_agreement(cat.codes=list(LDA.bin=LDA.bin, 
                             STM.bin=STM.bin))

nrow(WL); nrow(STM.con); nrow(STM.bin); nrow(HJ.con); nrow(HJ.bin)

# drop orth and phon in case there are superficial issues matching them with the join() below
WL <- dplyr::select(WL, -orth, -phon)
STM.con <- dplyr::select(STM.con, -orth, -phon)
STM.bin <- dplyr::select(STM.bin, -orth, -phon)
LDA.con <- dplyr::select(LDA.con, -orth, -phon)
LDA.bin <- dplyr::select(LDA.bin, -orth, -phon)
HJ.con <- dplyr::select(HJ.con, -orth, -phon)
HJ.bin <- dplyr::select(HJ.bin, -orth, -phon)
colnames(WL)[-1] <- paste0("WL_", colnames(WL)[-1])
colnames(STM.con)[-1] <- paste0("STM.con_", colnames(STM.con)[-1])
colnames(STM.bin)[-1] <- paste0("STM.bin_", colnames(STM.bin)[-1])
colnames(LDA.con)[-1] <- paste0("LDA.con_", colnames(LDA.con)[-1])
colnames(LDA.bin)[-1] <- paste0("LDA.bin_", colnames(LDA.bin)[-1])
colnames(HJ.con)[-1] <- paste0("HJ.con_", colnames(HJ.con)[-1])
colnames(HJ.bin)[-1] <- paste0("HJ.bin_", colnames(HJ.bin)[-1])

all.methods <- full_join(WL, STM.con, by="utt") %>% 
  full_join(STM.bin, by="utt") %>% 
  full_join(LDA.con, by="utt") %>% 
  full_join(LDA.bin, by="utt") %>%
  full_join(HJ.con, by="utt") %>% 
  full_join(HJ.bin, by="utt")
nrow(all.methods)  

# univariate logistic regressions
WL_STM.con <- logistic_regressions(all.methods, outcome_method="WL", predictor_method="STM.con", min.N.utt=300, save.to="graphs/agreement")
WL_LDA.con <- logistic_regressions(all.methods, outcome_method="WL", predictor_method="LDA.con", min.N.utt=300, save.to="graphs/agreement")
WL_HJ.con  <- logistic_regressions(all.methods, outcome_method="WL", predictor_method="HJ.con", min.N.utt=300, save.to="graphs/agreement")
HJ.bin_STM.con  <- logistic_regressions(all.methods, outcome_method="HJ.bin", predictor_method="STM.con", min.N.utt=300, save.to="graphs/agreement")
STM.bin_HJ.con  <- logistic_regressions(all.methods, outcome_method="STM.bin", predictor_method="HJ.con", min.N.utt=300, save.to="graphs/agreement")
HJ.bin_LDA.con  <- logistic_regressions(all.methods, outcome_method="HJ.bin", predictor_method="LDA.con", min.N.utt=300, save.to="graphs/agreement")
LDA.bin_HJ.con  <- logistic_regressions(all.methods, outcome_method="LDA.bin", predictor_method="HJ.con", min.N.utt=300, save.to="graphs/agreement")

all.log.estimates <- rbind(WL_STM.con$plot.data,
                           WL_LDA.con$plot.data,
                           WL_HJ.con$plot.data, 
                           HJ.bin_STM.con$plot.data, 
                           STM.bin_HJ.con$plot.data,
                           HJ.bin_LDA.con$plot.data, 
                           LDA.bin_HJ.con$plot.data)
cache('all.log.estimates')

unique(all.log.estimates$outcome)

# # multivariate logistic regression
# WL_concols <- as.matrix(dplyr::select(all.methods, starts_with("WL_")))
# STM.con_concols <- as.matrix(dplyr::select(all.methods, starts_with("STM.con_")))
# WL_multivariate <- MCMCglmm(WL_concols ~ STM.con_concols, data=all.methods, family="categorical")

############################
# finite mixture modeling
############################
cont_data <- all.methods %>% 
  dplyr::select(contains("WL_"), contains("con_")) %>%  # continuous variables only
  na.omit()
nrow(cont_data)

# # shows model fit (BIC) on y-axis and number of latent classes on x-axis
# BIC = mclustBIC(cont_data, 
#                 G=seq(from=4, to=80, by=2)) # G tells it how many latent classes to try
# cache("BIC")
# plot(BIC) # different lines represent different assumptions about the covariance structure
# mclustModelNames("EII")
# mclustModelNames("VII")

library(depmixS4)
use.vars <- all.methods %>% 
  dplyr::select(contains("WL_")) %>%
  na.omit()

colnames(use.vars) <- gsub(x = colnames(use.vars), pattern = " ", replacement = "_")

families <- list()
gaus <- gaussian()
bin <- multinomial()
for(i in 1:ncol(use.vars)){
  families[[i]] <- ifelse(grepl(x = colnames(use.vars)[i], pattern = "WL_"), bin,
                                ifelse(grepl(x = colnames(use.vars)[i], pattern = "con_"), gaus, NA))
}
responses <- list()
for(i in 1:ncol(use.vars)){
  responses[[i]] <- as.formula(paste0(colnames(use.vars)[i], " ~ 1"))
}

mod2 <- depmix(responses, 
            data=use.vars, # the dataset to use
            nstates=2, # the number of latent classes
            family=list(multinomial(),multinomial(),multinomial(),multinomial(),multinomial(),multinomial(),multinomial(),multinomial()),
            respstart=runif(32))

fit.mod2 <- fit(mod2)
posterior.states <- depmixS4::posterior(fit.mod2)
posterior.states$state <- as.factor(posterior.states$state)
