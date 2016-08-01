library(ProjectTemplate)
load.project()

# remove underscores from WL orth column
WL <- df_WL_bin
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
cat_WL_STM <- cat_agreement(cat.codes=list(WL=WL, 
                             STM.bin=STM.bin))
cat_WL_LDA <- cat_agreement(cat.codes=list(WL=WL, 
                             LDA.bin=LDA.bin))
cat_WL_HJ <- cat_agreement(cat.codes=list(WL=WL, 
                             HJ.bin=HJ.bin))
cat_HJ_STM <- cat_agreement(cat.codes=list(HJ.bin=HJ.bin, 
                             STM.bin=STM.bin))
cat_HJ_LDA <- cat_agreement(cat.codes=list(HJ.bin=HJ.bin, 
                             LDA.bin=LDA.bin))
# just out of curiosity...
cat_agreement(cat.codes=list(LDA.bin=LDA.bin, 
                             STM.bin=STM.bin))

nrow(WL); nrow(STM.con); nrow(STM.bin); nrow(HJ.con); nrow(HJ.bin)

###########################
# Correspondence Analysis #
###########################
# see http://www.statmethods.net/advstats/ca.html
# for interpretation: http://www.sthda.com/english/wiki/correspondence-analysis-in-r-the-ultimate-guide-for-the-analysis-the-visualization-and-the-interpretation-r-software-and-data-mining#interpretation-of-ca-outputs
# also see http://gastonsanchez.com/how-to/2012/10/13/MCA-in-R/ # not needed
ca_WL_STM <- ca(cat_WL_STM[[1]])
ca_WL_LDA <- ca(cat_WL_LDA[[1]])
ca_WL_HJ <- ca(cat_WL_HJ[[1]])
ca_HJ_STM <- ca(cat_HJ_STM[[1]])
ca_HJ_LDA <- ca(cat_HJ_LDA[[1]])
summary(ca_WL_STM)
plot(ca_WL_STM)
plot(ca_WL_STM, mass = TRUE, contrib = "absolute", map =
       "rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map

?FactoMineR::CA
ca_WL_STM <- CA(cat_WL_STM[[1]])
ca_WL_LDA <- CA(cat_WL_LDA[[1]])
ca_WL_HJ <- CA(cat_WL_HJ[[1]])
ca_HJ_STM <- CA(cat_HJ_STM[[1]])
ca_HJ_LDA <- CA(cat_HJ_LDA[[1]])
summary(ca_WL_STM)
#The result of the function summary() contains the chi-square statistic and 3 tables:
#Table 1 - Eigenvalues: table 1 contains the variances and the percentage of variances retained by each dimension.
#Table 2 contains the coordinates, the contribution and the cos2 (quality of representation [in 0-1]) of the first 10 active row variables on the dimensions 1 and 2.
#Table 3 contains the coordinates, the contribution and the cos2 (quality of representation [in 0-1]) of the first 10 active column variables on the dimensions 1 and 2.
plot(ca_WL_STM)
?factoextra::fviz_ca_biplot
# The graph above is called symetric plot and shows a global pattern within the data. Rows are represented by blue points and columns by red triangles.
#The distance between any row points or column points gives a measure of their similarity (or dissimilarity).
#Row points with similar profile are closed on the factor map. The same holds true for column points.
# The distance between any row and column items is not meaningful! You can only make a general statements about the observed pattern.
# In order to interpret the distance between column and row points, the column profiles must be presented in row space or vice-versa. This type of map is called asymmetric biplot.
fviz_ca_biplot(ca_WL_STM, map ="rowprincipal", arrow = c(TRUE, TRUE))
fviz_ca_biplot(ca_WL_LDA, map ="rowprincipal", arrow = c(TRUE, TRUE))
fviz_ca_biplot(ca_WL_HJ, map ="rowprincipal", arrow = c(TRUE, TRUE))
fviz_ca_biplot(ca_HJ_STM, map ="rowprincipal", arrow = c(TRUE, TRUE))
fviz_ca_biplot(ca_HJ_LDA, map ="rowprincipal", arrow = c(TRUE, TRUE))
# If the angle between two arrows is acute, then their is a strong association between the corresponding row and column.
# To interpret the distance between rows and and a column you should perpendicularly project row points on the column arrow.

###################################
# univariate logistic regressions #
###################################
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
# canonical correlation
############################
# see (probably out of date): http://www.ats.ucla.edu/stat/r/dae/canonical.htm
LDA_vars <- dplyr::select(df_LDA_prop, -utt, -orth, -phon)
colnames(LDA_vars) <- gsub(x=colnames(LDA_vars), pattern = "topic", replacement = "lda")
STM_vars <- dplyr::select(df_STM_prop, -utt, -orth, -phon)
colnames(STM_vars) <- gsub(x=colnames(STM_vars), pattern = "topic", replacement = "stm")
HJ_vars <- dplyr::select(df_HJ_prop, -utt, -orth, -phon)


correlate(cbind(LDA_vars, STM_vars)) %>% 
  shave() %>% 
  fashion() %>% 
  kable()
correlate(cbind(LDA_vars, HJ_vars)) %>% 
  shave() %>% 
  fashion() %>% 
  kable()
correlate(cbind(HJ_vars, STM_vars)) %>% 
  shave() %>% 
  fashion() %>% 
  kable()

# canonical correlations
cc1_LDA_STM <- cc(LDA_vars, STM_vars)
cc1_LDA_HJ <- cc(LDA_vars, HJ_vars)
cc1_HJ_STM <- cc(HJ_vars, STM_vars)

# display the canonical correlations
cc1_LDA_STM$cor
cc1_LDA_HJ$cor
cc1_HJ_STM$cor

# raw canonical coefficients
cc1_LDA_STM[3:4]
cc1_LDA_HJ[3:4]
cc1_HJ_STM[3:4]
# The raw canonical coefficients are interpreted in a manner analogous to interpreting regression coefficients 
# i.e., for the variable read, a one unit increase in reading leads to a .0446 decrease in the first canonical variate of set 2 when all of the other variables are held constant. 
# Here is another example: being female leads to a .6321 decrease in the dimension 1 for the academic set with the other predictors held constant.

# Next, we'll use comput to compute the loadings of the variables on the canonical dimensions (variates). 
# These loadings are correlations between variables and the canonical variates.
# compute canonical loadings
cc2_LDA_STM <- comput(LDA_vars, STM_vars, cc1_LDA_STM)
cc2_LDA_HJ <- comput(LDA_vars, HJ_vars, cc1_LDA_HJ)
cc2_HJ_STM <- comput(HJ_vars, STM_vars, cc1_HJ_STM)

# display canonical loadings
cc2_LDA_STM[3:6]
cc2_LDA_HJ[3:6]
cc2_HJ_STM[3:6]
# The above correlations are between observed variables and canonical variables which are known as the canonical loadings. 
# These canonical variates are actually a type of latent variable.

# In general, the number of canonical dimensions is equal to the number of variables in the smaller set; however, the number of significant dimensions may be even smaller. 
# Canonical dimensions, also known as canonical variates, are latent variables that are analogous to factors obtained in factor analysis. 
# For this particular model there are three canonical dimensions of which only the first two are statistically significant. 
# (Note: I was not able to find a way to have R automatically compute the tests of the canonical dimensions in any of the packages so I have included some R code below.)
# tests of canonical dimensions
ev <- (1 - cc1$cor^2)

n <- dim(vars_1)[1]
p <- length(vars_1)
q <- length(vars_2)
k <- min(p, q)
m <- n - 3/2 - (p + q)/2

w <- rev(cumprod(rev(ev)))

# initialize
d1 <- d2 <- f <- vector("numeric", k)

for (i in 1:k) {
  s <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
  si <- 1/s
  d1[i] <- p * q
  d2[i] <- m * s - p * q/2 + 1
  r <- (1 - w[i]^si)/w[i]^si
  f[i] <- r * d2[i]/d1[i]
  p <- p - 1
  q <- q - 1
}

pv <- pf(f, d1, d2, lower.tail = FALSE)
(dmat <- cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv))


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
