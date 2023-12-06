
# R version 4.2.1
# R studio version 2022.7.1.554

# Install packages only for first use
# install.packages('nlme')
# install.packages('splines')


# Load packages
library(nlme)
library(splines)

# -----------------------------------------------------------------------
# Mixed-effects models with a first-order autoregressive covariance structure and interaction terms (time-versus-treatment group)
lmm_model_1 <- lme(CD4~ ns(time,knots = c(1,2))*initial_ART+initial_cd4+ARTyear+agegroup+sex+trans_route+region+time_to_ART+backbone, random = ~time|id, corAR1(form = ~time | id), method = "REML", data = data1)

summary(lmm_model_1)

# Extract standard errors of fixed effects
fixed_se <- summary(lmm_model_1)$tTable[, "Std.Error"]

# Calculate confidence intervals for coefficients
lower_ci <- fixef(lmm_model_1) - 1.96 * fixed_se
upper_ci <- fixef(lmm_model_1) + 1.96 * fixed_se
