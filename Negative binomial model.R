
# R version 4.2.1
# R studio version 2022.7.1.554

# Install packages only for first use
# install.packages('MASS')
# install.packages('broom')


# Load packages
library(MASS)
library(broom)

# -----------------------------------------------------------------------
# Negative binomial model
fit<-glm.nb(days_recovery~initial_ART+initial_cd4+ARTyear+agegroup+sex+trans_route+region+time_to_ART+backbone,data2)

summary(fit)

# Summary the results
summary <- tidy(fit)
summary$CI_l <- summary$estimate - 1.96 * summary$std.error
summary$CI_u <- summary$estimate + 1.96 * summary$std.error
summary$p.value <-  2 * pnorm(abs(coef(fit)/summary$std.error), lower.tail=FALSE)
