# packages ----
library(broom)
library(performance)

#source tidy data script ----

source("scripts/data_tidying.R")

# anova ----

butterflies_lm1 <- lm(flight_inhibit_idx ~ inbreed_coeff, data = butterflies_tidy) #fits linear model

summary(butterflies_lm1) #produce summary table of lm

anova(butterflies_lm1) #produce anova table of lm

broom::tidy(butterflies_lm1, conf.int = TRUE) #add confidence intervals

## check assumptions ----

performance::check_model(butterflies_lm1,
                         check = c("qq", "outliers", "homogeneity"),
                         detrend = FALSE) #concerned about the q-q plot
performance::check_normality(butterflies_lm1) #non-normality detected
#also realised this lm doesn't fit well for a few other reasons so will be disregarding
performance::check_model((butterflies_lm1))

butterflies_lm2 <- lm(flight_inhibit_idx ~ inbreed_coeff + thorax_mg + rest_mg + total_mg, data = butterflies_tidy) #fit another lm with all variables included
performance::check_model((butterflies_lm2))#this model will violate assumption of multicollinearity due to total_mg being the sum of thorax_mg and rest_mg


butterflies_lm3 <- lm(flight_inhibit_idx ~ inbreed_coeff + thorax_mg + rest_mg, data = butterflies_tidy)
performance::check_model((butterflies_lm3))#this is the most appropriate model so far

butterflies_lm4 <- lm(flight_inhibit_idx ~ inbreed_coeff * thorax_mg + rest_mg, data = butterflies_tidy) #introduce an interaction term with thorax_mg
performance::check_model((butterflies_lm4)) #high collinearity
vif(butterflies_lm4) #confirmed severe multicollinearity

butterflies_lm5 <- lm(flight_inhibit_idx ~ inbreed_coeff * rest_mg+ thorax_mg, data = butterflies_tidy) #rest_mg interaction term
performance::check_model((butterflies_lm5)) #high collinearity but not as high as lm4

summary(butterflies_lm3)
summary(butterflies_lm1)

butterflies_lm6 <- lm(flight_inhibit_idx ~ inbreed_coeff + thorax_mg, data = butterflies_tidy)#lm3 without the rest_mg variable
performance::check_model((butterflies_lm6)) 
summary(butterflies_lm6)

#determined lm3 has best fit