# packages ----
library(broom)
library(performance)

#source tidy data script ----

source("scripts/data_tidying.R")

#fit linear model ----

butterflies_lm8 <- lm(flight_inhibit_idx ~ inbreed_coeff + thorax_mg, data = butterflies_tidy)#fit linear model

performance::check_model((butterflies_lm8))#check assumptions of lm

summary(butterflies_lm8)#print summary for analysis

broom::tidy(butterflies_lm8, conf.int = TRUE) #add confidence intervals
#assign to oject for visualisation with ggplot2

anova(butterflies_lm8) #perform anova on the model