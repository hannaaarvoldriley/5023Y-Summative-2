# packages ----
library(broom)
library(performance)

#source tidy data script ----

source("scripts/data_tidying.R")

# anova ----

butterflies_lm1 <- lm(flight_inhibit_idx ~ inbreed_coeff, data = butterflies_tidy) #fits finear model

summary(butterflies_lm1) #produce summary table of lm

anova(butterflies_lm1) #produce anova table of lm

broom::tidy(butterflies_lm1, conf.int = TRUE) #add confidence intervals

## check assumptions ----

performance::check_model(butterflies_lm1,
                         check = c("qq", "outliers", "homogeneity"))
