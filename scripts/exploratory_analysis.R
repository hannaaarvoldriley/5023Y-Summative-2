# clear environment ----
rm(list=ls())

# packages ----
library(ggplot2)

# source tidy data script ----
source("scripts/data_tidying.R")

ggplot(data = butterflies_tidy,
       aes(x = inbreed_coeff,
           y = flight_inhibit_idx))+
  geom_jitter() #ic vs fii - appears to be a correlation

ggplot(data = butterflies_tidy, aes(x = inbreed_coeff)) +
  geom_jitter(aes(y = total_mg, color = "total_mg")) +
  geom_jitter(aes(y = rest_mg, color = "rest_mg")) +
  geom_jitter(aes(y = thorax_mg, color = "thorax_mg")) #ic vs thorax/rest/total dry weights - no apparent correlation visible


ggplot(data = butterflies_tidy, aes(x = flight_inhibit_idx)) +
  geom_jitter(aes(y = total_mg, color = "total_mg")) +
  geom_jitter(aes(y = rest_mg, color = "rest_mg")) +
  geom_jitter(aes(y = thorax_mg, color = "thorax_mg")) #fii vs thorax/rest/total dry weight - appears to be correlation most prominantly in thorax_mg
