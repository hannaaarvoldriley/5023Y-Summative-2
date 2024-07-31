# clear environment ----
rm(list=ls())

# packages ----


# source tidy data script ----
source("scripts/data_tidying.R")

ggplot(data = butterflies_tidy,
       aes(x = inbreed_coeff,
           y = flight_inhibit_idx))+
  geom_jitter() #ic vs fii - appears to be a correlation



ggplot(data = butterflies_tidy, 
       aes(x = inbreed_coeff, 
           y = weight_mg, 
           color = body_part)) +
  geom_jitter() #ic vs weight - no obvious correlation

ggplot(data = butterflies_tidy, 
       aes(x = weight_mg, 
           color = body_part, 
           y = flight_inhibit_idx)) +
  geom_jitter() #weight vs fii - no obvious correlation

filtered_data <- butterflies_tidy %>% 
  filter(body_part == "total")
ggplot(data = filtered_data, 
       aes(x = weight_mg, 
           y = flight_inhibit_idx)) +
  geom_jitter() #tried filtering total, thorax and rest of body to check no correlations between any of those and fii
