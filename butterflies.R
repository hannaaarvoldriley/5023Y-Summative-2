rm(list=ls()) #start with a clear environment


# An analysis of the impact of inbreeding on flight inhibition in Bicyclus anynana----

#________________----

## PACKAGES ----

library(tidyverse) # tidy data packages
library(janitor) # clean data names
library(dplyr) # data manipulation
library(kableExtra) # present tables
library(ggplot2) # produce plots

## IMPORT DATA ----
butterflies <- read_csv("data/inbreeding_butterfly.csv")

head(butterflies) #check data has loaded

# TIDYING ----

butterflies_wide <- pivot_wider(data = butterflies,
                                names_from = body_part,
                                values_from = dry_weight) #pivot to wide format to make fitting lm easier

# column names

colnames(butterflies_wide) #view column names

butterflies_tidy <- clean_names(butterflies_wide) #change column names to snake_case

butterflies_tidy <- rename(butterflies_tidy,
                           "inbreed_coeff" = "ic",
                           "flight_inhibit_idx" = "fii",
                           "thorax_mg" = "drythor",
                           "rest_mg" = "dryrest",
                           "total_mg" = "drytotal") #rename column names to be informative and concise

glimpse(butterflies_tidy) #check new column names


# check for errors in data

butterflies_tidy %>%
  duplicated() %>%
  sum() #check for duplicated data


butterflies_tidy %>%
  is.na() %>%
  sum() #check for missing data


butterflies_tidy %>%
  summarise(min=min(flight_inhibit_idx, na.rm=TRUE), 
            max=max(flight_inhibit_idx, na.rm=TRUE)) #check for typos in fii
butterflies_tidy <- butterflies_tidy %>%
  filter(flight_inhibit_idx != "660") #filter out row with error in fii


butterflies_tidy %>%
  summarise(min=min(total_mg, na.rm=TRUE),
            max=max(total_mg, na.rm=TRUE)) #check for typos in weights of body parts

print(butterflies_tidy %>% 
        distinct(inbreed_coeff))#check for typos in inbreeding coefficient (there should only be 3 levels of ic)

#_____________________----

# EXPLORATORY ANALYSIS ----

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


#_____________________----

# LINEAR MODEL ----

butterflies_lm8 <- lm(flight_inhibit_idx ~ inbreed_coeff + thorax_mg, data = butterflies_tidy)#fit linear model

performance::check_model((butterflies_lm8))#check assumptions of lm

summary(butterflies_lm8)#print summary for analysis

broom::tidy(butterflies_lm8, conf.int = TRUE) #add confidence intervals

anova(butterflies_lm8) #perform anova on the model

#_______________________------

# FIGURES ----

## figure 1: jitter plot of inbreeding coefficient vs flight inhibition index ----

figure1 <- ggplot(data = butterflies_tidy, aes(x = factor(inbreed_coeff), y = flight_inhibit_idx)) +
  geom_jitter(aes(color = factor(inbreed_coeff)), width = 0.4, height = 0, alpha = 0.6, size = 2.5) +  #jitter plot
  scale_color_manual(values = c("0" = '#1F9E89', "0.25" = '#31688E', "0.375" = '#440154'),) +  #accessible colours
  labs(x = "Inbreeding Coefficient Level", #axis labels
       y = "Flight Inhibition Index",
       colour = "Inbreeding\nCoefficient") +
  theme_minimal() + 
  scale_y_continuous(breaks = seq(0, ceiling(max(butterflies_tidy$flight_inhibit_idx)), by = 5)) + #y axis line breaks
  theme( panel.grid.major.y = element_line(color = "grey", size = 0.2),  #appearance of line breaks
         panel.grid.minor = element_blank(), #remove minor lines
         legend.title = element_text(size = 8),  #make more aesthetically pleasing
         legend.text = element_text(size = 8),
         axis.title.y = element_text(size=8),
         axis.title.x = element_text(size=8))

# figure 2: summary of linear model ----

figure2 <- broom::tidy(butterflies_lm8, conf.int = TRUE) %>% #tidy and add confidence intervals
  kbl(caption="Table summarising the Multiple Linear Regression Analysis for Flight Inhibition Index. This table presents the results of the linear regression analysis assessing the effects of inbreeding coefficient and thorax dry weight on the flight inhibition index in Bicyclus anynana. The regression model indicates that the inbreeding coefficient is significantly positively associated with flight inhibition index (Estimate = 20.7, SE = 4.9, t = 4.2, p < 0.001), suggesting that higher inbreeding levels increase flight inhibition. Conversely, thorax dry weight has a significant negative effect on the flight inhibition index (Estimate = -3.1, SE = 1.4, t = -2.2, p = 0.031), indicating that greater thorax weight is associated with reduced flight inhibition. The R-squared value of 0.058 reflects the proportion of variance in flight inhibition explained by the model.", digits = 3) %>% #figure legend
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  kable_styling(bootstrap_options = c("bordered")) #make more aesthetically pleasing


# figure 3: scatter plot of thorax weight vs flight inhibition index ---- 

figure3 <- ggplot(data = butterflies_tidy, aes(x = thorax_mg, y = flight_inhibit_idx)) +
  geom_point(aes(color = factor(inbreed_coeff)), size = 1.8, alpha = 0.85) +  #scatter plot
  geom_smooth(method = "lm", se = TRUE, color = "#440154") +  # Add a regression line
  scale_color_manual(values = c("0" = '#1F9E89', "0.25" = '#6DCD59', "0.375" = '#3E4989')) +  #accessible colours
  labs(x = "Thorax Dry Weight (mg)", #labels
       y = "Flight Inhibition Index",
       colour = "Inbreeding\nCoefficient") +
  theme_minimal() + 
  scale_y_continuous(breaks = seq(0, ceiling(max(butterflies_tidy$flight_inhibit_idx)), by = 10)) +  #yaxis breaks line breaks
  theme(legend.title = element_text(size = 8),  #make titles more balanced
        legend.text = element_text(size = 8),  
        axis.title.x = element_text(size = 8), 
        axis.title.y = element_text(size = 8))

# figure 4: anova table ----

figure4 <- anova(butterflies_lm8) %>%
  as.data.frame() %>%
  `rownames<-`(c("Inbreeding Coefficient", "Thorax Dry Weight (mg)", "Residuals")) %>%
  kbl(caption = "Results of the ANOVA for Flight Inhibition Index in Bicyclus anynana. This table presents the analysis of variance assessing the effect of inbreeding coefficient and thorax dry weight on the flight inhibition index. The ANOVA showed statistical significant in both inbreeding coefficient (F(2, 308) = 12.5, p < 0.001) and thorax dry weight (F(1, 308) = 5.6, p = 0.018), indicating they both influence flight inhibition in butterflies. The overall model explains a proportion of the variance, with an F-statistic of 9.4 (p < 0.001), suggesting the predictors significantly impact the dependent variable..") %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

