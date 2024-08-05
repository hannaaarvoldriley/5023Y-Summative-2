rm(list=ls()) #start with a clear environment


# An Analysis of the impact of inbreeding on flight inhibition in Bicyclus anynana----

#________________----

## PACKAGES ----

library(tidyverse) # tidy data packages
library(janitor) # clean data names
library(dplyr) # data manipulation
library(kableExtra)

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
  labs(title = "Flight Inhibition Index Across Levels of Inbreeding Coefficient", #title
       x = "Inbreeding Coefficient Level", #axis labels
       y = "Flight Inhibition Index",
       colour = "Inbreeding Coefficient") +
  theme_minimal() + 
  scale_y_continuous(breaks = seq(0, ceiling(max(butterflies_tidy$flight_inhibit_idx)), by = 5)) + #y axis line breaks
  theme( panel.grid.major.y = element_line(color = "grey", size = 0.2),  #appearance of line breaks
         panel.grid.minor = element_blank(), #remove minor lines
         legend.title = element_text(size = 12),  #make more aesthetically pleasing
         legend.text = element_text(size = 8),
         axis.title.y = element_text(size=14),
         axis.title.x = element_text(size=14))

# figure 2: summary of linear model ----

figure2 <- broom::tidy(butterflies_lm8, conf.int = TRUE) %>% #tidy and add confidence intervals
  kbl(caption = "Summary of Linear Model", digits = 3) %>% #figure legend
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  kable_styling(bootstrap_options = c("bordered")) #make more aesthetically pleasing


# figure 3: scatter plot of thorax weight vs flight inhibition index ---- 

figure3 <- ggplot(data = butterflies_tidy, aes(x = thorax_mg, y = flight_inhibit_idx)) +
  geom_point(aes(color = factor(inbreed_coeff)), size = 1.8, alpha = 0.85) +  #scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "#440154") +  # Add a regression line
  scale_color_manual(values = c("0" = '#1F9E89', "0.25" = '#6DCD59', "0.375" = '#3E4989')) +  #accessible colours
  labs(title = "Scatter Plot of Thorax Dry Weight vs Flight Inhibition Index", #titles
       x = "Thorax Dry Weight (mg)",
       y = "Flight Inhibition Index",
       colour = "Inbreeding Coefficient") +
  theme_minimal() + 
  scale_y_continuous(breaks = seq(0, ceiling(max(butterflies_tidy$flight_inhibit_idx)), by = 10)) +  #yaxis breaks line breaks
  theme(legend.title = element_text(size = 12),  #make titles more balanced
        legend.text = element_text(size = 10),  
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14)) 

# figure 4: anova table ----

figure4 <- anova(butterflies_lm8) %>%
  as.data.frame() %>%
  `rownames<-`(c("Inbreeding Coefficient", "Thorax Dry Weight (mg)", "Residuals")) %>%
  kbl(caption = "ANOVA Table", digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

