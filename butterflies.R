rm(list=ls()) #start with a clear environment

# title (change)----

# packages ----
#add packages as necessary
library(tidyverse) # tidy data packages
library(janitor) # clean data names
library(dplyr) # data manipulation

# import data ----
butterflies <- read_csv("data/inbreeding_butterfly.csv")

head(butterflies) #check data has loaded

# tidying ----

## pivot data to wide format ---
#individuals only have 1 row each which makes fitting lm easier
butterflies_wide <- pivot_wider(data = butterflies,
                                names_from = body_part,
                                values_from = dry_weight)


## column names ----

colnames(butterflies_wide) #view column names

butterflies_tidy <- clean_names(butterflies_wide) #change column names to snake_case

butterflies_tidy <- rename(butterflies_tidy,
                      "inbreed_coeff" = "ic",
                      "flight_inhibit_idx" = "fii",
                      "thorax_mg" = "drythor",
                      "rest_mg" = "dryrest",
                      "total_mg" = "drytotal") #rename column names to be informative and concise

glimpse(butterflies_tidy) #check new column names


## check for errors in data ----

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

