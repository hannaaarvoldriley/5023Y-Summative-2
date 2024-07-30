# title (change)----

# packages ----
#add packages as necessary
library(tidyverse) # tidy data packages
library(janitor) # clean data names
library(dplyr) # data manipulation

# import data ----
butterflies <- read_csv("data/inbreeding_butterfly.csv")

# tidying ----

head(butterflies) #check data has loaded

## column names ----

colnames(butterflies) #view column names

butterflies <- clean_names(butterflies) #change column names to snake_case

butterflies <- rename(butterflies,
                      "inbreed_coeff" = "ic",
                      "flight_inhibit_idx" = "fii",
                      "weight_mg" = "dry_weight") #rename column names to be more informative while still concise

glimpse(butterflies) #check new column names


## rename text values ----

butterflies <- butterflies %>%
  mutate(body_part=case_when(body_part == "drythor" ~ "thorax",
                             body_part == "dryrest" ~ "rest",
                             body_part == "drytotal" ~ "total")) #remove unnecessary characters from text values to make more readable


## check for errors in data ----

butterflies %>%
  duplicated() %>%
  sum() #check for duplicated data


butterflies %>%
  is.na() %>%
  sum() #check for missing data



butterflies %>%
  summarise(min=min(flight_inhibit_idx, na.rm=TRUE), 
            max=max(flight_inhibit_idx, na.rm=TRUE)) #check for typos in fii
#one individual's fii is 660 which seems unlikely for a 2 minute period
#most likely meant to be 66 as this is within the range of other fiis
#i will remove to be safe
butterflies <- butterflies %>%
  filter(flight_inhibit_idx != "660") #filters out the row with the error

butterflies %>%
  group_by(body_part) %>%
  summarise(min=min(weight_mg, na.rm=TRUE),
            max=max(weight_mg, na.rm=TRUE)) #check for typos in weights of body parts

print(butterflies %>% 
        distinct(inbreed_coeff))#check for typos in inbreeding coefficient (there should only be 3 levels of ic)

print(butterflies %>% 
        distinct(body_part)) #check for typos in body parts

#make all values 2 decimal places?




