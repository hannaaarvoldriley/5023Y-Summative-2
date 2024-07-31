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

butterflies_tidy <- clean_names(butterflies) #change column names to snake_case

butterflies_tidy <- rename(butterflies_tidy,
                      "inbreed_coeff" = "ic",
                      "flight_inhibit_idx" = "fii",
                      "weight_mg" = "dry_weight") #rename column names to be more informative while still concise

glimpse(butterflies_tidy) #check new column names


## rename text values ----

butterflies_tidy <- butterflies_tidy %>%
  mutate(body_part=case_when(body_part == "drythor" ~ "thorax",
                             body_part == "dryrest" ~ "rest",
                             body_part == "drytotal" ~ "total")) #remove unnecessary characters from text values to make more readable


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
  filter(flight_inhibit_idx != "660") #filter out row with error

butterflies_tidy %>%
  group_by(body_part) %>%
  summarise(min=min(weight_mg, na.rm=TRUE),
            max=max(weight_mg, na.rm=TRUE)) #check for typos in weights of body parts

print(butterflies_tidy %>% 
        distinct(inbreed_coeff))#check for typos in inbreeding coefficient (there should only be 3 levels of ic)

print(butterflies_tidy %>% 
        distinct(body_part)) #check for typos in body parts

#make all values 2 decimal places?




