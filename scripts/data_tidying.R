# personal access token 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 (delete before submitting)
# title (change)----

# packages ----
#add packages as necessary
library(tidyverse) # tidy data packages
library(janitor) # clean data names

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



#check na

#check duplications (remove duplicates if necessary)

