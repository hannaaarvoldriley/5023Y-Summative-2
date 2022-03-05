library(tidyverse)
library(readxl)

elegans <- read_excel(sheet=1,"Data/elegans.xlsx")

elegans$death_date <- janitor::excel_numeric_to_date(elegans$death_date) ## make numeric first

warbler %>% mutate(malaria_consensus_1_yes=factor(malaria_consensus_1_yes)) %>% ggplot(aes(x=malaria_consensus_1_yes, y=body_mass_g))+geom_point()