# David Klinges
# 2019-05-18
## For querying phenological databases to determine leaf-on and leaf-off dates


## Workspace prep ##########

library(tidyverse)

gill <- read_csv("data/01_primary/temperate/phenology/gill2015_pheno.csv")


## Data curation #########

gill <- gill %>%
  mutate(lat = gsub("\\D", "", Latitude)) %>%
  mutate(long = gsub("\\D", "", Longitude))

gill$lat <- as.double(substr(gill$lat, 0, 2))
gill$long <- as.double(substr(gill$long, 0, 2))

# Data querying #######

idaho <- gill %>%
  filter(Continent == "North America") %>%
  filter(lat < 50 & lat > 35) %>%
  filter(long < -110 & long > -125) 
