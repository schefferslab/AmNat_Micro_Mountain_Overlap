## David Klinges
## klinges@si.edu

# This script determines average leaf-up dates for NH Hubbard Brook 
# https://portal.lternet.edu/nis/mapbrowse?scope=knb-lter-hbr&identifier=195


## Prep Workspace #########

library(tidyverse)
library(lubridate)

leafup_raw <- read_csv("data/01_primary/temperate/NH_whites/phenology/archive_knb-lter-hbr.195.2_155934250534690175/ccase_saplings_spring_phenology.txt")
pheno_raw <- read_csv("data/01_primary/temperate/NH_whites/phenology/phn.txt")

## Data curation, leafup ######

leafup <- leafup_raw %>% 
  # Replace -99 with NA
  mutate(pheno_class = ifelse(pheno_class == -99, NA, pheno_class)) %>% 
  
  # Filter to just ambient temperature plots
  filter(Treatment == "Reference") %>% 
  # Filter to just leaf unfolding events
  filter(pheno_class == 3 | pheno_class == 4) %>% 
  # Create year col
  mutate(year = year(Date)) %>% 
  # Group by year
  group_by(year, pheno_class) %>% 
  summarize(avg_date = mean(Date)) %>% 
  # Create julian data from avg datea
  mutate(julian = yday(avg_date)) %>% 

# We now have average dates for 3 ("Bud break, leaf tissue is visible, unfolding is occurring")
# and 4 ("Leaf unfolding, shape of leaf is discernible, and top is visible"). Let's
# take the average of these two dates, average again for both years, and call it
# a day
  ungroup() %>% 
  summarize(julian = mean(julian))

leafup # Average leaf up is day 138


## Data curation, pheno #####

spring <- pheno_raw %>% 
  # Replace -9 with NA
  mutate_all(funs(gsub( -9, NA, .))) %>%
  # Just spring
  filter(SEASON == "SPRING") %>% 
  # gather so all trees are in one column, all codes are in other
  gather(key = "tree", value = "pheno_class", "1B", "6T", "4B", "4T", "5B", 
         "5T", "7B", "7T", "HQ", "CONE") %>% 
  # Subset to only 5 years before/after out data (2011-2013)
  mutate(year = year(Date)) %>% 
  filter(year > 2006 & year < 2018) %>% 
  # Code 3: Leaves 1/2 of final length, leaves obscure half of sky as seen through crowns
  filter(pheno_class == 3) %>% 
  # Group by year
  group_by(year) %>% 
  summarize(avg_date = mean(as.double(DAY))) %>% 
  # Now get average for this set of years
  ungroup() %>% 
  summarize(avg_date = mean(avg_date))
  
spring # Average leaf up is day 138, TO THE DAY confirms other data. Good

fall <- pheno_raw %>% 
  # Replace -9 with NA
  mutate_all(funs(gsub( -9, NA, .))) %>%
  # Just fall
  filter(SEASON == "FALL") %>% 
  # gather so all trees are in one column, all codes are in other
  gather(key = "tree", value = "pheno_class", "1B", "6T", "4B", "4T", "5B", 
         "5T", "7B", "7T", "HQ", "CONE") %>% 
  # Subset to only 5 years before/after out data (2011-2013)
  mutate(year = year(Date)) %>% 
  filter(year > 2006 & year < 2018) %>% 
  # Code 1: no more green in canopy, half of leaves have fallen, leaves still 
  # obscure half of sky as seen through crown  
  filter(pheno_class == 1) %>% 
  # Group by year
  group_by(year) %>% 
  summarize(avg_date = mean(as.double(DAY))) %>% 
  # Now get average for this set of years
  ungroup() %>% 
  summarize(avg_date = mean(avg_date))

fall # 289
  
