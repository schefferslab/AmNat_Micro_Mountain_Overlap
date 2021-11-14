# David Klinges
# This script generates plot of the magnitude of change in overlap d-score 
#   values for each site

## 1. Workspace prep ##############

library(tidyverse)

mountains <- read_csv("data/03_compiled/mountains.csv")
mountains_extra <- read_csv("data/03_compiled/mountains_janz_month_avg.csv")


## 2. Curate site table ###########

sites <- mountains %>% 
  arrange(julian, year) %>% 
  group_by(site, micro, snow) %>% 
  select(-year, -julian, -mean, -max, -min, -foliage) %>% 
  left_join(select(mountains_extra, site, micro, elevation_change)) %>% 
  summarize_all(first) %>% 
  # Change height to meters and label
  mutate(height_meters = height / 100) %>%
  # 
  rename(elevation_change_meters = elevation_change) %>% 
  select(-height)

# Count number of years
count_years <- mountains %>% 
  select(site, year) %>% 
  distinct() %>% 
  group_by(site) %>% 
  count() %>% 
  rename(number_years = n) %>% 
  ungroup() %>% 
  select(site, number_years)

# flag if the site gets snow
snow_flag <- sites %>%
  group_by(site, micro) %>% 
  count() %>% 
  mutate(snow = ifelse(n > 1, "winter snow", "no winter snow")) %>% 
  select(site, snow)

sites <- sites %>% 
  select(-snow) %>% 
  left_join(snow_flag) %>% 
  left_join(count_years)

## 3. Write out table ########

write_csv(sites, "figures/tables/site_table_trim.csv")

