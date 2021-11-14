# David Klinges
# File creation date: 2019-07-03
# This script generates overlap scores across different temporal resolutions
# for each site, then combines together

## 0. Prep workspace ###########
library(tidyverse)

## ....A. Import all years data #############
mountains <- read_csv("data/03_compiled/mountains_wide.csv",
                      col_types = cols(
                        low_canopy_min = col_double(),
                        low_canopy_mean = col_double(),
                        low_canopy_max = col_double(),
                        high_canopy_min = col_double(),
                        high_canopy_mean = col_double(),
                        high_canopy_max = col_double()
                      ))

## ....B. Import avg years data ##############

mountains_avg <- read_csv("data/03_compiled/mountains_wide_avg.csv",
                      col_types = cols(
                        low_canopy_min = col_double(),
                        low_canopy_mean = col_double(),
                        low_canopy_max = col_double(),
                        high_canopy_min = col_double(),
                        high_canopy_mean = col_double(),
                        high_canopy_max = col_double()
                      ))

mountains_avg <- mountains_avg %>% 
  mutate(year = 1)

## 2. Generate temporal resolution ##########

## .....A. All years #########
source("scripts/data_processing/temporal_manipulation/temporal_rez.R")
mountains_rez <- temporal_rez(mountains, max_resolution = 730)
mountains_avg_rez <- temporal_rez(mountains_avg, max_resolution = 365)

## 4. Write out temporal rez data #######

write_csv(mountains_rez, "data/03_compiled/temporal_rez/mountains_rez_allyears.csv")

write_csv(mountains_avg_rez, "data/03_compiled/temporal_rez/temporal_rez_avgyears.csv")

