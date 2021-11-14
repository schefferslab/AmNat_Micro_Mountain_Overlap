# David Klinges
# File creation date: 2019.02.03
# This script curates Ameriflux Southern California Sierra forest sites to prep
#   for analysis and generating figures
# Note: I don't use data_curation_program.R because Sierra has surface data, and
#   that program doesn't currently account for surface data

## 1. Workspace Prep #############

library(tidyverse)
library(lubridate)

# Import data 
low_sierra_raw <- read_csv("./data/01_primary/temperate/CA_sierra/low/Soaproot_v3_7.csv")
mid_sierra_raw <- read_csv("./data/01_primary/temperate/CA_sierra/mid/P301_v3_7.csv")
high_sierra_raw <- read_csv("./data/01_primary/temperate/CA_sierra/high/Shorthair_v3_7.csv")
# Some of the time samplings are repeated an absured number of times. Remove
# high_sierra_raw <- unique(high_sierra_raw) # Overwrite raw because that took awhile


## 2. Prep Sierra data ############
## ....2A. low: US-CZ2: Sierra Critical Zone Ponderosa Pine Forest, Soaproot Saddle #############

# Curate
low_sierra <- low_sierra_raw %>%
  dplyr::select(TIME, T_HMP, T_Soil_Surface, StartT_10cm) %>%
  rename(date = TIME) %>%
  rename(canopy = T_HMP) %>%
  mutate(canopy = replace(canopy, is.nan(canopy), NA)) %>%
  rename(surface = T_Soil_Surface) %>%
  mutate(surface = replace(surface, is.nan(surface), NA)) %>%
  rename(soil = StartT_10cm) %>%
  mutate(soil = replace(soil, is.nan(soil), NA))

low_sierra_test <- na.omit(low_sierra)

# There's a few very obvious outliers that I'm going to remove here
low_sierra <- low_sierra[-c(117976, 96150),] # two instances when canopy temp is
# significantly lower than all other measurements

# Change to date-time
low_sierra$date <- as.Date(low_sierra$date, origin = "2006-01-01")

# Change format of dates
low_sierra <- low_sierra %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(julian = yday(date)) %>%
  dplyr::select(year, julian, soil, surface, canopy)

# Change day of year 366 to day of year 1 of next year
low_sierra <- low_sierra %>%
  mutate(year = ifelse(julian == 366, (year + 1), year)) %>%
  mutate(julian = ifelse(julian == 366, 1, julian))

## ....2B. mid: US-CZ3: Sierra Critical Zone Sierran Mixed Conifer, P301 #############

# Curate
mid_sierra <- mid_sierra_raw %>%
  dplyr::select(TIME, T_HMP, T_Soil_Surface, StartT_10cm) %>%
  rename(date = TIME) %>%
  rename(canopy = T_HMP) %>%
  mutate(canopy = replace(canopy, is.nan(canopy), NA)) %>%
  rename(surface = T_Soil_Surface) %>%
  mutate(surface = replace(surface, is.nan(surface), NA)) %>%
  rename(soil = StartT_10cm) %>%
  mutate(soil = replace(soil, is.nan(soil), NA))

mid_sierra_test <- na.omit(mid_sierra)

# There's a few very obvious outliers that I'm going to remove here
mid_sierra <- mid_sierra %>%
  filter(canopy > -13 & surface > -5) # three instances when surface temp is
# significantly lower than all other measurements

# Change to date-time
mid_sierra$date <- as.Date(mid_sierra$date, origin = "2006-01-01")

# Change format of dates
mid_sierra <- mid_sierra %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(julian = yday(date)) %>%
  dplyr::select(year, julian, soil, surface, canopy)

# Change day of year 366 to day of year 1 of next year
mid_sierra <- mid_sierra %>%
  mutate(year = ifelse(julian == 366, (year + 1), year)) %>%
  mutate(julian = ifelse(julian == 366, 1, julian))

## ....2C. high: US-CZ4: Sierra Critical Zone, Subalpine Forest, Shorthair #############
# THIS SITE IS MISSING ALL SOIL DATA

# Curate
high_sierra <- high_sierra_raw %>%
  dplyr::select(TIME, T_HMP, T_Soil_Surface, StartT_10cm) %>%
  rename(date = TIME) %>%
  rename(surface = T_HMP)

high_sierra_test <- na.omit(high_sierra)

# Change to date-time
high_sierra$date <- as.Date(high_sierra$date, origin = "2006-01-01")

# Change format of dates
high_sierra <- high_sierra %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(julian = yday(date)) %>%
  dplyr::select(year, julian, surface)

# Change day of year 366 to day of year 1 of next year
high_sierra <- high_sierra %>%
  mutate(year = ifelse(julian == 366, (year + 1), year)) %>%
  mutate(julian = ifelse(julian == 366, 1, julian))


## Curate data ##############
source("./scripts/data_processing/data_curation_program.R")
prep_flux_data(low_dataset = low_sierra, 
               high_dataset = mid_sierra, 
               filepath_highallyears = "./data/01_primary/temperate/CA_sierra/derivative/Sierra_high_all_years.csv", 
               filepath_lowallyears = "./data/01_primary/temperate/CA_sierra/derivative/Sierra_low_all_years.csv",
               filepath_allyearstall = "./data/01_primary/temperate/CA_sierra/derivative/Sierra_tall.csv", 
               filepath_allyearswide = "./data/01_primary/temperate/CA_sierra/derivative/Sierra_wide.csv", 
               filepath_avgyearstall = "./data/01_primary/temperate/CA_sierra/derivative/Sierra_avgyears_tall.csv", 
               filepath_avgyearswide = "./data/01_primary/temperate/CA_sierra/derivative/Sierra_avgyears_wide.csv"
)


## Low to high curate data ##############
low_sierra_select <- low_sierra %>%
  select(year, julian, surface)

source("./scripts/data_processing/data_curation_program.R")
prep_flux_data(low_dataset = low_sierra_select, 
               high_dataset = high_sierra, 
               filepath_highallyears = "./data/01_primary/temperate/CA_sierra/derivative/low_to_high/Sierra_high_all_years.csv",
               filepath_lowallyears = "./data/01_primary/temperate/CA_sierra/derivative/low_to_high/Sierra_low_all_years.csv",
               filepath_allyearstall = "./data/01_primary/temperate/CA_sierra/derivative/low_to_high/Sierra_tall.csv",
               filepath_allyearswide = "./data/01_primary/temperate/CA_sierra/derivative/low_to_high/Sierra_wide.csv",
               filepath_avgyearstall = "./data/01_primary/temperate/CA_sierra/derivative/low_to_high/Sierra_avgyears_tall.csv",
               filepath_avgyearswide = "./data/01_primary/temperate/CA_sierra/derivative/low_to_high/Sierra_avgyears_wide.csv"
)

## Mid to high curate data ################
mid_sierra_select <- mid_sierra %>%
  select(year, julian, surface)

source("./scripts/data_processing/data_curation_program.R")
prep_flux_data(low_dataset = mid_sierra_select, 
               high_dataset = high_sierra, 
               filepath_highallyears = "./data/01_primary/temperate/CA_sierra/derivative/mid_to_high/Sierra_high_all_years.csv",
               filepath_lowallyears = "./data/01_primary/temperate/CA_sierra/derivative/mid_to_high/Sierra_low_all_years.csv",
               filepath_allyearstall = "./data/01_primary/temperate/CA_sierra/derivative/mid_to_high/Sierra_tall.csv",
               filepath_allyearswide = "./data/01_primary/temperate/CA_sierra/derivative/mid_to_high/Sierra_wide.csv",
               filepath_avgyearstall = "./data/01_primary/temperate/CA_sierra/derivative/mid_to_high/Sierra_avgyears_tall.csv",
               filepath_avgyearswide = "./data/01_primary/temperate/CA_sierra/derivative/mid_to_high/Sierra_avgyears_wide.csv"
)
