# David Klinges
# File creation date: 2019.03.13
# This script curates Berkeley Angelo Reserve forest sites to prep
#   for analysis and generating figures


## 1. Workspace prep ###############

library(tidyverse)
library(readxl)
library(lubridate)

# Read in data

# Names will correspond to site acronym and elevation flag (high or low elev)
treebeard_low_raw <- read_csv("./data/01_primary/temperate/CA_angelo/low_elevation/treebeard/256080_Matlab.csv")
wscp_high_raw <- read_csv("data/01_primary/temperate/CA_angelo/high_elevation/188132_Matlab.csv")

## 2. Prep data #########
## ....2a. Prep low elevation data #############
treebeard_low <- treebeard_low_raw %>%
  dplyr::select(Timestamp, "Air Temp WSTB [Tree Beard WS]") %>%
  dplyr::rename(date_time = "Timestamp", surface = "Air Temp WSTB [Tree Beard WS]") %>%
  mutate(surface = as.numeric(replace(surface, surface == "null", NA)))

# Parse date time
treebeard_low <- treebeard_low %>%
  separate(date_time, into = c("date", "time"), sep = " ") %>%
  # Need to add zero's to single-digit months and days
  separate(date, into = c("year", "month", "day"), sep = "-") %>%
  mutate(month = as.numeric(month), day = as.numeric(day)) %>%
  mutate(month = ifelse(month < 10, paste0("0", as.character(month)), 
                        as.character(month))) %>%
  mutate(day = ifelse(day < 10, paste0("0", as.character(day)), 
                      as.character(day))) %>%
  mutate(date = paste(day, month, year, sep = "-"))

treebeard_low$date <- as.Date(treebeard_low$date, format = "%d-%m-%Y")

treebeard_low <- treebeard_low %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(julian = yday(date)) %>%
  select(year, julian, surface)

# Change day of year 366 to day of year 1 of next year
treebeard_low <- treebeard_low %>%
  mutate(year = ifelse(julian == 366, (year + 1), year)) %>%
  mutate(julian = ifelse(julian == 366, 1, julian))


## ....2b. Prep high elevation data #############
wscp_high <- wscp_high_raw %>%
  dplyr::select(Timestamp, `Air Temp WSCP [Cahto Peak WS]`) %>%
  dplyr::rename(date_time = Timestamp, surface = `Air Temp WSCP [Cahto Peak WS]`) %>%
  mutate(surface = as.numeric(replace(surface, surface == "null", NA)))

# Parse date time
wscp_high <- wscp_high %>%
  separate(date_time, into = c("date", "time"), sep = " ") %>%
  # Need to add zero's to single-digit months and days
  separate(date, into = c("month", "day", "year"), sep = "/") %>%
  mutate(month = as.numeric(month), day = as.numeric(day)) %>%
  mutate(month = ifelse(month < 10, paste0("0", as.character(month)), 
                        as.character(month))) %>%
  mutate(day = ifelse(day < 10, paste0("0", as.character(day)), 
                      as.character(day))) %>%
  mutate(date = paste(day, month, year, sep = "-"))

wscp_high$date <- as.Date(wscp_high$date, format = "%d-%m-%y")

wscp_high <- wscp_high %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(julian = yday(date)) %>%
  select(year, julian, surface)

# Change day of year 366 to day of year 1 of next year
wscp_high <- wscp_high %>%
  mutate(year = ifelse(julian == 366, (year + 1), year)) %>%
  mutate(julian = ifelse(julian == 366, 1, julian))

## ....2c. Split into leaf-off and leaf-on #############

# Low elevation
treebeard_low_leafoff <- treebeard_low %>%
  filter(julian < 151 | julian > 280)

treebeard_low_leafon <- treebeard_low %>%
  filter(julian > 151 & julian < 280)

# High elevation
wscp_high_leafoff <- wscp_high %>%
  filter(julian < 151 | julian > 280)

wscp_high_leafon <- wscp_high %>%
  filter(julian > 151 & julian < 280)
  
## 3. Curate data ########

## ....3a. Leaf-off #########
source("./scripts/data_processing/data_curation_program.R")

angelo_off_list <- prep_flux_data(low_dataset = treebeard_low_leafoff,
                 high_dataset = wscp_high_leafoff)


write_csv(angelo_off_list[[1]], "./data/01_primary/temperate/CA_angelo/derivative/Angelo_wscp_high_leafoff_all_years.csv")
write_csv(angelo_off_list[[2]], "./data/01_primary/temperate/CA_angelo/derivative/Angelo_treebead_low_leafoff_all_years.csv")
write_csv(angelo_off_list[[3]], "./data/01_primary/temperate/CA_angelo/derivative/Angelo_TB-wscp_leafoff_tall.csv")
write_csv(angelo_off_list[[4]], "./data/01_primary/temperate/CA_angelo/derivative/Angelo_TB-wscp_leafoff_wide.csv")
write_csv(angelo_off_list[[5]], "./data/01_primary/temperate/CA_angelo/derivative/Angelo_TB-wscp_leafoff_avgyears_tall.csv")
write_csv(angelo_off_list[[6]], "./data/01_primary/temperate/CA_angelo/derivative/Angelo__TB-wscp_leafoff_avgyears_wide.csv")
                 
angelo_on_list <- prep_flux_data(low_dataset = treebeard_low_leafon,
                                  high_dataset = wscp_high_leafon)


write_csv(angelo_on_list[[1]], "./data/01_primary/temperate/CA_angelo/derivative/Angelo_wscp_high_leafon_all_years.csv")
write_csv(angelo_on_list[[2]], "./data/01_primary/temperate/CA_angelo/derivative/Angelo_treebeard_low_leafon_all_years.csv")
write_csv(angelo_on_list[[3]], "./data/01_primary/temperate/CA_angelo/derivative/Angelo_TB-wscp_leafon_tall.csv")
write_csv(angelo_on_list[[4]], "./data/01_primary/temperate/CA_angelo/derivative/Angelo_TB-wscp_leafon_wide.csv")
write_csv(angelo_on_list[[5]], "./data/01_primary/temperate/CA_angelo/derivative/Angelo_TB-wscp_leafon_avgyears_tall.csv")
write_csv(angelo_on_list[[6]], "./data/01_primary/temperate/CA_angelo/derivative/Angelo_TB-wscp_leafon_avgyears_wide.csv")


