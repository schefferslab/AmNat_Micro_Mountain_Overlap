# David Klinges
# File creation date: 2019.02.03
# This script curates CZO Santa Catelina sites to prep for analysis and 
#   generating figures

## Workspace Prep #############

library(tidyverse)
library(lubridate)

## Import data

# Low elevation, soil temp
files <- list.files(path = paste0(getwd(),"/data/01_primary/temperate/AZ_santa_catelina/B2_Desert_low/"),
                    pattern = "B2D_GR_ST*", full.names = TRUE, recursive = FALSE)
for (i in 1:length(files)) {
  
  # Load file
  data <- read_csv(files[i], col_names = TRUE, skip = 3)
  
  if (i == 1) {
    B2_desertgranite_soil_raw <- data
  } else {
    B2_desertgranite_soil_raw <- B2_desertgranite_soil_raw %>%
      bind_rows(data)
  }
}

# Low elevation, surface temp
B2_desertgranite_surface_raw <- read_csv("./data/01_primary/temperate/AZ_santa_catelina/B2_Desert_low/HOBO_B2_data_airtemp.csv",
                                 col_names = TRUE, skip = 1)

# High elevation, soil temp
files <- list.files(path = paste0(getwd(),"/data/01_primary/temperate/AZ_santa_catelina/Bigelow_high"),
                    pattern = "BGZOB1_soilMTP*", full.names = TRUE, recursive = FALSE)
for (i in 1:length(files)) {
  
  # Load file
  data <- read_csv(files[i], col_names = TRUE, skip = 1)
  if (i == 1) {
    Big_soil_raw <- data
  } else {
    Big_soil_raw <- Big_soil_raw %>%
      bind_rows(data)
  }
}

# High elevation, surface temp
files <- list.files(path = paste0(getwd(),"/data/01_primary/temperate/AZ_santa_catelina/Marshall_Gulch_high/"),
                    pattern = "Granite_MetData*", full.names = TRUE, recursive = FALSE)
for (i in 1:length(files)) {
  
  # Load file
  data <- read_csv(files[i], col_names = TRUE)
  # Remove 2nd row but not first
  data <- data %>%
    filter(row_number() != 1)
  
  if (i == 1) {
    MG_granite_surface_raw <- data
  } else {
    MG_granite_surface_raw <- MG_granite_surface_raw %>%
      bind_rows(data)
  }
}

## Prep Catalina low elevation data ################

## surface temp
B2_desertgranite_surface <- B2_desertgranite_surface_raw %>%
  dplyr::rename(datetime = MST, surface = degC)

B2_desertgranite_surface <- B2_desertgranite_surface %>%
  separate(datetime, into = c("date", "time"), sep = " ")

# Parse dates and times
B2_desertgranite_surface$date <- ymd(B2_desertgranite_surface$date)
B2_desertgranite_surface$time <- hm(B2_desertgranite_surface$time)

B2_desertgranite_surface <- B2_desertgranite_surface %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(julian = yday(date)) %>%
  mutate(surface = as.numeric(replace(surface, surface == -9999, NA))) # Replace -9999 with NA

## Soil temp
# For now, just choosing site B2_GCSM2 at depth 15cm because appears pretty complete
B2_desertgranite_soil <- B2_desertgranite_soil_raw %>%
  dplyr::rename(datetime = MST, soil = degC_2) %>%
  dplyr::select(datetime, soil)

B2_desertgranite_soil <- B2_desertgranite_soil %>%
  separate(datetime, into = c("date", "time"), sep = " ")

# Parse dates and times
B2_desertgranite_soil$date <- mdy(B2_desertgranite_soil$date)
B2_desertgranite_soil$time <- hm(B2_desertgranite_soil$time)

B2_desertgranite_soil <- B2_desertgranite_soil %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(julian = yday(date)) %>%
  mutate(soil = as.numeric(replace(soil, soil == -9999, NA))) # Replace -9999 with NA

# Join surface and soil
Catalina_low <- B2_desertgranite_soil %>%
  left_join(B2_desertgranite_surface) %>%
  dplyr::select(year, julian, surface, soil)

## Prep Catalina high elevation data ############

## surface temp
MG_granite_surface <- MG_granite_surface_raw %>%
  dplyr::rename(datetime = DateTime, surface = AirTemperature) %>%
  dplyr::select(datetime, surface)

MG_granite_surface <- MG_granite_surface %>%
  separate(datetime, into = c("date", "time"), sep = " ")

# Parse dates and times
MG_granite_surface$date <- mdy(MG_granite_surface$date)
MG_granite_surface$time <- hm(MG_granite_surface$time)

MG_granite_surface <- MG_granite_surface %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(julian = yday(date)) %>%
  mutate(surface = as.numeric(replace(surface, surface == -9999, NA))) # Replace -9999 with NA

## Soil temp

Big_soil <- Big_soil_raw %>%
  dplyr::rename(datetime = MST, soil = degC) %>%
  dplyr::select(datetime, soil)

Big_soil <- Big_soil %>%
  separate(datetime, into = c("date", "time"), sep = " ")

# Parse dates and times
Big_soil$date <- mdy(Big_soil$date)
Big_soil$time <- hm(Big_soil$time)

Big_soil <- Big_soil %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(julian = yday(date)) %>%
  mutate(soil = as.numeric(replace(soil, soil == -9999, NA))) # Replace -9999 with NA

# Join surface and soil
Catalina_high <- MG_granite_surface %>%
  left_join(Big_soil) %>%
  dplyr::select(year, julian, surface, soil)

## Run curation program ############
source("./scripts/data_processing/data_curation_program.R")
prep_flux_data(low_dataset = Catalina_low, 
               high_dataset = Catalina_high, 
               filepath_highallyears = "./data/01_primary/temperate/AZ_santa_catelina/derivative/Catalina_high_granite_all_years.csv", 
               filepath_lowallyears = "./data/01_primary/temperate/AZ_santa_catelina/derivative/Catalina_low_all_years.csv",
               filepath_allyearstall = "./data/01_primary/temperate/AZ_santa_catelina/derivative/Catalina_tall.csv", 
               filepath_allyearswide = "./data/01_primary/temperate/AZ_santa_catelina/derivative/Catalina_wide.csv", 
               filepath_avgyearstall = "./data/01_primary/temperate/AZ_santa_catelina/derivative/Catalina_avgyears_tall.csv", 
               filepath_avgyearswide = "./data/01_primary/temperate/AZ_santa_catelina/derivative/Catalina_avgyears_wide.csv"
)
