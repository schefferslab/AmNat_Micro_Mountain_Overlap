# David Klinges
# 2019/03/31
# Curate tropical
# This script is modified from Trop_main.R

## 1. Workspace prep ############

library(tidyverse)
library(lubridate)

Mada_raw <- read_csv("data/01_primary/tropical/Madagascar/Madagascar.csv")
Phili_raw <- read_csv("data/01_primary/tropical/Philippines//Philippines.csv")
Aust_can <- read_csv("data/01_primary/tropical/Australia/Australia_canopy.csv")
Aust_soil <- read_csv("data/01_primary/tropical/Australia/Australia_soil.csv")

# Join Aust datasets
#prep Aust data
Aust_raw <- Aust_can %>% 
  dplyr::select("site_code", "date", "airrange", "site", "elevation", "micro", "min", "max") %>% 
  bind_rows(Aust_soil)

Aust_raw <- Aust_raw %>%
  dplyr::rename(elev = elevation) %>%
  dplyr::select(-site) %>%
  dplyr::rename(site = site_code)

## 2. Curate tropical data #############
## ....2a. Madagascar data ##############
Mada <- Mada_raw %>%
  mutate(year = as.numeric(year) + 2000)

## ...... Subset by elevation, microhabitat ##########

# Create function that subsets to a given elevation and microhabitat)
per_elev_micro <- function(df, elevation, microhabitat) {
  out <- df %>%
    filter(elev == elevation) %>%
    filter(micro == microhabitat) %>%
    mutate(temp = Value) %>%
    dplyr::select(site, buttonID, year, julian, hour, minute, temp)
  return(out)
}

soil_low <- per_elev_micro(Mada, 550, "soil")
surface_low <- per_elev_micro(Mada, 550, "ground")
canopy_low <- per_elev_micro(Mada, 550, "canopy")
soil_high <- per_elev_micro(Mada, 1150, "soil")
surface_high <- per_elev_micro(Mada, 1150, "ground")
canopy_high <- per_elev_micro(Mada, 1150, "canopy")

## ...... Get daily min/mean/max ###############

dailymean_trop <- function(df) {
  dailymean <- df %>%
    filter(is.na(temp) == FALSE) %>%
    group_by(year, julian) %>%
    summarize(mean = mean(temp))
  return(dailymean)
}

dailymin_trop <- function(df) {
  dailymin <- df %>%
    filter(is.na(temp) == FALSE) %>%
    group_by(year, julian) %>%
    summarize(min = min(temp))
  return(dailymin)
}

dailymax_trop <- function(df) {
  dailymax <- df %>%
    filter(is.na(temp) == FALSE) %>%
    group_by(year, julian) %>%
    summarize(max = max(temp))
  return(dailymax)
}

# Soil
low_soil_min <- dailymin_trop(soil_low) %>%
  dplyr::rename(low_soil_min = min)

low_soil_mean <- dailymean_trop(soil_low) %>%
  dplyr::rename(low_soil_mean = mean)

low_soil_max <- dailymax_trop(soil_low) %>%
  dplyr::rename(low_soil_max = max)

high_soil_min <- dailymin_trop(soil_high) %>%
  dplyr::rename(high_soil_min = min)

high_soil_mean <- dailymean_trop(soil_high) %>%
  dplyr::rename(high_soil_mean = mean)

high_soil_max <- dailymax_trop(soil_high) %>%
  dplyr::rename(high_soil_max = max)

# surface
low_surface_min <- dailymin_trop(surface_low) %>%
  dplyr::rename(low_surface_min = min)

low_surface_mean <- dailymean_trop(surface_low) %>%
  dplyr::rename(low_surface_mean = mean)

low_surface_max <- dailymax_trop(surface_low) %>%
  dplyr::rename(low_surface_max = max)

high_surface_min <- dailymin_trop(surface_high) %>%
  dplyr::rename(high_surface_min = min)

high_surface_mean <- dailymean_trop(surface_high) %>%
  dplyr::rename(high_surface_mean = mean)

high_surface_max <- dailymax_trop(surface_high) %>%
  dplyr::rename(high_surface_max = max)

# Canopy
low_canopy_min <- dailymin_trop(canopy_low) %>%
  dplyr::rename(low_canopy_min = min)

low_canopy_mean <- dailymean_trop(canopy_low) %>%
  dplyr::rename(low_canopy_mean = mean)

low_canopy_max <- dailymax_trop(canopy_low) %>%
  dplyr::rename(low_canopy_max = max)

high_canopy_min <- dailymin_trop(canopy_high) %>%
  dplyr::rename(high_canopy_min = min)

high_canopy_mean <- dailymean_trop(canopy_high) %>%
  dplyr::rename(high_canopy_mean = mean)

high_canopy_max <- dailymax_trop(canopy_high) %>%
  dplyr::rename(high_canopy_max = max)

## ...... Join Mada data ############
Mada_wide <- low_soil_min %>%
  left_join(low_soil_mean, by = c("year", "julian")) %>%
  left_join(low_soil_max, by = c("year", "julian")) %>%
  left_join(high_soil_min, by = c("year", "julian")) %>%
  left_join(high_soil_mean, by = c("year", "julian")) %>%
  left_join(high_soil_max, by = c("year", "julian")) %>%
  left_join(low_surface_min, by = c("year", "julian")) %>%
  left_join(low_surface_mean, by = c("year", "julian")) %>%
  left_join(low_surface_max, by = c("year", "julian")) %>%
  left_join(high_surface_min, by = c("year", "julian")) %>%
  left_join(high_surface_mean, by = c("year", "julian")) %>%
  left_join(high_surface_max, by = c("year", "julian")) %>%
  left_join(low_canopy_min, by = c("year", "julian")) %>%
  left_join(low_canopy_mean, by = c("year", "julian")) %>%
  left_join(low_canopy_max, by = c("year", "julian")) %>%
  left_join(high_canopy_min, by = c("year", "julian")) %>%
  left_join(high_canopy_mean, by = c("year", "julian")) %>%
  left_join(high_canopy_max, by = c("year", "julian"))

## ....2b. Philippines data ##############

# Change uppercase to lowercase 
Phili <- Phili_raw
Phili$micro <- gsub(pattern = '([[:upper:]])', perl = TRUE, replacement = '\\L\\1', Phili$micro)

## ...... Subset by elevation, microhabitat ##########
per_elev_micro <- function(df, elevation, microhabitat) {
  out <- df %>%
    filter(elev == elevation) %>%
    filter(micro == microhabitat) %>%
    mutate(temp = Temp) %>%
    dplyr::select(site, year, julian, hour, minute, temp)
  return(out)
}

# No soil data in the Philippines
surface_low <- per_elev_micro(Phili, 900, "ground")
canopy_low <- per_elev_micro(Phili, 900, "canopy")
surface_high <- per_elev_micro(Phili, 1700, "ground")
canopy_high <- per_elev_micro(Phili, 1700, "canopy")

## ...... Get daily min/mean/max ###############

# Remove buttonID column for dailysubset functions

dailymean_trop_Phili <- function(df) {
  dailymean <- df %>%
    filter(is.na(temp) == FALSE) %>%
    group_by(year, julian) %>%
    summarize(mean = mean(temp))
  return(dailymean)
}

dailymin_trop_Phili <- function(df) {
  dailymin <- df %>%
    filter(is.na(temp) == FALSE) %>%
    group_by(year, julian) %>%
    summarize(min = min(temp))
  return(dailymin)
}

dailymax_trop_Phili <- function(df) {
  dailymax <- df %>%
    filter(is.na(temp) == FALSE) %>%
    group_by(year, julian) %>%
    summarize(max = max(temp))
  return(dailymax)
}

# surface
low_surface_min <- dailymin_trop_Phili(surface_low) %>%
  dplyr::rename(low_surface_min = min)

low_surface_mean <- dailymean_trop_Phili(surface_low) %>%
  dplyr::rename(low_surface_mean = mean)

low_surface_max <- dailymax_trop_Phili(surface_low) %>%
  dplyr::rename(low_surface_max = max)

high_surface_min <- dailymin_trop_Phili(surface_high) %>%
  dplyr::rename(high_surface_min = min)

high_surface_mean <- dailymean_trop_Phili(surface_high) %>%
  dplyr::rename(high_surface_mean = mean)

high_surface_max <- dailymax_trop_Phili(surface_high) %>%
  dplyr::rename(high_surface_max = max)

# Canopy
low_canopy_min <- dailymin_trop_Phili(canopy_low) %>%
  dplyr::rename(low_canopy_min = min)

low_canopy_mean <- dailymean_trop_Phili(canopy_low) %>%
  dplyr::rename(low_canopy_mean = mean)

low_canopy_max <- dailymax_trop_Phili(canopy_low) %>%
  dplyr::rename(low_canopy_max = max)

high_canopy_min <- dailymin_trop_Phili(canopy_high) %>%
  dplyr::rename(high_canopy_min = min)

high_canopy_mean <- dailymean_trop_Phili(canopy_high) %>%
  dplyr::rename(high_canopy_mean = mean)

high_canopy_max <- dailymax_trop_Phili(canopy_high) %>%
  dplyr::rename(high_canopy_max = max)

## ...... Join Phili data ############
Phili_wide <- low_surface_min %>%
  left_join(low_surface_mean, by = c("year", "julian")) %>%
  left_join(low_surface_max, by = c("year", "julian")) %>%  
  left_join(high_surface_min, by = c("year", "julian")) %>%
  left_join(high_surface_mean, by = c("year", "julian")) %>%
  left_join(high_surface_max, by = c("year", "julian")) %>%
  left_join(low_canopy_min, by = c("year", "julian")) %>%
  left_join(low_canopy_mean, by = c("year", "julian")) %>%
  left_join(low_canopy_max, by = c("year", "julian")) %>%  
  left_join(high_canopy_min, by = c("year", "julian")) %>%
  left_join(high_canopy_mean, by = c("year", "julian")) %>%
  left_join(high_canopy_max, by = c("year", "julian"))

## ....2c. Australia data ##############

# Curation of date data 
Aust <- Aust_raw %>%
  separate(date, into = c("month", "day", "year"), sep = "/") %>%
  mutate(year = as.numeric(year) + 2000) %>%
  mutate(date = as.Date(ymd(paste(year, month, day, sep = "-")))) %>%
  mutate(julian = yday(date)) %>%
  dplyr::select(site, elev, micro, year, julian, min, max)

# Sometimes the `min` is actually higher than the `max`....
#   So I'm just going to gather the two into one column for temp
Aust <- Aust %>%
  gather(key = "minmax", value = "temp", min, max)

## ...... Subset by elevation, microhabitat ##########

per_elev_micro <- function(df, elevation, microhabitat) {
  out <- df %>%
    filter(elev == elevation) %>%
    filter(micro == microhabitat)
  return(out)
}

soil_low <- per_elev_micro(Aust, 1, "soil")
surface_low <- per_elev_micro(Aust, 1, "ground")
canopy_low <- per_elev_micro(Aust, 1, "canopy")
soil_high <- per_elev_micro(Aust, 10, "soil")
surface_high <- per_elev_micro(Aust, 10, "ground")
canopy_high <- per_elev_micro(Aust, 10, "canopy")

## ...... Get daily min/mean/max (see note) ###############
# Australia data is already grouped by day, and just min and max readings
# But still need to group_by for year and for site

dailymin_trop_ozzy <- function(df) {
  dailymin_trop <- df %>%
    filter(is.na(temp) == FALSE) %>%
    group_by(year, julian) %>%
    summarize(min = min(temp))
  return(dailymin_trop)
}

dailymax_trop_ozzy <- function(df) {
  dailymax_trop <- df %>%
    filter(is.na(temp) == FALSE) %>%
    group_by(year, julian) %>%
    summarize(max = max(temp))
  return(dailymax_trop)
}


# Soil
low_soil_min <- dailymin_trop_ozzy(soil_low) %>%
  dplyr::rename(low_soil_min = min)

low_soil_max <- dailymax_trop_ozzy(soil_low) %>%
  dplyr::rename(low_soil_max = max)

high_soil_min <- dailymin_trop_ozzy(soil_high) %>%
  dplyr::rename(high_soil_min = min)

high_soil_max <- dailymax_trop_ozzy(soil_high) %>%
  dplyr::rename(high_soil_max = max)

# surface
low_surface_min <- dailymin_trop_ozzy(surface_low) %>%
  dplyr::rename(low_surface_min = min)

low_surface_max <- dailymax_trop_ozzy(surface_low) %>%
  dplyr::rename(low_surface_max = max)

high_surface_min <- dailymin_trop_ozzy(surface_high) %>%
  dplyr::rename(high_surface_min = min)

high_surface_max <- dailymax_trop_ozzy(surface_high) %>%
  dplyr::rename(high_surface_max = max)

# Canopy
low_canopy_min <- dailymin_trop_ozzy(canopy_low) %>%
  dplyr::rename(low_canopy_min = min)

low_canopy_max <- dailymax_trop_ozzy(canopy_low) %>%
  dplyr::rename(low_canopy_max = max)

high_canopy_min <- dailymin_trop_ozzy(canopy_high) %>%
  dplyr::rename(high_canopy_min = min)

high_canopy_max <- dailymax_trop_ozzy(canopy_high) %>%
  dplyr::rename(high_canopy_max = max)

## ...... Join Aust data ############

Aust_wide <- low_soil_min %>%
  full_join(low_soil_max, by = c("year", "julian")) %>%  
  full_join(high_soil_min, by = c("year", "julian")) %>%
  full_join(high_soil_max, by = c("year", "julian")) %>%
  full_join(low_surface_min, by = c("year", "julian")) %>%
  full_join(low_surface_max, by = c("year", "julian")) %>%  
  full_join(high_surface_min, by = c("year", "julian")) %>%
  full_join(high_surface_max, by = c("year", "julian")) %>%
  full_join(low_canopy_min, by = c("year", "julian")) %>%
  full_join(low_canopy_max, by = c("year", "julian")) %>%  
  full_join(high_canopy_min, by = c("year", "julian")) %>%
  full_join(high_canopy_max, by = c("year", "julian"))

Aust_wide_soil <- low_soil_min %>%
  full_join(low_soil_max, by = c("year", "julian")) %>%  
  full_join(high_soil_min, by = c("year", "julian")) %>%
  full_join(high_soil_max, by = c("year", "julian"))

Aust_wide_surface <- low_surface_min %>%
  full_join(low_surface_max, by = c("year", "julian")) %>%  
  full_join(high_surface_min, by = c("year", "julian")) %>%
  full_join(high_surface_max, by = c("year", "julian")) 

Aust_wide_canopy <- low_canopy_min %>%
  full_join(low_canopy_max, by = c("year", "julian")) %>%  
  full_join(high_canopy_min, by = c("year", "julian")) %>%
  full_join(high_canopy_max, by = c("year", "julian"))

## Exclude outliers ##########
# There's clearly a few outlier days in the Mada data, which are reflected
#   in Brett's notes in his code
Mada_wide <- Mada_wide %>%
  filter_at(vars(contains("min")), all_vars(. < 40)) %>%
  filter_at(vars(contains("mean")), all_vars(. < 40)) %>%
  filter_at(vars(contains("max")), all_vars(. < 40))

Mada_wide <- Mada_wide %>%
  filter_at(vars(contains("soil")), all_vars(. < 24))

# Slightly less clear, but appears to be outliers in Australia soil as well
Aust_wide <- Aust_wide %>%
  filter(low_soil_max < 30 | is.na(low_soil_max)) %>%
  filter(high_soil_max < 25 | is.na(high_soil_max))

# Australia can temps were collected a few years after soil/surface, so let's just
# average for each julian day
Aust_wide <- Aust_wide %>%
  ungroup() %>% 
  dplyr::select(-year) %>% 
  group_by(julian) %>% 
  summarize_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(year = 1) %>% 
  mutate_all(~ replace(., is.nan(.), NA))


## Add data flags #############

Mada_wide <- Mada_wide %>%
  mutate(site = "MD") %>% 
  mutate(macro = "tropical broadleaf") %>% 
  mutate(foliage = "leaf-on") %>% 
  mutate(foliage_cover = 1) %>% 
  mutate(flora = "tropical broadleaf") %>% 
  mutate(latitude = -21) %>% 
  mutate(snow = "no snow")

Phili_wide <- Phili_wide %>%
  mutate(site = "PH") %>% 
  mutate(macro = "tropical broadleaf") %>% 
  mutate(foliage = "leaf-on") %>% 
  mutate(foliage_cover = 1) %>% 
  mutate(flora = "tropical broadleaf") %>% 
  mutate(latitude = 14) %>% 
  mutate(snow = "no snow")

Aust_wide <- Aust_wide %>%
  mutate(site = "AU") %>% 
  mutate(macro = "tropical broadleaf") %>%
  mutate(foliage = "leaf-on") %>%
  mutate(foliage_cover = 1) %>% 
  mutate(flora = "tropical broadleaf") %>% 
  mutate(latitude = -15) %>% 
  mutate(snow = "no snow")

## Gather data for derivative products -------
## ....Mada #######

lowmin <- Mada_wide %>%
  gather(key = "micro", value = "min", "low_soil_min", "low_surface_min", "low_canopy_min") %>%
  mutate(micro = dplyr::recode(micro, low_soil_min = "soil", low_surface_min = "surface", low_canopy_min = "canopy")) %>%
  mutate(elevation = "low") %>%
  dplyr::select(year, julian, min, micro, elevation)

lowmean <- Mada_wide %>%
  gather(key = "micro", value = "mean", "low_soil_mean", "low_surface_mean", "low_canopy_mean") %>%
  mutate(micro = dplyr::recode(micro, low_soil_mean = "soil", low_surface_mean = "surface", low_canopy_mean = "canopy")) %>%
  mutate(elevation = "low") %>%
  dplyr::select(year, julian, mean, micro, elevation)

lowmax <- Mada_wide %>%
  gather(key = "micro", value = "max", "low_soil_max", "low_surface_max", "low_canopy_max") %>%
  mutate(micro = dplyr::recode(micro, low_soil_max = "soil", low_surface_max = "surface", low_canopy_max = "canopy")) %>%
  mutate(elevation = "low") %>%
  dplyr::select(year, julian, max, micro, elevation)

highmin <- Mada_wide %>%
  gather(key = "micro", value = "min", "high_soil_min", "high_surface_min", "high_canopy_min") %>%
  mutate(micro = dplyr::recode(micro, high_soil_min = "soil", high_surface_min = "surface", high_canopy_min = "canopy")) %>%
  mutate(elevation = "high") %>%
  dplyr::select(year, julian, min, micro, elevation)

highmean <- Mada_wide %>%
  gather(key = "micro", value = "mean", "high_soil_mean", "high_surface_mean", "high_canopy_mean") %>%
  mutate(micro = dplyr::recode(micro, high_soil_mean = "soil", high_surface_mean = "surface", high_canopy_mean = "canopy")) %>%
  mutate(elevation = "high") %>%
  dplyr::select(year, julian, mean, micro, elevation)

highmax <- Mada_wide %>%
  gather(key = "micro", value = "max", "high_soil_max", "high_surface_max", "high_canopy_max") %>%
  mutate(micro = dplyr::recode(micro, high_soil_max = "soil", high_surface_max = "surface", high_canopy_max = "canopy")) %>%
  mutate(elevation = "high") %>%
  dplyr::select(year, julian, max, micro, elevation)

low <- lowmin %>%
  full_join(lowmean) %>%
  full_join(lowmax)

high <- highmin %>%
  full_join(highmean) %>%
  full_join(highmax)

mada <- low %>%
  bind_rows(high) %>%
  dplyr::select(year, julian, micro, elevation, min, mean, max) %>% 
  mutate(site = "MD") %>% 
  mutate(macro = "tropical broadleaf") %>% 
  mutate(foliage = "leaf-on") %>% 
  mutate(foliage_cover = 1) %>% 
  mutate(flora = "tropical broadleaf") %>% 
  mutate(height = dplyr::recode(micro, "soil" = -0.07, "surface" = 1, 
                                # Brett: low elev is 20m, high elev is 18 m...
                                # so I'm averaging
                                "canopy" = 19)) %>% 
  mutate(height_notes = "soil and surface measured, canopy estimated") %>%
  mutate(latitude = -21) %>% 
  mutate(altitude = dplyr::recode(elevation, "low" = 550, "high" = 1500)) %>% 
  mutate(snowdepth = 0)

## ....Gather Aust #######

# No mean data for Australia

lowmin <- Aust_wide %>%
  gather(key = "micro", value = "min", "low_soil_min", "low_surface_min", "low_canopy_min") %>%
  mutate(micro = dplyr::recode(micro, low_soil_min = "soil", low_surface_min = "surface", low_canopy_min = "canopy")) %>%
  mutate(elevation = "low") %>%
  dplyr::select(year, julian, min, micro, elevation)

lowmax <- Aust_wide %>%
  gather(key = "micro", value = "max", "low_soil_max", "low_surface_max", "low_canopy_max") %>%
  mutate(micro = dplyr::recode(micro, low_soil_max = "soil", low_surface_max = "surface", low_canopy_max = "canopy")) %>%
  mutate(elevation = "low") %>%
  dplyr::select(year, julian, max, micro, elevation)

highmin <- Aust_wide %>%
  gather(key = "micro", value = "min", "high_soil_min", "high_surface_min", "high_canopy_min") %>%
  mutate(micro = dplyr::recode(micro, high_soil_min = "soil", high_surface_min = "surface", high_canopy_min = "canopy")) %>%
  mutate(elevation = "high") %>%
  dplyr::select(year, julian, min, micro, elevation)

highmax <- Aust_wide %>%
  gather(key = "micro", value = "max", "high_soil_max", "high_surface_max", "high_canopy_max") %>%
  mutate(micro = dplyr::recode(micro, high_soil_max = "soil", high_surface_max = "surface", high_canopy_max = "canopy")) %>%
  mutate(elevation = "high") %>%
  dplyr::select(year, julian, max, micro, elevation)

low <- lowmin %>%
  full_join(lowmax)

high <- highmin %>%
  full_join(highmax)

aust <- low %>%
  bind_rows(high) %>%
  dplyr::select(year, julian, micro, elevation, min, max) %>% 
  mutate(site = "AU") %>% 
  mutate(macro = "tropical broadleaf") %>%
  mutate(foliage = "leaf-on") %>% 
  mutate(foliage_cover = 1) %>% 
  mutate(flora = "tropical broadleaf") %>% 
  mutate(height = dplyr::recode(micro, "soil" = -.07, "surface" = 1, 
                                # Brett says heights varied between 18-20 m
                                "canopy" = 19)) %>%
  mutate(height_notes = "soil and surface measured, canopy estimated") %>%
  mutate(latitude = -11) %>% 
  mutate(altitude = dplyr::recode(elevation, "low" = 100, "high" = 1000)) %>% 
  mutate(snowdepth = 0)

## ....Gather Phili ######

# Remember no soil data 

lowmin <- Phili_wide %>%
  gather(key = "micro", value = "min", "low_surface_min", "low_canopy_min") %>%
  mutate(micro = dplyr::recode(micro, low_surface_min = "surface", low_canopy_min = "canopy")) %>%
  mutate(elevation = "low") %>%
  dplyr::select(year, julian, min, micro, elevation)

lowmean <- Phili_wide %>%
  gather(key = "micro", value = "mean", "low_surface_mean", "low_canopy_mean") %>%
  mutate(micro = dplyr::recode(micro, low_surface_mean = "surface", low_canopy_mean = "canopy")) %>%
  mutate(elevation = "low") %>%
  dplyr::select(year, julian, mean, micro, elevation)

lowmax <- Phili_wide %>%
  gather(key = "micro", value = "max", "low_surface_max", "low_canopy_max") %>%
  mutate(micro = dplyr::recode(micro, low_surface_max = "surface", low_canopy_max = "canopy")) %>%
  mutate(elevation = "low") %>%
  dplyr::select(year, julian, max, micro, elevation)

highmin <- Phili_wide %>%
  gather(key = "micro", value = "min", "high_surface_min", "high_canopy_min") %>%
  mutate(micro = dplyr::recode(micro, high_surface_min = "surface", high_canopy_min = "canopy")) %>%
  mutate(elevation = "high") %>%
  dplyr::select(year, julian, min, micro, elevation)

highmean <- Phili_wide %>%
  gather(key = "micro", value = "mean", "high_surface_mean", "high_canopy_mean") %>%
  mutate(micro = dplyr::recode(micro, high_surface_mean = "surface", high_canopy_mean = "canopy")) %>%
  mutate(elevation = "high") %>%
  dplyr::select(year, julian, mean, micro, elevation)

highmax <- Phili_wide %>%
  gather(key = "micro", value = "max", "high_surface_max", "high_canopy_max") %>%
  mutate(micro = dplyr::recode(micro, high_surface_max = "surface", high_canopy_max = "canopy")) %>%
  mutate(elevation = "high") %>%
  dplyr::select(year, julian, max, micro, elevation)

low <- lowmin %>%
  full_join(lowmean) %>%
  full_join(lowmax)

high <- highmin %>%
  full_join(highmean) %>%
  full_join(highmax)

phili <- low %>%
  bind_rows(high) %>%
  dplyr::select(year, julian, micro, elevation, min, mean, max) %>% 
  mutate(site = "PH") %>% 
  mutate(macro = "tropical broadleaf") %>% 
  mutate(foliage = "leaf-on") %>% 
  mutate(foliage_cover = 1) %>% 
  mutate(flora = "tropical broadleaf") %>% 
  mutate(height = dplyr::recode(micro, "soil" = -.07, "surface" = 1, 
                                # Brett says low elev 20m, and high elev 16.5 m...
                                # so averaging
                                "canopy" = 18.25)) %>% 
  mutate(height_notes = "soil and surface measured, canopy estimated") %>%
  mutate(latitude = 14) %>% 
  mutate(altitude = dplyr::recode(elevation, "low" = 900, "high" = 1700)) %>% 
  mutate(snowdepth = 0)

## Write data ###########

write_csv(Mada_wide, "data/01_primary/tropical/Madagascar/derivative/Mada_wide.csv")
write_csv(Phili_wide, "data/01_primary/tropical/Philippines/derivative/Phili_wide.csv")
write_csv(Aust_wide, "data/01_primary/tropical/Australia/derivative/Aust_wide.csv")
write_csv(Aust_wide_soil, "data/01_primary/tropical/Australia/derivative/Aust_wide_soil.csv")
write_csv(Aust_wide_surface, "data/01_primary/tropical/Australia/derivative/Aust_wide_surface.csv")
write_csv(Aust_wide_canopy, "data/01_primary/tropical/Australia/derivative/Aust_wide_canopy.csv")

write_csv(mada, "data/01_primary/tropical/Madagascar/derivative/Mada_tall.csv")
write_csv(aust, "data/01_primary/tropical/Australia/derivative/Aust_tall.csv")
write_csv(phili, "data/01_primary/tropical/Philippines/derivative/Phili_tall.csv")
