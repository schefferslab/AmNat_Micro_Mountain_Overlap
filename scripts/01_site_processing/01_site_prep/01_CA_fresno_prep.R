# David Klinges
# File creation date: 2019.08.19
# This script curates Fresno-Bishop California weather station (open-air) 
# data for analysis and generating figures

## 1. Workspace prep ##############

library(tidyverse)
library(lubridate)

fresno_low_raw <- read_csv("data/01_primary/temperate/CA_fresno/original/fresno_low.csv")

bishop_high_raw <- read_csv("data/01_primary/temperate/CA_fresno/original/bishop_high.csv")


## 2. Data curation #########

fresno_low <- fresno_low_raw %>%
  gather(key = "minmax", value = "surface", High_degrees_F, Low_degrees_F) %>% 
  # Change to celsius
  mutate(surface = (as.double(surface) - 32) * (5/9)) %>% 
  mutate(year = year(as_date(date))) %>% 
  mutate(julian = yday(as_date(date))) %>% 
  dplyr::select(year, julian, surface)

bishop_high <- bishop_high_raw %>%
  gather(key = "minmax", value = "surface", High_degrees_F, Low_degrees_F) %>% 
  # Change to celsius
  mutate(surface = (as.double(surface) - 32) * (5/9)) %>% 
  mutate(year = year(as_date(date))) %>% 
  mutate(julian = yday(as_date(date))) %>% 
  dplyr::select(year, julian, surface)

## 3. Wrangle snow data #########

source("scripts/00_source_code/noaa_query.R")

## ....A. High elevation snowdepth ##########

bishop_snow <- noaa_query( 
  stationid = "GHCND:USW00023157", # BISHOP AIRPORT CA US
  startdate = "2009-01-01", enddate = "2012-12-31",
  datatypeid = "SNWD")

# Of interesting note....very little snow in Fresno and Bishop, but in the 
# forest between them there's a reasonable amount of snow (appears to be 10-25 cm
# snowdepth for a good chunk of the winter)

# Curate NOAA query
bishop_snow <- bishop_snow %>% 
  mutate(date = as_date(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  rename(snowdepth = value) %>% 
  # Currently in mm, switch to cm
  mutate(snowdepth = snowdepth / 10) %>% 
  mutate(elevation = "high") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

bishop_snow_avg <- bishop_snow %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE)) %>% 
  mutate(elevation = "high")

## ....B. Low elevation snowdepth ##########

fresno_snow <- noaa_query( 
  stationid = "GHCND:USW00093193", # Station is FRESNO YOSEMITE INTERNATIONAL, CA US
  startdate = "2009-01-01", enddate = "2012-12-31",
  datatypeid = "SNWD")

# Curate NOAA query
fresno_snow <- fresno_snow %>% 
  mutate(date = as_date(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  rename(snowdepth = value) %>% 
  mutate(complete.cases(snowdepth)) %>% 
  # Currently in mm, switch to cm
  mutate(snowdepth = snowdepth / 10) %>% 
  mutate(elevation = "low") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

fresno_snow_avg <- fresno_snow %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE)) %>% 
  mutate(elevation = "low")

## ....C. Bind together high and low ##########

snowdepth <- fresno_snow %>%
  bind_rows(bishop_snow)

snowdepth_avg <- fresno_snow_avg  %>%
  bind_rows(bishop_snow_avg)

# # Average low and high site snowdepths
# 
# snowdepth <- fresno_snow %>% 
#   full_join(bishop_snow) %>% 
#   # Assuming an NA is 0, which is needed in order to have a fleshed-out timeseries
#   mutate(high_snowdepth = ifelse(is.na(high_snowdepth), 0, high_snowdepth)) %>% 
#   mutate(low_snowdepth = ifelse(is.na(low_snowdepth), 0, low_snowdepth)) %>% 
#   mutate(snowdepth = (low_snowdepth + high_snowdepth)/2)
# 
# snowdepth_avg <- fresno_snow_avg  %>% 
#   full_join(bishop_snow_avg ) %>% 
#   # Assuming an NA is 0, which is needed in order to have a fleshed-out timeseries
#   mutate(high_snowdepth = ifelse(is.na(high_snowdepth), 0, high_snowdepth)) %>% 
#   mutate(low_snowdepth = ifelse(is.na(low_snowdepth), 0, low_snowdepth)) %>% 
#   mutate(snowdepth = (low_snowdepth + high_snowdepth)/2)

## 4. Conduct overlap data curation #########

source("scripts/00_source_code/data_curation_program.R")
fresno_list <- prep_flux_data(low_dataset = fresno_low, 
                                          high_dataset = bishop_high)


for (i in 1:length(fresno_list)) {
  
  fresno_list[[i]] <- fresno_list[[i]] %>% 
    mutate(site = "CA") %>% 
    mutate(macro = "developed") %>% 
    mutate(foliage = "leaf-off") %>% 
    mutate(foliage_cover = 0) %>% 
    mutate(flora = "dry scrub") %>% 
    mutate(latitude = 35)
  
  if (i == 3 | i == 5) {
    
    fresno_list[[i]] <- fresno_list[[i]] %>% 
      mutate(height = dplyr::recode(micro, "surface" = 1.0)) %>%
      mutate(height_notes = "from metadata") %>% 
      mutate(altitude = dplyr::recode(elevation, "low" = 683.1, "high" = 1382))
  }
  
  # Add snow data
  if (i == 3) {
    # For all years
    fresno_list[[i]] <- fresno_list[[i]] %>% 
      left_join(snowdepth) %>% 
      # Evidence seems pretty clear: very little snow.
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "confident_estimate", 
                                       "measured_daily")) %>% 
      # Few observations show snow. 
      ## Looking at WRCC suggests the same
      # https://www1.ncdc.noaa.gov/pub/orders/IPS/IPS-68941593-A935-4D1F-8E08-892EF3BEE911.pdf
      # If that link is broken you can query again here
      # https://wrcc.dri.edu/Climate/west_lcd.php
      mutate(snowdepth = ifelse(is.na(snowdepth), 0, 
                                snowdepth))
  }
  if (i == 5) {
    # For avg years
    fresno_list[[i]] <- fresno_list[[i]] %>% 
      left_join(snowdepth_avg) %>% 
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "confident_estimate", 
                                       "measured_daily")) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth), 0, 
                                snowdepth))
    
  }
}

## 4. Write out data ##############

write_csv(fresno_list[[1]], "data/01_primary/temperate/CA_fresno/derivative/CA_fresno_high_all_years.csv")
write_csv(fresno_list[[2]],  "data/01_primary/temperate/CA_fresno/derivative/CA_fresno_low_all_years.csv")
write_csv(fresno_list[[3]], "data/01_primary/temperate/CA_fresno/derivative/CA_fresno_tall.csv")
write_csv(fresno_list[[4]],  "data/01_primary/temperate/CA_fresno/derivative/CA_fresno_wide.csv")
write_csv(fresno_list[[5]], "data/01_primary/temperate/CA_fresno/derivative/CA_fresno_avgyears_tall.csv")
write_csv(fresno_list[[6]], "data/01_primary/temperate/CA_fresno/derivative/CA_fresno_avgyears_wide.csv")

