# David Klinges
# File creation date: 2019.01.21
# This script curates Ameriflux Idaho sage brush sites to prep for analysis
#   and generating figures

## 1. Workspace Prep #############

library(tidyverse)
library(lubridate)

# Import data
low_sage_raw <- read_csv("./data/01_primary/temperate/ID_sage_brush/low/AMF_US-Rws_BASE_HH_2-5.csv",
                         col_types = cols(
                           TA = col_double(),
                           TS_PI_3 = col_double()
                         ))
mid_sage_raw <- read_csv("./data/01_primary/temperate/ID_sage_brush/mid/AMF_US-Rls_BASE_HH_2-5.csv",
                         col_types = cols(
                           TA = col_double(),
                           TS_PI_3 = col_double()
                         ))
high_sage_raw <- read_csv("./data/01_primary/temperate/ID_sage_brush/high/AMF_US-Rms_BASE_HH_2-5.csv",
                          col_types = cols(
                            TA = col_double(),
                            TS_PI_3 = col_double()
                          ))

# Import snowdepth data 
# From: 

## 2. Prep Idaho Sage Brush data ############
## ** low: US-Rws: Reynolds Creek Idaho big sagebrush #############

# Curate
low_sage <- low_sage_raw %>%
  # Choosing just TS_PI_3, which I believe is 10cm depth
  dplyr::select(TIMESTAMP_START, TA, TS_PI_3) %>%
  dplyr::rename(time = TIMESTAMP_START) %>%
  dplyr::rename(surface = TA) %>%
  mutate(surface = replace(surface, surface == -9999, NA)) %>%
  dplyr::rename(soil = TS_PI_3) %>%
  mutate(soil = replace(soil, soil == -9999, NA)) %>%
  mutate(time = time/1000000)

# Convert time strings to dates
low_sage <- low_sage %>%
  separate(time, into = c("year", "month"), sep = 4) %>%
  dplyr::select(-month) %>%
  mutate(year = as.numeric(year))

low_sage <- low_sage %>% 
  dplyr::group_by(year) %>%
  mutate(julian = row_number()) %>%
  mutate(julian = ceiling(julian/48)) %>%
  dplyr::ungroup(year)

# Remove day 366 so I don't need to worry about changing the year or anything
low_sage <- low_sage %>%
  filter(julian <= 365)
  
## ** mid: US-Rls/RCEW Low Sagebrush #############

# Curate
mid_sage <- mid_sage_raw %>%
  dplyr::select(TIMESTAMP_START, TA, TS_PI_3) %>%
  dplyr::rename(time = TIMESTAMP_START) %>%
  dplyr::rename(surface = TA) %>%
  mutate(surface = replace(surface, surface == -9999, NA)) %>%
  dplyr::rename(soil = TS_PI_3) %>%
  mutate(soil = replace(soil, soil == -9999, NA)) %>%
  mutate(time = time/1000000)

# Convert time strings to dates
mid_sage <- mid_sage %>%
  separate(time, into = c("year", "month"), sep = 4) %>%
  dplyr::select(-month) %>%
  mutate(year = as.numeric(year))

mid_sage <- mid_sage %>% 
  dplyr::group_by(year) %>%
  mutate(julian = row_number()) %>%
  mutate(julian = ceiling(julian/48)) %>%
  dplyr::ungroup(year)

# Remove day 366 so I don't need to worry about changing the year or anything
mid_sage <- mid_sage %>%
  filter(julian <= 365)

## ** high: US-Rms/RCEW Mountain Big Sagebrush #############

# Curate
high_sage <- high_sage_raw %>%
  dplyr::select(TIMESTAMP_START, TA, TS_PI_3) %>%
  dplyr::rename(time = TIMESTAMP_START) %>%
  dplyr::rename(surface = TA) %>%
  mutate(surface = replace(surface, surface == -9999, NA)) %>%
  dplyr::rename(soil = TS_PI_3) %>%
  mutate(soil = replace(soil, soil == -9999, NA)) %>%
  mutate(time = time/1000000)

# Convert time strings to dates
high_sage <- high_sage %>%
  separate(time, into = c("year", "month"), sep = 4) %>%
  dplyr::select(-month) %>%
  mutate(year = as.numeric(year))

high_sage <- high_sage %>% 
  dplyr::group_by(year) %>%
  mutate(julian = row_number()) %>%
  mutate(julian = ceiling(julian/48)) %>%
  dplyr::ungroup(year)

# Remove day 366 so I don't need to worry about changing the year or anything
high_sage <- high_sage %>%
  filter(julian <= 365)

## 3. Wrangle snow data #########

source("scripts/00_source_code/noaa_query.R")

## ....A. High elevation snowdepth ##########
idaho_high_snow <- noaa_query( 
  stationid = "GHCND:USS0016F08S", # Station is REYNOLDS CREEK
  startdate = "2014-01-01", enddate = "2017-12-31",
  datatypeid = "SNWD")

# Curate NOAA query
idaho_high_snow <- idaho_high_snow %>% 
  mutate(date = as_date(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  dplyr::rename(snowdepth = value) %>% 
  # Currently in mm, switch to cm
  mutate(snowdepth = snowdepth / 10) %>% 
  mutate(elevation = "high") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

idaho_high_snow_avg <- idaho_high_snow %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE)) %>% 
  mutate(elevation = "high")

## ....B. Low elevation snowdepth ##########
idaho_low_snow <- noaa_query( 
  stationid = "GHCND:USC00104318", # Station is HOMEDALE 1 SE, ID US
  startdate = "2014-01-01", enddate = "2017-12-31",
  datatypeid = "SNWD")

# Curate NOAA query
idaho_low_snow <- idaho_low_snow %>% 
  mutate(date = as_date(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  dplyr::rename(snowdepth = value) %>% 
  mutate(complete.cases(snowdepth)) %>% 
  # Currently in mm, switch to cm
  mutate(snowdepth = snowdepth / 10) %>% 
  mutate(elevation = "low") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

idaho_low_snow_avg <- idaho_low_snow %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE)) %>% 
  mutate(elevation = "low")

## ....C. Bind together high and low ##########

snowdepth <- idaho_high_snow %>%
  bind_rows(idaho_low_snow)

snowdepth_avg <- idaho_high_snow_avg %>%
  bind_rows(idaho_low_snow_avg)

# snowdepth <- idaho_high_snow %>% 
#   full_join(idaho_low_snow) %>% 
#   # Assuming an NA is 0, which is needed in order to have a fleshed-out timeseries
#   mutate(high_snowdepth = ifelse(is.na(high_snowdepth), 0, high_snowdepth)) %>% 
#   mutate(low_snowdepth = ifelse(is.na(low_snowdepth), 0, low_snowdepth)) %>% 
#   mutate(snowdepth = (low_snowdepth + high_snowdepth)/2)
# 
# snowdepth_avg <- idaho_high_snow_avg  %>% 
#   full_join(idaho_low_snow_avg ) %>% 
#   # Assuming an NA is 0, which is needed in order to have a fleshed-out timeseries
#   mutate(high_snowdepth = ifelse(is.na(high_snowdepth), 0, high_snowdepth)) %>% 
#   mutate(low_snowdepth = ifelse(is.na(low_snowdepth), 0, low_snowdepth)) %>% 
#   mutate(snowdepth = (low_snowdepth + high_snowdepth)/2)

## 4. Run curation program ############

source("scripts/00_source_code/data_curation_program.R")
sage_list <- prep_flux_data(low_dataset = low_sage,
               high_dataset = high_sage)

## Set spatial and temporal flags #######

for (i in 1:length(sage_list)) {
  
  sage_list[[i]] <- sage_list[[i]] %>% 
    mutate(site = "ID") %>% 
    mutate(macro = "scrub shrub") %>% 
    mutate(foliage = "leaf-off") %>% 
    mutate(foliage_cover = 0) %>% 
    mutate(flora = "sage brush") %>% 
    mutate(latitude = 43)
  
  if (i == 3 | i == 5) {
    
    sage_list[[i]] <- sage_list[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.1, "surface" = 1.0)) %>% 
      mutate(height_notes = "from metadata") %>%
      mutate(altitude = dplyr::recode(elevation, "low" = 1425, "high" = 2111))
  }
  
  ## Add snow data
  if (i == 3) {
    # For all years
    sage_list[[i]] <- sage_list[[i]] %>% 
      left_join(snowdepth) %>% 
      
      ## To fill in snowdepth NAs, pulling from two WRCC sources:
      # https://wrcc.dri.edu/cgi-bin/cliMAIN.pl?id1022
      # https://www1.ncdc.noaa.gov/pub/orders/IPS/IPS-74E1C1B9-3B2C-4BCD-B890-93E556BFEEDA.pdf
      # If link broken try:
      # https://wrcc.dri.edu/Climate/west_lcd.php
      # Designate a flag for snow data quality
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "single_avg", 
                                       "measured_daily")) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 32 | julian > 358),
                                2.032,  snowdepth)) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 60 | julian > 305),
                                .76, snowdepth)) %>% 
      mutate(snowdepth =  ifelse(is.na(snowdepth) & (julian >= 60 & julian <= 305),
                                 0, snowdepth))

  }
  
  if (i == 5) {
    # For average years
    sage_list[[i]] <- sage_list[[i]] %>% 
      left_join(snowdepth_avg) %>% 
      # Designate a flag for snow data quality
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "single_avg", 
                                       "measured_daily")) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 32 | julian > 358),
                                2.032,  snowdepth)) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 60 | julian > 305),
                                .76, snowdepth)) %>% 
      mutate(snowdepth =  ifelse(is.na(snowdepth) & (julian >= 60 & julian <= 305),
                                 0, snowdepth))
  }
}

## 5. Write out data ###############
write_csv(sage_list[[1]], "./data/01_primary/temperate/ID_sage_brush/derivative/ID_high_elev.csv")
write_csv(sage_list[[2]], "./data/01_primary/temperate/ID_sage_brush/derivative/ID_low_elev.csv")
write_csv(sage_list[[3]], "./data/01_primary/temperate/ID_sage_brush/derivative/ID_tall.csv")
write_csv(sage_list[[4]], "./data/01_primary/temperate/ID_sage_brush/derivative/ID_wide.csv")
write_csv(sage_list[[5]], "./data/01_primary/temperate/ID_sage_brush/derivative/ID_avgyears_tall.csv")
write_csv(sage_list[[6]], "./data/01_primary/temperate/ID_sage_brush/derivative/ID_avgyears_wide.csv")
