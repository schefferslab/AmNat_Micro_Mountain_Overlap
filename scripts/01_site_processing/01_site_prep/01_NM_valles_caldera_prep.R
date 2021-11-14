# David Klinges
# File creation date: 2019.02.03
# This script curates Ameriflux New Mexico Valles Caldera juniper forest sites to 
#   prep for analysis and generating figures

## 1. Workspace Prep #############

library(tidyverse)
library(lubridate)

# Import data 
low_valles_raw <- read_csv("./data/01_primary/temperate/NM_valles_caldera/low/AMF_US-Vcp_BASE_HH_6-5.csv")
high_valles_raw <- read_csv("./data/01_primary/temperate/NM_valles_caldera/high/AMF_US-Vcm_BASE_HH_10-5.csv")

jemez_soil_low_list <- c(list.files(path = "data/01_primary/temperate/NM_valles_caldera/soil/low",
                           # All of the data files are in their own subdirectories
                           #   so need to be recursive
                           pattern = ".csv", recursive = TRUE))

# Import all the data and bind together
for (i in 1:length(jemez_soil_low_list)) {

  # Read in data
  iter_data <- read_csv(paste0(getwd(), 
                                "/data/01_primary/temperate/NM_valles_caldera/soil/low/", 
                                 jemez_soil_low_list[[i]]))
  
  # We only care about date-time col and 10cm soil temp data
  iter_data <- iter_data %>% 
    dplyr::select(DateTime, Temp10cm)
  
  if (i == 1) {
    jemez_soil_low_raw <- iter_data
  } else {
    jemez_soil_low_raw <- bind_rows(jemez_soil_low_raw, iter_data)
  }
}

jemez_soil_high_list <- c(list.files(path = "data/01_primary/temperate/NM_valles_caldera/soil/high",
                                    # All of the data files are in their own subdirectories
                                    #   so need to be recursive
                                    pattern = ".csv", recursive = TRUE))

# Import all the data and bind together
for (i in 1:length(jemez_soil_high_list)) {
  
  # Read in data
  iter_data <- read_csv(paste0(getwd(), 
                               "/data/01_primary/temperate/NM_valles_caldera/soil/high/", 
                               jemez_soil_high_list[[i]]))
  
  # We only care about date-time col and 10cm soil temp data
  iter_data <- iter_data %>% 
    dplyr::select(DateTime, Temp12cm)
  
  if (i == 1) {
    jemez_soil_high_raw <- iter_data
  } else {
    jemez_soil_high_raw <- bind_rows(jemez_soil_high_raw, iter_data)
  }
}

## 2. Prep Valles Caldera data ############
## ....A. canopy data #######
## ** low: US-SCd: Southern California Climate Gradient - Valles Caldera #############

# Curate
low_valles <- low_valles_raw %>%
  dplyr::select(TIMESTAMP_START, TA, TA_PI_F) %>%
  dplyr::rename(time = TIMESTAMP_START) %>%
  dplyr::rename(canopy1 = TA) %>%
  dplyr::rename(canopy2 = TA_PI_F) %>%
  mutate(canopy1 = replace(canopy1, canopy1 == -9999, NA)) %>%
  mutate(canopy2 = replace(canopy2, canopy2 == -9999, NA)) %>%
  mutate(time = time/1000000) %>%
  separate(time, into = c("year", "month"), sep = 4) %>%
  mutate(year = as.double(year), month = as.double(month))

# Create julian days from year and month data
# Data was sampled on 30-min intervals, so can calc julian days

for (i in 1:length(unique(low_valles$year))) {
  
  current_year <- low_valles %>%
    filter(year == unique(low_valles$year)[i])
  current_year <- current_year %>%
    rowid_to_column(var = "julian") %>%
    mutate(julian = ceiling(julian/48)) %>%
    ungroup(year)
  
  if (i == 1) {
    output <- current_year
  } else {
    output <- bind_rows(output, current_year)
  }
}

low_valles <- output

# The mountain had a massive burn in May 2013. Clip to before then.

low_valles1 <- low_valles %>%
  filter(year < 2013)

low_valles2 <- low_valles %>%
  filter(year == 2013 & month < 05)

low_valles <- low_valles1 %>%
  bind_rows(low_valles2)

# Average between the sensors
valles_canopy_low <- low_valles %>%
  mutate(canopy = (canopy1 + canopy2) / 2) %>%
  mutate(canopy = ifelse(is.na(canopy), canopy1, canopy)) %>%
  mutate(canopy = ifelse(is.na(canopy), canopy2, canopy)) %>%
  dplyr::select(year, julian, canopy)

## ** high: US-SCc: Southern California Climate Gradient - Desert Chaparral #############
# Curate
high_valles <- high_valles_raw %>%
  dplyr::select(IMESTAMP_START, TA, TA_PI_F) %>%
  dplyr::rename(time = IMESTAMP_START) %>%
  dplyr::rename(canopy1 = TA) %>%
  dplyr::rename(canopy2 = TA_PI_F) %>%
  mutate(canopy1 = replace(canopy1, canopy1 == -9999, NA)) %>%
  mutate(canopy2 = replace(canopy2, canopy2 == -9999, NA)) %>%
  mutate(time = time/1000000) %>%
  separate(time, into = c("year", "month"), sep = 4) %>%
  mutate(year = as.double(year), month = as.double(month))

# Create julian days from year and month data
# Data was sampled on 30-min intervals, so can calc julian days
rm(i)
for (i in 1:length(unique(high_valles$year))) {
  
  current_year <- high_valles %>%
    filter(year == unique(high_valles$year)[i])
  current_year <- current_year %>%
    rowid_to_column(var = "julian") %>%
    mutate(julian = ceiling(julian/48)) %>%
    ungroup(year)
  
  if (i == 1) {
    output <- current_year
  } else {
    output <- bind_rows(output, current_year)
  }
}

high_valles <- output

# The mountain had a massive burn in May 2013. Clip to before then.

high_valles1 <- high_valles %>%
  filter(year < 2013)

high_valles2 <- high_valles %>%
  filter(year == 2013 & month < 05)

high_valles <- high_valles1 %>%
  bind_rows(high_valles2)

# Average between the sensors
valles_canopy_high <- high_valles %>%
  mutate(canopy = (canopy1 + canopy2) / 2) %>%
  mutate(canopy = ifelse(is.na(canopy), canopy1, canopy)) %>%
  mutate(canopy = ifelse(is.na(canopy), canopy2, canopy)) %>%
  dplyr::select(year, julian, canopy)

## ....B. Soil data #########

jemez_soil_low <- jemez_soil_low_raw %>% 
  slice(-1) %>% 
  mutate(datetime = mdy_hm(DateTime)) %>%
  filter(complete.cases(datetime)) %>% 
  mutate(year = year(datetime)) %>% 
  mutate(julian = yday(datetime)) %>% 
  dplyr::rename(soil = Temp10cm) %>% 
  mutate(soil = as.double(soil)) %>% 
  mutate(soil = ifelse(soil == -9999, NA, soil)) %>% 
  filter(complete.cases(soil)) %>%
  # Filter to before the big fire (May 2013)
  filter(year < 2013 | (year == 2013 & julian < 121)) %>% 
  dplyr::select(year, julian, soil)

jemez_soil_high <- jemez_soil_high_raw %>% 
  slice(-1) %>% 
  mutate(datetime = mdy_hm(DateTime)) %>% 
  filter(complete.cases(datetime)) %>% 
  mutate(year = year(datetime)) %>% 
  mutate(julian = yday(datetime)) %>% 
  dplyr::rename(soil = Temp12cm) %>% 
  mutate(soil = as.double(soil)) %>% 
  mutate(soil = ifelse(soil == -9999, NA, soil)) %>% 
  filter(complete.cases(soil)) %>% 
  # Filter to before the big fire (May 2013)
  filter(year < 2013 | (year == 2013 & julian < 121)) %>% 
  dplyr::select(year, julian, soil)


## 3. Wrangle snow data #########

source("scripts/00_source_code/noaa_query.R")

## ....A. High elevation snowdepth ##########
valles_high_snow <- noaa_query( 
  stationid = "GHCND:USS0006P01S", # Station is QUEMAZON, NM US (2880 m)
  startdate = "2010-01-01", enddate = "2013-12-31",
  datatypeid = "SNWD")

# Curate NOAA query
valles_high_snow <- valles_high_snow %>% 
  mutate(date = as_date(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  dplyr::rename(snowdepth = value) %>% 
  # Currently in mm, switch to cm
  mutate(snowdepth = snowdepth / 10) %>% 
  mutate(elevation = "high") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

valles_high_snow_avg <- valles_high_snow %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE)) %>% 
  mutate(elevation = "high")

## ....B. Low elevation snowdepth ##########
valles_low_snow <- noaa_query( 
  stationid = "GHCND:USC00294369", # Station is JEMEZ SPRINGS, NM US (1951 m)
  startdate = "2010-01-01", enddate = "2013-12-31",
  datatypeid = "SNWD")

# Curate NOAA query
valles_low_snow <- valles_low_snow %>% 
  mutate(date = as_date(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  dplyr::rename(snowdepth = value) %>% 
  mutate(complete.cases(snowdepth)) %>% 
  # Currently in mm, switch to cm
  mutate(snowdepth = snowdepth / 10) %>% 
  mutate(elevation = "low") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

valles_low_snow_avg <- valles_low_snow %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE)) %>% 
  mutate(elevation = "low")

## ....C. Bind together high and low ##########

snowdepth <- valles_high_snow %>%
  bind_rows(valles_low_snow)

snowdepth_avg <- valles_high_snow_avg %>%
  bind_rows(valles_low_snow_avg)

## 4. Run curation program ###########
  
## Note: canopy and soil temps sampled at different intervals (30 mins and 15 mins),
# and the canopy timestamps only include years and months....easier to run the 
# program separately for canopy and soil, and then join them together afterwards
# ALSO they low elevations sites for soil and canopy are different elevations,
# so I'm just going to list them as different sites

# Need to add canopy cols so as to not break data curation program
valles_canopy_low <- valles_canopy_low %>% 
  mutate(surface = NA)

valles_canopy_high <- valles_canopy_high %>% 
  mutate(surface = NA)
  
source("scripts/00_source_code/data_curation_program.R")

## ....A. Canopy #########

valles_list_canopy <- prep_flux_data(low_dataset = valles_canopy_low,
               high_dataset = valles_canopy_high)

# Now remove surface cols
for (i in 1:length(valles_list_canopy)) {
  
  valles_list_canopy[[i]] <- valles_list_canopy[[i]] %>% 
    dplyr::select(-contains("surface"))
}

# Now remove surface variable
valles_list_canopy[[3]] <- valles_list_canopy[[3]] %>% 
  filter(micro == "canopy")

valles_list_canopy[[5]] <- valles_list_canopy[[5]] %>% 
  filter(micro == "canopy")

for (i in 1:length(valles_list_canopy)) {
  
  valles_list_canopy[[i]] <- valles_list_canopy[[i]] %>% 
    mutate(site = "NM_canopy") %>% 
    mutate(macro = "Ponderosa pine") %>% 
    mutate(foliage = "leaf-on") %>% 
    mutate(foliage_cover = 1) %>% 
    mutate(flora = "Ponderosa pine overstory, Quercus gambelii understory") %>%
    mutate(latitude = 36)
  
  if (i == 3 | i == 5) {
    
    valles_list_canopy[[i]] <- valles_list_canopy[[i]] %>% 
      # Pulled from base height datasheet, US-VCp = , US-VCm = 
      mutate(height = mean(c(18.4, 23.6))) %>% 
      mutate(height_notes = "from metadata") %>%
      # Low canopy is 2500, low soil is 2700, so putting 2600
      mutate(altitude = ifelse(elevation == "low", 2500,
                                      ifelse(elevation == "high", 3030,
                                                    NA)))
  }
  
  ## Add snow data
  if (i == 3) {
    # For all years
    valles_list_canopy[[i]] <- valles_list_canopy[[i]] %>% 
      left_join(snowdepth) %>% 
      # Evidence seems pretty clear: very little snow.
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "confident_estimate", 
                                       "measured_daily")) %>% 
      # Few observations show snow. 
      ## Looking at WRCC suggests the same
      # https://www1.ncdc.noaa.gov/pub/orders/IPS/IPS-D69A9D9B-49BF-4831-AC7C-EEA140F2E9AD.pdf
      # If that link is broken you can query again here
      # https://wrcc.dri.edu/Climate/west_lcd.php
      mutate(snowdepth = ifelse(is.na(snowdepth), 0, 
                                snowdepth))
  }
   
  if (i == 5) {
    # For average years
    valles_list_canopy[[i]] <- valles_list_canopy[[i]] %>% 
      left_join(snowdepth_avg) %>% 
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "confident_estimate", 
                                       "measured_daily")) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth), 0, 
                                snowdepth))
  }
}

## ....B. Soil ###########

jemez_soil_low <- jemez_soil_low %>% 
  mutate(surface = NA)

jemez_soil_high <- jemez_soil_high %>% 
  mutate(surface = NA)

valles_list_soil <- prep_flux_data(low_dataset = jemez_soil_low,
                              high_dataset = jemez_soil_high)

# Now remove surface cols
for (i in 1:length(valles_list_canopy)) {
  
  valles_list_soil[[i]] <- valles_list_soil[[i]] %>% 
    dplyr::select(-contains("surface"))
}

# Now remove surface variable
valles_list_soil[[3]] <- valles_list_soil[[3]] %>% 
  filter(micro == "soil")

valles_list_soil[[5]] <- valles_list_soil[[5]] %>% 
  filter(micro == "soil")

# Now remove canopy cols
for (i in 1:length(valles_list_soil)) {
  
  valles_list_soil[[i]] <- valles_list_soil[[i]] %>% 
    dplyr::select(-contains("canopy"))
}

# Now remove canopy variable
valles_list_soil[[3]] <- valles_list_soil[[3]] %>% 
  filter(micro == "soil")

valles_list_soil[[5]] <- valles_list_soil[[5]] %>% 
  filter(micro == "soil")

for (i in 1:length(valles_list_soil)) {
  
  valles_list_soil[[i]] <- valles_list_soil[[i]] %>% 
    mutate(site = "NM_soil") %>% 
    mutate(macro = "Ponderosa pine") %>% 
    mutate(foliage = "leaf-on") %>% 
    mutate(foliage_cover = 1) %>% 
    mutate(flora = "Ponderosa pine overstory, Quercus gambelii understory") %>%
    mutate(latitude = 36)
  
  if (i == 3 | i == 5) {
    
    valles_list_soil[[i]] <- valles_list_soil[[i]] %>% 
      # Soil loggers are 10cm at low elev, 12cm at high elev, so close enough to
      # average to 11cm
      mutate(height = -.11) %>% 
      mutate(height_notes = "from metadata") %>%
      # Low canopy is 2500, low soil is 2700, so putting 2600
      mutate(altitude = ifelse(elevation == "low", 2700,
                              ifelse(elevation == "high", 3048.3,
                                                    NA)))
  }
  
  ## Add snow data
  if (i == 3) {
    # For all years
    valles_list_soil[[i]] <- valles_list_soil[[i]] %>% 
      left_join(snowdepth) %>% 
      # Evidence seems pretty clear: very little snow.
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "confident_estimate", 
                                       "measured_daily")) %>% 
      # Few observations show snow. 
      ## Looking at WRCC suggests the same
      # https://www1.ncdc.noaa.gov/pub/orders/IPS/IPS-D69A9D9B-49BF-4831-AC7C-EEA140F2E9AD.pdf
      # If that link is broken you can query again here
      # https://wrcc.dri.edu/Climate/west_lcd.php
      mutate(snowdepth = ifelse(is.na(snowdepth), 0, 
                                snowdepth))
  }
  
  if (i == 5) {
    # For average years
    valles_list_soil[[i]] <- valles_list_soil[[i]] %>% 
      left_join(snowdepth_avg) %>% 
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "confident_estimate", 
                                       "measured_daily")) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth), 0, 
                                snowdepth))
  }
}


## 5. Write out data ############

write_csv(valles_list_canopy[[1]], "data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_canopy_high_allyears.csv")
write_csv(valles_list_canopy[[2]], "data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_canopy_low_allyears.csv")
write_csv(valles_list_canopy[[3]], "data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_canopy_tall.csv")
write_csv(valles_list_canopy[[4]], "data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_canopy_wide.csv")
write_csv(valles_list_canopy[[5]], "data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_canopy_avgyears_tall.csv")
write_csv(valles_list_canopy[[6]], "data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_canopy_avgyears_wide.csv")

write_csv(valles_list_soil[[1]], "data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_soil_high_allyears.csv")
write_csv(valles_list_soil[[2]], "data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_soil_low_allyears.csv")
write_csv(valles_list_soil[[3]], "data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_soil_tall.csv")
write_csv(valles_list_soil[[4]], "data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_soil_wide.csv")
write_csv(valles_list_soil[[5]], "data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_soil_avgyears_tall.csv")
write_csv(valles_list_soil[[6]], "data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_soil_avgyears_wide.csv")

