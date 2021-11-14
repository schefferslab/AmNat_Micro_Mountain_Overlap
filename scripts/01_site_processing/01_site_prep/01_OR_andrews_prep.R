# David Klinges
# File creation date: 2019.06.02
# This script curates OR HJ Andrews exp forest sites to prep for analysis and 
#   generating figures

## 1. Workspace prep ########

library(tidyverse)
library(lubridate)

# Read in data

## ....A. Load daily data from all sites #########


daily_air <- read_csv("data/01_primary/temperate/OR_andrews/original/all_sites/MS00501_v4.csv")

daily_soil <- read_csv("data/01_primary/temperate/OR_andrews/original/all_sites/MS00521_v4.csv")
daily_soil19 <- daily_soil %>% 
  filter(SITECODE == "RS19__")


## ....B. Load fine resolution from one high and one low #######

RS14air_15min <- read_csv("data/01_primary/temperate/OR_andrews/original/high/RS14air_15min.csv")
RS14soil_15min <- read_csv("data/01_primary/temperate/OR_andrews/original/high/RS14soil_15min.csv")
RS04soil_1min <- read_csv("data/01_primary/temperate/OR_andrews/original/high/RS04_soil_1min.csv")


RS89air_15min <- read_csv("data/01_primary/temperate/OR_andrews/original/low/RS89air_15min.csv")
RS89soil_15min <- read_csv("data/01_primary/temperate/OR_andrews/original/low/RS89soil_15min.csv")

## ....C. Load snow depth data ########3

snowdepth_raw <- read_csv("data/01_primary/temperate/OR_andrews/snow/MS00701_v3.csv")

## 2. Curate data #########

## ....A. Curate low ##########
## ....Aa. Curate dailys ######

## RS89 is 473m, TS75 is 460m, RS01 is 499m

# Air temps
low_air <- daily_air %>% 
  filter(SITECODE == "RS89__" | SITECODE == "TS75__" | SITECODE == "RS01__") %>% 
  
  # Filter to only days that have min and max temps
  filter(!is.na(AIRTEMP_MAX_DAY) & !is.na(AIRTEMP_MIN_DAY)) %>% 
  gather(key = "minmax", value = "surface", AIRTEMP_MEAN_DAY, AIRTEMP_MAX_DAY,
         AIRTEMP_MIN_DAY) %>% 

  # dplyr::recode for later join
  mutate(minmax = dplyr::recode(minmax, AIRTEMP_MEAN_DAY = "mean", AIRTEMP_MIN_DAY = "min",
                         AIRTEMP_MAX_DAY = "max")) %>% 
  # Remove questional values (removing from max removes from mean and min too)
  filter(AIRTEMP_MAX_FLAG != "Q") %>% 
  # Even though the 80, 85, and 100 cm heights are only measured up until 1998, 
  # the other height--285-- is too high
  # The highest probe is 285 cm, but we're keeping it for now because it's the 
  #   the only height measured after 1998. So filtering out the 80 and 85 cm
  #   heights because those aren't measured at high elevation
  filter(HEIGHT == 100) %>%
  # Change date-times
  mutate(year = year(DATE)) %>% 
  mutate(julian = yday(DATE)) %>% 
  dplyr::select(SITECODE, year, julian, surface, HEIGHT, minmax) %>% 
  filter(!is.na(surface))
  
# Soil temps
low_soil <- daily_soil %>% 
  filter(SITECODE == "RS89__" | SITECODE == "TS75__" | SITECODE == "RS01__") %>%
  # We'll keep all loggers
  # Filter to only days that have min and max temps
  filter(!is.na(SOILTEMP_MAX_DAY) & !is.na(SOILTEMP_MIN_DAY)) %>% 
  gather(key = "minmax", value = "soil", SOILTEMP_MEAN_DAY, SOILTEMP_MAX_DAY,
         SOILTEMP_MIN_DAY) %>% 
  # dplyr::recode for later join
  mutate(minmax = dplyr::recode(minmax, SOILTEMP_MEAN_DAY = "mean", 
                         SOILTEMP_MIN_DAY = "min",
                         SOILTEMP_MAX_DAY = "max")) %>% 
  # Just 10cm soil depth
  filter(DEPTH == 10) %>% 
  # Remove questional values (removing from max removes from mean and min too)
  filter(SOILTEMP_MAX_FLAG != "Q") %>% 
  # Change date-times
  mutate(year = year(DATE)) %>% 
  mutate(julian = yday(DATE)) %>% 
  dplyr::select(SITECODE, year, julian, soil, DEPTH, minmax) %>% 
  filter(!is.na(soil))

# Join air and soil
low <- low_air %>% 
  full_join(low_soil) %>% 
  # Remove the unneccesary descriptive columns
  dplyr::select(year, julian, soil, surface)

# Because we have so much data, we can afford to NA omit
# low <- na.omit(low) 

## ....Aa. Curate fine resoultion ######

## RS89 is 473m

# Air temps
RS89_surface <- RS89air_15min %>% 

  # Rename
  mutate(surface = as.double(AIRTEMP_MEAN)) %>% 
  # Remove questional values (removing from max removes from mean and min too)
  filter(AIRTEMP_MEAN_FLAG != "Q") %>% 
  # Change date-times
  mutate(year = year(DATE_TIME)) %>% 
  mutate(julian = yday(DATE_TIME)) %>% 
  dplyr::select(year, julian, DATE_TIME, surface) %>% 
  filter(!is.na(surface))

# Soil temps
RS89_soil <- RS89soil_15min %>% 
  
  # Rename
  mutate(soil = as.double(SOILTEMP_MEAN)) %>% 
  # Remove questional values (removing from max removes from mean and min too)
  filter(SOILTEMP_MEAN_FLAG != "Q") %>% 
  # Change date-times
  mutate(year = year(DATE_TIME)) %>% 
  mutate(julian = yday(DATE_TIME)) %>% 
  dplyr::select(year, julian, DATE_TIME, soil) %>% 
  filter(!is.na(soil))

# Join air and soil
RS89 <- RS89_surface %>% 
  full_join(RS89_soil) %>% 
  # Remove the unneccesary descriptive columns
  dplyr::select(year, julian, soil, surface)

# Because we have so much data, we can afford to NA omit
RS89 <- na.omit(RS89)

## Curate high-res
RS89_highRes <- RS89_surface %>% 
  full_join(RS89_soil) %>% 
  mutate(hour = hour(DATE_TIME)) %>% 
  mutate(minute = minute(DATE_TIME)) %>% 
  dplyr::select(-DATE_TIME)

## ....B. Curate high #########

## ....Ba. Curate dailys #########

## RS03 is 978m, RS04 is 1307m, RS14 is 1430m

high_air <- daily_air %>% 
  filter(SITECODE == "RS03__") %>% 
  # filter(SITECODE == "RS03__" | SITECODE == "RS04__" | SITECODE == "RS14__") %>% 
  # Filter to only days that have min and max temps
  filter(!is.na(AIRTEMP_MAX_DAY) & !is.na(AIRTEMP_MIN_DAY)) %>% 
  gather(key = "minmax", value = "surface", AIRTEMP_MEAN_DAY, AIRTEMP_MAX_DAY,
         AIRTEMP_MIN_DAY) %>% 
  # dplyr::recode for later join
  mutate(minmax = dplyr::recode(minmax, AIRTEMP_MEAN_DAY = "mean", AIRTEMP_MIN_DAY = "min",
                         AIRTEMP_MAX_DAY = "max")) %>% 
  # Probes are at different heights than low. Select only those that are same heights
  filter(HEIGHT == 100) %>%
  # Remove questional values (removing from max removes from mean and min too)
  filter(AIRTEMP_MAX_FLAG != "Q") %>% 
  # Change date-times
  mutate(year = year(DATE)) %>% 
  mutate(julian = yday(DATE)) %>% 
  dplyr::select(SITECODE, year, julian, surface, HEIGHT, minmax) %>% 
  filter(!is.na(surface))

# 
high_soil <- daily_soil %>% 
  filter(SITECODE == "RS03__" | SITECODE == "RS04__" | SITECODE == "RS14__") %>%
  # We'll keep all loggers
  # Filter to only days that have min and max temps
  filter(!is.na(SOILTEMP_MAX_DAY) & !is.na(SOILTEMP_MIN_DAY)) %>% 
  gather(key = "minmax", value = "soil", SOILTEMP_MEAN_DAY, SOILTEMP_MAX_DAY,
         SOILTEMP_MIN_DAY) %>% 
  
  # dplyr::recode for later join
  mutate(minmax = dplyr::recode(minmax, SOILTEMP_MEAN_DAY = "mean", SOILTEMP_MIN_DAY = "min",
                         SOILTEMP_MAX_DAY = "max")) %>% 
  # Filter to just 10cm depth
  filter(DEPTH == 10) %>% 
  # Remove questional values (removing from max removes from mean and min too)
  filter(SOILTEMP_MAX_FLAG != "Q") %>%   
  # Change date-times
  mutate(year = year(DATE)) %>% 
  mutate(julian = yday(DATE)) %>% 
  dplyr::select(SITECODE, year, julian, soil, DEPTH, minmax) %>% 
  filter(!is.na(soil))

# Join air and soil
high <- high_air %>% 
  full_join(high_soil) %>% 
  # Remove the unneccesary descriptive columns
  dplyr::select(year, julian, soil, surface)

# Because we have so much data, we can afford to NA omit
high <- na.omit(high)

## ....Bb. Curate fine resolution #######

# Air temps
RS14_surface <- RS14air_15min %>% 
  
  # Rename
  mutate(surface = as.double(AIRTEMP_MEAN)) %>% 
  # Remove questional values (removing from max removes from mean and min too)
  filter(AIRTEMP_MEAN_FLAG != "Q") %>% 
  # Change date-times
  mutate(year = year(DATE_TIME)) %>% 
  mutate(julian = yday(DATE_TIME)) %>% 
  dplyr::select(year, julian, DATE_TIME, surface) %>% 
  filter(!is.na(surface))

# Soil temps
RS14_soil <- RS14soil_15min %>% 
  
  # Rename
  mutate(soil = as.double(SOILTEMP_MEAN)) %>% 
  # Remove questional values (removing from max removes from mean and min too)
  filter(SOILTEMP_MEAN_FLAG != "Q") %>% 
  # Change date-times
  mutate(year = year(DATE_TIME)) %>% 
  mutate(julian = yday(DATE_TIME)) %>% 
  dplyr::select(year, julian, DATE_TIME, soil) %>% 
  filter(!is.na(soil))

# Join air and soil
RS14 <- RS14_surface %>% 
  full_join(RS14_soil) %>% 
  # Remove the unneccesary descriptive columns
  dplyr::select(year, julian, soil, surface)

# Because we have so much data, we can afford to NA omit
RS14 <- na.omit(RS14)

RS04_soil <- RS04soil_1min %>% 
  # Rename
  mutate(soil = as.double(SOILTEMP_MEAN)) %>% 
  # Remove questional values (removing from max removes from mean and min too)
  filter(SOILTEMP_MEAN_FLAG != "Q") %>% 
  # Change date-times
  mutate(year = year(DATE_TIME)) %>% 
  mutate(julian = yday(DATE_TIME)) %>% 
  dplyr::select(year, julian, DATE_TIME, soil) %>% 
  filter(!is.na(soil))

## Curate high-res
RS14_highRes <- RS14_surface %>% 
  full_join(RS14_soil) %>% 
  mutate(hour = hour(DATE_TIME)) %>% 
  mutate(minute = minute(DATE_TIME)) %>% 
  dplyr::select(-DATE_TIME)

RS04_highRes <- RS04_soil %>% 
  mutate(hour = hour(DATE_TIME)) %>% 
  mutate(minute = minute(DATE_TIME)) %>% 
  dplyr::select(-DATE_TIME)


## ....3. Wrangle snow data #########

# These are hand-derived measurements

# low we'll use R506F1, which is low elevation (430 m), and R506F6 (1330 m) for high
# source: http://andlter.forestry.oregonstate.edu/data/abstractdetail.aspx?dbcode=MS007
# tried google maps to determine road names (SITECODEs correspond to road names),
# but isn't very helpful...

snowdepth <- snowdepth_raw %>% 
  filter(SITECODE == "R506F1" | SITECODE == "R506F6") %>% 
  mutate(year = year(DATE_TIME)) %>% 
  mutate(julian = yday(DATE_TIME)) %>% 
  dplyr::rename(snowdepth = SNOW_DEPTH_INST) %>% 
  mutate(elevation = recode(SITECODE, "R506F1" = "low", "R506F6" = "high")) %>% 
  dplyr::select(year, julian, snowdepth, elevation)

## 3. Wrangle snow data #########

source("scripts/00_source_code/noaa_query.R")

## ....A. High elevation snowdepth ##########
oregon_high_snow <- noaa_query( 
  stationid = "GHCND:USC00350652", # Station is BELKNAP SPRINGS 8 N, OR US
  startdate = "1995-01-01", enddate = "2016-12-31",
  datatypeid = "SNWD")

# Curate NOAA query
oregon_high_snow <- oregon_high_snow %>% 
  mutate(date = as_date(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  dplyr::rename(snowdepth = value) %>% 
  # Currently in mm, switch to cm
  mutate(snowdepth = snowdepth / 10) %>% 
  mutate(elevation = "high") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

# We're going to join hand measurements with NOAA data
oregon_high_snow <- oregon_high_snow %>% 
  dplyr::rename(snowdepth_NOAA = snowdepth) %>% 
  full_join(filter(snowdepth, elevation == "high")) %>% 
  mutate(snowdepth = ifelse(is.na(snowdepth), snowdepth_NOAA, snowdepth)) %>% 
  dplyr::select(-snowdepth_NOAA)

oregon_high_snow_avg <- oregon_high_snow %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE)) %>% 
  mutate(elevation = "high")

## ....B. Low elevation snowdepth ##########

# Cougar station has a pretty good timeseries, but is recoding 0 snowdepth for
# every single observation...while the hand-measured snowdepth has snow. So 
# Cougar dam must not be representative of what is actually in the forest.

# Therefore using hand measurements, even though not comprehensive timeseries
oregon_low_snow <- snowdepth %>% 
  filter(elevation == "low")

# oregon_low_snow <- noaa_query( 
#   stationid = "GHCND:USC00351914", # Station is COUGAR DAM, OR US
#   startdate = "2006-01-01", enddate = "2016-12-31",
#   datatypeid = "SNWD")

oregon_low_snow_avg <- oregon_low_snow %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE)) %>% 
  mutate(elevation = "low")

## ....C. Bind together high and low ##########

snowdepth <- oregon_high_snow %>%
  bind_rows(oregon_low_snow)

snowdepth_avg <- oregon_high_snow_avg %>%
  bind_rows(oregon_low_snow_avg)

## 3. Conduct overlap data curation #############

## ....A. Surface only ###########
low_air <- low_air %>% 
  dplyr::select(year, julian, surface)
high_air <- high_air %>% 
  dplyr::select(year, julian, surface)

source("scripts/00_source_code/data_curation_program.R")
andrews_list <- prep_flux_data(low_dataset = low_air, 
                               high_dataset = high_air)

for (i in 1:length(andrews_list)) {
  
  andrews_list[[i]] <- andrews_list[[i]] %>% 
    mutate(site = "OR") %>% 
    mutate(macro = "Dense coniferous") %>% 
    mutate(foliage = "leaf-on") %>% 
    mutate(foliage_cover = 1) %>% 
    mutate(flora = "conifers") %>% 
    mutate(latitude = 44) %>% 
    mutate(snow = "variable between high and low")
  
  if (i == 3 | i == 5) {
    
    andrews_list[[i]] <- andrews_list[[i]] %>% 
      filter(micro == "surface") %>% 
      mutate(height = dplyr::recode(micro, "surface" = 1)) %>% 
      mutate(height_notes = "from metadata") %>%
      # low elevation: (473+460+499)/3 =  477
      mutate(altitude = dplyr::recode(elevation, "low" = 473, "high" = 978)) %>% 
      mutate(snow = ifelse(
        (elevation == "high" & (julian < 130 | julian > 330)), "snowpack", 
        ifelse(elevation == "low" & (julian < 50 | julian > 340), "snowpack",
               "no snow")))
  }
}

write_csv(andrews_list[[1]], "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsAir_high_all_years.csv")
write_csv(andrews_list[[2]],  "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsAir_low_all_years.csv")
write_csv(andrews_list[[3]], "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsAir_tall.csv")
write_csv(andrews_list[[4]],  "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsAir_wide.csv")
write_csv(andrews_list[[5]], "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsAir_avgyears_tall.csv")
write_csv(andrews_list[[6]], "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsAir_avgyears_wide.csv")

## ....B. Soil only ##########

low_soil <- low_soil %>% 
  dplyr::select(year, julian, soil) %>% 
  mutate(surface = NA)

high_soil <- high_soil %>% 
  dplyr::select(year, julian, soil) %>% 
  mutate(surface = NA)


andrews_list <- prep_flux_data(low_dataset = low_soil, 
                               high_dataset = high_soil)

# Now remove surface cols
for (i in 1:length(andrews_list)) {
  
  andrews_list[[i]] <- andrews_list[[i]] %>% 
    dplyr::select(-contains("surface")) %>% 
    filter_all(any_vars(!is.infinite(.)))
}

for (i in 1:length(andrews_list)) {
  
  andrews_list[[i]] <- andrews_list[[i]] %>% 
    mutate(site = "OR") %>% 
    mutate(macro = "Dense coniferous") %>% 
    mutate(foliage = "leaf-on") %>% 
    mutate(foliage_cover = 1) %>% 
    mutate(flora = "conifers") %>% 
    mutate(latitude = 44) %>% 
    mutate(snow = "variable between high and low")
  
  if (i == 3 | i == 5) {
    
    andrews_list[[i]] <- andrews_list[[i]] %>% 
      filter(micro == "soil") %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.1, "surface" = 2.85)) %>% 
      mutate(height_notes = "from metadata") %>%
      # low elevation: (473+460+499)/3 =  477
      mutate(altitude = dplyr::recode(elevation, "low" = 473, "high" = 978)) %>% 
      mutate(snow = ifelse(
      (elevation == "high" & (julian < 130 | julian > 330)), "snowpack", 
      ifelse(elevation == "low" & (julian < 50 | julian > 340), "snowpack",
               "no snow")))
  }
}

write_csv(andrews_list[[1]], "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsSoil_high_all_years.csv")
write_csv(andrews_list[[2]],  "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsSoil_low_all_years.csv")
write_csv(andrews_list[[3]], "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsSoil_tall.csv")
write_csv(andrews_list[[4]],  "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsSoil_wide.csv")
write_csv(andrews_list[[5]], "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsSoil_avgyears_tall.csv")
write_csv(andrews_list[[6]], "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsSoil_avgyears_wide.csv")

## ....C. RS89 and RS14: SITES USED GOING FORWARD IN KLINGES AND SCHEFFERS AmNat ########



andrews_list <- prep_flux_data(low_dataset = RS89, 
                               high_dataset = RS14)

for (i in 1:length(andrews_list)) {
  
  andrews_list[[i]] <- andrews_list[[i]] %>% 
    mutate(site = "OR") %>% 
    mutate(macro = "Dense coniferous") %>% 
    mutate(foliage = "leaf-on") %>% 
    mutate(foliage_cover = 1) %>% 
    mutate(flora = "conifers") %>% 
    mutate(latitude = 44)
  
  if (i == 3 | i == 5) {
    
    andrews_list[[i]] <- andrews_list[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.1, "surface" = 2.85)) %>% 
      mutate(height_notes = "from metadata") %>%
      mutate(altitude = dplyr::recode(elevation, "low" = 473, "high" = 1430))
  }
  
  ## Add snow data
  if (i == 3) {
    # For all years
    andrews_list[[i]] <- andrews_list[[i]] %>% 
      left_join(snowdepth) %>% 
      ## To fill in snowdepth NAs, pulling from WRCC:
      # Eugene is somewhat representative of HJ Andrews:
      # https://wrcc.dri.edu/cgi-bin/cliMAIN.pl?or2706
      # Luckily the HJ Andrews snowdepth time series is pretty good
      # Designate a flag for snow data quality
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "single_avg", 
                                       "measured_daily")) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 32 | julian > 358),
                                2.3, snowdepth)) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 69 | julian > 315),
                                1.2, snowdepth)) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian >= 69 & julian <= 315),
                                0, snowdepth))
  }
  
  if (i == 5) {
    # For average years
    andrews_list[[i]] <- andrews_list[[i]] %>% 
      left_join(snowdepth_avg) %>% 
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "single_avg", 
                                       "measured_daily")) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 32 | julian > 358),
                                2.3, snowdepth)) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 69 | julian > 315),
                                1.2, snowdepth)) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian >= 69 & julian <= 315),
                                0, snowdepth))
  }
}

write_csv(andrews_list[[1]], "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsFineRes_high_all_years.csv")
write_csv(andrews_list[[2]],  "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsFineRes_low_all_years.csv")
write_csv(andrews_list[[3]], "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsFineRes_tall.csv")
write_csv(andrews_list[[4]],  "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsFineRes_wide.csv")
write_csv(andrews_list[[5]], "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsFineRes_avgyears_tall.csv")
write_csv(andrews_list[[6]], "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsFineRes_avgyears_wide.csv")

## ....D. RS89 and RS04, soil only ########

RS04_soil <- RS04_soil %>% 
  mutate(surface = 1)


andrews_list <- prep_flux_data(low_dataset = RS89, 
                               high_dataset = RS04_soil)

# Now remove surface col
for (i in 1:length(andrews_list)) {
  andrews_list[[i]] <- andrews_list[[i]] %>% 
    dplyr::select(-contains("surface")) %>% 
    filter_all(any_vars(!is.infinite(.)))
}

for (i in 1:length(andrews_list)) {
  
  andrews_list[[i]] <- andrews_list[[i]] %>% 
    mutate(site = "OR") %>% 
    mutate(macro = "Dense coniferous") %>% 
    mutate(foliage = "leaf-on") %>% 
    mutate(foliage_cover = 1) %>% 
    mutate(flora = "conifers") %>% 
    mutate(latitude = 44) %>% 
    mutate(snow = "variable between high and low")

  if (i == 3 | i == 5) {
    andrews_list[[i]] <- andrews_list[[i]] %>% 
      filter(micro == "soil") %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.1, "surface" = 2.85)) %>%
      mutate(height_notes = "from metadata") %>%
      mutate(altitude = dplyr::recode(elevation, "low" = 473, "high" = 1307)) %>% 
      mutate(snow = ifelse(
        (elevation == "high" & (julian < 130 | julian > 330)), "snowpack", 
        ifelse(elevation == "low" & (julian < 50 | julian > 340), "snowpack",
               "no snow")))
  }
}

write_csv(andrews_list[[1]], "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsRS89RS04_soil_high_all_years.csv")
write_csv(andrews_list[[2]],  "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsRS89RS04_soil_low_all_years.csv")
write_csv(andrews_list[[3]], "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsRS89RS04_soil_tall.csv")
write_csv(andrews_list[[4]],  "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsRS89RS04_soil_wide.csv")
write_csv(andrews_list[[5]], "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsRS89RS04_soil_avgyears_tall.csv")
write_csv(andrews_list[[6]], "data/01_primary/temperate/OR_andrews/derivative/OR_andrewsRS89RS04_soil_avgyears_wide.csv")

## ....4. Write out fine-resolution data ########

RS89_highRes <- RS89_highRes %>% 
  dplyr::rename(low_soil = soil, low_surface = surface)

RS14_highRes <- RS14_highRes %>% 
  dplyr::rename(high_soil = soil, high_surface = surface)
  
OR_andrews_fineRes <- RS89_highRes %>% 
  left_join(RS14_highRes) %>% 
  mutate(site = "OR") %>% 
  mutate(macro = "Dense coniferous") %>% 
  mutate(foliage = "leaf-on") %>% 
  mutate(foliage_cover = 1) %>% 
  mutate(flora = "conifers") %>% 
  mutate(latitude = 44) %>% 
  mutate(snow = "variable between high and low")

write_csv(OR_andrews_fineRes, "data/01_primary/temperate/OR_andrews/derivative/fineRes/OR_andrews_fineResRS89RS14.csv")


RS04_highRes <- RS04_highRes %>% 
  dplyr::rename(high_soil = soil)

OR_andrews_fineRes <- RS89_highRes %>% 
  left_join(RS04_highRes) %>% 
  # Remove surface cols
  dplyr::select(-low_surface)%>% 
  mutate(site = "OR") %>% 
  mutate(macro = "Dense coniferous") %>% 
  mutate(foliage = "leaf-on") %>% 
  mutate(foliage_cover = 1) %>% 
  mutate(flora = "Pseudotsuga menziesii, Abies spp., Tsuga spp., others") %>% 
  mutate(latitude = 44) %>% 
  mutate(snow = "variable between high and low")

write_csv(OR_andrews_fineRes, "data/01_primary/temperate/OR_andrews/derivative/fineRes/OR_andrews_fineResRS89RS04.csv")
