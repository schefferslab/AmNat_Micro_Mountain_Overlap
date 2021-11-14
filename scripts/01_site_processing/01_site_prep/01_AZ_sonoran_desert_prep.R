# David Klinges
# File creation date: 2019.02.03
# This script curates Ameriflux Southern California Sonoran desert sites to 
#   prep for analysis and generating figures

## 1. Workspace Prep #############

library(tidyverse)
library(lubridate)

# Import data 

# Pulling surface temps from here
low_sonoran_raw <- read_csv("./data/01_primary/temperate/AZ_sonoran_desert/AMF_US-SCd_BASE-BADM_1-5/AMF_US-SCd_BASE_HH_1-5.csv",
                            col_types = cols(TA = col_double(), 
                                             TS = col_double()))

# Attempted to pull soil temps from here, but file is corrupted
# low_sonoran_raw_mat <- readMat("data/01_primary/temperate/AZ_sonoran_desert/AMF_US-SCd_BASE-BADM_1-5/LowDesert_v3_3.mat")


high_sonoran_raw <- read_csv("./data/01_primary/temperate/AZ_sonoran_desert/PinyonBurn_v3_7.csv")
# Some of the time samplings are repeated an absured number of times. Remove
# high_sonoran_raw <- unique(high_sonoran_raw) # Overwrite raw because that took awhile


## 2. Curate data ############
## ** low: US-SCd: Southern California Climate Gradient - Sonoran Desert #############


# This was an attempt (2019-09-19) to extract soil temps from matlab file, but the
# file is corrupted and very few cols have any data. Just surface temp.

# Going to assume that I'm working with 10cm depth

# # Soil depth 10cm is col number 28
# time <- low_sonoran_raw_mat$DATA[,1]
# 
# StartT_10cm <- low_sonoran_raw_mat$DATA[,28]
# 
# low_sonoran_raw_mat$HEADER[28]
# 
# soil_low <- tibble(
#   time = time,
#   soil = StartT_10cm) 
# 
# soil_low <- soil_low %>% 
#   mutate(time = as.Date(time, origin = "2006-01-01")) %>% 
#   filter(complete.cases(soil)) %>% 
#   mutate(year = year(time)) %>% 
#   mutate(julian = yday(time)) %>% 
#   dplyr::select(time, year, julian, soil)

# Curate
low_sonoran <- low_sonoran_raw %>%
  dplyr::select(TIMESTAMP_START, TA, TS) %>%
  dplyr::rename(time = TIMESTAMP_START) %>%
  # mutate(time = as.Date(time, origin = "2006-01-01")) %>% 
  dplyr::rename(surface = TA) %>%
  mutate(surface = replace(surface, surface == -9999, NA)) %>%
  dplyr::rename(soil = TS) %>%
  mutate(soil = replace(soil, soil == -9999, NA)) %>%
  mutate(time = time/1000000) %>%
  separate(time, into = c("year", "month"), sep = 4) %>%
  mutate(year = as.double(year))

# Create julian days from year and month data
# Data was sampled on 30-min intervals, so can calc julian days

for (i in 1:length(unique(low_sonoran$year))) {
  
  current_year <- low_sonoran %>%
    filter(year == unique(low_sonoran$year)[i])
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

low_sonoran <- output %>%
  # Remove month col so it doesn't cause issues for joining later
  dplyr::select(-month)


## ** high: US-SCc: Southern California Climate Gradient - Desert Chaparral #############

high_sonoran <- high_sonoran_raw %>%
  dplyr::select(TIME, T_HMP, StartT_10cm) %>%
  dplyr::rename(time = TIME) %>%
  dplyr::rename(surface = T_HMP) %>%
  mutate(surface = replace(surface, is.nan(surface), NA)) %>%
  dplyr::rename(soil = StartT_10cm) %>%
  mutate(soil = replace(soil, is.nan(soil), NA))

# There's a few very obvious outliers that I'm going to remove here
high_sonoran <- high_sonoran[-c(176479, 135630, 99126),] # one time where surface = 48 C while soil
# = -0.39 C, other time soil = 710 C, a third time where time is recorded wrong

# Change to date-time
high_sonoran$time <- as.Date(high_sonoran$time, origin = "2006-01-01")

high_sonoran <- high_sonoran %>%
  mutate(year = year(time)) %>%
  mutate(julian = yday(time))


high_sonoran <- high_sonoran %>%
  # Remove day 366 so as to uncomplicate things
  filter(julian <= 365) %>%
  # Get rid of time stamp column
  dplyr::select(-time)

## 3. Wrangle snow data #########
# Does't look like it snows really at all, even at high elevation, but just in case
source("scripts/00_source_code/noaa_query.R")

## ....A. High elevation snowdepth ##########

az_high_snow <- noaa_query( 
  stationid = "GHCND:USC00042327", # Station is DEEP CANYON LAB, CA US
  startdate = "2009-01-01", enddate = "2011-12-31",
  datatypeid = "SNWD")

# Curate NOAA query
az_high_snow <- az_high_snow %>% 
  mutate(date = as_date(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  dplyr::rename(snowdepth = value) %>% 
  # Currently in mm, switch to cm
  mutate(snowdepth = snowdepth / 10) %>% 
  mutate(elevation = "high") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

az_high_snow_avg <- az_high_snow %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE)) %>% 
  mutate(elevation = "high")

## ....B. Low elevation snowdepth ##########
az_low_snow <- az_high_snow

az_low_snow_avg <- az_high_snow_avg

## ....C. Bind together high and low ##########

snowdepth <- az_high_snow %>%
  bind_rows(az_low_snow)

snowdepth_avg <- az_high_snow_avg %>%
  bind_rows(az_low_snow_avg)


## 4. Run curation program ###########

# Subset to just years with viable data
low_sonoran <- low_sonoran %>%
  filter(year == 2009 | year == 2010 | year == 2011)

high_sonoran <- high_sonoran %>%
  filter(year == 2009 | year == 2010 | year == 2011)

source("scripts/00_source_code/data_curation_program.R")
sonoran_list <- prep_flux_data(low_dataset = low_sonoran, 
                               high_dataset = high_sonoran)

for (i in 1:length(sonoran_list)) {
  
  sonoran_list[[i]] <- sonoran_list[[i]] %>% 
    mutate(site = "AZ") %>% 
    mutate(macro = "hot desert") %>% 
    mutate(foliage = "leaf-off") %>% 
    mutate(foliage_cover = 0) %>% 
    mutate(flora = "sparse shrub") %>% 
    mutate(latitude = 34)
  
  if (i == 3 | i == 5) {
    
    sonoran_list[[i]] <- sonoran_list[[i]] %>% 
      # 200 cm is the most common height in the Ameriflux network for surface temps
      mutate(height = dplyr::recode(micro, "soil" = -0.1, "surface" = 2.0)) %>% 
      mutate(height_notes = "informed estimate") %>% 
      mutate(altitude = dplyr::recode(elevation, "low" = 275, "high" = 1280))
  }
  
  
  ## Add snow data
  if (i == 3) {
    # For all years
    sonoran_list[[i]] <- sonoran_list[[i]] %>% 
      left_join(snowdepth) %>% 
      # Evidence seems pretty clear: there's no snow here.
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "confident_estimate", 
                                       "measured_daily")) %>% 
      # No observation shows snow. 
      ## Looking at WRCC suggests the same
      # https://wrcc.dri.edu/cgi-bin/cliMAIN.pl?az0415
      mutate(snowdepth = ifelse(is.na(snowdepth), 0, 
                                       snowdepth))
  }
  
  if (i == 5) {
    # For average years
    sonoran_list[[i]] <- sonoran_list[[i]] %>% 
      left_join(snowdepth_avg) %>% 
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "confident_estimate", 
                                       "measured_daily")) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth), 0, 
                                snowdepth))
  }
}

## 5. Write out data ############

write_csv(sonoran_list[[1]], "data/01_primary/temperate/AZ_sonoran_desert/derivative/AZ_sonoran_desert_high_allyears.csv")
write_csv(sonoran_list[[2]], "data/01_primary/temperate/AZ_sonoran_desert/derivative/AZ_sonoran_desert_low_allyears.csv")
write_csv(sonoran_list[[3]], "data/01_primary/temperate/AZ_sonoran_desert/derivative/AZ_sonoran_desert_tall.csv")
write_csv(sonoran_list[[4]], "data/01_primary/temperate/AZ_sonoran_desert/derivative/AZ_sonoran_desert_wide.csv")
write_csv(sonoran_list[[5]], "data/01_primary/temperate/AZ_sonoran_desert/derivative/AZ_sonoran_desert_avgyears_tall.csv")
write_csv(sonoran_list[[6]], "data/01_primary/temperate/AZ_sonoran_desert/derivative/AZ_sonoran_desert_avgyears_wide.csv")
