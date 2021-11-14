# David Klinges
# File creation date: 2019.02.03
# This script curates CZO Boulder Colorado sites to prep for analysis and 
#   generating figures

## Workspace Prep #############

library(tidyverse)
library(lubridate)

# Import data

boulder_gg_p3_air <- read_csv("./data/01_primary/temperate/CO_boulder/mid_elevation/original/0211194345__snowsenLGGP3_L1.csv")
boulder_gg_p3_soil <- read_csv("./data/01_primary/temperate/CO_boulder/mid_elevation/original/0211194505slmotsen_lgg_p3.csv")
boulder_gg_p10_air <- read_csv("./data/01_primary/temperate/CO_boulder/mid_elevation/original/0211212425__snowsenLGGP10_L1.csv")
boulder_gg_p4_air <- read_csv("./data/01_primary/temperate/CO_boulder/mid_elevation/original/0211213808__snowsenLGGP4_L1.csv")
boulder_gg_met1 <- read_csv("./data/01_primary/temperate/CO_boulder/mid_elevation/original/0211214822GGL_SF_MET.csv", 
                            col_types = cols(`AIRTEMP(C)-2.5M(AVG)` = col_double(),
                                             `SOIL TEMP(C)-22CM` = col_double()))
boulder_betasso_met1 <- read_csv("./data/01_primary/temperate/CO_boulder/low_elevation/original/entire_BT_Met.csv")

boulder_glv_soil_raw <- read_csv("./data/01_primary/temperate/CO_boulder/high_elevation/original/0220190109slmotsen_glv_catena.csv")
boulder_glv_air_raw <- read_csv("./data/01_primary/temperate/CO_boulder/high_elevation/original/d-1tdayv.ml.data.csv")

# Import snowdepth averages
# From: https://wrcc.dri.edu/cgi-bin/cliRECt.pl?co0848
snowdepth_avg <- read_csv("data/01_primary/temperate/CO_boulder/snow/noaa_weather.csv")

## Prep Betasso low elevation data #########
## * Meteorological Tower 1 ###########
boulder_low <- boulder_betasso_met1 %>%
  dplyr::select(`Date Time`, `SOIL TEMP(C) -15CM`, `AIRTEMP(C) -2M(AVG)`) %>%
  dplyr::rename(date_time = `Date Time`, surface = `AIRTEMP(C) -2M(AVG)`, 
         soil = `SOIL TEMP(C) -15CM`) %>%
  mutate(surface = as.numeric(replace(surface, surface == "null", NA))) %>%
  mutate(soil = as.numeric(replace(soil, soil == "null", NA)))

# Parse date time
boulder_low <- boulder_low %>%
  separate(date_time, into = c("date", "time"), sep = " ") %>%
  # Need to add zero's to single-digit months and days
  separate(date, into = c("month", "day", "year"), sep = "/") %>%
  mutate(month = as.numeric(month), day = as.numeric(day)) %>%
  mutate(month = ifelse(month < 10, paste0("0", as.character(month)), 
                        as.character(month))) %>%
  mutate(day = ifelse(day < 10, paste0("0", as.character(day)), 
                        as.character(day))) %>%
  mutate(date = paste(day, month, year, sep = "-"))

boulder_low$date <- as.Date(boulder_low$date, format = "%d-%m-%y")

boulder_low <- boulder_low %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(julian = yday(date)) %>%
  dplyr::select(year, julian, surface, soil)

# Change day of year 366 to day of year 1 of next year
boulder_low <- boulder_low %>%
  mutate(year = ifelse(julian == 366, (year + 1), year)) %>%
  mutate(julian = ifelse(julian == 366, 1, julian))

## Prep Boulder Gordon Gulch mid elevation data #############
## * Meteorological Tower 1 ##############

boulder_mid <- boulder_gg_met1 %>%
  dplyr::select(`Date Time`, `SOIL TEMP(C)-22CM`, `AIRTEMP(C)-2.5M(AVG)`) %>%
  dplyr::rename(date_time = `Date Time`, surface = `AIRTEMP(C)-2.5M(AVG)`, 
         soil = `SOIL TEMP(C)-22CM`) %>%
  mutate(surface = as.numeric(replace(surface, surface == "null", NA))) %>%
  mutate(soil = as.numeric(replace(soil, soil == "null", NA)))

# Parse date time
boulder_mid <- boulder_mid %>%
  separate(date_time, into = c("date", "time"), sep = " ")

boulder_mid$date <- as.Date(boulder_mid$date, format = "%m-%d-%Y")

boulder_mid <- boulder_mid %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(julian = yday(date)) %>%
  dplyr::select(-date, -time, -month)

## * Pole 3 air ############
boulder_mid3 <- boulder_gg_p3_air %>%
  dplyr::select(`Date Time`, `GGSD_1_AIRTEMP(C)`, `GGSD_2_AIRTEMP(C)`, `GGSD_3_AIRTEMP(C)`,
         `GGSD_4_AIRTEMP(C)`) %>%
  dplyr::rename(date_time = `Date Time`, surface = `GGSD_1_AIRTEMP(C)`, 
         surface_2 = `GGSD_2_AIRTEMP(C)`, surface_3 = `GGSD_3_AIRTEMP(C)`, surface_4 = `GGSD_4_AIRTEMP(C)`) %>%
  mutate(surface = as.numeric(replace(surface, surface == "null", NA))) %>%
  mutate(surface_2 = as.numeric(replace(surface_2, surface_2 == "null", NA))) %>%
  mutate(surface_3 = as.numeric(replace(surface_3, surface_3 == "null", NA))) %>%
  mutate(surface_4 = as.numeric(replace(surface_4, surface_4 == "null", NA)))
  
# Parse date time
boulder_mid3 <- boulder_mid3 %>%
  separate(date_time, into = c("date", "time"), sep = " ")

boulder_mid3$date <- as.Date(boulder_mid3$date, format = "%m-%d-%Y")

boulder_mid3 <- boulder_mid3 %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(julian = yday(date)) %>%
  dplyr::select(-date, -time, -month)

boulder_mid3_julian <- na.omit(boulder_mid3)
boulder_mid3_julian <- boulder_mid3_julian %>% 
  group_by(julian) %>%
  summarize(surface = mean(surface))

boulder_mid3test <- na.omit(boulder_mid3)


## * Pole 3 soil ##########
boulder_mid3 <- boulder_gg_p3_soil %>%
  dplyr::select(`DATE TIME`, `SR-TEMP(C)-5CM`, `SM-TEMP(C)-5CM`) %>%
  dplyr::rename(date_time = `DATE TIME`, soil = `SR-TEMP(C)-5CM`, 
         soil_2 = `SM-TEMP(C)-5CM`) %>%
  mutate(soil = as.numeric(replace(soil, soil == "null", NA))) %>%
  mutate(soil_2 = as.numeric(replace(soil_2, soil_2 == "null", NA)))

# Parse date time
boulder_mid3 <- boulder_mid3 %>%
  separate(date_time, into = c("date", "time"), sep = " ")

boulder_mid3$date <- as.Date(boulder_mid3$date, format = "%m-%d-%Y")

boulder_mid3 <- boulder_mid3 %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(julian = yday(date)) %>%
  dplyr::select(-date, -time, -month)

boulder_mid3_julian <- na.omit(boulder_mid3)

## * Pole 10 air ##############

boulder_mid10 <- boulder_gg_p10_air %>%
  dplyr::select(`Date Time`, `GGSD_10_Airtemp(C)`, `GGSD_11_Airtemp(C)`) %>%
  dplyr::rename(date_time = `Date Time`, surface = `GGSD_10_Airtemp(C)`, 
         surface_2 = `GGSD_11_Airtemp(C)`) %>%
  mutate(surface = as.numeric(replace(surface, surface == "null", NA))) %>%
  mutate(surface_2 = as.numeric(replace(surface_2, surface_2 == "null", NA)))
  
# Parse date time
boulder_mid10 <- boulder_mid10 %>%
  separate(date_time, into = c("date", "time"), sep = " ")

boulder_mid10$date <- as.Date(boulder_mid10$date, format = "%m-%d-%Y")

boulder_mid10 <- boulder_mid10 %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(julian = yday(date)) %>%
  dplyr::select(-date, -time, -month)

# plot tests. Commenting out so doesn't slow down processing
# boulder_mid10_julian <- na.omit(boulder_mid10)
# plot(air ~ julian, boulder_mid10_julian)

## * Pole 10 air ##############

boulder_mid10 <- boulder_gg_p4_air %>%
  dplyr::select(`Date Time`, `GGSD_5_AIRTEMP(C)`, `GGSD_6_AIRTEMP(C)`,
         `GGSD_7_AIRTEMP(C)`, `GGSD_8_AIRTEMP(C)`) %>%
  dplyr::rename(date_time = `Date Time`, surface = `GGSD_5_AIRTEMP(C)`, 
         surface_2 = `GGSD_6_AIRTEMP(C)`, surface_3 = `GGSD_7_AIRTEMP(C)`,
         surface_4 = `GGSD_8_AIRTEMP(C)`) %>%
  mutate(surface = as.numeric(replace(surface, surface == "null", NA))) %>%
  mutate(surface_2 = as.numeric(replace(surface_2, surface_2 == "null", NA))) %>%
  mutate(surface_3 = as.numeric(replace(surface_3, surface_3 == "null", NA))) %>%
  mutate(surface_4 = as.numeric(replace(surface_4, surface_4 == "null", NA)))

# Parse date time
boulder_mid10 <- boulder_mid10 %>%
  separate(date_time, into = c("date", "time"), sep = " ")

boulder_mid10$date <- as.Date(boulder_mid10$date, format = "%m-%d-%Y")

boulder_mid10 <- boulder_mid10 %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(julian = yday(date)) %>%
  dplyr::select(-date, -time, -month)

## Prep Green Lakes Valley high elevation data ##########
## * GLV soil data #############
boulder_glv_soil <- boulder_glv_soil_raw %>%
  dplyr::select(`DATE TIME`, `Pit1-TEMP(C)-15cm`, `Pit2-TEMP(C)-15cm`,
                `Pit3-TEMP(C)-15cm`, `Pit4-TEMP(C)-11cm`, `Pit5-TEMP(C)-12cm`) %>%
  dplyr::rename(date_time = `DATE TIME`, soil1 = `Pit1-TEMP(C)-15cm`,
                soil2 = `Pit2-TEMP(C)-15cm`, soil3 = `Pit3-TEMP(C)-15cm`,
                soil4 = `Pit4-TEMP(C)-11cm`, soil5 = `Pit5-TEMP(C)-12cm`) %>%
  mutate(soil1 = as.numeric(replace(soil1, soil1 == "null", NA))) %>%
  mutate(soil2 = as.numeric(replace(soil2, soil2 == "null", NA))) %>%
  mutate(soil3 = as.numeric(replace(soil3, soil3 == "null", NA))) %>%
  mutate(soil4 = as.numeric(replace(soil4, soil4 == "null", NA))) %>%
  mutate(soil5 = as.numeric(replace(soil5, soil5 == "null", NA)))


# Parse date time
boulder_glv_soil <- boulder_glv_soil %>%
  separate(date_time, into = c("date", "time"), sep = " ")

boulder_glv_soil$date <- as.Date(boulder_glv_soil$date, format = "%m-%d-%Y")

boulder_glv_soil <- boulder_glv_soil %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(julian = yday(date))

# Average soil temperature replicates
boulder_glv_soil <- boulder_glv_soil %>%
  mutate(soil = (soil1 + soil2 + soil3 + soil4 + soil5) / 5) %>%
  dplyr::select(year, julian, soil)

## * GLV air data ################

# GLV air data is structured differently, and is summarized to daily values 
boulder_glv_surface <- boulder_glv_air_raw %>%
  # Some data points were modeled because measurements were missing. This is 
  #   what the flag columns designate. For our purposes I htink it's ok to
  #   group measured and modeled data points together
 dplyr::select(date, airtemp_avg, airtemp_max, airtemp_min) %>%
  dplyr::rename(high_surface_mean = airtemp_avg, high_surface_max = airtemp_max, 
         high_surface_min = airtemp_min)

# Parse date time
boulder_glv_surface$date <- as.Date(boulder_glv_surface$date, format = "%m-%d-%Y")

boulder_glv_surface <- boulder_glv_surface %>%
  mutate(year = year(date)) %>%
  mutate(julian = yday(date)) %>%
  dplyr::select(-date) %>%
  filter(year > 2008)

# Reshape data to cater to prep_flux_data shape
boulder_glv_surface <- boulder_glv_surface %>%
  # gather() is such a magical function
  gather(key = "metric", value = "surface", high_surface_mean, high_surface_min, high_surface_max) %>%
  dplyr::select(-metric)

# Bind Green Valley Lakes high elevation air and soil together
boulder_glv_high <- boulder_glv_soil %>%
  left_join(boulder_glv_surface)

## 5. Wrangle snow data #########

source("scripts/00_source_code/noaa_query.R")

## ....A. High elevation snowdepth ##########
boulder_high_snow <- noaa_query( 
  stationid = "GHCND:USS0005K14S", # Station is BERTHOUD SUMMIT, CO US
  startdate = "2009-01-01", enddate = "2012-12-31",
  datatypeid = "SNWD")

# Curate NOAA query
boulder_high_snow <- boulder_high_snow %>% 
  mutate(date = as_date(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  dplyr::rename(snowdepth = value) %>% 
  # Currently in mm, switch to cm
  mutate(snowdepth = snowdepth / 10) %>% 
  mutate(elevation = "high") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

boulder_high_snow_avg <- boulder_high_snow %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE)) %>% 
  mutate(elevation = "high")

## ....B. Low elevation snowdepth ##########
boulder_low_snow <- noaa_query( 
  stationid = "GHCND:USC00050848", # Station is BOULDER, CO US
  startdate = "2009-01-01", enddate = "2012-12-31",
  datatypeid = "SNWD")

# Curate NOAA query
boulder_low_snow <- boulder_low_snow %>% 
  mutate(date = as_date(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  dplyr::rename(snowdepth = value) %>% 
  mutate(complete.cases(snowdepth)) %>% 
  # Currently in mm, switch to cm
  mutate(snowdepth = snowdepth / 10) %>% 
  mutate(elevation = "low") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

boulder_low_snow_avg <- boulder_low_snow %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE)) %>% 
  mutate(elevation = "low")

## ....C. Bind together high and low ##########

snowdepth <- boulder_high_snow %>%
  bind_rows(boulder_low_snow)

snowdepth_avg <- boulder_high_snow_avg %>%
  bind_rows(boulder_low_snow_avg)

# snowdepth <- boulder_high_snow %>% 
#   full_join(boulder_low_snow) %>% 
#   # Assuming an NA is 0, which is needed in order to have a fleshed-out timeseries
#   mutate(high_snowdepth = ifelse(is.na(high_snowdepth), 0, high_snowdepth)) %>% 
#   mutate(low_snowdepth = ifelse(is.na(low_snowdepth), 0, low_snowdepth)) %>% 
#   mutate(snowdepth = (low_snowdepth + high_snowdepth)/2)
# 
# snowdepth_avg <- boulder_high_snow_avg  %>% 
#   full_join(boulder_low_snow_avg ) %>% 
#   # Assuming an NA is 0, which is needed in order to have a fleshed-out timeseries
#   mutate(high_snowdepth = ifelse(is.na(high_snowdepth), 0, high_snowdepth)) %>% 
#   mutate(low_snowdepth = ifelse(is.na(low_snowdepth), 0, low_snowdepth)) %>% 
#   mutate(snowdepth = (low_snowdepth + high_snowdepth)/2)

## Conduct overlap data curation #############

source("scripts/00_source_code/data_curation_program.R")
boulder_list <- prep_flux_data(low_dataset = boulder_low, 
               high_dataset = boulder_glv_high)

for (i in 1:length(boulder_list)) {
  
  boulder_list[[i]] <- boulder_list[[i]] %>% 
    mutate(site = "CO") %>% 
    mutate(macro = "alpine meadow") %>% 
    mutate(foliage = "leaf-off") %>% 
    mutate(foliage_cover = 0) %>% 
    mutate(flora = "Scattered Pinus ponderosa, Pseudotsuga menziesii at low, largely barren at high") %>% 
    mutate(latitude = 40)
  
  if (i == 3 | i == 5) {
    
    boulder_list[[i]] <- boulder_list[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.12, "surface" = 2.0)) %>%
      mutate(height_notes = "from metadata") %>% 
      mutate(altitude = dplyr::recode(elevation, "low" = 1948, "high" = 3750))
  }
  
  # Add snow data
  if (i == 3) {
    # All years
    boulder_list[[i]] <- boulder_list[[i]] %>% 
      left_join(snowdepth) %>%
      # Remove infinite values
      filter(is.finite(min) & is.finite(max)) %>% 
      # Designate a flag for snow data quality
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "single_avg", 
                                       "measured_daily")) %>% 
      
      ## To fill in snowdepth NAs, pulling from WRCC
      # https://wrcc.dri.edu/cgi-bin/cliMAIN.pl?coboul
      # https://wrcc.dri.edu/cgi-bin/cliMAIN.pl?co3500
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 121 | julian > 305),
                                # average of Boulder and Grand Lake 6 SSW
                                (2.286 + 30)/2, snowdepth)) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & julian >= 121 & julian <= 305,
                                0, snowdepth))
  }
  if (i == 5) {
    # Avg years
    boulder_list[[i]] <- boulder_list[[i]] %>% 
      left_join(snowdepth_avg) %>% 
      # Designate a flag for snow data quality
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "single_avg", 
                                       "measured_daily")) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 121 | julian > 305),
                                # average of Boulder and Grand Lake 6 SSW
                                (2.286 + 30)/2, snowdepth)) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & julian >= 121 & julian <= 305,
                                0, snowdepth))
  }
}


## Write out data ##########

write_csv(boulder_list[[1]], "data/01_primary/temperate/CO_Boulder/derivative/Boulder_high_GLV_all_years.csv")
write_csv(boulder_list[[2]],  "data/01_primary/temperate/CO_Boulder/derivative/Boulder_low_met1_all_years.csv")
write_csv(boulder_list[[3]], "data/01_primary/temperate/CO_Boulder/derivative/Boulder_met1-GLV_tall.csv")
write_csv(boulder_list[[4]],  "data/01_primary/temperate/CO_Boulder/derivative/Boulder_met1-GLV_wide.csv")
write_csv(boulder_list[[5]], "data/01_primary/temperate/CO_Boulder/derivative/Boulder_met1-GLV_avgyears_tall.csv")
write_csv(boulder_list[[6]], "data/01_primary/temperate/CO_Boulder/derivative/Boulder_met1-GLV_avgyears_wide.csv")

