# David Klinges
# File creation date: 2019.06.13
# This script curates Tennessee Coweeta data for analysis and 
#   generating figures


## 1. Workspace prep #########

library(tidyverse)
library(lubridate)
library(scales)
library(imputeTS)

CWT_305_low_raw <- as.data.frame(read_csv("data/01_primary/temperate/TN_coweeta/original/CWT_305_low/CWT_305_1_1304.csv",
                                          skip = 2))

CWT_205_low_raw <- as.data.frame(read_csv("data/01_primary/temperate/TN_coweeta/original/CWT_205_low/CWT_205_1_1304.csv",
                                          skip = 2))

CWT_SM_high_raw <- as.data.frame(read_csv("data/01_primary/temperate/TN_coweeta/original/WS27_CWT_SM4_high/1040_5min_1_2080.csv",
                                    skip = 2))

CWT_136_high_raw <- as.data.frame(read_csv("data/01_primary/temperate/TN_coweeta/original/136_high/CWT_136_1_1309.csv",
                                          skip = 2))

CWT_236_high_raw <- as.data.frame(read_csv("data/01_primary/temperate/TN_coweeta/original/236_high/CWT_236_1_1309.csv",
                                           skip = 2))

CWT_336_high_raw <- as.data.frame(read_csv("data/01_primary/temperate/TN_coweeta/original/336_high/CWT_336_1_2618.csv",
                                           skip = 2))


# Import phenology data
spring <- read_csv("data/01_primary/temperate/TN_coweeta/original/phenology/Spring_1142_1_1142.csv",
                   skip = 2)
fall <- read_csv("data/01_primary/temperate/TN_coweeta/original/phenology/Fall_1142_1_1142.csv",
                 skip = 2)

## 2. Curate data ###########

## ....A. Low: CWT 305 ############
CWT_305_low <- CWT_305_low_raw %>% 
  mutate(date = as_date(Date)) %>% 
  # Just in case, but appears none are NA. Just removes two metadata rows at top
  filter(!is.na(date)) %>%   
  # There's some QA/QC flags built in, and they appear to accurately reflect
  # wonky temps. Let's remove
  filter(is.na(Flag_atemp)) %>% 
  # Coerce temps to numeric
  mutate(surface = as.numeric(atemp), soil_05 = as.numeric(stemp05), 
           soil_20 = as.numeric(stemp20)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date))  %>% 
  # There's a few days that surface temps jumped up dramatically with no soil jump, 
  # we'll NA those temps
  mutate(surface = ifelse(year == 2019 & julian < 15 & surface > 20, 
                      NA, surface)) %>% 
  # Remove julian day 366
  filter(julian != 366) %>% 
  dplyr::select(year, julian, surface, soil_05, soil_20)
           

## ....B. Low: CWT 205 ############
CWT_205_low <- CWT_205_low_raw %>% 
  mutate(date = as_date(Date)) %>% 
  # Just in case, but appears none are NA. Just removes two metadata rows at top
  filter(!is.na(date)) %>%   
  # There's some QA/QC flags built in, and they appear to accurately reflect
  # wonky temps. Let's remove
  filter(is.na(Flag_stemp05)) %>% 
  # Coerce temps to numeric
  mutate(surface = as.numeric(atemp), soil_05 = as.numeric(stemp05), 
         soil_20 = as.numeric(stemp20)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date))  %>% 
  # There's a few days that surface temps jumped up dramatically with no soil jump, 
  # we'll NA those temps
  mutate(surface = ifelse(year == 2019 & julian < 15 & surface > 20, 
                          NA, surface)) %>% 
  # Remove julian day 366
  filter(julian != 366) %>% 
  dplyr::select(year, julian, surface, soil_05, soil_20)


## ....C. High: CWT SM4 ############

CWT_SM4_high <- CWT_SM_high_raw %>% 
  filter(Site == "CWT_SM4") %>% 
  dplyr::select(Date, atemp100, Flag_atemp100, stemp05, Flag_stemp05, stemp20) %>% 
  mutate(date = as_date(Date)) %>% 
  filter(!is.na(date)) %>%  # Just in case, but appears none are NA
  # There's some QA/QC flags built in, and they appear to accurately reflect
  # wonky temps. Let's remove
  filter(is.na(Flag_atemp100)) %>% 
  filter(is.na(Flag_stemp05)) %>%
  # Coerce temps to numeric
  mutate(surface = as.numeric(atemp100), soil_05 = as.numeric(stemp05), 
         soil_20 = as.numeric(stemp20)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  # There's a few days that soil 5cm depth jumped upwards with no corresponding
  # jump in surface or soil 20. Let's convert to NA
  mutate(soil_05 = ifelse(julian > 320 & julian < 350 & soil_05 > 20, NA, soil_05)) %>% 
  # There's also a few days that surface temps jumped up dramatically, we'll remove
  # those too
  mutate(surface = ifelse(year == 2018 & julian > 150 & julian < 300 & surface > 32, 
                      NA, surface)) %>% 
  # Remove julian day 366
  filter(julian != 366) %>% 
  # All soil 20 temps seem to be reasonable, with some spikes corresponding to 
  # spikes seen in soil 05
  dplyr::select(year, julian, surface, soil_05, soil_20)

## ....D. High: CWT SM3 ############

CWT_SM3_high <- CWT_SM_high_raw %>% 
  filter(Site == "CWT_SM3") %>% 
  dplyr::select(Date, atemp100, Flag_atemp100, stemp05, Flag_stemp05, stemp20) %>% 
  mutate(date = as_date(Date)) %>% 
  filter(!is.na(date)) %>%  # Just in case, but appears none are NA
  # There's some QA/QC flags built in, and they appear to accurately reflect
  # wonky temps. Let's remove
  filter(is.na(Flag_atemp100)) %>% 
  filter(is.na(Flag_stemp05)) %>%
  # Coerce temps to numeric
  mutate(surface = as.numeric(atemp100), soil_05 = as.numeric(stemp05), 
         soil_20 = as.numeric(stemp20)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  # There's a few days that soil 5cm depth jumped upwards with no corresponding
  # jump in surface or soil 20. Let's convert to NA
  mutate(soil_05 = ifelse(julian > 320 & julian < 350 & soil_05 > 20, NA, soil_05)) %>% 
  # There's also a few days that surface temps jumped up dramatically, we'll remove
  # those too
  mutate(surface = ifelse(year == 2018 & julian > 150 & julian < 300 & surface > 32, 
                          NA, surface)) %>% 
  # Remove julian day 366
  filter(julian != 366) %>% 
  # All soil 20 temps seem to be reasonable, with some spikes corresponding to 
  # spikes seen in soil 05
  dplyr::select(year, julian, surface, soil_05, soil_20)

## ....E. High: 136 ############

CWT_136_high <- CWT_136_high_raw %>% 
  filter(Site == "CWT_136") %>% 
  dplyr::select(Date, atemp, Flag_atemp, stemp_surfaceA, stemp_surfaceB, 
                stemp05, Flag_stemp_surfaceB, Flag_stemp05, stemp20, Flag_stemp20) %>% 
  mutate(date = as_date(Date)) %>% 
  filter(!is.na(date)) %>%  # Just in case, but appears none are NA
  # There's some QA/QC flags built in, and they appear to accurately reflect
  # wonky temps. Let's remove
  filter(is.na(Flag_atemp)) %>% 
  filter(is.na(Flag_stemp05)) %>%
  filter(is.na(Flag_stemp20)) %>%
  filter(is.na(Flag_stemp_surfaceB)) %>%
  # Coerce temps to numeric
  mutate(surface = as.numeric(atemp), soil_05 = as.numeric(stemp05), 
         soil_20 = as.numeric(stemp20), soil_00A = as.numeric(stemp_surfaceA),
         soil_00B = as.numeric(stemp_surfaceB)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  # Average two surface soil loggers
  mutate(soil_00 = (soil_00A + soil_00B) / 2) %>% 
  # There's a few days that soil 5cm depth jumped upwards with no corresponding
  # jump in surface or soil 20. Let's convert to NA
  mutate(soil_05 = ifelse(julian > 80 & julian < 150 & 
                            (soil_05 > 15 | soil_05 < 10), NA, soil_05)) %>% 
  mutate(soil_20 = ifelse((julian < 80 | julian > 290) & 
                            (soil_20  > 20), NA, soil_20)) %>% 
  # Remove julian day 366
  filter(julian != 366) %>% 
  # All soil 20 temps seem to be reasonable, with some spikes corresponding to 
  # spikes seen in soil 05
  dplyr::select(year, julian, surface, soil_00, soil_05, soil_20)

## ....F. High: 236 ############

CWT_236_high <- CWT_236_high_raw %>% 
  filter(Site == "CWT_236") %>% 
  dplyr::select(Date, atemp, stemp05, stemp_surfaceA, Flag_stemp_surfaceA, stemp20) %>% 
  mutate(date = as_date(Date)) %>% 
  filter(!is.na(date)) %>%  # Just in case, but appears none are NA
  # There's some QA/QC flags built in, and they appear to accurately reflect
  # wonky temps. Let's remove
  filter(is.na(Flag_stemp_surfaceA)) %>%
  # Coerce temps to numeric
  mutate(surface = as.numeric(atemp), soil_05 = as.numeric(stemp05), 
         soil_20 = as.numeric(stemp20)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  
  # Remove julian day 366
  filter(julian != 366) %>% 
  # All soil 20 temps seem to be reasonable, with some spikes corresponding to 
  # spikes seen in soil 05
  dplyr::select(year, julian, surface, soil_05, soil_20)

## ....G. High: 336 ############

CWT_336_high <- CWT_336_high_raw %>% 
  filter(Site == "CWT_336") %>% 
  dplyr::select(Date, atemp, Flag_atemp, stemp05, Flag_stemp_surfaceA, stemp20) %>% 
  mutate(date = as_date(Date)) %>% 
  filter(!is.na(date)) %>%  # Just in case, but appears none are NA
  # There's some QA/QC flags built in, and they appear to accurately reflect
  # wonky temps. Let's remove
  filter(is.na(Flag_atemp)) %>% 
  filter(is.na(Flag_stemp_surfaceA)) %>%
  # Coerce temps to numeric
  mutate(surface = as.numeric(atemp), soil_05 = as.numeric(stemp05), 
         soil_20 = as.numeric(stemp20)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  
  # There's a few days that surface temps jumped upwards with no corresponding
  # jump in soil temps. Let's convert to NA
  mutate(surface = ifelse(julian > 160 & julian < 290 & 
                            (surface > 32), NA, surface)) %>% 

  # Remove julian day 366
  filter(julian != 366) %>% 
  # All soil 20 temps seem to be reasonable, with some spikes corresponding to 
  # spikes seen in soil 05
  dplyr::select(year, julian, surface, soil_05, soil_20)

## 3. Wrangle snow data #########

source("scripts/00_source_code/noaa_query.R")

## ....A. High elevation snowdepth ##########

# The only NOAA sensor in the area, and conveniently the only with snow 
# measurements, is pretty much right on to of our low site. We'll just have to 
# use for both low and high
coweeta_high_snow <- noaa_query( 
  stationid = "GHCND:USC00312102", # Station is COWEETA EXPERIMENT STATION, NC US
  startdate = "2015-01-01", enddate = "2019-12-31",
  datatypeid = "SNWD")

# Curate NOAA query
coweeta_high_snow <- coweeta_high_snow %>% 
  mutate(date = as_date(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  dplyr::rename(snowdepth = value) %>% 
  # Currently in mm, switch to cm
  mutate(snowdepth = snowdepth / 10) %>% 
  mutate(elevation = "high") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

coweeta_high_snow_avg <- coweeta_high_snow %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE)) %>% 
  mutate(elevation = "high")

## ....B. Low elevation snowdepth ##########
coweeta_low_snow <- coweeta_high_snow

coweeta_low_snow_avg <- coweeta_high_snow_avg

## ....C. Bind together high and low ##########

snowdepth <- coweeta_high_snow %>%
  bind_rows(coweeta_low_snow)

snowdepth_avg <- coweeta_high_snow_avg %>%
  bind_rows(coweeta_low_snow_avg)

## 4. Calculate foliage cover values #############

spring <- spring %>% 
  slice(3:n()) 

fall <- fall %>% 
  slice(3:n()) 

# Bud and leaf stage based on a rank of 1-5, with 1 being a dormant bud and 5 
# being a fully formed leaf
# "Stage 1 is a bud in its winter stage with no sign of expansion and 
# 5 is a fully elongated leaf"

spring_pheno <- spring %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(julian = yday(Date)) %>% 
  # Convert bud stage to number
  mutate(Bud_Stage = as.double(Bud_Stage)) %>% 
  # Get the summary for all species for each day
  group_by(julian) %>% 
  summarize(Bud_Stage = mean(Bud_Stage)) %>% 
  # Gap fill NAs
  mutate(lead = lead(Bud_Stage)) %>% 
  mutate(lag = lag(Bud_Stage)) %>% 
  mutate(Bud_Stage = ifelse(is.na(Bud_Stage), (lead + lag)/2, Bud_Stage)) %>% 
  # Scale leaf off to foliage values: they are positively correlated
  # Scale bud stages to foliage values: they are positively correlated
  # From visual inspection, the mountain is ~20% coniferous. Therefore, the 
  # low value for foliage should be 0.2 (in winter there's still 20% foliage)
  mutate(foliage_cover = scales::rescale(Bud_Stage, to = c(.2, 1))) %>% 
  dplyr::select(julian, foliage_cover)

fall_pheno <- fall %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(julian = yday(Date)) %>% 
  mutate(Leaf_Off = as.double(Leaf_Off)) %>% 
  # Get the summary for all species for each day
  group_by(julian) %>% 
  summarize(Leaf_Off = mean(Leaf_Off)) %>% 
  # Scale leaf off to foliage values: they are negatively correlated
  # Scale bud stages to foliage values: they are positively correlated
  # From visual inspection, the mountain is ~20% coniferous. Therefore, the 
  # low value for foliage should be 0.2 (in winter there's still 20% foliage)
  mutate(foliage_cover = scales::rescale(Leaf_Off, to = c(1, .2))) %>% 
  dplyr::select(julian, foliage_cover)

pheno <- spring_pheno %>% 
  bind_rows(fall_pheno)

## 5. Conduct overlap data curation #########

## ....A. 305, SM4, and depth == 5 #############
# CWT_305_low and CWT_SM4_high, soil depth == 5
low <- CWT_305_low %>% 
  dplyr::rename(soil = soil_05) %>% 
  dplyr::select(-soil_20)

high <- CWT_SM4_high %>% 
  dplyr::rename(soil = soil_05) %>% 
  dplyr::select(-soil_20)

source("scripts/00_source_code/data_curation_program.R")
coweeta_list_305_SM4_D5 <- prep_flux_data(low_dataset = low, 
                                  high_dataset = high)

for (i in 1:length(coweeta_list_305_SM4_D5)) {
  
  coweeta_list_305_SM4_D5[[i]] <- coweeta_list_305_SM4_D5[[i]] %>% 
    mutate(site = "TN") %>% 
    mutate(macro = "deciduous") %>% 
    mutate(foliage = ifelse(julian < 138 | julian > 289, "leaf-off", "leaf-on")) %>% 
    left_join(pheno) %>% 
    # For all days before first and after later measurement, set as floor value
    mutate(foliage_cover = ifelse(julian < min(spring_pheno$julian), 
                                  min(pheno$foliage_cover),
                                  ifelse(julian > max(fall_pheno$julian),
                                         min(pheno$foliage_cover), foliage_cover))) %>% 
    mutate(flora = "mixed deciduous, hardwoods") %>% 
    mutate(latitude = 35) %>% 
    mutate(snow = "no snow")
  
  if (i == 3 | i == 5) {
    
    coweeta_list_305_SM4_D5[[i]] <- coweeta_list_305_SM4_D5[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.05, "surface" = 1)) %>% 
      mutate(height_notes = "from metadata") %>%
      mutate(altitude = dplyr::recode(elevation, "low" = 683.1, "high" = 1382))
  }
}

## ....B. 305, SM4, and depth == 20 #############

# CWT_305_low and CWT_SM4_high, soil depth == 20
low <- CWT_305_low %>% 
  dplyr::rename(soil = soil_20) %>% 
  dplyr::select(-soil_05)

high <- CWT_SM4_high %>% 
  dplyr::rename(soil = soil_20) %>% 
  dplyr::select(-soil_05)

coweeta_list_305_SM4_D20 <- prep_flux_data(low_dataset = low, 
                               high_dataset = high)


for (i in 1:length(coweeta_list_305_SM4_D20)) {
  
  coweeta_list_305_SM4_D20[[i]] <- coweeta_list_305_SM4_D20[[i]] %>% 
    mutate(site = "TN") %>% 
    mutate(macro = "deciduous") %>% 
    mutate(foliage = ifelse(julian < 138 | julian > 289, "leaf-off", "leaf-on")) %>% 
    left_join(pheno) %>% 
    # For all days before first and after later measurement, set as floor value
    mutate(foliage_cover = ifelse(julian < min(spring_pheno$julian), 
                                  min(pheno$foliage_cover),
                                  ifelse(julian > max(fall_pheno$julian),
                                         min(pheno$foliage_cover), foliage_cover))) %>% 
    mutate(flora = "mixed deciduous, hardwoods") %>% 
    mutate(latitude = 35) %>% 
    mutate(snow = "no snow")
  
  if (i == 3 | i == 5) {
    
    coweeta_list_305_SM4_D20[[i]] <- coweeta_list_305_SM4_D20[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.2, "surface" = 1)) %>% 
      mutate(height_notes = "from metadata") %>%
      mutate(altitude = dplyr::recode(elevation, "low" = 683.1, "high" = 1382))
  }
}

## ....C. 305, SM3, and depth == 5 #############


# CWT_305_low and CWT_SM3_high, soil depth == 5
low <- CWT_305_low %>% 
  dplyr::rename(soil = soil_05) %>% 
  dplyr::select(-soil_20)

high <- CWT_SM3_high %>% 
  dplyr::rename(soil = soil_05) %>% 
  dplyr::select(-soil_20)

coweeta_list_305_SM3_D5 <- prep_flux_data(low_dataset = low, 
                                  high_dataset = high)

for (i in 1:length(coweeta_list_305_SM3_D5)) {
  
  coweeta_list_305_SM3_D5[[i]] <- coweeta_list_305_SM3_D5[[i]] %>% 
    mutate(site = "TN") %>% 
    mutate(macro = "deciduous") %>% 
    mutate(foliage = ifelse(julian < 138 | julian > 289, "leaf-off", "leaf-on")) %>%
    left_join(pheno) %>% 
    # For all days before first and after later measurement, set as floor value
    mutate(foliage_cover = ifelse(julian < min(spring_pheno$julian), 
                                  min(pheno$foliage_cover),
                                  ifelse(julian > max(fall_pheno$julian),
                                         min(pheno$foliage_cover), foliage_cover))) %>% 
  
    mutate(flora = "mixed deciduous, hardwoods") %>% 
    mutate(latitude = 35) %>% 
    mutate(snow = "no snow")
  
  if (i == 3 | i == 5) {
    
    coweeta_list_305_SM3_D5[[i]] <- coweeta_list_305_SM3_D5[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.05, "surface" = 1)) %>% 
      mutate(height_notes = "from metadata") %>%
      # NOTE: did not find exact coordinates so could not calculate exact elevation.
      # Top of watershed 36 is 1542, assuming logger is ~ 100 below top (as SM4
      # logger is about 100m from top of WS27)
      mutate(altitude = dplyr::recode(elevation, "low" = 683.1, "high" = 1440))
  }
}

## ....D. 305, SM3, and depth == 20 #############


# CWT_305_low and CWT_SM4_high, soil depth == 20
low <- CWT_305_low %>% 
  dplyr::rename(soil = soil_20) %>% 
  dplyr::select(-soil_05)

high <- CWT_SM3_high %>% 
  dplyr::rename(soil = soil_20) %>% 
  dplyr::select(-soil_05)

coweeta_list_305_SM3_D20 <- prep_flux_data(low_dataset = low, 
                                  high_dataset = high)

for (i in 1:length(coweeta_list_305_SM3_D20)) {
  
  coweeta_list_305_SM3_D20[[i]] <- coweeta_list_305_SM3_D20[[i]] %>% 
    mutate(site = "TN") %>% 
    mutate(macro = "deciduous") %>% 
    mutate(foliage = ifelse(julian < 138 | julian > 289, "leaf-off", "leaf-on")) %>% 
    left_join(pheno) %>% 
    # For all days before first and after later measurement, set as floor value
    mutate(foliage_cover = ifelse(julian < min(spring_pheno$julian), 
                                  min(pheno$foliage_cover),
                                  ifelse(julian > max(fall_pheno$julian),
                                         min(pheno$foliage_cover), foliage_cover))) %>% 
    
    mutate(flora = "mixed deciduous, hardwoods") %>% 
    mutate(latitude = 35) %>% 
    mutate(snow = "no snow")
  
  if (i == 3 | i == 5) {
    
    coweeta_list_305_SM3_D20[[i]] <- coweeta_list_305_SM3_D20[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.2, "surface" = 1)) %>% 
      mutate(height_notes = "from metadata") %>%
      mutate(altitude = dplyr::recode(elevation, "low" = 683.1, "high" = 1440))
  }
}

## ....E. 305, 136, and depth == 5 #############


# CWT_305_low and CWT_136_high, soil depth == 5
low <- CWT_305_low %>% 
  dplyr::rename(soil = soil_05) %>% 
  dplyr::select(-soil_20)

high <- CWT_136_high %>% 
  dplyr::rename(soil = soil_05) %>% 
  dplyr::select(-soil_20, -soil_00)

coweeta_list_305_136_D5 <- prep_flux_data(low_dataset = low, 
                                      high_dataset = high)

for (i in 1:length(coweeta_list_305_136_D5)) {
  
  coweeta_list_305_136_D5[[i]] <- coweeta_list_305_136_D5[[i]] %>% 
    mutate(site = "TN") %>% 
    mutate(macro = "deciduous") %>% 
    mutate(foliage = ifelse(julian < 138 | julian > 289, "leaf-off", "leaf-on")) %>% 
    left_join(pheno) %>% 
    # For all days before first and after later measurement, set as floor value
    mutate(foliage_cover = ifelse(julian < min(spring_pheno$julian), 
                                  min(pheno$foliage_cover),
                                  ifelse(julian > max(fall_pheno$julian),
                                         min(pheno$foliage_cover), foliage_cover))) %>% 
    
    mutate(flora = "mixed deciduous, hardwoods") %>% 
    mutate(latitude = 35) %>% 
    mutate(snow = "no snow")
  
  if (i == 3 | i == 5) {
    
    coweeta_list_305_136_D5[[i]] <- coweeta_list_305_136_D5[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.05, "surface" = 1)) %>% 
      mutate(height_notes = "from metadata") %>%
      mutate(altitude = dplyr::recode(elevation, "low" = 683.1, "high" = 1271.1))
  }
}

## ....F. 305, 136, and depth == 20 #############


# CWT_305_low and CWT_SM4_high, soil depth == 20
low <- CWT_305_low %>% 
  dplyr::rename(soil = soil_20) %>% 
  dplyr::select(-soil_05)

high <- CWT_136_high %>% 
  dplyr::rename(soil = soil_20) %>% 
  dplyr::select(-soil_05, -soil_00)

coweeta_list_305_136_D20 <- prep_flux_data(low_dataset = low, 
                                       high_dataset = high)

for (i in 1:length(coweeta_list_305_136_D20)) {
  
  coweeta_list_305_136_D20[[i]] <- coweeta_list_305_136_D20[[i]] %>% 
    mutate(site = "TN") %>% 
    mutate(macro = "deciduous") %>% 
    mutate(foliage = ifelse(julian < 138 | julian > 289, "leaf-off", "leaf-on")) %>% 
    left_join(pheno) %>% 
    # For all days before first and after later measurement, set as floor value
    mutate(foliage_cover = ifelse(julian < min(spring_pheno$julian), 
                                  min(pheno$foliage_cover),
                                  ifelse(julian > max(fall_pheno$julian),
                                         min(pheno$foliage_cover), foliage_cover))) %>% 
    
    mutate(flora = "mixed deciduous, hardwoods") %>% 
    mutate(latitude = 35) %>% 
    mutate(snow = "no snow")
  
  if (i == 3 | i == 5) {
    
    coweeta_list_305_136_D20[[i]] <- coweeta_list_305_136_D20[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.2, "surface" = 1)) %>% 
      mutate(height_notes = "from metadata") %>%
      mutate(altitude = dplyr::recode(elevation, "low" = 683.1, "high" = 1271.1))
  }
}

## ....G. 305, 236, and depth == 5 #############

# CWT_305_low and CWT_236_high, soil depth == 5
low <- CWT_305_low %>% 
  dplyr::rename(soil = soil_05) %>% 
  dplyr::select(-soil_20)

high <- CWT_236_high %>% 
  dplyr::rename(soil = soil_05) %>% 
  dplyr::select(-soil_20)


coweeta_list_305_236_D5 <- prep_flux_data(low_dataset = low, 
                                      high_dataset = high)


for (i in 1:length(coweeta_list_305_236_D5)) {
  
  coweeta_list_305_236_D5[[i]] <- coweeta_list_305_236_D5[[i]] %>% 
    mutate(site = "TN") %>% 
    mutate(macro = "deciduous") %>% 
    mutate(foliage = ifelse(julian < 138 | julian > 289, "leaf-off", "leaf-on")) %>% 
    left_join(pheno) %>% 
    # For all days before first and after later measurement, set as floor value
    mutate(foliage_cover = ifelse(julian < min(spring_pheno$julian), 
                                  min(pheno$foliage_cover),
                                  ifelse(julian > max(fall_pheno$julian),
                                         min(pheno$foliage_cover), foliage_cover))) %>% 
    
    mutate(flora = "mixed deciduous, hardwoods") %>% 
    mutate(latitude = 35) %>% 
    mutate(snow = "no snow")
  
  if (i == 3 | i == 5) {
    
    coweeta_list_305_236_D5[[i]] <- coweeta_list_305_236_D5[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.05, "surface" = 1)) %>% 
      mutate(height_notes = "from metadata") %>%
      mutate(altitude = dplyr::recode(elevation, "low" = 683.1, "high" = 1134.5))
  }
}

## ....H. 305, 236, and depth == 20 #############


# CWT_305_low and CWT_SM4_high, soil depth == 20
low <- CWT_305_low %>% 
  dplyr::rename(soil = soil_20) %>% 
  dplyr::select(-soil_05)

high <- CWT_236_high %>% 
  dplyr::rename(soil = soil_20) %>% 
  dplyr::select(-soil_05)


coweeta_list_305_236_D20 <- prep_flux_data(low_dataset = low, 
                                       high_dataset = high)

for (i in 1:length(coweeta_list_305_236_D20)) {
  
  coweeta_list_305_236_D20[[i]] <- coweeta_list_305_236_D20[[i]] %>% 
    mutate(site = "TN") %>% 
    mutate(macro = "deciduous") %>% 
    mutate(foliage = ifelse(julian < 138 | julian > 289, "leaf-off", "leaf-on")) %>% 
    left_join(pheno) %>% 
    # For all days before first and after later measurement, set as floor value
    mutate(foliage_cover = ifelse(julian < min(spring_pheno$julian), 
                                  min(pheno$foliage_cover),
                                  ifelse(julian > max(fall_pheno$julian),
                                         min(pheno$foliage_cover), foliage_cover))) %>% 
    
    mutate(flora = "mixed deciduous, hardwoods") %>% 
    mutate(latitude = 35) %>% 
    mutate(snow = "no snow")
  
  if (i == 3 | i == 5) {
    
    coweeta_list_305_236_D20[[i]] <- coweeta_list_305_236_D20[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.2, "surface" = 1)) %>% 
      mutate(height_notes = "from metadata") %>%
      mutate(altitude = dplyr::recode(elevation, "low" = 683.1, "high" = 1134.5))
  }
}

## ....I. 305, 336, and depth == 5 #############


# CWT_305_low and CWT_336_high, soil depth == 5
low <- CWT_305_low %>% 
  dplyr::rename(soil = soil_05) %>% 
  dplyr::select(-soil_20)

high <- CWT_336_high %>% 
  dplyr::rename(soil = soil_05) %>% 
  dplyr::select(-soil_20)


coweeta_list_305_336_D5 <- prep_flux_data(low_dataset = low, 
                                      high_dataset = high)

for (i in 1:length(coweeta_list_305_336_D5)) {
  
  coweeta_list_305_336_D5[[i]] <- coweeta_list_305_336_D5[[i]] %>% 
    mutate(site = "TN") %>% 
    mutate(macro = "deciduous") %>% 
    mutate(foliage = ifelse(julian < 138 | julian > 289, "leaf-off", "leaf-on")) %>% 
    left_join(pheno) %>% 
    # For all days before first and after later measurement, set as floor value
    mutate(foliage_cover = ifelse(julian < min(spring_pheno$julian), 
                                  min(pheno$foliage_cover),
                                  ifelse(julian > max(fall_pheno$julian),
                                         min(pheno$foliage_cover), foliage_cover))) %>% 
    
    mutate(flora = "mixed deciduous, hardwoods") %>% 
    mutate(latitude = 35) %>% 
    mutate(snow = "no snow")
  
  if (i == 3 | i == 5) {
    
    coweeta_list_305_336_D5[[i]] <- coweeta_list_305_336_D5[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.05, "surface" = 1.00)) %>% 
      mutate(height_notes = "from metadata") %>%
      mutate(altitude = dplyr::recode(elevation, "low" = 683.1, "high" = 1059.3))
  }
}

## ....J. 305, 336, and depth == 20 #############


# CWT_305_low and CWT_SM4_high, soil depth == 20
low <- CWT_305_low %>% 
  dplyr::rename(soil = soil_20) %>% 
  dplyr::select(-soil_05)

high <- CWT_336_high %>% 
  dplyr::rename(soil = soil_20) %>% 
  dplyr::select(-soil_05)


coweeta_list_305_336_D20 <- prep_flux_data(low_dataset = low, 
                                       high_dataset = high)


for (i in 1:length(coweeta_list_305_336_D20)) {
  
  coweeta_list_305_336_D20[[i]] <- coweeta_list_305_336_D20[[i]] %>% 
    mutate(site = "TN") %>% 
    mutate(macro = "deciduous") %>% 
    mutate(foliage = ifelse(julian < 138 | julian > 289, "leaf-off", "leaf-on")) %>%
    left_join(pheno) %>% 
    # For all days before first and after later measurement, set as floor value
    mutate(foliage_cover = ifelse(julian < min(spring_pheno$julian), 
                                  min(pheno$foliage_cover),
                                  ifelse(julian > max(fall_pheno$julian),
                                         min(pheno$foliage_cover), foliage_cover))) %>% 
    
    mutate(flora = "mixed deciduous, hardwoods") %>% 
    mutate(latitude = 35) %>% 
    mutate(snow = "no snow")
  
  if (i == 3 | i == 5) {
    
    coweeta_list_305_336_D20[[i]] <- coweeta_list_305_336_D20[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -.2, "surface" = 1)) %>% 
      mutate(height_notes = "from metadata") %>%
      mutate(altitude = dplyr::recode(elevation, "low" = 683.1, "high" = 1059.3))
  }
}

## ....K. 205, 136, and depth == 5 #############


# CWT_205_low and CWT_136_high, soil depth == 5
low <- CWT_205_low %>% 
  dplyr::rename(soil = soil_05) %>% 
  dplyr::select(-soil_20)

high <- CWT_136_high %>% 
  dplyr::rename(soil = soil_05) %>% 
  dplyr::select(-soil_20, -soil_00)


coweeta_list_205_136_D5 <- prep_flux_data(low_dataset = low, 
                                      high_dataset = high)


for (i in 1:length(coweeta_list_205_136_D5)) {
  
  coweeta_list_205_136_D5[[i]] <- coweeta_list_205_136_D5[[i]] %>% 
    mutate(site = "TN") %>% 
    mutate(macro = "deciduous") %>% 
    mutate(foliage = ifelse(julian < 138 | julian > 289, "leaf-off", "leaf-on")) %>% 
    left_join(pheno) %>% 
    # For all days before first and after later measurement, set as floor value
    mutate(foliage_cover = ifelse(julian < min(spring_pheno$julian), 
                                  min(pheno$foliage_cover),
                                  ifelse(julian > max(fall_pheno$julian),
                                         min(pheno$foliage_cover), foliage_cover))) %>% 
    
    mutate(flora = "mixed deciduous, hardwoods") %>% 
    mutate(latitude = 35) %>% 
    mutate(snow = "no snow")
  
  if (i == 3 | i == 5) {
    
    coweeta_list_205_136_D5[[i]] <- coweeta_list_205_136_D5[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -.05, "surface" = 1)) %>% 
      mutate(height_notes = "from metadata") %>%
      mutate(altitude = dplyr::recode(elevation, "low" = 735.3, "high" = 1271.1))
  }
}

## ....L. 205, SM4, and depth == 5 #############
# CWT_205_low and CWT_SM4_high, soil depth == 5
low <- CWT_205_low %>% 
  dplyr::rename(soil = soil_05) %>% 
  dplyr::select(-soil_20)

high <- CWT_SM4_high %>% 
  dplyr::rename(soil = soil_05) %>% 
  dplyr::select(-soil_20)


coweeta_list_205_SM4_D5 <- prep_flux_data(low_dataset = low, 
                                          high_dataset = high)

for (i in 1:length(coweeta_list_205_SM4_D5)) {
  
  coweeta_list_205_SM4_D5[[i]] <- coweeta_list_205_SM4_D5[[i]] %>% 
    mutate(site = "TN") %>% 
    mutate(macro = "deciduous") %>% 
    mutate(foliage = ifelse(julian < 138 | julian > 289, "leaf-off", "leaf-on")) %>% 
    left_join(pheno) %>% 
    # For all days before first and after later measurement, set as floor value
    mutate(foliage_cover = ifelse(julian < min(spring_pheno$julian), 
                                  min(pheno$foliage_cover),
                                  ifelse(julian > max(fall_pheno$julian),
                                         min(pheno$foliage_cover), foliage_cover))) %>% 
    
    mutate(flora = "mixed deciduous, hardwoods") %>% 
    mutate(latitude = 35) %>% 
    mutate(snow = "no snow")
  
  if (i == 3 | i == 5) {
    
    coweeta_list_205_SM4_D5[[i]] <- coweeta_list_205_SM4_D5[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.05, "surface" = 1)) %>% 
      mutate(height_notes = "from metadata") %>%
      mutate(altitude = dplyr::recode(elevation, "low" = 735.3, "high" = 1382))
  }
}

## ....M. 205, SM3, and depth == 5: SITES USED GOING FORWARD IN KLINGES AND SCHEFFERS AmNat #############


# CWT_205_low and CWT_SM3_high, soil depth == 5
low <- CWT_205_low %>% 
  dplyr::rename(soil = soil_05) %>% 
  dplyr::select(-soil_20)

high <- CWT_SM3_high %>% 
  dplyr::rename(soil = soil_05) %>% 
  dplyr::select(-soil_20)

source("scripts/00_source_code/data_curation_program.R")
coweeta_list_205_SM3_D5 <- prep_flux_data(low_dataset = low, 
                                          high_dataset = high)


for (i in 1:length(coweeta_list_205_SM3_D5)) {
  
  coweeta_list_205_SM3_D5[[i]] <- coweeta_list_205_SM3_D5[[i]] %>% 
    mutate(site = "TN") %>% 
    mutate(macro = "deciduous") %>% 
    mutate(foliage = ifelse(julian < 138 | julian > 289, "leaf-off", "leaf-on")) %>% 
    left_join(pheno) %>% 
    # For all days before first and after later measurement, set as floor value
    mutate(foliage_cover = ifelse(julian < min(spring_pheno$julian), 
                                  min(pheno$foliage_cover),
                                  ifelse(julian > max(fall_pheno$julian),
                                         min(pheno$foliage_cover), foliage_cover))) %>% 
    # Gap-fill
    mutate(foliage_cover = na_interpolation(foliage_cover, option = "linear")) %>% 
    
    mutate(flora = "mixed deciduous, hardwoods") %>% 
    mutate(latitude = 35)
  
  if (i == 3 | i == 5) {
    
    coweeta_list_205_SM3_D5[[i]] <- coweeta_list_205_SM3_D5[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.05, "surface" = 1)) %>% 
      mutate(height_notes = "from metadata") %>%
      mutate(altitude = dplyr::recode(elevation, "low" = 735.3, "high" = 1440))
  }
  
  ## Add snow data
  if (i == 3) {
    # For all years
    coweeta_list_205_SM3_D5[[i]] <- coweeta_list_205_SM3_D5[[i]] %>% 
      left_join(snowdepth) %>% 
      # Evidence seems pretty clear: there's no snow here.
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "confident_estimate", 
                                       "measured_daily")) %>% 
      # No observation shows snow. 
      ## Looking at WRCC suggests the same
      # https://wrcc.dri.edu/cgi-bin/cliMAIN.pl?tn4871
      mutate(snowdepth = ifelse(is.na(snowdepth), 0, 
                                snowdepth))
      
  }
  
  if (i == 5) {
    # For average years
    coweeta_list_205_SM3_D5[[i]] <- coweeta_list_205_SM3_D5[[i]] %>% 
      left_join(snowdepth_avg) %>% 
      # Evidence seems pretty clear: there's no snow here.
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "confident_estimate", 
                                       "measured_daily")) %>% 
      # No observation shows snow. 
      ## Looking at WRCC suggests the same
      # https://wrcc.dri.edu/cgi-bin/cliMAIN.pl?tn4871
      mutate(snowdepth = ifelse(is.na(snowdepth), 0, 
                                snowdepth))
  }
}

## 6. Write out data ##########

write_csv(coweeta_list_305_SM4_D5[[1]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM4_D5_high_all_years.csv")
write_csv(coweeta_list_305_SM4_D5[[2]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM4_D5_low_all_years.csv")
write_csv(coweeta_list_305_SM4_D5[[3]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM4_D5_tall.csv")
write_csv(coweeta_list_305_SM4_D5[[4]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM4_D5_wide.csv")
write_csv(coweeta_list_305_SM4_D5[[5]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM4_D5_avgyears_tall.csv")
write_csv(coweeta_list_305_SM4_D5[[6]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM4_D5_avgyears_wide.csv")

write_csv(coweeta_list_305_SM4_D20[[1]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM4_D20_high_all_years.csv")
write_csv(coweeta_list_305_SM4_D20[[2]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM4_D20_low_all_years.csv")
write_csv(coweeta_list_305_SM4_D20[[3]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM4_D20_tall.csv")
write_csv(coweeta_list_305_SM4_D20[[4]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM4_D20_wide.csv")
write_csv(coweeta_list_305_SM4_D20[[5]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM4_D20_avgyears_tall.csv")
write_csv(coweeta_list_305_SM4_D20[[6]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM4_D20_avgyears_wide.csv")

write_csv(coweeta_list_305_SM3_D5[[1]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM3_D5_high_all_years.csv")
write_csv(coweeta_list_305_SM3_D5[[2]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM3_D5_low_all_years.csv")
write_csv(coweeta_list_305_SM3_D5[[3]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM3_D5_tall.csv")
write_csv(coweeta_list_305_SM3_D5[[4]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM3_D5_wide.csv")
write_csv(coweeta_list_305_SM3_D5[[5]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM3_D5_avgyears_tall.csv")
write_csv(coweeta_list_305_SM3_D5[[6]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM3_D5_avgyears_wide.csv")

write_csv(coweeta_list_305_SM3_D20[[1]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM3_D20_high_all_years.csv")
write_csv(coweeta_list_305_SM3_D20[[2]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM3_D20_low_all_years.csv")
write_csv(coweeta_list_305_SM3_D20[[3]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM3_D20_tall.csv")
write_csv(coweeta_list_305_SM3_D20[[4]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM3_D20_wide.csv")
write_csv(coweeta_list_305_SM3_D20[[5]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM3_D20_avgyears_tall.csv")
write_csv(coweeta_list_305_SM3_D20[[6]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_SM3_D20_avgyears_wide.csv")

write_csv(coweeta_list_305_136_D5[[1]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_136_D5_high_all_years.csv")
write_csv(coweeta_list_305_136_D5[[2]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_136_D5_low_all_years.csv")
write_csv(coweeta_list_305_136_D5[[3]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_136_D5_tall.csv")
write_csv(coweeta_list_305_136_D5[[4]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_136_D5_wide.csv")
write_csv(coweeta_list_305_136_D5[[5]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_136_D5_avgyears_tall.csv")
write_csv(coweeta_list_305_136_D5[[6]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_136_D5_avgyears_wide.csv")

write_csv(coweeta_list_305_136_D20[[1]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_136_D20_high_all_years.csv")
write_csv(coweeta_list_305_136_D20[[2]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_136_D20_low_all_years.csv")
write_csv(coweeta_list_305_136_D20[[3]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_136_D20_tall.csv")
write_csv(coweeta_list_305_136_D20[[4]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_136_D20_wide.csv")
write_csv(coweeta_list_305_136_D20[[5]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_136_D20_avgyears_tall.csv")
write_csv(coweeta_list_305_136_D20[[6]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_136_D20_avgyears_wide.csv")

write_csv(coweeta_list_305_236_D5[[1]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_236_D5_high_all_years.csv")
write_csv(coweeta_list_305_236_D5[[2]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_236_D5_low_all_years.csv")
write_csv(coweeta_list_305_236_D5[[3]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_236_D5_tall.csv")
write_csv(coweeta_list_305_236_D5[[4]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_236_D5_wide.csv")
write_csv(coweeta_list_305_236_D5[[5]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_236_D5_avgyears_tall.csv")
write_csv(coweeta_list_305_236_D5[[6]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_236_D5_avgyears_wide.csv")

write_csv(coweeta_list_305_236_D20[[1]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_236_D20_high_all_years.csv")
write_csv(coweeta_list_305_236_D20[[2]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_236_D20_low_all_years.csv")
write_csv(coweeta_list_305_236_D20[[3]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_236_D20_tall.csv")
write_csv(coweeta_list_305_236_D20[[4]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_236_D20_wide.csv")
write_csv(coweeta_list_305_236_D20[[5]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_236_D20_avgyears_tall.csv")
write_csv(coweeta_list_305_236_D20[[6]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_236_D20_avgyears_wide.csv")

write_csv(coweeta_list_305_336_D5[[1]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_336_D5_high_all_years.csv")
write_csv(coweeta_list_305_336_D5[[2]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_336_D5_low_all_years.csv")
write_csv(coweeta_list_305_336_D5[[3]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_336_D5_tall.csv")
write_csv(coweeta_list_305_336_D5[[4]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_336_D5_wide.csv")
write_csv(coweeta_list_305_336_D5[[5]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_336_D5_avgyears_tall.csv")
write_csv(coweeta_list_305_336_D5[[6]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_336_D5_avgyears_wide.csv")

write_csv(coweeta_list_305_336_D20[[1]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_336_D20_high_all_years.csv")
write_csv(coweeta_list_305_336_D20[[2]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_336_D20_low_all_years.csv")
write_csv(coweeta_list_305_336_D20[[3]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_336_D20_tall.csv")
write_csv(coweeta_list_305_336_D20[[4]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_336_D20_wide.csv")
write_csv(coweeta_list_305_336_D20[[5]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_336_D20_avgyears_tall.csv")
write_csv(coweeta_list_305_336_D20[[6]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_305_336_D20_avgyears_wide.csv")

write_csv(coweeta_list_205_136_D5[[1]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_205_136_D5_high_all_years.csv")
write_csv(coweeta_list_205_136_D5[[2]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_205_136_D5_low_all_years.csv")
write_csv(coweeta_list_205_136_D5[[3]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_205_136_D5_tall.csv")
write_csv(coweeta_list_205_136_D5[[4]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_205_136_D5_wide.csv")
write_csv(coweeta_list_205_136_D5[[5]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_205_136_D5_avgyears_tall.csv")
write_csv(coweeta_list_205_136_D5[[6]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_205_136_D5_avgyears_wide.csv")

write_csv(coweeta_list_205_SM4_D5[[1]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_205_SM4_D5_high_all_years.csv")
write_csv(coweeta_list_205_SM4_D5[[2]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_205_SM4_D5_low_all_years.csv")
write_csv(coweeta_list_205_SM4_D5[[3]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_205_SM4_D5_tall.csv")
write_csv(coweeta_list_205_SM4_D5[[4]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_205_SM4_D5_wide.csv")
write_csv(coweeta_list_205_SM4_D5[[5]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_205_SM4_D5_avgyears_tall.csv")
write_csv(coweeta_list_205_SM4_D5[[6]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_205_SM4_D5_avgyears_wide.csv")

write_csv(coweeta_list_205_SM3_D5[[1]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_205_SM3_D5_high_all_years.csv")
write_csv(coweeta_list_205_SM3_D5[[2]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_205_SM3_D5_low_all_years.csv")
write_csv(coweeta_list_205_SM3_D5[[3]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_205_SM3_D5_tall.csv")
write_csv(coweeta_list_205_SM3_D5[[4]],  "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_205_SM3_D5_wide.csv")
write_csv(coweeta_list_205_SM3_D5[[5]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_205_SM3_D5_avgyears_tall.csv")
write_csv(coweeta_list_205_SM3_D5[[6]], "data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_205_SM3_D5_avgyears_wide.csv")
