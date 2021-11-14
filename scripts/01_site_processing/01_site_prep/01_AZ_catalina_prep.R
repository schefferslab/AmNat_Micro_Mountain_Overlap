# David Klinges
# File creation date: 2019.08.23
# This script curates CZO Catalina-Jemez forest sites to 
#   prep for analysis and generating figures

## 1. Workspace prep ###########

library(tidyverse)
library(lubridate)

# Pits 1-3 are the lowest elevation, but pit 1 is deeper depth, so using 2 and 3
low_soil_pit1 <- read_csv("data/01_primary/temperate/AZ_santa_catelina/Oracle_mid/or_SM_2010.csv")

oracle_soil_low_list <- c(list.files(path = "data/01_primary/temperate/AZ_santa_catelina/Oracle_mid/soil",
                                    # All of the data files are in their own subdirectories
                                    #   so need to be recursive
                                    pattern = ".csv", recursive = TRUE))

# Import all the data and bind together
for (i in 1:length(oracle_soil_low_list)) {
  
  # Read in data
  iter_data <- read_csv(paste0(getwd(), 
                               "/data/01_primary/temperate/AZ_santa_catelina/Oracle_mid/soil/", 
                               oracle_soil_low_list[[i]]))
  
  # We only care about date-time col and 10cm soil temp data
  iter_data <- iter_data %>% 
    dplyr::select(1, "OR_Pit2", "OR_Pit3") %>% 
    slice(4:n())
  
  # Rename cols
  colnames(iter_data) <- c("datetime", "pit2_soil", "pit3_soil")
  
  if (i == 1) {
    oracle_soil_low_raw <- iter_data
  } else {
    oracle_soil_low_raw <- bind_rows(oracle_soil_low_raw, iter_data)
  }
}

bigelow_soil_high_list <- c(list.files(path = "data/01_primary/temperate/AZ_santa_catelina/Bigelow_high/",
                                     # All of the data files are in their own subdirectories
                                     #   so need to be recursive
                                     pattern = ".csv", recursive = TRUE))

# Import all the data and bind together
for (i in 1:length(bigelow_soil_high_list)) {
  
  # Read in data
  iter_data <- read_csv(paste0(getwd(), 
                               "/data/01_primary/temperate/AZ_santa_catelina/Bigelow_high/", 
                               bigelow_soil_high_list[[i]]))
  
  # We only care about date-time col and 10cm soil temp data
  iter_data <- iter_data %>% 
    dplyr::select(1, 4) %>% 
    slice(3:n())

  if (i == 1) {
    bigelow_soil_high_raw <- iter_data
  } else {
    bigelow_soil_high_raw <- bind_rows(bigelow_soil_high_raw, iter_data)
  }
}

## 2. Curate data ###########
oracle_soil_low <- oracle_soil_low_raw %>% 
  mutate(pit2_soil = as.double(pit2_soil),
         pit3_soil = as.double(pit3_soil)) %>% 
  mutate(pit2_soil = ifelse(pit2_soil == -9999, NA, pit2_soil)) %>% 
  mutate(pit3_soil = ifelse(pit3_soil == -9999, NA, pit3_soil)) %>% 
  filter(complete.cases(pit2_soil) | complete.cases(pit3_soil)) %>% 
  mutate(soil = (pit2_soil + pit3_soil)/2) %>% 
  # Having some datetime parsing issues so separating into date and time
  separate(datetime, into = c("date", "time"), sep = " ") %>% 
  mutate(date = mdy(date)) %>%
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  dplyr::select(year, julian, soil)
  
bigelow_soil_high <- bigelow_soil_high_raw %>% 
  separate(TIMESTAMP, into = c("date", "time"), sep = " ") %>% 
  mutate(date = mdy(date)) %>%
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  dplyr::rename(soil = "TempMPS1(10cm)") %>% 
  mutate(soil = as.double(soil)) %>% 
  # Some wonky numbers that need to be filtered out
  filter(soil > -50) %>% 
  dplyr::select(year, julian, soil)

bigelow_test <- bigelow_soil_high %>% 
  group_by(julian) %>% 
  summarize(soil = mean(soil, na.rm = TRUE))

ggplot(oracle_test, aes(julian, pit2_soil)) +
  geom_point(color = "blue") +
  geom_point(data = oracle_test, aes(julian, pit3_soil), color = "red") +
  geom_point(data = bigelow_test, aes(julian, soil), color = "green")

## 3. Wrangle snow data #########

source("scripts/00_source_code/noaa_query.R")

## ....A. High elevation snowdepth ##########

# There aren't too many stations around here, and those with snowdepth data are
# limited...best I have is for year 2011. But looking up the lit and general 
# info of Mt Lemmon, it gets some snow, and there's a ski resort....but the snow
# is pretty sporadic, which is reflected in the 2011 year.
# So just going to paste the 2011 for each year at both elevations, unfortunately

catalina_high_snow <- noaa_query( 
  stationid = "GHCND:USC00025735", # Station is MOUNT LEMMON SKI VALLEY, AZ US
  startdate = "2011-01-01", enddate = "2011-12-31",
  datatypeid = "SNWD")

# Curate NOAA query
catalina_high_snow <- catalina_high_snow %>% 
  mutate(date = as_date(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  dplyr::rename(snowdepth = value) %>% 
  # Currently in mm, switch to cm
  mutate(snowdepth = snowdepth / 10) %>% 
  dplyr::select(year, julian, snowdepth)

for (i in 2010:2017) {
  catalina_high_snow <- catalina_high_snow %>% 
    mutate(year = i) %>% 
    bind_rows(catalina_high_snow)
}

catalina_high_snow <- catalina_high_snow %>% 
  group_by(year, julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(elevation = "high")

catalina_high_snow_avg <- catalina_high_snow %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE)) %>% 
  mutate(elevation = "high")

## ....B. Low elevation snowdepth ##########

# See notes in section A
catalina_low_snow <- catalina_high_snow %>% 
  mutate(elevation = "low")
catalina_low_snow_avg <- catalina_high_snow_avg %>% 
  mutate(elevation = "low")

## ....C. Bind together high and low ##########

snowdepth <- catalina_high_snow %>%
  bind_rows(catalina_low_snow)

snowdepth_avg <- catalina_high_snow_avg %>%
  bind_rows(catalina_low_snow_avg)

## 4. Run curation program ############

oracle_soil_low <- oracle_soil_low %>% 
  mutate(surface = NA)

bigelow_soil_high <- bigelow_soil_high %>% 
  mutate(surface = NA)

source("scripts/00_source_code/data_curation_program.R")
catalina_list <- prep_flux_data(low_dataset = oracle_soil_low,
                            high_dataset = bigelow_soil_high)

for (i in 1:length(catalina_list)) {
  catalina_list[[i]] <- catalina_list[[i]] %>% 
    dplyr::select(-contains("surface"))
  
  if (i == 3 | i == 5) {
    catalina_list[[i]] <- catalina_list[[i]] %>% 
      filter(micro == "soil")
  }
}

## Set spatial and temporal flags #######
for (i in 1:length(catalina_list)) {
  
  catalina_list[[i]] <- catalina_list[[i]] %>% 
    mutate(site = "AZ_cata") %>% 
    mutate(macro = "Ponderosa pine") %>% 
    mutate(foliage = "leaf-on") %>% 
    mutate(foliage_cover = 1) %>% 
    mutate(flora = "Ponerosa pine, Douglas fir") %>% 
    mutate(latitude = 32.4)
  
  if (i == 3 | i == 5) {
    
    catalina_list[[i]] <- catalina_list[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.1)) %>% 
      mutate(height_notes = "from metadata") %>% 
      mutate(altitude = dplyr::recode(elevation, "low" = 2115, "high" = 2543))
  }
  
  ## Add snow data
  if (i == 3) {
    # For all years
    catalina_list[[i]] <- catalina_list[[i]] %>% 
      left_join(snowdepth) %>% 
      # Designate a flag for snow data quality
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "single_avg", 
                                       "measured_daily")) %>% 
    
    ## To fill in snowdepth NAs, pulling from WRCC
    ## ASSUMPTIONS: WRCC data is from the 1950s...
    # https://wrcc.dri.edu/cgi-bin/cliMAIN.pl?az5733
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 135 | julian > 305),
                                .5, snowdepth)) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian >= 135 & julian <= 305),
                                0, snowdepth))
    
  }
  
  if (i == 5) {
    # For average years
    catalina_list[[i]] <- catalina_list[[i]] %>% 
      left_join(snowdepth_avg) %>% 
      # Designate a flag for snow data quality
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "single_avg", 
                                       "measured_daily")) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 135 | julian > 305),
                                .5, snowdepth)) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian >= 135 & julian <= 305),
                                0, snowdepth))
    
  }
}

## 5. Write out data ###############

write_csv(catalina_list[[1]], "data/01_primary/temperate/AZ_santa_catelina/derivative/AZ_cata_high_elev.csv")
write_csv(catalina_list[[2]], "data/01_primary/temperate/AZ_santa_catelina/derivative/AZ_cata_low_elev.csv")
write_csv(catalina_list[[3]], "data/01_primary/temperate/AZ_santa_catelina/derivative/AZ_cata_tall.csv")
write_csv(catalina_list[[4]], "data/01_primary/temperate/AZ_santa_catelina/derivative/AZ_cata_wide.csv")
write_csv(catalina_list[[5]], "data/01_primary/temperate/AZ_santa_catelina/derivative/AZ_cata_avgyears_tall.csv")
write_csv(catalina_list[[6]], "data/01_primary/temperate/AZ_santa_catelina/derivative/AZ_cata_avgyears_wide.csv")

