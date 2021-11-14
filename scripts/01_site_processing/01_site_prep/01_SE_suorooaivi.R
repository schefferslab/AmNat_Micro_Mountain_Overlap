# David Klinges
# File creation date: 2019.06.09
# This script curates Sweden Suorooaivi mountain data for analysis and 
#   generating figures

## 1. Workspace prep ########

library(tidyverse)
library(lubridate)
library(readxl)

# Read in data

soil_raw <- read_excel("data/01_primary/arctic/SE_suorooaivi/original/Altitude_v2.xls",
                       sheet = 1)

snowdepth_raw <- read_delim("data/01_primary/arctic/SE_suorooaivi/snow/smhi-opendata_8_188800_20190819_165104.csv",
                      delim = ";", skip = 9)
# Well...it's in Swedish. Google translate!

## 2. Curate data #########

## ....A. Meadow #########

# Low
low_soil_meadow <- soil_raw %>% 
  dplyr::rename(date = Day, mean500 = "500H", min500 = "Min...8", max500 = "Max...9") %>%
  dplyr::select(date, mean500, min500, max500) %>% 
  gather(key = "minmax", value = "soil", -date) %>% 
  mutate(soil = as.double(soil)) %>% 
  filter(!is.na(date))  %>% # There's some extra rows at the bottom of the excel 
  # spreadsheet that are empty/summary stats
  mutate(year = year(date)) %>%                   
  mutate(julian = yday(date)) %>% 
  mutate(surface = 1) %>% # Adding false surface col for curation program
  dplyr::select(year, julian, soil, surface) 


# High
high_soil_meadow <- soil_raw %>% 
  dplyr::rename(date = Day, mean900 = "900H", min900 = "Min...53", max900 = "Max...54") %>%
  dplyr::select(date, mean900, min900, max900) %>% 
  gather(key = "minmax", value = "soil", -date) %>% 
  mutate(soil = as.double(soil)) %>% 
  filter(!is.na(date))  %>% # There's some extra rows at the bottom of the excel 
  # spreadsheet that are empty/summary stats
  mutate(year = year(date)) %>%                   
  mutate(julian = yday(date)) %>%
  mutate(surface = 1) %>% # Adding false surface col for curation program
  dplyr::select(year, julian, soil, surface) 

## ....B. Salix #########

# Low
low_soil_salix <- soil_raw %>% 
  dplyr::rename(date = Day, mean500 = "500S", min500 = "Min...14", max500 = "Max...15") %>%
  dplyr::select(date, mean500, min500, max500) %>% 
  gather(key = "minmax", value = "soil", -date) %>% 
  mutate(soil = as.double(soil)) %>% 
  filter(!is.na(date))  %>% # There's some extra rows at the bottom of the excel 
  # spreadsheet that are empty/summary stats
  mutate(year = year(date)) %>%                   
  mutate(julian = yday(date)) %>% 
  mutate(surface = 1) %>% # Adding false surface col for curation program
  dplyr::select(year, julian, soil, surface) 

# High
high_soil_salix <- soil_raw %>% 
  dplyr::rename(date = Day, mean900 = "900S", min900 = "Min...62", max900 = "Max...63") %>%
  dplyr::select(date, mean900, min900, max900) %>% 
  gather(key = "minmax", value = "soil", -date) %>% 
  mutate(soil = as.double(soil)) %>% 
  filter(!is.na(date))  %>% # There's some extra rows at the bottom of the excel 
  # spreadsheet that are empty/summary stats
  mutate(year = year(date)) %>%                   
  mutate(julian = yday(date)) %>%
  mutate(surface = 1) %>% # Adding false surface col for curation program
  dplyr::select(year, julian, soil, surface) 

## ....C. Salix and Meadow #######

# Low
low_soil_SM <- soil_raw %>% 
  dplyr::rename(date = Day, mean500S = "500S", min500S = "Min...14", max500S = "Max...15",
         mean500M = "500H", min500M = "Min...8", max500M = "Max...9") %>%
  # Change from character to double
  mutate_if(is.character, as.numeric) %>% 
  dplyr::select(date, mean500S, min500S, max500S, mean500M, min500M, max500M) %>% 
  mutate(soil_mean = (mean500S + mean500M)/2) %>%
  mutate(soil_min = (min500S + min500M)/2) %>%
  mutate(soil_max = (max500S + max500M)/2) %>%
  gather(key = "minmax", value = "soil", soil_mean, soil_min, soil_max) %>% 
  mutate(soil = as.double(soil)) %>% 
  filter(!is.na(date))  %>% # There's some extra rows at the bottom of the excel 
  # spreadsheet that are empty/summary stats
  mutate(year = year(date)) %>%                   
  mutate(julian = yday(date)) %>% 
  mutate(surface = 1) %>% # Adding false surface col for curation program
  dplyr::select(year, julian, soil, surface) 

high_soil_SM <- soil_raw %>% 
  dplyr::rename(date = Day, mean900S = "900S", min900S = "Min...62", max900S = "Max...63",
         mean900M = "900H", min900M = "Min...53", max900M = "Max...54") %>%
  # Change from character to double
  mutate_if(is.character, as.numeric) %>% 
  dplyr::select(date, mean900S, min900S, max900S, mean900M, min900M, max900M) %>% 
  mutate(soil_mean = (mean900S + mean900M)/2) %>%
  mutate(soil_min = (min900S + min900M)/2) %>%
  mutate(soil_max = (max900S + max900M)/2) %>%
  gather(key = "minmax", value = "soil", soil_mean, soil_min, soil_max) %>% 
  mutate(soil = as.double(soil)) %>% 
  filter(!is.na(date))  %>% # There's some extra rows at the bottom of the excel 
  # spreadsheet that are empty/summary stats
  mutate(year = year(date)) %>%                   
  mutate(julian = yday(date)) %>% 
  mutate(surface = 1) %>% # Adding false surface col for curation program
  dplyr::select(year, julian, soil, surface) 

## 3. Wrangle snow data ########

# Unfortunately can only find low-elevation stations... ~ same elev so just
# using one

snowdepth <- snowdepth_raw %>% 
  # Using Google Translate, G = Green (good), Y = Yellow ("suspiciuos or 
  # aggregated values)
  filter(Kvalitet == "G") %>% 
  mutate(year = year(Datum)) %>% 
  mutate(julian = yday(Datum)) %>% 
  dplyr::rename(snowdepth = Snödjup) %>% 
  # Metadata at top of snowdepth .csv says it's in meters, so convert to cm
  mutate(snowdepth = snowdepth * 100) %>% 
  dplyr::select(year, julian, snowdepth)
  
# We have LOTS of years of snow data...and yet have none for 2007 or 2008, which
# our thermal data is from. So just going to average to day of year and apply
snowdepth_avg <- snowdepth %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE))

## 3. Conduct overlap data curation #########

source("scripts/00_source_code/data_curation_program.R")
suorooaivi_list <- prep_flux_data(low_dataset = low_soil_meadow, 
                                  high_dataset = high_soil_meadow)


suorooaivi_list <- prep_flux_data(low_dataset = low_soil_salix, 
                                  high_dataset = high_soil_salix)



suorooaivi_list <- prep_flux_data(low_dataset = low_soil_SM, 
                                  high_dataset = high_soil_SM)


# Now remove surface cols
for (i in 1:length(suorooaivi_list)) {
  
  suorooaivi_list[[i]] <- suorooaivi_list[[i]] %>% 
    dplyr::select(-contains("surface"))
}

# Now remove surface variable
suorooaivi_list[[3]] <- suorooaivi_list[[3]] %>% 
  filter(micro == "soil")

suorooaivi_list[[5]] <- suorooaivi_list[[5]] %>% 
  filter(micro == "soil")


for (i in 1:length(suorooaivi_list)) {
  
  suorooaivi_list[[i]] <- suorooaivi_list[[i]] %>% 
    mutate(site = "SE") %>% 
    mutate(macro = "scrub shrub") %>% 
    mutate(foliage = "leaf-off") %>% 
    mutate(foliage_cover = 0) %>% 
    mutate(flora = "Empetrum hermaphroditum, Vaccinium myrtillus, Deschampsia flexuosa") %>% 
    mutate(latitude = 68)
  
  if (i == 3 | i == 5) {
    suorooaivi_list[[i]] <- suorooaivi_list[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.02)) %>% 
      mutate(height_notes = "from metadata") %>%
      mutate(altitude = dplyr::recode(elevation, "low" = 500, "high" = 900))
  }
  
  ## Add snow data
  if (i == 3) {
    # For all years
    suorooaivi_list[[i]] <- suorooaivi_list[[i]] %>% 
      left_join(snowdepth_avg)
  }
  
  if (i == 5) {
    # For average years
    suorooaivi_list[[i]] <- suorooaivi_list[[i]] %>% 
      left_join(snowdepth_avg)
  }
}

## 4. Write out data ##########
write_csv(suorooaivi_list[[1]], "data/01_primary/arctic/SE_suorooaivi/derivative/SE_suorooaiviSM_high_all_years.csv")
write_csv(suorooaivi_list[[2]],  "data/01_primary/arctic/SE_suorooaivi/derivative/SE_suorooaiviSM_low_all_years.csv")
write_csv(suorooaivi_list[[3]], "data/01_primary/arctic/SE_suorooaivi/derivative/SE_suorooaiviSM_tall.csv")
write_csv(suorooaivi_list[[4]],  "data/01_primary/arctic/SE_suorooaivi/derivative/SE_suorooaiviSM_wide.csv")
write_csv(suorooaivi_list[[5]], "data/01_primary/arctic/SE_suorooaivi/derivative/SE_suorooaiviSM_avgyears_tall.csv")
write_csv(suorooaivi_list[[6]], "data/01_primary/arctic/SE_suorooaivi/derivative/SE_suorooaiviSM_avgyears_wide.csv")

