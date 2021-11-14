# David Klinges
# File creation date: 2019.07.23
# This script curates Switzerland Long-term Forest Ecosystem Research Programme 
# data for analysis and generating figures


## 1. Workspace prep ############

library(tidyverse)
library(lubridate)

## Loop in loading all data ##########
# IF NEEDED: import all .tab files
# plots_list <- c(list.files(path = "data/01_primary/temperate/CH_LWF/original/forest", 
#                            # Most of the data files are in their own subdirectories
#                            #   so need to be recursive
#                            pattern = ".tab", recursive = FALSE))
# 
# plots_list <- lapply(plots_list, FUN = function(x) {
#   data <- read_tsv(paste0(getwd(), "/data/01_primary/temperate/CH_LWF/original/forest/", x),
#                    skip = 18)
#   })

## Select sites of interest (see README for info on sites) ############

## Forest
# Coniferous: 500.0 m
VOB_forest_low_raw <- read_tsv("data/01_primary/temperate/CH_LWF/original/forest/VOB_meteo.tab",
                           skip = 18)

# Coniferous: 1893.0
NAB_forest_high_raw <- read_tsv("data/01_primary/temperate/CH_LWF/original/forest/NAB_meteo.tab",
                            skip = 18)

# Deciduous (beech): 497.0 m
OTB_forest_low_raw <- read_tsv("data/01_primary/temperate/CH_LWF/original/forest/OTB_meteo.tab",
                               skip = 18)
# Deciduous (beech): 1214.0 m
ISB_forest_high_raw <- read_tsv("data/01_primary/temperate/CH_LWF/original/forest/ISB_meteo.tab",
                                skip = 18)
  
 
## Open
# 488 m 
VOF_open_low_raw <- read_tsv("data/01_primary/temperate/CH_LWF/original/open/VOF_meteo.tab",
                           skip = 18)

# 1920 m 
NAF_open_high_raw <- read_tsv("data/01_primary/temperate/CH_LWF/original/open/NAF_meteo.tab",
                            skip = 18)

# Other low sites: OTB, NEB
# Other high sites: CLB/CLF 

## 2. Data curation ##########

## ....A. Forest low ########

# Coniferous
VOB_forest_low <- VOB_forest_low_raw %>% 
  dplyr::rename(datetime = `Date/Time`, surface = `TTT [°C]`) %>% 
  mutate(date = as_date(datetime)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  dplyr::select(year, julian, surface)

# Deciduous
OTB_forest_low <- OTB_forest_low_raw %>% 
  dplyr::rename(datetime = `Date/Time`, surface = `TTT [°C]`) %>% 
  mutate(date = as_date(datetime)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  dplyr::select(year, julian, surface)

## ....B. Forest high ########

# Coniferous
NAB_forest_high <- NAB_forest_high_raw %>% 
  dplyr::rename(datetime = `Date/Time`, surface = `TTT [°C]`) %>% 
  mutate(date = as_date(datetime)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  dplyr::select(year, julian, surface)

# Deciduous
ISB_forest_high <- ISB_forest_high_raw %>% 
  dplyr::rename(datetime = `Date/Time`, surface = `TTT [°C]`) %>% 
  mutate(date = as_date(datetime)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  dplyr::select(year, julian, surface)


## ....C. Open low ########

VOF_open_low <- VOF_open_low_raw %>% 
  dplyr::rename(datetime = `Date/Time`, surface = `TTT [°C]`) %>% 
  mutate(date = as_date(datetime)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  dplyr::select(year, julian, surface)

## ....D. Open high ########

NAF_open_high <- NAF_open_high_raw %>% 
  dplyr::rename(datetime = `Date/Time`, surface = `TTT [°C]`) %>% 
  mutate(date = as_date(datetime)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  dplyr::select(year, julian, surface)


## 3. Snowdepth curation ###########

source("scripts/00_source_code/extract_snowdepth.R")

## ..1. VOB/NAB: Coniferous ###########
## ....A. High elevation snowdepth ##########

CH_conif_high_snow <- extract_snowdepth(x_coord = 4044106.95046, 
                                        y_coord = -2807539.187614,
                                        year = 2000)

CH_conif_high_snow <- CH_conif_high_snow %>% 
  mutate(elevation = "high") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

CH_conif_high_snow_avg <- CH_conif_high_snow %>% 
  group_by(julian, elevation) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE))

## ....B. Low elevation snowdepth ##########

CH_conif_low_snow <- extract_snowdepth(x_coord = 3865081.50307, 
                                       y_coord = -2924531.833013,
                                       year = 2000)

CH_conif_low_snow <- CH_conif_low_snow %>% 
  mutate(elevation = "low") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

CH_conif_low_snow_avg <- CH_conif_low_snow %>% 
  group_by(julian, elevation) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE))

## ....C. Bind together high and low ##########

conif_snowdepth <- CH_conif_high_snow %>%
  bind_rows(CH_conif_low_snow)

conif_snowdepth_avg <- CH_conif_high_snow_avg %>%
  bind_rows(CH_conif_low_snow_avg)



## ..2. OTB/ISB: Deciduous ###########
## ....A. High elevation snowdepth ##########

CH_decid_high_snow <- extract_snowdepth(x_coord = 4037554.252828, 
                                        y_coord = -2932626.999426,
                                        year = 2000)

CH_decid_high_snow <- CH_decid_high_snow %>% 
  mutate(elevation = "high") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

CH_decid_high_snow_avg <- CH_decid_high_snow %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE))

## ....B. Low elevation snowdepth ##########

CH_decid_low_snow <- extract_snowdepth(x_coord = 3869831.55101, 
                                       y_coord = -2891809.843123,
                                       year = 2000)

CH_decid_low_snow <- CH_decid_low_snow %>% 
  mutate(elevation = "low") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

CH_decid_low_snow_avg <- CH_decid_low_snow %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE))

## ....C. Bind together high and low ##########

decid_snowdepth <- CH_decid_high_snow %>%
  bind_rows(CH_decid_low_snow)

decid_snowdepth_avg <- CH_decid_high_snow_avg %>%
  bind_rows(CH_decid_low_snow_avg)



## ..3. VOF/NAF: Open ###########
## ....A. High elevation snowdepth ##########

CH_open_high_snow <- extract_snowdepth(x_coord = 4044828.906782, 
                                        y_coord = -2806936.796814,
                                        year = 2000)

CH_open_high_snow <- CH_open_high_snow %>% 
  mutate(elevation = "high") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

CH_open_high_snow_avg <- CH_open_high_snow %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE))

## ....B. Low elevation snowdepth ##########

CH_open_low_snow <- extract_snowdepth(x_coord = 3866834.59969, 
                                        y_coord = -2923316.293838,
                                        year = 2000)

CH_open_low_snow <- CH_open_low_snow %>% 
  mutate(elevation = "low") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

CH_open_low_snow_avg <- CH_open_low_snow %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE))

## ....C. Bind together high and low ##########

open_snowdepth <- CH_open_high_snow %>%
  bind_rows(CH_open_low_snow)

open_snowdepth_avg <- CH_open_high_snow_avg %>%
  bind_rows(CH_open_low_snow_avg)


## 3. Conduct data curation program ##########

## Forest, VOB/NAB: Coniferous ###########

source("scripts/00_source_code/data_curation_program.R")
CH_LWF_forest_list_VOB_NAB <- prep_flux_data(low_dataset = VOB_forest_low, 
                                  high_dataset = NAB_forest_high)

# Disaggregated data 
glimpse(CH_LWF_forest_list_VOB_NAB[[7]])

for (i in 1:length(CH_LWF_forest_list_VOB_NAB)) {
  
  CH_LWF_forest_list_VOB_NAB[[i]] <- CH_LWF_forest_list_VOB_NAB[[i]] %>% 
    mutate(site = "CH_conif") %>% 
      mutate(macro = "Dense coniferous") %>% 
    mutate(foliage = "leaf-on") %>% 
    mutate(foliage_cover = 1) %>% 
    mutate(flora = "mountain pine (Pinus mugo; 210 yrs) at high elev, European silver fir (Abies alba; 110 yrs) and oak trees (Quercus sp.; 190-210 yrs) at low elev") %>% 
    mutate(latitude = 47)
  
  if (i == 3 | i == 5 | i == 7) {
    CH_LWF_forest_list_VOB_NAB[[i]] <- CH_LWF_forest_list_VOB_NAB[[i]] %>% 
      mutate(height = dplyr::recode(micro, "surface" = 2.0)) %>% 
      mutate(height_notes = "from metadata") %>% 
      mutate(altitude = dplyr::recode(elevation, "low" = 500, "high" = 1893.0)) 
  }
  
  # Add snow data
  if (i == 3 | i == 7) {
    # For all years
    CH_LWF_forest_list_VOB_NAB[[i]] <- CH_LWF_forest_list_VOB_NAB[[i]] %>% 
      left_join(conif_snowdepth) %>% 
      mutate(snow_source_flag = "measured daily")
  }
  if (i == 5) {
    # For avg years
    CH_LWF_forest_list_VOB_NAB[[i]] <- CH_LWF_forest_list_VOB_NAB[[i]] %>% 
      left_join(conif_snowdepth_avg) %>% 
      mutate(snow_source_flag = "measured daily") %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth), 0, 
                                snowdepth))
    
  }
}

## Forest, OTB/ISB: Deciduous ###########


CH_LWF_forest_list_OTB_ISB <- prep_flux_data(low_dataset = OTB_forest_low, 
                                             high_dataset = ISB_forest_high)


for (i in 1:length(CH_LWF_forest_list_OTB_ISB)) {
  
  CH_LWF_forest_list_OTB_ISB[[i]] <- CH_LWF_forest_list_OTB_ISB[[i]] %>% 
    mutate(site = "CH_decid") %>% 
    mutate(macro = "deciduous") %>% 
    mutate(foliage = ifelse(julian > 100 & julian < 200, "leaf-on", "leaf-off")) %>%
    mutate(foliage_cover = ifelse(julian > 100 & julian < 200, 1.0, 0.4)) %>%
    mutate(flora = "European beech (Fagus sylvatica; 120-140 yrs) and lime trees (Tilia sp.; 120-140 yrs)") %>% 
    mutate(latitude = 46)
  
  if (i == 3 | i == 5 | i == 7) {
    CH_LWF_forest_list_OTB_ISB[[i]] <- CH_LWF_forest_list_OTB_ISB[[i]] %>% 
      mutate(height = dplyr::recode(micro, "surface" = 2.0)) %>% 
      mutate(height_notes = "from metadata") %>% 
      mutate(altitude = dplyr::recode(elevation, "low" = 497.0, "high" = 1214.0))
  }
  
  # Add snow data
  if (i == 3 | i == 7) {
    # For all years
    CH_LWF_forest_list_OTB_ISB[[i]] <- CH_LWF_forest_list_OTB_ISB[[i]] %>% 
      left_join(decid_snowdepth) %>% 
      mutate(snow_source_flag = "measured daily")
  }
  if (i == 5) {
    # For avg years
    CH_LWF_forest_list_OTB_ISB[[i]] <- CH_LWF_forest_list_OTB_ISB[[i]] %>% 
      left_join(decid_snowdepth_avg) %>% 
      mutate(snow_source_flag = "measured daily") %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth), 0, 
                                snowdepth))
    
  }
}


## Open, VOF/NAF (meadow near forest) ###########


CH_LWF_open_list_VOF_NAF <- prep_flux_data(low_dataset = VOF_open_low, 
                                             high_dataset = NAF_open_high)

for (i in 1:length(CH_LWF_open_list_VOF_NAF)) {
  
  CH_LWF_open_list_VOF_NAF[[i]] <- CH_LWF_open_list_VOF_NAF[[i]] %>% 
    mutate(site = "CH_open") %>% 
    mutate(macro = "meadow near forest") %>% 
    mutate(foliage = "leaf-off") %>% 
    mutate(foliage_cover = 0) %>% 
    mutate(flora = NA) %>% 
    mutate(latitude = 47)
  
  if (i == 3 | i == 5 | i == 7) {
    CH_LWF_open_list_VOF_NAF[[i]] <- CH_LWF_open_list_VOF_NAF[[i]] %>% 
      mutate(height = dplyr::recode(micro, "surface" = 2.0)) %>% 
      mutate(height_notes = "from metadata") %>% 
      mutate(altitude = dplyr::recode(elevation, "low" = 488.0, "high" = 1920.0))
  }
  
  # Add snow data
  if (i == 3 | i == 7) {
    # For all years
    CH_LWF_open_list_VOF_NAF[[i]] <- CH_LWF_open_list_VOF_NAF[[i]] %>% 
      left_join(open_snowdepth) %>% 
      mutate(snow_source_flag = "measured daily")
  }
  if (i == 5) {
    # For avg years
    CH_LWF_open_list_VOF_NAF[[i]] <- CH_LWF_open_list_VOF_NAF[[i]] %>% 
      left_join(open_snowdepth_avg) %>% 
      mutate(snow_source_flag = "measured daily") %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth), 0, 
                                snowdepth))
  }
}


## 4. Write out data ##########

# Coniferous
write_csv(CH_LWF_forest_list_VOB_NAB[[1]], "data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_VOB_NAB_high_all_years.csv")
write_csv(CH_LWF_forest_list_VOB_NAB[[2]],  "data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_VOB_NAB_low_all_years.csv")
write_csv(CH_LWF_forest_list_VOB_NAB[[3]], "data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_VOB_NAB_tall.csv")
write_csv(CH_LWF_forest_list_VOB_NAB[[4]],  "data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_VOB_NAB_wide.csv")
write_csv(CH_LWF_forest_list_VOB_NAB[[5]], "data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_VOB_NAB_avgyears_tall.csv")
write_csv(CH_LWF_forest_list_VOB_NAB[[6]], "data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_VOB_NAB_avgyears_wide.csv")
write_csv(CH_LWF_forest_list_VOB_NAB[[7]], "data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_VOB_NAB_disagg.csv")

# Deciduous
write_csv(CH_LWF_forest_list_OTB_ISB[[1]], "data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_OTB_ISB_high_all_years.csv")
write_csv(CH_LWF_forest_list_OTB_ISB[[2]],  "data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_OTB_ISB_low_all_years.csv")
write_csv(CH_LWF_forest_list_OTB_ISB[[3]], "data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_OTB_ISB_tall.csv")
write_csv(CH_LWF_forest_list_OTB_ISB[[4]],  "data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_OTB_ISB_wide.csv")
write_csv(CH_LWF_forest_list_OTB_ISB[[5]], "data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_OTB_ISB_avgyears_tall.csv")
write_csv(CH_LWF_forest_list_OTB_ISB[[6]], "data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_OTB_ISB_avgyears_wide.csv")
write_csv(CH_LWF_forest_list_OTB_ISB[[7]], "data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_OTB_ISB_disagg.csv")

# Open
write_csv(CH_LWF_open_list_VOF_NAF[[1]], "data/01_primary/temperate/CH_LWF/derivative/open/CH_LWF_VOF_NAF_high_all_years.csv")
write_csv(CH_LWF_open_list_VOF_NAF[[2]],  "data/01_primary/temperate/CH_LWF/derivative/open/CH_LWF_VOF_NAF_low_all_years.csv")
write_csv(CH_LWF_open_list_VOF_NAF[[3]], "data/01_primary/temperate/CH_LWF/derivative/open/CH_LWF_VOF_NAF_tall.csv")
write_csv(CH_LWF_open_list_VOF_NAF[[4]],  "data/01_primary/temperate/CH_LWF/derivative/open/CH_LWF_VOF_NAF_wide.csv")
write_csv(CH_LWF_open_list_VOF_NAF[[5]], "data/01_primary/temperate/CH_LWF/derivative/open/CH_LWF_VOF_NAF_avgyears_tall.csv")
write_csv(CH_LWF_open_list_VOF_NAF[[6]], "data/01_primary/temperate/CH_LWF/derivative/open/CH_LWF_VOF_NAF_avgyears_wide.csv")
write_csv(CH_LWF_open_list_VOF_NAF[[7]], "data/01_primary/temperate/CH_LWF/derivative/open/CH_LWF_VOF_NAF_disagg.csv")

