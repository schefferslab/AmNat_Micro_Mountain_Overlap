# David Klinges
# File creation date: 2019.03.13
# This script curates Serra da Estrela data for analysis and for generating figures

## 1. Workspace prep ###############

library(tidyverse)
library(readxl)
library(lubridate)

# Read in data

# Names will correspond to site acronym and elevation flag (high or low elev)
zezere_low_raw <- read_tsv("./data/01_primary/temperate/PT_serra_estrala/low/Zezere_ASE_airT_2000.tab",
                             skip = 16)
cantaro_high_raw <- read_tsv("data/01_primary/temperate/PT_serra_estrala/high/Cantaro_Gordo_airT_2000.tab",
                             skip = 16)

## 2. Prep data for curation #########

## ....2a. Prep low elevation data #########
# Rename attributes, parse date time
zezere_low <- zezere_low_raw %>%
  dplyr::rename(surface = "TTT [°C]") %>%
  separate("Date/Time", into = c("date", "time"), sep = " ") %>%
  # Need to add zero's to single-digit months and days
  separate(date, into = c("year", "month", "day"), sep = "-") %>%
  mutate(month = as.numeric(month), day = as.numeric(day)) %>%
  mutate(month = ifelse(month < 10, paste0("0", as.character(month)), 
                        as.character(month))) %>%
  mutate(day = ifelse(day < 10, paste0("0", as.character(day)), 
                      as.character(day))) %>%
  mutate(date = paste(day, month, year, sep = "-"))

zezere_low$date <- as.Date(zezere_low$date, format = "%d-%m-%Y")

zezere_low <- zezere_low %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(julian = yday(date)) %>%
  dplyr::select(year, julian, surface)

# Change day of year 366 to day of year 1 of next year
zezere_low <- zezere_low %>%
  mutate(year = ifelse(julian == 366, (year + 1), year)) %>%
  mutate(julian = ifelse(julian == 366, 1, julian))

## ....2b. Prep high elevation data #########

# Rename attributes, parse date time
cantaro_high <- cantaro_high_raw %>%
  dplyr::rename(surface = "TTT [°C]") %>%
  separate("Date/Time", into = c("date", "time"), sep = " ") %>%
  # Need to add zero's to single-digit months and days
  separate(date, into = c("year", "month", "day"), sep = "-") %>%
  mutate(month = as.numeric(month), day = as.numeric(day)) %>%
  mutate(month = ifelse(month < 10, paste0("0", as.character(month)), 
                        as.character(month))) %>%
  mutate(day = ifelse(day < 10, paste0("0", as.character(day)), 
                      as.character(day))) %>%
  mutate(date = paste(day, month, year, sep = "-"))

cantaro_high$date <- as.Date(cantaro_high$date, format = "%d-%m-%Y")

cantaro_high <- cantaro_high %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(julian = yday(date)) %>%
  dplyr::select(year, julian, surface)

# Change day of year 366 to day of year 1 of next year
cantaro_high <- cantaro_high %>%
  mutate(year = ifelse(julian == 366, (year + 1), year)) %>%
  mutate(julian = ifelse(julian == 366, 1, julian))

## 3. Snowdepth curation ###########

source("scripts/00_source_code/extract_snowdepth.R")

## ....A. High elevation snowdepth ##########

serra_estrala_high_snow <- extract_snowdepth(x_coord = 3482219.053238, 
                                     y_coord = -4550437.282344,
                                     year = 2000)

serra_estrala_high_snow <- serra_estrala_high_snow %>% 
  mutate(elevation = "high") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

serra_estrala_high_snow_avg <- serra_estrala_high_snow %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE))

## ....B. Low elevation snowdepth ##########

# Only had one set of coordinates for high and low

serra_estrala_low_snow <- serra_estrala_high_snow %>% 
  mutate(elevation = "low")

serra_estrala_low_snow_avg <- serra_estrala_high_snow_avg %>% 
  mutate(elevation = "low")

## ....C. Bind together high and low ##########

snowdepth <- serra_estrala_high_snow %>%
  bind_rows(serra_estrala_low_snow)

snowdepth_avg <- serra_estrala_high_snow_avg %>%
  bind_rows(serra_estrala_low_snow_avg)

## 4.  Curate data #########

# There's 366 days of low data, but only 347 days of high data...this is causing
# issues downstream when compiling the database, so filter the low data so that
# we only have days with both low and high
zezere_low <- zezere_low %>% 
  filter(julian %in% unique(cantaro_high$julian))

source("scripts/00_source_code/data_curation_program.R")
serra_list <- prep_flux_data(low_dataset = zezere_low,
               high_dataset = cantaro_high)

for (i in 1:length(serra_list)) {
  
  serra_list[[i]] <- serra_list[[i]] %>% 
    mutate(site = "PT") %>% 
    mutate(macro = "scrub shrub") %>% 
    mutate(foliage = "leaf-off") %>% 
    mutate(foliage_cover = 0) %>% 
    mutate(flora = "scrub") %>% 
    mutate(height = 1.3) %>% 
    mutate(height_notes = "from metadata") %>%
    mutate(latitude = 40)
  
  if (i == 3 | i == 5) {
    
    serra_list[[i]] <- serra_list[[i]] %>% 
      mutate(altitude = dplyr::recode(elevation, "low" = 1100, "high" = 1900))

  }
  
  # Add snow data
  if (i == 3) {
    # For all years
    serra_list[[i]] <- serra_list[[i]] %>% 
      left_join(snowdepth) %>% 
      mutate(snow_source_flag = "measured daily") %>% 
      # All snowdepth data is extracted from snowdepth raster
      mutate(snowdepth = ifelse(is.na(snowdepth), 0, 
                                snowdepth))
  }
  if (i == 5) {
    # For avg years
    serra_list[[i]] <- serra_list[[i]] %>% 
      left_join(snowdepth_avg) %>% 
      mutate(snow_source_flag = "measured daily") %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth), 0, 
                                snowdepth))
    
  }
}

## 5. Write out data ############

write_csv(serra_list[[1]], "data/01_primary/temperate/PT_serra_estrala/derivative/SerraEstrala_cantaro_all_years.csv")
write_csv(serra_list[[2]], "data/01_primary/temperate/PT_serra_estrala/derivative/SerraEstrala_zezere_all_years.csv")
write_csv(serra_list[[3]], "data/01_primary/temperate/PT_serra_estrala/derivative/SerraEstrala_cantaro_zezere_tall.csv")
write_csv(serra_list[[4]], "data/01_primary/temperate/PT_serra_estrala/derivative/SerraEstrala_cantaro_zezere_wide.csv")
write_csv(serra_list[[5]], "data/01_primary/temperate/PT_serra_estrala/derivative/SerraEstrala_cantaro_zezere_avgyears_tall.csv")
write_csv(serra_list[[6]], "data/01_primary/temperate/PT_serra_estrala/derivative/SerraEstrala_cantaro_zezere_avgyears_wide.csv")

