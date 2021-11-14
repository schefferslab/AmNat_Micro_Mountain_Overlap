# David Klinges
# File creation date: 2019.07.23
# This script curates Ed Basham's Costa Rica elevation gradient 
# data for analysis and generating figures


## 1. Workspace prep ############

library(tidyverse)
library(lubridate)


# Load in metadata 

metadata <- read_csv("data/01_primary/tropical/Costa_Rica/CR_metadata.csv")

plots_list <- c(list.files(path = "data/01_primary/tropical/Costa_Rica/original/HOBOware",
                           # Most of the data files are in their own subdirectories
                           #   so need to be recursive
                           pattern = ".csv", recursive = TRUE))

# Import all the data and bind together
for (i in 1:length(plots_list)) {
  
  # Save site and logger IDs from filename
  # Sites are the folders, so everything after / is removed
  site <- gsub("/.*$", "", plots_list[[i]])
  # Now remove the site name
  filename <- gsub(site, "", plots_list[[i]])
  # There's still a / at the beginning of the string, so just set first = 2
  logger_ID <- substring(filename, first = 2, last = 9)
  
  # Read in data
  thermal_data <- read_csv(paste0(getwd(), "/data/01_primary/tropical/Costa_Rica/original/HOBOware/", 
                          plots_list[[i]]),
                   skip = 1)

  # We only care about date-time col and full temp col, remove the rest as 
  # they're all uniquely named
  thermal_data <- thermal_data %>% 
    dplyr::select(2:3)
  
  # Rename cols so no longer unique
  colnames(thermal_data) <- c("datetime", "temp")

    # Now join site and logger names
  thermal_data <- thermal_data %>% 
    mutate(site = site, logger_ID = as.character(logger_ID))
  
  if (i == 1) {
    thermal_data_out <- thermal_data
  } else {
    thermal_data_out <- bind_rows(thermal_data_out, thermal_data)
  }
}

# Bind to metadata 
metadata <- metadata %>% 
  mutate(logger_ID = as.character(logger_ID))

CR_data_raw <- thermal_data_out %>% 
  left_join(metadata)


## 2. Data curation #########

## Select down to only attributes of interest
CR_data <- CR_data_raw %>% 
  dplyr::select(site, plot, micro, logger_ID, datetime, temp, elevation, lat, lon) %>% 
  ## Now spread the data by micros
  spread(key = micro, value = temp) %>% 

  # Date wrangling
  mutate(datetime = mdy_hms(datetime)) %>% 
  mutate(year = year(datetime), 
         julian = yday(datetime),
         hour = hour(datetime))

# The group by and summarize is working tediously slow...so we're going to do it 
# separately for soil, surface, and canopy

CR_data_soil <- CR_data %>% 
  filter(complete.cases(soil)) %>% 
  # Times are to the second, let's group up to the hour
  # NOTE: we're grouping by plot and NOT site...confusing col names by Ed but
  # this means we're aggregating multiple sites together
  group_by(plot, logger_ID, year, julian, hour) %>%
  summarize(soil = mean(soil, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # Remove logger ID col so can join with other micros properly
  dplyr::select(-logger_ID)

CR_data_surface <- CR_data %>% 
  filter(complete.cases(surface)) %>% 
  # Times are to the second, let's group up to the hour
  # NOTE: we're grouping by plot and NOT site...confusing col names by Ed but
  # this means we're aggregating multiple sites together
  group_by(plot, logger_ID, year, julian, hour) %>%
  summarize(surface = mean(surface, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # Remove logger ID col so can join with other micros properly
  dplyr::select(-logger_ID)

CR_data_canopy <- CR_data %>% 
  filter(complete.cases(canopy)) %>% 
  # Times are to the second, let's group up to the hour
  # NOTE: we're grouping by plot and NOT site...confusing col names by Ed but
  # this means we're aggregating multiple sites together
  group_by(plot, logger_ID, year, julian, hour) %>%
  summarize(canopy = mean(canopy, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # Remove logger ID col so can join with other micros properly
  dplyr::select(-logger_ID)

# Now join back together
CR_data <- CR_data_soil %>% 
  full_join(CR_data_surface) %>% 
  full_join(CR_data_canopy)

# When loggers were being set up and taken down, temps are wonky, so filter out
# the first and last 100 observations
CR_data <- CR_data %>% 
  group_by(plot) %>% 
  arrange(year, julian) %>% 
  slice(200 : (n()-200)) %>% 
  ungroup()

# Now separate out by site
# Elev = 2062 and 2066
CR_2K_high <- CR_data %>% 
  filter(plot == "2K") %>% 
  # Select down only to cols viable for data curation program
  dplyr::select(year, julian, soil, surface, canopy)

# Elev = 45 and 111
CR_LS_low <- CR_data %>% 
  filter(plot == "LS") %>% 
  # dplyr::select down only to cols viable for data curation program
  dplyr::select(year, julian, soil, surface, canopy)

## 3. Conduct data curation program ###########

source("scripts/00_source_code/data_curation_program.R")
CR_LS_2K <- prep_flux_data(low_dataset = CR_LS_low, 
                          high_dataset = CR_2K_high)
  


for (i in 1:length(CR_LS_2K)) {
  
  # There's just a few NA values, let's remove
  CR_LS_2K[[i]] <- na.omit(CR_LS_2K[[i]])
    
  CR_LS_2K[[i]] <- CR_LS_2K[[i]] %>% 
    mutate(site = "CR_basham") %>% 
    mutate(macro = "tropical broadleaf") %>% 
    mutate(foliage = "leaf-on") %>% 
    mutate(foliage_cover = 1) %>% 
    mutate(flora = "tropical broadleaf unknown") %>% 
    mutate(latitude = 10) %>% 
    mutate(snowdepth = 0)
  
  if (i == 3 | i == 5) {
    CR_LS_2K[[i]] <- CR_LS_2K[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.07, "surface" = 1.0, 
                             "canopy" = 20)) %>% 
      mutate(height_notes = "informed estimates") %>% 
      # Average just as a reminder that these are two sites each
      mutate(altitude = dplyr::recode(elevation, "low" = mean(c(45, 111)), 
                               "high" = mean(c(2062, 2066))))
  }
}

## 4. Write out data ###########

# LS and 2K
write_csv(CR_LS_2K[[1]], "data/01_primary/tropical/Costa_Rica/derivative/CR_high_all_years.csv")
write_csv(CR_LS_2K[[2]],  "data/01_primary/tropical/Costa_Rica/derivative/CR_low_all_years.csv")
write_csv(CR_LS_2K[[3]], "data/01_primary/tropical/Costa_Rica/derivative/CR_tall.csv")
write_csv(CR_LS_2K[[4]],  "data/01_primary/tropical/Costa_Rica/derivative/CR_wide.csv")
write_csv(CR_LS_2K[[5]], "data/01_primary/tropical/Costa_Rica/derivative/CR_avgyears_tall.csv")
write_csv(CR_LS_2K[[6]], "data/01_primary/tropical/Costa_Rica/derivative/CR_avgyears_wide.csv")

  
