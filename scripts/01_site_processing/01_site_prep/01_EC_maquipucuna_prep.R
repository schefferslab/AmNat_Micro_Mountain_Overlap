# David Klinges
# 2019-09-20
# This script curates thermal data from Maquipucuna Cloud Forest Reserve, Ecuador, 
# submitted by ??, for analysis and generating figures

## 1. Workspace prep ############

library(tidyverse)
library(lubridate)
library(readxl)


ecuador_low_raw <- read_excel("data/01_primary/tropical/Ecuador/original/AllData.xls",
                        sheet = "Waterfall", skip = 3)

ecuador_high_raw <- read_excel("data/01_primary/tropical/Ecuador/original/AllData.xls",
                          sheet = "Old Observation Tower", skip = 3)


## 2. Data curation #############

ecuador_low <- ecuador_low_raw %>% 
  dplyr::rename(surface = Temperature, datetime = `Date / Time`) %>%
  mutate(year = year(datetime)) %>% 
  mutate(julian = yday(datetime)) %>% 
  dplyr::select(year, julian, surface)

ecuador_high <- ecuador_high_raw %>% 
  dplyr::rename(surface = Temperature, datetime = `Date / Time`) %>%
  mutate(year = year(datetime)) %>% 
  mutate(julian = yday(datetime)) %>% 
  dplyr::select(year, julian, surface)


## 3. Data curation program ###########

source("scripts/00_source_code/data_curation_program.R")
maquipucuna_list <- prep_flux_data(low_dataset = ecuador_low,
                                high_dataset = ecuador_high)

for (i in 1:length(maquipucuna_list)) {
  
  maquipucuna_list[[i]] <- maquipucuna_list[[i]] %>% 
    mutate(site = "EC_maquipucuna") %>% 
    mutate(macro = "tropical broadleaf") %>% 
    mutate(foliage = "leaf-on") %>% 
    mutate(foliage_cover = 1) %>% 
    mutate(flora = "Montane primary forest with minimal human disturbance") %>% 
    mutate(snowdepth = 0) %>% 
    mutate(latitude = 0.12)
  
  if (i == 3 | i == 5) {
    
    maquipucuna_list[[i]] <- maquipucuna_list[[i]] %>% 
      mutate(height = dplyr::recode(micro, surface = 1.5)) %>%
      mutate(height_notes = "from metadata") %>% 
      # Elevations are averages of the four sensors used at both low and high
      mutate(altitude = dplyr::recode(elevation, "low" = 1444, "high" = 2496))
  }
}

## 4. Write out data #########

write_csv(maquipucuna_list[[1]], "./data/01_primary/tropical/Ecuador/derivative/EC_maquipucuna_high_elev.csv")
write_csv(maquipucuna_list[[2]], "./data/01_primary/tropical/Ecuador/derivative/EC_maquipucuna_low_elev.csv")
write_csv(maquipucuna_list[[3]], "./data/01_primary/tropical/Ecuador/derivative/EC_maquipucuna_tall.csv")
write_csv(maquipucuna_list[[4]], "./data/01_primary/tropical/Ecuador/derivative/EC_maquipucuna_wide.csv")
write_csv(maquipucuna_list[[5]], "./data/01_primary/tropical/Ecuador/derivative/EC_maquipucuna_avgyears_tall.csv")
write_csv(maquipucuna_list[[6]], "./data/01_primary/tropical/Ecuador/derivative/EC_maquipucuna_avgyears_wide.csv")
