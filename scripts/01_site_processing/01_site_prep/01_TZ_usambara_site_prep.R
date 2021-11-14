# David Klinges
# 2019-09-20
# This script curates thermal data from the East Usambara Mountains, submitted by
# Philip Shirk, for analysis and generating figures

## 1. Workspace prep ############

library(tidyverse)
library(lubridate)


usambara_data_raw <- read_csv("data/01_primary/tropical/Tanzania_usambara/original/ibutton_temps.csv")

usambara_metadata <- read_csv("data/01_primary/tropical/Tanzania_usambara/original/metadata.csv")

## 2. Data curation #############

usambara_low <- usambara_data_raw %>% 
  filter(Elevation %in% c(877.9, 879.1, 878.5)) %>% 
  dplyr::rename(surface = Temp) %>% 
  mutate(datetime = mdy_hm(Times)) %>% 
  mutate(year = year(datetime)) %>% 
  mutate(julian = yday(datetime)) %>% 
  dplyr::select(year, julian, surface)


usambara_high <- usambara_data_raw %>% 
  filter(Elevation %in% c(1057.8, 1024, 1030.7)) %>% 
  dplyr::rename(surface = Temp) %>% 
  mutate(datetime = mdy_hm(Times)) %>% 
  mutate(year = year(datetime)) %>% 
  mutate(julian = yday(datetime)) %>% 
  dplyr::select(year, julian, surface)


## 3. Data curation program ###########

## ....A. Old secondary 1 ############

source("scripts/00_source_code/data_curation_program.R")
usambara_list <- prep_flux_data(low_dataset = usambara_low,
                             high_dataset = usambara_high)

for (i in 1:length(usambara_list)) {
  
  usambara_list[[i]] <- usambara_list[[i]] %>% 
    mutate(site = "TZ_usambara") %>% 
    mutate(macro = "degraded tropical broadleaf") %>% 
    mutate(foliage = "leaf-on") %>% 
    mutate(foliage_cover = 1) %>% 
    mutate(flora = "submontane tropical broadleaf, uplands may be primary, 
           canopy trees Anthocleista grandiflora and Cephalosphaera usambarensis in valley,
          and Isoberlinia scheffleri and Ocotea usambarensis on ridge,
           undestory includes Soriendeia madagascariensis and tree fern 
           Cyathea manniana up to 10 m tall") %>% 
    mutate(snowdepth = 0) %>% 
    mutate(latitude = -5.087)
  
  if (i == 3 | i == 5) {
    
    usambara_list[[i]] <- usambara_list[[i]] %>% 
      mutate(height = dplyr::recode(micro, surface = 1.0)) %>%
      mutate(height_notes = "from metadata") %>% 
      # Elevations are averages of the four sensors used at both low and high
      mutate(altitude = dplyr::recode(elevation, "low" = 878.5, "high" = 1037.5))
  }
}

## 4. Write out data #########

write_csv(usambara_list[[1]], "./data/01_primary/tropical/Tanzania_usambara//derivative/TZ_usambara_high_elev.csv")
write_csv(usambara_list[[2]], "./data/01_primary/tropical/Tanzania_usambara/derivative/TZ_usambara_low_elev.csv")
write_csv(usambara_list[[3]], "./data/01_primary/tropical/Tanzania_usambara/derivative/TZ_usambara_tall.csv")
write_csv(usambara_list[[4]], "./data/01_primary/tropical/Tanzania_usambara/derivative/TZ_usambara_wide.csv")
write_csv(usambara_list[[5]], "./data/01_primary/tropical/Tanzania_usambara/derivative/TZ_usambara_avgyears_tall.csv")
write_csv(usambara_list[[6]], "./data/01_primary/tropical/Tanzania_usambara/derivative/TZ_usambara_avgyears_wide.csv")
