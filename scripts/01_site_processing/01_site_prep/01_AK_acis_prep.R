# David Klinges
# File creation date: 2019.09.04
# This script curates Alaska ACIS sites to 
#   prep for analysis and generating figures

## 1. Workspace Prep #############

library(tidyverse)
library(lubridate)

# Import data 
low_tok_raw <- as_tibble(read_tsv("data/01_primary/temperate/AK_nabesna/low/tok.tab"))

high_nabesna_raw <- read_tsv("data/01_primary/temperate/AK_nabesna/high/nabesna.tab")

## 2. Curate data #########

colnames(low_tok_raw) <- c("date", "max", "mean", "min", "precip", "snowfall", "snowdepth") 
low_tok <- low_tok_raw %>% 
  gather(minmax, surface, max, mean, min) %>% 
  mutate(surface = as.double(surface)) %>% 
  filter(complete.cases(surface)) %>% 
  filter(is.finite(surface)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  mutate(snowdepth = as.double(snowdepth)) %>% 
  dplyr::select(year, julian, surface, snowdepth)

# Convert from F to C
low_tok <- low_tok %>% 
  mutate(surface = (surface - 32) * 5/9 )

colnames(high_nabesna_raw) <- c("date", "max", "mean", "min", "precip", "snowfall", "snowdepth") 
high_nabesna <- high_nabesna_raw %>% 
  gather(minmax, surface, max, mean, min) %>% 
  mutate(surface = as.double(surface)) %>% 
  filter(complete.cases(surface)) %>% 
  filter(is.finite(surface)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  mutate(snowdepth = as.double(snowdepth)) %>% 
  dplyr::select(year, julian, surface, snowdepth)

# Convert from F to C
high_nabesna <- high_nabesna %>% 
  mutate(surface = (surface - 32) * 5/9 )

## 3. Wrangle snow data ###########

# Conveniently these sites already had snowdepth data logged locally...s'pose
# this is Alaska, after all

## ....A. High elevation snowdepth #########

nabesna_snow <- high_nabesna %>% 
  mutate(elevation = "high") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

nabesna_snow_avg <- high_nabesna %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE)) %>% 
  mutate(elevation = "low")

## ....B. Low elevation snowdepth #########

tok_snow <- low_tok %>% 
  mutate(elevation = "low") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

tok_snow_avg <- low_tok %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE)) %>% 
  mutate(elevation = "low")

## ....C. Bind together high and low ##########

snowdepth <- tok_snow %>%
  bind_rows(nabesna_snow)

snowdepth_avg <- tok_snow_avg  %>%
  bind_rows(nabesna_snow_avg)

## 3. Conduct overlap data curation #########

source("scripts/00_source_code/data_curation_program.R")
acis_list <- prep_flux_data(low_dataset = low_tok, 
                              high_dataset = high_nabesna)


for (i in 1:length(acis_list)) {
  
  acis_list[[i]] <- acis_list[[i]] %>% 
    mutate(site = "AK_nabesna") %>% 
    mutate(macro = "alpine meadow") %>% 
    mutate(foliage = "leaf-off") %>% 
    mutate(foliage_cover = 0) %>% 
    mutate(flora = "unknown but likely irrelevant") %>% 
    mutate(latitude = 62.3977)
  
  if (i == 3 | i == 5) {
    
    acis_list[[i]] <- acis_list[[i]] %>% 
      mutate(height = 3) %>% 
      mutate(height_notes = "nabesna informed estimate, Tok uninformed estimate") %>% 
      mutate(altitude = dplyr::recode(elevation, "low" = 495.3, "high" = 890))
  }
  
  # Add snow data
  if (i == 3) {
    # For all years
    acis_list[[i]] <- acis_list[[i]] %>% 
      left_join(snowdepth) %>% 
      # Evidence seems pretty clear: very little snow.
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "confident_estimate", 
                                       "measured_daily")) %>% 
      # All snowdepth data is recorded at the weather station
      mutate(snowdepth = ifelse(is.na(snowdepth), 0, 
                                snowdepth))
  }
  if (i == 5) {
    # For avg years
    acis_list[[i]] <- acis_list[[i]] %>% 
      left_join(snowdepth_avg) %>% 
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "confident_estimate", 
                                       "measured_daily")) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth), 0, 
                                snowdepth))
    
  }
}

## 4. Write out data ##############

write_csv(acis_list[[1]], "data/01_primary/temperate/AK_nabesna/derivative/AK_nabesna_high_all_years.csv")
write_csv(acis_list[[2]],  "data/01_primary/temperate/AK_nabesna/derivative/AK_nabesna_low_all_years.csv")
write_csv(acis_list[[3]], "data/01_primary/temperate/AK_nabesna/derivative/AK_nabesna_tall.csv")
write_csv(acis_list[[4]],  "data/01_primary/temperate/AK_nabesna/derivative/AK_nabesna_wide.csv")
write_csv(acis_list[[5]], "data/01_primary/temperate/AK_nabesna/derivative/AK_nabesna_avgyears_tall.csv")
write_csv(acis_list[[6]], "data/01_primary/temperate/AK_nabesna/derivative/AK_nabesna_avgyears_wide.csv")

