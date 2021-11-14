# David Klinges
# File creation date: 2019.08.19
# This script curates Costa Rica sites from Janzen (1967) to prep for analysis and 
#   generating figures


## 1. Workspacep prep #########

library(tidyverse)

sites <- read_csv("data/01_primary/tropical/CR_janzen/original/CR_janzen_sites.csv")

## 2. Data curation #######

northwest <- sites %>% 
  filter(site == "Naranjo" | site == "Puntarenas") %>% 
  gather(key = "minmax", value = "surface", min, max) %>% 
  mutate(year = 1964) %>% 
  # Temporarily rename month to julian for data curation program
  dplyr::rename(julian = month) %>% 
  dplyr::select(site, year, julian, surface)

puntarenas_low <- northwest %>% 
  filter(site == "Puntarenas") %>% 
  dplyr::select(year, julian, surface)

naranjo_high <- northwest %>% 
  filter(site == "Naranjo") %>% 
  dplyr::select(year, julian, surface)

southwest <- sites %>% 
  filter(site == "Villa Mills" | site == "Palma Sur") %>% 
  gather(key = "minmax", value = "surface", min, max) %>% 
  mutate(year = 1961) %>% 
  # Temporarily rename month to julian for data curation program
  dplyr::rename(julian = month) %>% 
  dplyr::select(site, year, julian, surface)

palma_low <- southwest %>% 
  filter(site == "Palma Sur") %>% 
  dplyr::select(year, julian, surface)

villa_high <- southwest %>% 
  filter(site == "Villa Mills") %>% 
  dplyr::select(year, julian, surface)

## 3. Conduct overlap data curation #############

## ....3a. Overlap curation for northwest gradient ########

source("scripts/00_source_code/data_curation_program.R")
northwest_list <- prep_flux_data(low_dataset = puntarenas_low, 
                               high_dataset = naranjo_high)


for (i in 1:length(northwest_list)) {
  
  northwest_list[[i]] <- northwest_list[[i]] %>% 
    mutate(site = "CR_northwest") %>% 
    mutate(macro = "developed") %>% 
    mutate(foliage = "leaf-off") %>% 
    mutate(foliage_cover = 0) %>% 
    mutate(flora = "unknown") %>% 
    mutate(latitude = 10) %>% 
    mutate(snowdepth = 0) %>% 
    # Change julian back to month
    dplyr::rename(month = julian)
  
  if (i == 3 | i == 5) {
    northwest_list[[i]] <- northwest_list[[i]] %>% 
      mutate(height = dplyr::recode(micro, "surface" = 1.6)) %>% 
      mutate(height_notes = "from metadata") %>%
      mutate(altitude = dplyr::recode(elevation, "low" = 3, "high" = 1042))
  }
}

## ....3a. Overlap curation for southwest gradient ########

southwest_list <- prep_flux_data(low_dataset = palma_low, 
                                 high_dataset = villa_high)


for (i in 1:length(southwest_list)) {
  
  southwest_list[[i]] <- southwest_list[[i]] %>% 
    mutate(site = "CR_southwest") %>% 
    mutate(macro = "developed") %>% 
    mutate(foliage = "leaf-off") %>%
    mutate(foliage_cover = 0) %>% 
    mutate(flora = "nearby forest has Quercus copeyensis and Quercus costaricensis") %>% 
    mutate(latitude = 9) %>% 
    mutate(snowdepth = 0) %>% 
    # Change julian back to month
    dplyr::rename(month = julian)
  
  if (i == 3 | i == 5) {
    southwest_list[[i]] <- southwest_list[[i]] %>% 
      mutate(height = dplyr::recode(micro, "surface" = 1.6)) %>% 
      mutate(height_notes = "from metadata") %>% 
      mutate(altitude = dplyr::recode(elevation, "low" = 16, "high" = 3096))
  }
}


## 4. Write out data ##########

write_csv(northwest_list[[1]], "data/01_primary/tropical/CR_janzen//derivative/CR_northwest_high_all_years.csv")
write_csv(northwest_list[[2]],  "data/01_primary/tropical/CR_janzen/derivative/CR_northwest_low_all_years.csv")
write_csv(northwest_list[[3]], "data/01_primary/tropical/CR_janzen/derivative/CR_northwest_tall.csv")
write_csv(northwest_list[[4]],  "data/01_primary/tropical/CR_janzen/derivative/CR_northwest_wide.csv")
write_csv(northwest_list[[5]], "data/01_primary/tropical/CR_janzen/derivative/CR_northwest_avgyears_tall.csv")
write_csv(northwest_list[[6]], "data/01_primary/tropical/CR_janzen/derivative/CR_northwest_avgyears_wide.csv")

write_csv(southwest_list[[1]], "data/01_primary/tropical/CR_janzen//derivative/CR_southwest_high_all_years.csv")
write_csv(southwest_list[[2]],  "data/01_primary/tropical/CR_janzen/derivative/CR_southwest_low_all_years.csv")
write_csv(southwest_list[[3]], "data/01_primary/tropical/CR_janzen/derivative/CR_southwest_tall.csv")
write_csv(southwest_list[[4]],  "data/01_primary/tropical/CR_janzen/derivative/CR_southwest_wide.csv")
write_csv(southwest_list[[5]], "data/01_primary/tropical/CR_janzen/derivative/CR_southwest_avgyears_tall.csv")
write_csv(southwest_list[[6]], "data/01_primary/tropical/CR_janzen/derivative/CR_southwest_avgyears_wide.csv")

