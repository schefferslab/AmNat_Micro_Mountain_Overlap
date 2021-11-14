# David Klinges
# File creation date: 2019-08-20
# This script curates Colombia data from

## 1. Workspace prep ########

library(tidyverse)
library(lubridate)

sites_raw <- read_csv("data/01_primary/tropical/Colombia/original/ibutton_data_for_BrettS.csv")

## 2. Data curation ########

sites <- sites_raw %>% 
  mutate(datetime = dmy_hm(date))

# Note: not all of Pamela's sites has soil, ground, AND canopy, and they are 
# either young secondary, old secondary, or primary.

# I BELIEVE that Montezume is reserve == 1 (high), and La Mesenia is reserve == 2 (low)

## ....A. Old Secondary: surface + canopy #########

## ......** low OSF 1: surface and canopy ################
low_osf_1 <- sites %>% 
  dplyr::rename(micro = mh) %>% 
  filter(elev == 1315) %>% 
  filter(complete.cases(temp) & complete.cases(micro))

low_osf_1_surface <- low_osf_1 %>% 
  filter(micro == "ambient") %>% 
  dplyr::rename(surface = temp) %>% 
  dplyr::select(datetime, surface)

low_osf_1_canopy <- low_osf_1 %>% 
  filter(micro == "canopy") %>% 
  dplyr::rename(canopy = temp) %>% 
  dplyr::select(datetime, canopy)

low_osf_1 <- low_osf_1_surface %>% 
  full_join(low_osf_1_canopy) %>% 
  mutate(year = year(datetime)) %>% 
  mutate(julian = yday(datetime)) %>% 
  dplyr::select(year, julian, surface, canopy)

## ......** high OSF 1: surface and canopy ################

high_osf_1 <- sites %>% 
  dplyr::rename(micro = mh) %>% 
  filter(elev == 2357) %>% 
  filter(complete.cases(temp) & complete.cases(micro))

high_osf_1_surface <- high_osf_1 %>% 
  filter(micro == "ambient") %>% 
  dplyr::rename(surface = temp) %>% 
  dplyr::select(datetime, surface)

high_osf_1_canopy <- high_osf_1 %>% 
  filter(micro == "canopy") %>% 
  dplyr::rename(canopy = temp) %>% 
  dplyr::select(datetime, canopy)

high_osf_1 <- high_osf_1_surface %>% 
  full_join(high_osf_1_canopy) %>% 
  mutate(year = year(datetime)) %>% 
  mutate(julian = yday(datetime)) %>% 
  dplyr::select(year, julian, surface, canopy)


## ....B. Primary: 1x surface + canopy and 1x just soil ########

## ......** low PF 1: surface and canopy ################

low_pf_1 <- sites %>% 
  dplyr::rename(micro = mh) %>% 
  filter(elev == 1455) %>% 
  filter(complete.cases(temp) & complete.cases(micro))

low_pf_1_surface <- low_pf_1 %>% 
  filter(micro == "ambient") %>% 
  dplyr::rename(surface = temp) %>% 
  dplyr::select(datetime, surface)

low_pf_1_canopy <- low_pf_1 %>% 
  filter(micro == "canopy") %>% 
  dplyr::rename(canopy = temp) %>% 
  dplyr::select(datetime, canopy)

low_pf_1 <- low_pf_1_surface %>% 
  full_join(low_pf_1_canopy) %>% 
  mutate(year = year(datetime)) %>% 
  mutate(julian = yday(datetime)) %>% 
  dplyr::select(year, julian, surface, canopy)

## ......** low PF 2: soil ###############

low_pf_2 <- sites %>% 
  dplyr::rename(micro = mh) %>% 
  filter(elev == 1439) %>% 
  filter(complete.cases(temp) & complete.cases(micro)) %>% 
  filter(micro == "soil") %>% 
  dplyr::rename(soil = temp) %>% 
  dplyr::select(datetime, soil) %>% 
  mutate(year = year(datetime)) %>% 
  mutate(julian = yday(datetime)) %>% 
  dplyr::select(year, julian, soil) %>% 
  # Add surface data 
  mutate(surface = NA)

## ......** high PF 1: surface and canopy ###############

high_pf_1 <- sites %>% 
  dplyr::rename(micro = mh) %>% 
  filter(elev == 2494) %>% 
  filter(complete.cases(temp) & complete.cases(micro))

high_pf_1_surface <- high_pf_1 %>% 
  filter(micro == "ambient") %>% 
  dplyr::rename(surface = temp) %>% 
  dplyr::select(datetime, surface)

high_pf_1_canopy <- high_pf_1 %>% 
  filter(micro == "canopy") %>% 
  dplyr::rename(canopy = temp) %>% 
  dplyr::select(datetime, canopy)

high_pf_1 <- high_pf_1_surface %>% 
  full_join(high_pf_1_canopy) %>% 
  mutate(year = year(datetime)) %>% 
  mutate(julian = yday(datetime)) %>% 
  dplyr::select(year, julian, surface, canopy)

## ......** high PF 2: soil ###############

high_pf_2 <- sites %>% 
  dplyr::rename(micro = mh) %>% 
  filter(elev == 2494) %>% 
  filter(complete.cases(temp) & complete.cases(micro)) %>% 
  filter(micro == "soil") %>% 
  dplyr::rename(soil = temp) %>% 
  dplyr::select(datetime, soil) %>% 
  mutate(year = year(datetime)) %>% 
  mutate(julian = yday(datetime)) %>% 
  dplyr::select(year, julian, soil) %>% 
  # Add surface data 
  mutate(surface = NA)

## ....C. Young Secondary: ##################

## ......** low YSF 1: surface and canopy ################

low_ysf_1 <- sites %>% 
  dplyr::rename(micro = mh) %>% 
  filter(elev == 1499) %>% 
  filter(complete.cases(temp) & complete.cases(micro))

low_ysf_1_surface <- low_ysf_1 %>% 
  filter(micro == "ambient") %>% 
  dplyr::rename(surface = temp) %>% 
  dplyr::select(datetime, surface)

low_ysf_1_canopy <- low_ysf_1 %>% 
  filter(micro == "canopy") %>% 
  dplyr::rename(canopy = temp) %>% 
  dplyr::select(datetime, canopy)

low_ysf_1 <- low_ysf_1_surface %>% 
  full_join(low_ysf_1_canopy) %>% 
  mutate(year = year(datetime)) %>% 
  mutate(julian = yday(datetime)) %>% 
  dplyr::select(year, julian, surface, canopy)

## ......** high YSF 1: surface and canopy ###############

high_ysf_1 <- sites %>% 
  dplyr::rename(micro = mh) %>% 
  filter(elev == 2331) %>% 
  filter(complete.cases(temp) & complete.cases(micro))

high_ysf_1_surface <- high_ysf_1 %>% 
  filter(micro == "ambient") %>% 
  dplyr::rename(surface = temp) %>% 
  dplyr::select(datetime, surface)

high_ysf_1_canopy <- high_ysf_1 %>% 
  filter(micro == "canopy") %>% 
  dplyr::rename(canopy = temp) %>% 
  dplyr::select(datetime, canopy)

high_ysf_1 <- high_ysf_1_surface %>% 
  full_join(high_ysf_1_canopy) %>% 
  mutate(year = year(datetime)) %>% 
  mutate(julian = yday(datetime)) %>% 
  dplyr::select(year, julian, surface, canopy)


## 3. Data curation program ###########

## ....A. Old secondary 1 ############


osf_list_1 <- prep_flux_data(low_dataset = low_osf_1,
                            high_dataset = high_osf_1)

for (i in 1:length(osf_list_1)) {
  
  osf_list_1[[i]] <- osf_list_1[[i]] %>% 
    mutate(site = "CO_OldSecondary_1") %>% 
    mutate(macro = "tropical broadleaf") %>% 
    mutate(foliage = "leaf-on") %>% 
    mutate(foliage_cover = 1) %>% 
    mutate(flora = "unknown") %>% 
    mutate(snowdepth = 0) %>% 
    mutate(latitude = 5.36)
  
  if (i == 3 | i == 5) {
    
    osf_list_1[[i]] <- osf_list_1[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.2, surface = 1.0, canopy = 11.5)) %>%
      mutate(height_notes = "soil and surface measured, canopy estimate") %>% 
      mutate(altitude = dplyr::recode(elevation, "low" = 1315, "high" = 2357))
  }
}

## ....B. Primary 1 ############

source("scripts/00_source_code/data_curation_program.R")
pf_list_1 <- prep_flux_data(low_dataset = low_pf_1,
                             high_dataset = high_pf_1)

for (i in 1:length(pf_list_1)) {
  
  pf_list_1[[i]] <- pf_list_1[[i]] %>% 
    mutate(site = "CO_PrimaryForest_1") %>% 
    mutate(macro = "tropical broadleaf") %>% 
    mutate(foliage = "leaf-on") %>% 
    mutate(foliage_cover = 1) %>% 
    mutate(flora = "unknown") %>% 
    mutate(snowdepth = 0) %>% 
    mutate(latitude = 5.36)
  
  if (i == 3 | i == 5) {
    
    pf_list_1[[i]] <- pf_list_1[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.2, surface = 1.0, canopy = 11.5)) %>%
      mutate(height_notes = "soil and surface measured, canopy estimate") %>% 
      mutate(altitude = dplyr::recode(elevation, "low" = 1455, "high" = 2494))
  }
}


## ....C. Primary 2 ############


pf_list_2 <- prep_flux_data(low_dataset = low_pf_2,
                          high_dataset = high_pf_2)

for (i in 1:length(pf_list_2)) {
  
  pf_list_2[[i]] <- pf_list_2[[i]] %>% 
    # Remove surface, which is all NA
    dplyr::select(-contains("surface")) %>% 
    mutate(site = "CO_PrimaryForest_2") %>% 
    mutate(macro = "tropical broadleaf") %>% 
    mutate(foliage = "leaf-on") %>% 
    mutate(foliage_cover = 1) %>% 
    mutate(snowdepth = 0) %>% 
    mutate(flora = "unknown") %>% 
    mutate(latitude = 5.36)
  
  if (i == 3 | i == 5) {
    
    pf_list_2[[i]] <- pf_list_2[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.2, surface = 1.0, canopy = 11.5)) %>%
      mutate(height_notes = "soil and surface measured, canopy estimate") %>% 
      mutate(altitude = dplyr::recode(elevation, "low" = 1439, "high" = 2494))
  }
}


## ....D. Young secondary 1 ############


ysf_list_1 <- prep_flux_data(low_dataset = low_ysf_1,
                             high_dataset = high_ysf_1)

for (i in 1:length(ysf_list_1)) {
  
  ysf_list_1[[i]] <- ysf_list_1[[i]] %>% 
    mutate(site = "CO_YoungSecondary_1") %>% 
    mutate(macro = "tropical broadleaf") %>% 
    mutate(foliage = "leaf-on") %>% 
    mutate(foliage_cover = 1) %>% 
    mutate(flora = "unknown") %>% 
    mutate(snowdepth = 0) %>% 
    mutate(latitude = 5.36)
  
  if (i == 3 | i == 5) {
    
    ysf_list_1[[i]] <- ysf_list_1[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.2, surface = 1.0, canopy = 11.5)) %>%
      mutate(height_notes = "soil and surface measured, canopy estimate") %>% 
      mutate(altitude = dplyr::recode(elevation, "low" = 1499, "high" = 2331))
  }
}


## 4. Write out data #########

write_csv(osf_list_1[[1]], "./data/01_primary/tropical/Colombia//derivative/CO_osf_1_high_elev.csv")
write_csv(osf_list_1[[2]], "./data/01_primary/tropical/Colombia/derivative/CO_osf_1_low_elev.csv")
write_csv(osf_list_1[[3]], "./data/01_primary/tropical/Colombia/derivative/CO_osf_1_tall.csv")
write_csv(osf_list_1[[4]], "./data/01_primary/tropical/Colombia/derivative/CO_osf_1_wide.csv")
write_csv(osf_list_1[[5]], "./data/01_primary/tropical/Colombia/derivative/CO_osf_1_avgyears_tall.csv")
write_csv(osf_list_1[[6]], "./data/01_primary/tropical/Colombia/derivative/CO_osf_1_avgyears_wide.csv")

write_csv(pf_list_1[[1]], "./data/01_primary/tropical/Colombia//derivative/CO_pf_1_high_elev.csv")
write_csv(pf_list_1[[2]], "./data/01_primary/tropical/Colombia/derivative/CO_pf_1_low_elev.csv")
write_csv(pf_list_1[[3]], "./data/01_primary/tropical/Colombia/derivative/CO_pf_1_tall.csv")
write_csv(pf_list_1[[4]], "./data/01_primary/tropical/Colombia/derivative/CO_pf_1_wide.csv")
write_csv(pf_list_1[[5]], "./data/01_primary/tropical/Colombia/derivative/CO_pf_1_avgyears_tall.csv")
write_csv(pf_list_1[[6]], "./data/01_primary/tropical/Colombia/derivative/CO_pf_1_avgyears_wide.csv")

write_csv(pf_list_2[[1]], "./data/01_primary/tropical/Colombia//derivative/CO_pf_2_high_elev.csv")
write_csv(pf_list_2[[2]], "./data/01_primary/tropical/Colombia/derivative/CO_pf_2_low_elev.csv")
write_csv(pf_list_2[[3]], "./data/01_primary/tropical/Colombia/derivative/CO_pf_2_tall.csv")
write_csv(pf_list_2[[4]], "./data/01_primary/tropical/Colombia/derivative/CO_pf_2_wide.csv")
write_csv(pf_list_2[[5]], "./data/01_primary/tropical/Colombia/derivative/CO_pf_2_avgyears_tall.csv")
write_csv(pf_list_2[[6]], "./data/01_primary/tropical/Colombia/derivative/CO_pf_2_avgyears_wide.csv")

write_csv(ysf_list_1[[1]], "./data/01_primary/tropical/Colombia//derivative/CO_ysf_1_high_elev.csv")
write_csv(ysf_list_1[[2]], "./data/01_primary/tropical/Colombia/derivative/CO_ysf_1_low_elev.csv")
write_csv(ysf_list_1[[3]], "./data/01_primary/tropical/Colombia/derivative/CO_ysf_1_tall.csv")
write_csv(ysf_list_1[[4]], "./data/01_primary/tropical/Colombia/derivative/CO_ysf_1_wide.csv")
write_csv(ysf_list_1[[5]], "./data/01_primary/tropical/Colombia/derivative/CO_ysf_1_avgyears_tall.csv")
write_csv(ysf_list_1[[6]], "./data/01_primary/tropical/Colombia/derivative/CO_ysf_1_avgyears_wide.csv")


