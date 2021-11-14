# David Klinges
# File creation date: 2019.09.16
# This script queries and curates sites from R. Senior's pantropical synthesis to 
#   prep for analysis and generating figures

## 1. Workspace prep ############

library(tidyverse)
library(lubridate)

senior_synthesis_raw <- read_rds("data/01_primary/tropical/Senior_global/original/SeniorRA_et_al_data_2017-06-28.Rds")
glimpse(senior_synthesis_raw)

## 2. Query appropriate sites (with elev gradient) ################

# Determine number of elevations represented in each country
multi_elev <- senior_synthesis_raw %>% 
  dplyr::select(country, elevation, latitude, longitude) %>% 
  distinct() %>% 
  # Round lat and long to nearest whole number
  mutate(latitude = round(latitude, 0)) %>% 
  mutate(longitude = round(longitude, 0)) 


# Plot to determine elevation ranges
ggplot(multi_elev, aes(elevation, country))

## 3. Data curation ###############

## ....A. Mexico transects ###########

# Subset to mex data
mexico <- senior_synthesis_raw %>% 
  filter(country == "mexico") %>% 
  dplyr::select(date, site_no, rep_no, cover, elevation, LUT, position, 
                latitude, longitude, min_temp, max_temp, temp, study_doi)

# Investigate
mex_sites <- mexico %>% 
  dplyr::select(date, site_no, cover, elevation, latitude, longitude, position, study_doi) %>% 
  distinct() %>% 
  mutate(date = dmy(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date))

mex_sites_soil <- mex_sites %>% 
  filter(position == "below-ground")

# I scanned through soil data, and there's no two sites that were sampled
# at the same time with min and max daily temps

mex_sites_surface<- mex_sites %>% 
  filter(position == "above-ground")

# I scanned through surface data, and there's no elevation gradient


## ....B. Costa Rica transects ###########

costa_rica <- senior_synthesis_raw %>% 
  filter(country == "costa rica") %>% 
  dplyr::select(date, site_no, rep_no, cover, elevation, LUT, position, 
                latitude, longitude, min_temp, max_temp, temp, study_doi)

costa_rica_sites <- costa_rica  %>% 
  dplyr::select(date, site_no, cover, elevation, latitude, longitude, position, study_doi) %>% 
  distinct() %>% 
  mutate(date = dmy(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date))

# There's no temporal overlap (same julian) between sites.....


## ....C. Malaysia transects ##########

malaysia <- senior_synthesis_raw %>% 
  filter(country == "malaysia")%>% 
  dplyr::select(date, site_no, rep_no, cover, elevation, LUT, position, 
                latitude, longitude, min_temp, max_temp, temp, study_doi)

malaysia_sites <- malaysia  %>% 
  dplyr::select(date, site_no, cover, elevation, latitude, longitude, position, study_doi) %>% 
  distinct() %>% 
  mutate(date = dmy(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date))

# Code that I have now removed demonstrates there's temporal overlap. Find out
# what sites to choose

## .......** Degraded forest ##################

degrad <- malaysia %>% 
  mutate(date = dmy(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>%
  filter(complete.cases(min_temp)) %>% 
  filter(LUT == "degraded_forest")

malaysia_degrad_low <- degrad %>% 
  filter(elevation < 310) %>% 
  dplyr::select(year, julian)

malaysia_degrad_high <- degrad %>% 
  filter(elevation > 430) %>% 
  dplyr::select(year, julian)

overlap <- inner_join(malaysia_degrad_low, malaysia_degrad_high)
# There's overlapping days in the same years

# Elevation is 309
malaysia_degrad_low <- degrad %>% 
  dplyr::select(-site_no) %>% 
  filter(complete.cases(min_temp)) %>% 
  filter(elevation == 309) %>% 
  gather(key = "minmax", value = "temp", min_temp, max_temp, temp) %>% 
  spread(key = "position", value = "temp") %>% 
  dplyr::rename(surface = "above-ground") %>% 
  dplyr::select(year, julian, surface)

# Elevation is 435 and 433.8462
malaysia_degrad_high <- degrad %>% 
  filter(elevation > 430 & elevation < 437) %>% 
  # filter(latitude < 4.7) %>%
  gather(key = "minmax", value = "temp", min_temp, max_temp, temp) %>% 
  spread(key = "position", value = "temp") %>% 
  dplyr::rename(surface = "above-ground") %>% 
  dplyr::select(year, julian, surface)

unique(filter(degrad, elevation == 309)$latitude)
unique(filter(degrad, elevation %in% c(436, 435, 432, 433.8462))$latitude)

unique(filter(degrad, elevation == 309)$longitude)
unique(filter(degrad, elevation %in% c(436, 435, 432, 433.8462))$longitude)

unique(filter(degrad, elevation == 309)$study_doi)
unique(filter(degrad, elevation %in% c(436, 435, 432, 433.8462))$study_doi)

## .......** Primary forest ######################

primary <- malaysia %>% 
  mutate(date = dmy(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>%
  filter(complete.cases(min_temp)) %>% 
  filter(LUT == "primary_forest")

malaysia_primary_low <- primary %>% 
  filter(elevation < 320) %>% 
  dplyr::select(year, julian)

malaysia_primary_high <- primary %>% 
  filter(elevation > 430) %>% 
  dplyr::select(year, julian)

overlap <- inner_join(malaysia_primary_low, malaysia_primary_high)
# There's overlapping days in the same years

# Elevation is 313 and 318
malaysia_primary_low <- primary %>% 
  dplyr::select(-site_no) %>% 
  filter(complete.cases(min_temp)) %>% 
  filter(elevation < 320) %>% 
  gather(key = "minmax", value = "temp", min_temp, max_temp, temp) %>% 
  spread(key = "position", value = "temp") %>% 
  dplyr::rename(surface = "above-ground") %>% 
  dplyr::select(year, julian, surface)

# Elevation is 438.00
malaysia_primary_high <- primary %>% 
  filter(elevation == 438) %>% 
  gather(key = "minmax", value = "temp", min_temp, max_temp, temp) %>% 
  spread(key = "position", value = "temp") %>% 
  dplyr::rename(surface = "above-ground") %>% 
  dplyr::select(year, julian, surface)


unique(filter(primary, elevation == 438)$latitude)
unique(filter(primary, elevation %in% c(313, 318))$latitude)

unique(filter(primary, elevation == 438)$longitude)
unique(filter(primary, elevation %in% c(313, 318))$longitude)

unique(filter(primary, elevation == 438)$study_doi)
unique(filter(primary, elevation %in% c(313, 318))$study_doi)

## 4. Conduct overlap data curation #########

source("scripts/00_source_code/data_curation_program.R")
malaysia_degraded_list <- prep_flux_data(low_dataset = malaysia_degrad_low, 
                            high_dataset = malaysia_degrad_high)

malaysia_primary_list <- prep_flux_data(low_dataset = malaysia_primary_low, 
                                high_dataset = malaysia_primary_high)



for (i in 1:length(malaysia_primary_list)) {
  
  malaysia_primary_list[[i]] <- malaysia_primary_list[[i]] %>% 
    mutate(site = "MY_SAFE_primary") %>% 
    mutate(macro = "tropical broadleaf") %>% 
    mutate(foliage = "leaf-on") %>% 
    mutate(foliage_cover = 1) %>% 
    mutate(flora = "various broadleaf") %>% 
    mutate(latitude = mean(c(4.750758, 4.747551, 4.747724)))
  
  if (i == 3 | i == 5) {
    
    malaysia_primary_list[[i]] <- malaysia_primary_list[[i]] %>% 
      mutate(height = 1.5) %>% 
      mutate(altitude = dplyr::recode(elevation, "low" = mean(c(313, 318)), "high" = 438)) %>% 
      mutate(snowdepth = 0)
  }
}

for (i in 1:length(malaysia_degraded_list)) {
  
  malaysia_degraded_list[[i]] <- malaysia_degraded_list[[i]] %>% 
    mutate(site = "MY_SAFE_degraded") %>% 
    mutate(macro = "degraded tropical broadleaf") %>% 
    mutate(foliage = "leaf-on") %>% 
    mutate(foliage_cover = 1) %>% 
    mutate(flora = "degraded broadleaf") %>% 
    mutate(latitude = mean(c(4.711417, 4.693722, 4.771226, 4.756744, 4.724393)))
  
  if (i == 3 | i == 5) {
    
    malaysia_degraded_list[[i]] <- malaysia_degraded_list[[i]] %>% 
      mutate(height = 1.5) %>% 
      mutate(altitude = dplyr::recode(elevation, "low" = 309, "high" = mean(c(435, 433.8462)))) %>% 
      mutate(snowdepth = 0)
  }
}

## 5. Write out data ##############

write_csv(malaysia_primary_list[[1]], "data/01_primary/tropical/Senior_global/MY_SAFE/derivative/MY_SAFE_primary_high_all_years.csv")
write_csv(malaysia_primary_list[[2]],  "data/01_primary/tropical/Senior_global/MY_SAFE/derivative/MY_SAFE_primary_low_all_years.csv")
write_csv(malaysia_primary_list[[3]], "data/01_primary/tropical/Senior_global/MY_SAFE/derivative/MY_SAFE_primary_tall.csv")
write_csv(malaysia_primary_list[[4]],  "data/01_primary/tropical/Senior_global/MY_SAFE/derivative/MY_SAFE_primary_wide.csv")
write_csv(malaysia_primary_list[[5]], "data/01_primary/tropical/Senior_global/MY_SAFE/derivative/MY_SAFE_primary_avgyears_tall.csv")
write_csv(malaysia_primary_list[[6]], "data/01_primary/tropical/Senior_global/MY_SAFE/derivative/MY_SAFE_primary_avgyears_wide.csv")

write_csv(malaysia_degraded_list[[1]], "data/01_primary/tropical/Senior_global/MY_SAFE/derivative/MY_SAFE_degraded_high_all_years.csv")
write_csv(malaysia_degraded_list[[2]],  "data/01_primary/tropical/Senior_global/MY_SAFE/derivative/MY_SAFE_degraded_low_all_years.csv")
write_csv(malaysia_degraded_list[[3]], "data/01_primary/tropical/Senior_global/MY_SAFE/derivative/MY_SAFE_degraded_tall.csv")
write_csv(malaysia_degraded_list[[4]],  "data/01_primary/tropical/Senior_global/MY_SAFE/derivative/MY_SAFE_degraded_wide.csv")
write_csv(malaysia_degraded_list[[5]], "data/01_primary/tropical/Senior_global/MY_SAFE/derivative/MY_SAFE_degraded_avgyears_tall.csv")
write_csv(malaysia_degraded_list[[6]], "data/01_primary/tropical/Senior_global/MY_SAFE/derivative/MY_SAFE_degraded_avgyears_wide.csv")
