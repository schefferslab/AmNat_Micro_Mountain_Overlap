# David Klinges
# File creation date: 2019.05.25
# This script curates NH White Mountains sites to prep for analysis and 
#   generating figures

## 1. Workspace prep ########

library(tidyverse)
library(lubridate)
library(scales)
library(imputeTS)

# Read in data

## ....1a. Loop through NEON data #########
# Load in names of zip files
files <- list.files("data/01_primary/temperate/NH_whites/bartlett_low/original/NEON_temp-air-single")

# Unzip everything
lapply(files, FUN = function(x) {
  unzip(paste0(getwd(), "/data/01_primary/temperate/NH_whites/bartlett_low/original/", x), 
        exdir = paste0(getwd(), "/data/01_primary/temperate/NH_whites/bartlett_low/original"))
})



# Identify paths for all data files
data_list <- c(list.files(path = "data/01_primary/temperate/NH_whites/bartlett_low/original", 
                          # Most of the data files are in their own subdirectories
                          #   so need to be recursive
                          pattern = "SAAT_30min", recursive = TRUE),
               list.files(path = "data/01_primary/temperate/NH_whites/bartlett_low/original", 
                          # But two date files are loose in the main directory
                          pattern = "SAAT_30min", recursive = FALSE))

# Import all excel files
data_list <- lapply(data_list, FUN = function(x) {
  data <- read_csv(paste0(getwd(), "/data/01_primary/temperate/NH_whites/bartlett_low/original/", x)
                   
  )
  # NOTE: most data files have one row of metadata below col names, but some have
  #   two. Bad data management from NERRS. With this method the 2nd row of 
  #   metadata gets swept in, which is an annoyance
})

# Join together data files
bartlett_low_raw <- do.call(bind_rows, data_list)

leafup_raw <- read_csv("data/01_primary/temperate/NH_whites/phenology/archive_knb-lter-hbr.195.2_155934250534690175/ccase_saplings_spring_phenology.txt")
pheno_raw <- read_csv("data/01_primary/temperate/NH_whites/phenology/phn.txt")

## ....1b. Load all other data #########



franc_high_raw <-  read_delim("data/01_primary/temperate/NH_whites/franc_ridge_high/original/history_export_2019-05-25T13_17_39.csv",
                          delim = ";", skip = 11)

# means and mins are getting parsed as logical because so many NAs. Changing to
# double just the ones that I want
hubbard_mean <- read_csv("data/01_primary/temperate/NH_whites/hubbard/original/tdm_.txt",
                         na = c("NA", "-99"),
                         col_types = cols(
                           STA_14 = col_double(),
                           STA_17 = col_double()
                         ))
hubbard_minmax <- read_csv("data/01_primary/temperate/NH_whites/hubbard/original/tdmm_.txt",
                           na = c("NA", "-99"),
                           col_types = cols(
                             STA14_MIN = col_double(),
                             STA14_MAX = col_double(),
                             STA17_MAX = col_double(),
                             STA17_MIN = col_double()
                           ))

soilair <- read_csv("data/01_primary/temperate/NH_whites/hubbard/original/ccase_soiltemp.txt",
                    na = "-99.99")


soil_gradient <- read_csv("data/01_primary/temperate/NH_whites/hubbard/original/archive_knb-lter-hbr.137.3_155879892302093734/snowgrad_soil.txt",
                          na = "-9999")


## 2. Curate data #########

## ....2a. Curate Bartlett low ##########

bartlett_low <- bartlett_low_raw %>% 
  dplyr::rename(mean = tempSingleMean, min = tempSingleMinimum, max = tempSingleMaximum) %>% 
  dplyr::select(startDateTime, mean, min, max) %>% 
  mutate(julian = yday(startDateTime)) %>% 
  mutate(year = year(startDateTime))

# NEON temp data is min mean max for each timestamp. Gather those into one col
bartlett_low <- bartlett_low %>% 
  gather(key = "minmax", value = "surface", min, mean, max) %>% 
  dplyr::select(year, julian, surface)

## ....2b. Curate Franc ridge high #########

franc_high <- franc_high_raw %>% 
  dplyr::rename(surface = `Temperature  [2 m above gnd]`, year = Year) %>% 
  mutate(date = as_date(paste(year, Month, Day, sep = "-"))) %>% 
  mutate(julian = yday(date)) %>% 
  dplyr::select(year, julian, surface)

## ....2c. Curate Hubbard brook surface temp ########

# A file for mean temps of multiple stations, another file for mins and maxs

surface_low <- hubbard_mean %>% 
  full_join(hubbard_minmax) %>% 
  dplyr::rename(date = DATE, mean = STA_HQ, min = STA_HQ_MIN, max = STA_HQ_MAX) %>% 
  mutate(julian = yday(date)) %>% 
  mutate(year = year(date)) %>% 
  gather(key = "minmax", value = "surface", min, mean, max) %>%
  mutate(surface = as.double(surface)) %>% 
  dplyr::select(year, julian, surface) %>% 
  filter(complete.cases(surface))
  
surface_high <- hubbard_mean %>% 
  full_join(hubbard_minmax) %>% 
  dplyr::rename(date = DATE,
         mean17 = STA_17, min17 = STA17_MIN, max17 = STA17_MAX) %>% 
  mutate(julian = yday(date)) %>% 
  mutate(year = year(date)) %>% 
  gather(key = "minmax", value = "surface", min17, mean17, max17) %>%
  mutate(surface = as.double(surface)) %>% 
  dplyr::select(year, julian, surface) %>% 
  filter(complete.cases(surface))

## ....2d. Curate Hubbard brook soil gradient ######

# Low: IL1 (375), Il2 (401), E14 (487)
# High: E3, (770), E5 (724), E11-B (766)

soil_low <- soil_gradient %>% 
  dplyr::select(Timestamp, IL1_SoilTemp, IL2_SoilTemp) %>% 
  dplyr::rename(soil1 = IL1_SoilTemp, soil2 = IL2_SoilTemp) %>% 
  # To make the data curation program easier, we're going to just add a surface col
  mutate(surface = 1) %>% 
  mutate(year = year(Timestamp)) %>% 
  mutate(julian = yday(Timestamp)) %>% 
  dplyr::select(year, julian, soil1, soil2, surface)


soil_high <- soil_gradient %>% 
  dplyr::select(Timestamp, E03_SoilTemp, E11B_SoilTemp) %>% 
  dplyr::rename(soil1 = E03_SoilTemp, soil2 = E11B_SoilTemp) %>% 
  mutate(soil2 = as.double(soil2)) %>% 
  # To make the data curation program easier, we're going to just add a surface col
  mutate(surface = 1) %>% 
  mutate(year = year(Timestamp)) %>% 
  mutate(julian = yday(Timestamp)) %>% 
  dplyr::select(year, julian, soil1, soil2, surface)

# Average across replicates

soil_low <- soil_low %>%
  mutate(soil = rowMeans(dplyr::select(soil_low, soil1, soil2), na.rm = TRUE)) %>%
  # If missing values, take them from one sensor or another
  mutate(soil = ifelse(is.na(soil), soil1, soil)) %>%
  mutate(soil = ifelse(is.na(soil), soil2, soil)) %>% 
  dplyr::select(year, julian, surface, soil)
  
  
soil_high <- soil_high %>%
    mutate(soil = rowMeans(dplyr::select(soil_high, soil1, soil2), na.rm = TRUE)) %>%
    # If missing values, take them from one sensor or another
    mutate(soil = ifelse(is.na(soil), soil1, soil)) %>%
    mutate(soil = ifelse(is.na(soil), soil2, soil)) %>% 
    dplyr::select(year, julian, surface, soil)


# Separate leaf on from leaf off

# 138 and 289, pulled from pheno data
soil_low_off <- soil_low %>% 
  filter(julian < 138 | julian > 289)

soil_low_on <- soil_low %>% 
  filter(julian >= 138 & julian <= 289)

soil_high_off <- soil_high %>% 
  filter(julian < 138 | julian > 289)

soil_high_on <- soil_high %>% 
  filter(julian >= 138 & julian <= 289)
  

## 3. Wrangle snow data #########

source("scripts/00_source_code/noaa_query.R")

## ....A. High elevation snowdepth ##########

# NOTE: USING SAME SENSOR FOR BOTH HIGH AND LOW ELEVATIONS
hubbard_high_snow <- noaa_query( 
  stationid = "GHCND:USC00270681", # Station is BENTON 5 SW, NH US
  startdate = "2010-01-01", enddate = "2013-12-31",
  datatypeid = "SNWD")

# Curate NOAA query
hubbard_high_snow <- hubbard_high_snow %>% 
  mutate(date = as_date(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  dplyr::rename(snowdepth = value) %>% 
  # Currently in mm, switch to cm
  mutate(snowdepth = snowdepth / 10) %>% 
  mutate(elevation = "high") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

hubbard_high_snow_avg <- hubbard_high_snow %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE)) %>% 
  mutate(elevation = "high")

## ....B. Low elevation snowdepth ##########

# NOTE: USING SAME SENSOR FOR BOTH HIGH AND LOW ELEVATIONS

hubbard_low_snow <- hubbard_high_snow

hubbard_low_snow_avg <- hubbard_high_snow_avg

## ....C. Bind together high and low ##########

snowdepth <- hubbard_high_snow %>%
  bind_rows(hubbard_low_snow)

snowdepth_avg <- hubbard_high_snow_avg %>%
  bind_rows(hubbard_low_snow_avg)

## 4. Calculate foliage cover values ##########

spring_pheno <- leafup_raw %>% 
  # Replace -99 with NA
  mutate(pheno_class = ifelse(pheno_class == -99, NA, pheno_class)) %>% 
  
  # Filter to just ambient temperature plots
  filter(Treatment == "Reference") %>% 
  # Create julian data from avg date
  mutate(julian = yday(Date)) %>%
  # Get the summary for all species for each day
  group_by(julian) %>% 
  summarize(pheno_class = mean(pheno_class)) %>% 
  ungroup() %>% 
  # Gap fill NAs
  mutate(lead = lead(pheno_class)) %>% 
  mutate(lag = lag(pheno_class)) %>% 
  mutate(pheno_class = ifelse(is.na(pheno_class), (lead + lag)/2, pheno_class)) %>% 
  # Scale leaf off to foliage values: they are positively correlated
  # Scale bud stages to foliage values: they are positively correlated
  # From visual inspection, the mountain is ~20% coniferous. Therefore, the 
  # low value for foliage should be 0.4 (in winter there's still 40% foliage)
  mutate(foliage_cover = scales::rescale(pheno_class, to = c(0.4, 1))) %>% 
  # Gap-fill
  mutate(foliage_cover = na_interpolation(foliage_cover,option = "linear")) %>% 
  dplyr::select(julian, foliage_cover)

fall_pheno <- pheno_raw %>% 
  # Replace -9 with NA
  mutate_all(~ gsub( -9, NA, .)) %>%
  # Just fall
  filter(SEASON == "FALL") %>% 
  # gather so all trees are in one column, all codes are in other
  gather(key = "tree", value = "pheno_class", "1B", "6T", "4B", "4T", "5B", 
         "5T", "7B", "7T", "HQ", "CONE") %>% 
  mutate(julian = as.double(DAY)) %>% 
  mutate(pheno_class = as.double(pheno_class)) %>% 
  # Get the summary for all species for each day
  group_by(julian) %>% 
  summarize(pheno_class = mean(pheno_class, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # Gap fill NAs
  mutate(lead = lead(pheno_class)) %>% 
  mutate(lag = lag(pheno_class)) %>% 
  mutate(pheno_class = ifelse(is.na(pheno_class), (lead + lag)/2, pheno_class)) %>% 
  # Scale leaf off to foliage values: they are positively correlated
  # Scale bud stages to foliage values: they are positively correlated
  # From visual inspection, the mountain is ~40% coniferous. Therefore, the 
  # low value for foliage should be 0.4 (in winter there's still 40% foliage)
  mutate(foliage_cover = scales::rescale(pheno_class, to = c(0.4, 1))) %>% 
  # Gap-fill
  mutate(foliage_cover = na_interpolation(foliage_cover, option = "linear")) %>% 
  dplyr::select(julian, foliage_cover)
  
pheno <- spring_pheno %>% 
  bind_rows(fall_pheno)

## 5. Conduct overlap data curation #############

## ....A. Overlap curation for HB soil gradient ########

# Combined leaf off and leaf on
source("scripts/00_source_code/data_curation_program.R")
hubbard_list_soil <- prep_flux_data(low_dataset = soil_low, 
                                   high_dataset = soil_high)

# Now remove surface col
for (i in 1:length(hubbard_list_soil)) {
  hubbard_list_soil[[i]] <- hubbard_list_soil[[i]] %>% 
    dplyr::select(-contains("surface"))
}


for (i in 1:length(hubbard_list_soil)) {
  
  hubbard_list_soil[[i]] <- hubbard_list_soil[[i]] %>% 
    mutate(site = "NH_forest") %>% 
    mutate(macro = "deciduous") %>% 
    mutate(foliage = ifelse(julian < 138 | julian > 289, "leaf-off", "leaf-on")) %>% 
    left_join(pheno) %>% 
    # For all days before first and after later measurement, set as floor value
    mutate(foliage_cover = ifelse(julian < min(spring_pheno$julian), 
                                  min(pheno$foliage_cover),
                                  ifelse(julian > max(fall_pheno$julian),
                                         min(pheno$foliage_cover), foliage_cover))) %>% 
    # Gap-fill
    mutate(foliage_cover = na_interpolation(foliage_cover, option = "linear")) %>% 
    mutate(flora = "mixed northern hardwood") %>% 
    mutate(latitude = 44)
  
  if (i == 3 | i == 5) {
    
    hubbard_list_soil[[i]] <- hubbard_list_soil[[i]] %>% 
      # Now remove surface variable
      filter(micro == "soil") %>% 
      mutate(height = -.05) %>% 
      mutate(height_notes = "from metadata") %>%
      mutate(altitude = dplyr::recode(elevation, "low" = 375, "high" = 770))
  }
  
  ## Add snow data
  if (i == 3) {
    # For all years
    hubbard_list_soil[[3]] <- hubbard_list_soil[[3]] %>% 
      left_join(snowdepth) %>% 
      # Because average data has every day represented, just draw from the avg
      left_join(dplyr::rename(snowdepth_avg, snowdepth_avg = snowdepth)) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth), snowdepth_avg, snowdepth)) %>% 
      dplyr::select(-snowdepth_avg) %>% 
      # Designate a flag for snow data quality
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "single_avg", 
                                       "measured_daily")) %>% 
      # Gap-filling:
      # https://www.nohrsc.noaa.gov/interactive/html/graph.html?station=BIGN3&w=600&h=400&o=a&uc=0&by=2006&bm=1&bd=1&bh=6&ey=2006&em=12&ed=31&eh=6&data=0&units=0&region=us
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 100 | julian > 330),
                                45.7,  snowdepth)) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 110 | julian > 293),
                                1.76, snowdepth)) %>% 
      mutate(snowdepth =  ifelse(is.na(snowdepth) & (julian >= 110 & julian <= 293),
                                 0, snowdepth))
  }
  
  if (i == 5) {
    # For average years
    hubbard_list_soil[[i]] <- hubbard_list_soil[[i]] %>% 
      left_join(snowdepth_avg) %>% 
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "single_avg", 
                                       "measured_daily")) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 100 | julian > 330),
                                45.7,  snowdepth)) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 110 | julian > 293),
                                1.76, snowdepth)) %>% 
      mutate(snowdepth =  ifelse(is.na(snowdepth) & (julian >= 110 & julian <= 293),
                                 0, snowdepth))
  }
}


## ....B. Overlap curation for HB non-forest surface temps #######


hubbard_list_surface <- prep_flux_data(low_dataset = surface_low, 
                               high_dataset = surface_high)
hubbard_snow <- read_csv("data/01_primary/temperate/NH_whites/derivative/NH_hubbardNonforest_surface_tall.csv")

snowdepth <- hubbard_snow %>% 
  dplyr::select(year, julian, snowdepth)

hubbard_snow_avg <- read_csv("data/01_primary/temperate/NH_whites/derivative/NH_hubbardNonforest_surface_avgyears_tall.csv")

snowdepth_avg <- hubbard_snow_avg %>% 
  dplyr::select(julian, snowdepth)

test <- hubbard_list_surface[[3]]

hubbard_snow <- hubbard_snow %>% 
  dplyr::rename(old_mean=mean, old_min = min, old_max = max)
test <- test %>% 
  left_join(dplyr::select(hubbard_snow, year, julian, old_mean, old_min, old_max))
testplot <- test %>% 
  filter_at(vars(old_max, max), complete.cases)
ggplot(testplot, aes(old_max, max)) +
  geom_point()

for (i in 1:length(hubbard_list_surface)) {
  
  hubbard_list_surface[[i]] <- hubbard_list_surface[[i]] %>% 
    mutate(site = "NH_nonforest") %>% 
    mutate(macro = "meadow near forest") %>% 
    mutate(foliage = "leaf-off") %>% 
    mutate(foliage_cover = 0) %>% 
    left_join(pheno) %>% 
    mutate(flora = NA) %>% 
    mutate(latitude = 44)
  
  if (i == 3 | i == 5) {
    
    hubbard_list_surface[[i]] <- hubbard_list_surface[[i]] %>% 
      # Now remove surface variable
      filter(micro == "surface") %>% 
      # Webpage says 5 feet off the ground = 1.524 m
      mutate(height = 1.524) %>% 
      mutate(height_notes = "from metadata") %>%
      # High is average of 729 + 898
      mutate(altitude = dplyr::recode(elevation, "low" = 259, "high" = 898))
  }
  
  ## Add snow data
  if (i == 3) {
    # For all years
    hubbard_list_surface[[i]] <- hubbard_list_surface[[i]] %>% 
      left_join(snowdepth) %>% 
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "single_avg", 
                                       "measured_daily")) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 100 | julian > 330),
                                45.7,  snowdepth)) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 110 | julian > 293),
                                1.76, snowdepth)) %>% 
      mutate(snowdepth =  ifelse(is.na(snowdepth) & (julian >= 110 & julian <= 293),
                                 0, snowdepth))
  }
  
  if (i == 5) {
    # For average years
    hubbard_list_surface[[i]] <- hubbard_list_surface[[i]] %>% 
      left_join(snowdepth_avg) %>% 
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "single_avg", 
                                       "measured_daily")) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 100 | julian > 330),
                                45.7,  snowdepth)) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 110 | julian > 293),
                                1.76, snowdepth)) %>% 
      mutate(snowdepth =  ifelse(is.na(snowdepth) & (julian >= 110 & julian <= 293),
                                 0, snowdepth))
  }
}

## 6. Write out data ############

write_csv(hubbard_list_soil[[1]], "data/01_primary/temperate/NH_whites/derivative/NH_hubbardForest_Soil_high_all_years.csv")
write_csv(hubbard_list_soil[[2]],  "data/01_primary/temperate/NH_whites/derivative/NH_hubbardForest_Soil_low_all_years.csv")
write_csv(hubbard_list_soil[[3]], "data/01_primary/temperate/NH_whites/derivative/NH_hubbardForest_Soil_tall.csv")
write_csv(hubbard_list_soil[[4]],  "data/01_primary/temperate/NH_whites/derivative/NH_hubbardForest_Soil_wide.csv")
write_csv(hubbard_list_soil[[5]], "data/01_primary/temperate/NH_whites/derivative/NH_hubbardForest_Soil_avgyears_tall.csv")
write_csv(hubbard_list_soil[[6]], "data/01_primary/temperate/NH_whites/derivative/NH_hubbardForest_Soil_avgyears_wide.csv")


write_csv(hubbard_list_surface[[1]], "data/01_primary/temperate/NH_whites/derivative/NH_hubbardNonforest_surface_high_all_years.csv")
write_csv(hubbard_list_surface[[2]],  "data/01_primary/temperate/NH_whites/derivative/NH_hubbardNonforest_surface_low_all_years.csv")
write_csv(hubbard_list_surface[[3]], "data/01_primary/temperate/NH_whites/derivative/NH_hubbardNonforest_surface_tall.csv")
write_csv(hubbard_list_surface[[4]],  "data/01_primary/temperate/NH_whites/derivative/NH_hubbardNonforest_surface_wide.csv")
write_csv(hubbard_list_surface[[5]], "data/01_primary/temperate/NH_whites/derivative/NH_hubbardNonforest_surface_avgyears_tall.csv")
write_csv(hubbard_list_surface[[6]], "data/01_primary/temperate/NH_whites/derivative/NH_hubbardNonforest_surface_avgyears_wide.csv")
