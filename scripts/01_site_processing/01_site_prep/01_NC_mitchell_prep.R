# David Klinges
# File creation date: 2019.08.02
# This script curates NC Mt Mitchell elevation gradient 
# raw HOBO data for analysis and generating figures


## 1. Workspace prep ############

library(tidyverse)
library(lubridate)
library(imputeTS)

## 2. Load in and bind data ###############

plots_list <- c(list.files(path = "data/01_primary/temperate/NC_mt_mitchell/original/",
                           # All of the data files are in their own subdirectories
                           #   so need to be recursive
                           pattern = ".csv", recursive = TRUE))

# Import all the data and bind together
for (i in 1:length(plots_list)) {
  ## Save metadata in the filename
  # Save year, logger IDs and sites from filename
  
  # Years are the folders, so everything after / is removed
  year <- gsub("/.*$", "", plots_list[[i]])
  
  # Now get just the filename by removing folder name and /
  filename <- substring(plots_list[[i]], first = 6)
  
  # Logger ID
  logger_ID <- substring(filename, first = 1, last = 8)
  # Elevation and site name
  site <- substring(filename, first = 22)
  site <- gsub(".csv", "", site)
  
  # 2019 data doesn't have dates in the filenames, so substring site accordingly
  if (year == "2019") {
    # Elevation and site name
    site <- substring(filename, first = 10)
    site <- gsub(".csv", "", site)
  }
  
  # Read in data
  thermal_data <- read_csv(paste0(getwd(), "/data/01_primary/temperate/NC_mt_mitchell/original/", 
                                  plots_list[[i]]),
                           skip = 1)
  
  # We only care about date-time col and full temp col, remove the rest as 
  # they're all uniquely named
  thermal_data <- thermal_data %>% 
    dplyr::select(2:4)
  
  # Rename cols so no longer unique
  colnames(thermal_data) <- c("datetime", "temp", "light_intensity")
  
  # Now join site and logger names
  thermal_data <- thermal_data %>% 
    mutate(site = site, logger_ID = as.character(logger_ID), year = year)
  
  if (i == 1) {
    thermal_data_out <- thermal_data
  } else {
    thermal_data_out <- bind_rows(thermal_data_out, thermal_data)
  }
}

## 3. Data curation #########

## ....A. Curate looped batch #########

NC_mitchell_data_tall <- thermal_data_out %>% 
  
  # Remove any loggers that fell to ground
  mutate(warning_flags = gsub("TRUE", "fell_to_ground", grepl("fell", site))) %>% 
  filter(warning_flags != "fell_to_ground") %>% 
  
  # Isolate elevation
  # For all filenames that had "low", isolate that
  mutate(elevation = gsub("TRUE", "low", grepl("low", site))) %>% 
  # IF the file didn't have "low" in it, and therefore grepl returned FALSE,
  # then search again for "high"
  mutate(elevation = ifelse(elevation == FALSE,
                            gsub("TRUE", "high", grepl("high", site)), elevation)) %>% 
  # Some filenames had neither low nor high, so just make these NA (for now)
  mutate(elevation = ifelse(elevation == FALSE, NA, elevation)) %>% 
  # And only keep observations that we're certain are from low or high
  filter(complete.cases(elevation)) %>% 
  
  # Isolate micros by the same process
  mutate(micro = gsub("TRUE", "soil", grepl("soil", site))) %>% 
  mutate(micro = ifelse(micro == FALSE,
                        # Note that Brett called them ground, I call them surface
                        gsub("TRUE", "surface", grepl("ground", site)), micro)) %>% 
  mutate(micro = ifelse(micro == FALSE,
                        gsub("TRUE", "canopy", grepl("canopy", site)), micro)) %>%
  mutate(micro = ifelse(micro == FALSE, NA, micro)) %>% 
  # And only keep observations that we're certain are from low or high
  filter(complete.cases(micro)) %>% 
  
  # Strip down the site strings
  mutate(site = gsub("low", "", site)) %>% 
  mutate(site = gsub("high", "", site)) %>% 
  mutate(site = gsub("elev", "", site)) %>% 
  mutate(site = gsub("soil", "", site)) %>% 
  mutate(site = gsub("ground", "", site)) %>% 
  mutate(site = gsub("canopy", "", site)) %>% 
  mutate(site = gsub("light", "", site)) %>% 
  mutate(site = gsub("temp", "", site)) %>% 
  mutate(site = gsub("_", "", site)) %>% 
  
  
  # NOTE: Some files had no distinguishing site name, which will now be just a blank
  # site string. Keeping them in for now
  
  # Date wrangling
  mutate(datetime = mdy_hms(datetime)) %>% 
  mutate(year = year(datetime), 
         julian = yday(datetime),
         hour = hour(datetime),
         minute = minute(datetime))

## ....B. Bind in 2016 data ##########

# Stored separately so as to not get looped into pipeline above
NC_2016_data <- read_csv("data/01_primary/temperate/NC_mt_mitchell/original_2016/NorthCarolina.csv")
NC_2016_light <- read_csv("data/01_primary/temperate/NC_mt_mitchell/original_2016/NorthCarolina_temp_and_light.csv")

NC_2016_data <- NC_2016_data %>% 
  dplyr::rename(temp = Value, datetime = Date.Time, logger_ID = buttonID) %>% 
  mutate(datetime = mdy_hm(datetime)) %>% 
  filter(para == "temp") %>% 
  mutate(micro = dplyr::recode(micro, "ground" = "surface")) %>% 
  mutate(elevation = ifelse(elev == 1700, "high", "low")) %>% 
  mutate(year = 2016) %>% 
  mutate(logger_ID = as.character(logger_ID)) %>% 
  dplyr::select(elevation, site, logger_ID, micro, datetime, year, julian, hour, minute, temp)

NC_mitchell_data_tall <- NC_mitchell_data_tall %>% 
  filter(year > 2016) %>% 
  bind_rows(NC_2016_data)

## 4. Data cleaning ##########

# Remove day 366
NC_mitchell_data_tall <- NC_mitchell_data_tall %>% 
  filter(julian <= 365)

# Plot to see if any big jumps in temp 
soil <- ggplot(NC_mitchell_data_tall, aes(julian, soil)) +
  geom_point() +
  facet_wrap(~site)

soil <- ggplot(filter(NC_mitchell_data_tall, site == "camp5", year == 2017), aes(julian, soil)) +
  geom_point()

surface <- ggplot(NC_mitchell_data_tall, aes(julian, surface)) +
  geom_point() +
  facet_wrap(~site)

canopy <- ggplot(NC_mitchell_data_tall, aes(julian, canopy)) +
  geom_point() +
  facet_wrap(~site)

canopy <- ggplot(filter(NC_mitchell_data_tall, site == "clearing"), aes(julian, canopy)) +
  geom_point() +
  facet_wrap(~year)

NC_mitchell_data_tall <- NC_mitchell_data_tall %>% 
  # Flag big jumps
  # Find difference between current value and previous value
  mutate(delta_previous_temp = temp - lag(temp)) %>% 
  # If difference is greater than 4, flag the most recent value
  mutate(warning_flags = ifelse(delta_previous_temp > 4, "TEMP_JUMP", NA)) %>% 
  
  # Flag extreme values
  mutate(warning_flags = ifelse(
    # Set extreme values for soil
    is.na(warning_flags) & micro == "soil" & (temp > 30 | temp < -10),
    "EXTREME_TEMP",
    
    # Set extreme values for surface                          
    ifelse(is.na(warning_flags) & micro == "surface" & (temp > 30 | temp < -20),
    "EXTREME_TEMP",
    
    # Set extreme values for canopy
    ifelse(is.na(warning_flags) & micro == "canopy" & (temp > 40 | temp < -20),
                                "EXTREME_TEMP", warning_flags
 
    ))))

## 5. Wrangle snow data #########

source("scripts/00_source_code/noaa_query.R")

## ....A. High elevation snowdepth ##########
mitchell_high_snow <- noaa_query( 
  stationid = "GHCND:USC00315923", # Station is MT MITCHELL
  startdate = "2016-01-01", enddate = "2019-12-31",
  datatypeid = "SNWD")

# Curate NOAA query
mitchell_high_snow <- mitchell_high_snow %>% 
  mutate(date = as_date(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  dplyr::rename(snowdepth = value) %>% 
  # Currently in mm, switch to cm
  mutate(snowdepth = snowdepth / 10) %>% 
  mutate(elevation = "high") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

mitchell_high_snow_avg <- mitchell_high_snow %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE)) %>% 
  mutate(elevation = "high")

## ....B. Low elevation snowdepth ##########
mitchell_low_snow <- noaa_query( 
  stationid = "GHCND:US1NCMD0008", # Station is OLD FORT 3.6 NW, NC US
  startdate = "2016-01-01", enddate = "2019-12-31",
  datatypeid = "SNWD")

# Curate NOAA query
mitchell_low_snow <- mitchell_low_snow %>% 
  mutate(date = as_date(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(julian = yday(date)) %>% 
  dplyr::rename(snowdepth = value) %>% 
  mutate(complete.cases(snowdepth)) %>% 
  # Currently in mm, switch to cm
  mutate(snowdepth = snowdepth / 10) %>% 
  mutate(elevation = "low") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

mitchell_low_snow_avg <- mitchell_low_snow %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE)) %>% 
  mutate(elevation = "low")

## ....C. Bind together high and low ##########

snowdepth <- mitchell_high_snow %>%
  bind_rows(mitchell_low_snow)

snowdepth_avg <- mitchell_high_snow_avg %>%
  bind_rows(mitchell_low_snow_avg)

# snowdepth <- mitchell_high_snow %>% 
#   full_join(mitchell_low_snow) %>% 
#   # Assuming an NA is 0, which is needed in order to have a fleshed-out timeseries
#   mutate(high_snowdepth = ifelse(is.na(high_snowdepth), 0, high_snowdepth)) %>% 
#   mutate(low_snowdepth = ifelse(is.na(low_snowdepth), 0, low_snowdepth)) %>% 
#   mutate(snowdepth = (low_snowdepth + high_snowdepth)/2)
# 
# snowdepth_avg <- mitchell_high_snow_avg  %>% 
#   full_join(mitchell_low_snow_avg ) %>% 
#   # Assuming an NA is 0, which is needed in order to have a fleshed-out timeseries
#   mutate(high_snowdepth = ifelse(is.na(high_snowdepth), 0, high_snowdepth)) %>% 
#   mutate(low_snowdepth = ifelse(is.na(low_snowdepth), 0, low_snowdepth)) %>% 
#   mutate(snowdepth = (low_snowdepth + high_snowdepth)/2)

## 6. Determine foliage from light intesity data ########

## ....A. Determine foliage values #########

NC_mitchell_light <- NC_2016_light %>% 
  mutate(year = year + 2000) %>% 
  dplyr::rename(light_intensity = Value2) %>% 
  bind_rows(NC_mitchell_data_tall) %>% 
  dplyr::select(year, julian, light_intensity)

light_data <- NC_mitchell_light %>% 
  filter(complete.cases(light_intensity)) %>% 
  group_by(julian) %>% 
  summarize(light_intensity = mean(light_intensity, na.rm = TRUE)) %>% 
  ungroup()

ggplot(light_data, aes(julian, light_intensity)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), 
              aes(outfit = fit <<-..y..)) # Saving smooth predictions to "fit"

fit <- tibble(
  julian = c(seq(90, 135), seq(260, 293)),
  fit = fit
)

light_data <- light_data %>% 
  left_join(fit) %>% 
  mutate(light_intensity = ifelse(complete.cases(fit), fit, light_intensity)) %>% 
  # Scale leaf off to foliage values: they are positively correlated
  # Scale bud stages to foliage values: they are positively correlated
  # From visual inspection, the mountain is ~30% coniferous. Therefore, the 
  # low value for foliage should be 0.3 (in winter there's still 30% foliage)
  mutate(foliage_cover = scales::rescale(light_intensity, to = c(1, 0.3))) %>% 
  # Gap-fill
  mutate(foliage_cover = na_interpolation(foliage_cover, option = "linear")) %>% 
  dplyr::select(julian, foliage_cover) %>% 
  mutate(foliage_cover = ifelse(julian < 90, 0.3,
                                ifelse(julian > 293, 0.3, foliage_cover)))

plot(foliage_cover ~ julian, light_data)

## 7. Conduct data curation program ###########

## ....A. Curation for Klinges and Scheffers AmNat #########
## Now spread the data by micros, but save as new object to keep a tall version
NC_mitchell_data <- NC_mitchell_data_tall %>% 
  filter(complete.cases(temp)) %>% 
  spread(key = micro, value = temp)

NC_mitchell_data <- NC_mitchell_data %>% 
  ## Filter out the clearing site 
  filter(site != "clearing") %>% 
  ## Filter out the bad temps from warning flags
  filter(is.na(warning_flags))

# The group by and summarize is working tediously slow...so we're going to do it 
# separately for soil, surface, and canopy

NC_mitchell_data_soil <- NC_mitchell_data %>% 
  filter(complete.cases(soil)) %>% 
  # Times are to the second, let's group up to the hour
  # NOTE: we're grouping by plot and NOT site...confusing col names by Ed but
  # this means we're aggregating multiple sites together
  group_by(logger_ID, site, elevation, year, julian, hour) %>%
  summarize(soil = mean(soil, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # Remove logger ID col so can join with other micros properly
  dplyr::select(-logger_ID)

NC_mitchell_data_surface <- NC_mitchell_data %>% 
  filter(complete.cases(surface)) %>% 
  group_by(logger_ID, site, elevation, year, julian, hour) %>%
  summarize(surface = mean(surface, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # Remove logger ID col so can join with other micros properly
  dplyr::select(-logger_ID)

NC_mitchell_data_canopy <- NC_mitchell_data %>% 
  filter(complete.cases(canopy)) %>% 
  group_by(logger_ID, site, elevation, year, julian, hour) %>%
  summarize(canopy = mean(canopy, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # Remove logger ID col so can join with other micros properly
  dplyr::select(-logger_ID)

# Now join back together
NC_mitchell_data <- NC_mitchell_data_soil %>% 
  full_join(NC_mitchell_data_surface) %>% 
  full_join(NC_mitchell_data_canopy)

# Now separate out by elevation
# Elev = ~1700
NC_mitchell_data_high <- NC_mitchell_data %>% 
  filter(elevation == "high") %>% 
  # Select down only to cols viable for data curation program
  dplyr::select(year, julian, soil, surface, canopy)

# Elev = ~500
NC_mitchell_data_low <- NC_mitchell_data %>% 
  filter(elevation == "low") %>% 
  # Select down only to cols viable for data curation program
  dplyr::select(year, julian, soil, surface, canopy)

## ....C. Data curation program ###########

source("scripts/00_source_code/data_curation_program.R")
NC_mitchell_curated <- prep_flux_data(low_dataset = NC_mitchell_data_low, 
                           high_dataset = NC_mitchell_data_high)

for (i in 1:length(NC_mitchell_curated)) {
  
  NC_mitchell_curated[[i]] <- NC_mitchell_curated[[i]] %>% 
    mutate(site = "NC") %>% 
    mutate(macro = "deciduous") %>% 
    mutate(flora = "mixed deciduous") %>% 
    left_join(light_data) %>% 
    # Gap-fill
    mutate(foliage_cover = na_interpolation(foliage_cover, option = "linear")) %>% 
    mutate(latitude = 36)
  
  if (i == 3 | i == 5) {
    NC_mitchell_curated[[i]] <- NC_mitchell_curated[[i]] %>% 
      mutate(height = dplyr::recode(micro, "soil" = -0.07, "surface" = 1.0, "canopy" = 20)) %>% 
      mutate(height_notes = "soil and surface measured, canopy estimated") %>%
      mutate(altitude = dplyr::recode(elevation, "low" = 500, "high" = 1700))
  }
  
  ## Add snow data
  if (i == 3) {
    # For all years
    NC_mitchell_curated[[i]] <- NC_mitchell_curated[[i]] %>% 
      left_join(snowdepth) %>% 
      ## To fill in snowdepth NAs, pulling from two WRCC sources:
      # top of Mt Mitchell
      # https://www.nohrsc.noaa.gov/interactive/html/graph.html?station=MMTN7&w=600&h=400&o=a&uc=0&by=2016&bm=1&bd=1&bh=6&ey=2019&em=5&ed=1&eh=6&data=0&units=0&region=us
      # Somewhat representative of bottom of Mt Mitchell:
      # https://wrcc.dri.edu/cgi-bin/cliMAIN.pl?sc1256
      # Designate a flag for snow data quality
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "single_avg", 
                                       "measured_daily")) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 32 | julian > 358),
                                (15.24 + .84)/2, snowdepth)) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 60 | julian > 305),
                                         (1.6 + .5)/2, snowdepth)) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian >= 60 & julian <= 305),
                                 0, snowdepth))
               
  }

  if (i == 5) {
    # For average years
    NC_mitchell_curated[[i]] <- NC_mitchell_curated[[i]] %>% 
      left_join(snowdepth_avg) %>% 
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "single_avg", 
                                       "measured_daily")) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 32 | julian > 358),
                                (15.24 + .84)/2, snowdepth)) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian < 60 | julian > 305),
                                (1.6 + .5)/2, snowdepth)) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth) & (julian >= 60 & julian <= 305),
                                0, snowdepth))
  }
}

## ....C. Determine foliage on/off #########

light_data <- NC_mitchell_data_tall %>% 
  filter(complete.cases(light_intensity)) %>% 
  # 2016 foliage dates already determined
  filter(year != 2016) %>% 
  # All 2019 data collected by Aug 2019 is leaf-off, so take out
  filter(year != 2019)

# Plot to determine limit of low light intensity
plot <- ggplot(light_data, aes(julian, light_intensity)) +
  geom_point() +
  facet_wrap(vars(site, year))

# ggsave("lightplots.png")

mean_light_data <- light_data %>% 
  group_by(site, logger_ID, year, julian) %>% 
  summarize(mean_light = mean(light_intensity))

plot <- ggplot(mean_light_data, aes(julian, mean_light)) +
  geom_point(aes(color = site), alpha = 0.5)

# ggsave("lowlightplots.png")

bySite_foliageDays <- mean_light_data %>% 
  # Filter down to just low-light days
  filter(mean_light < 40000 & mean_light != 0) %>%
  # Filter out days that I know there's no foliage
  filter(julian > 140 & julian < 300) %>%
  # Filter out days that I know there's already foliage
  filter(julian < 200 | julian > 270) %>% 
  
  # Filtering to 10000, which both is ~50% of max light exposure and corresponds
  # to the time of steep drop-off in light
  filter(mean_light < 10000) %>% 
  group_by(site, logger_ID, year) %>% 
  mutate(first_day = min(julian)) %>% 
  mutate(last_day = max(julian)) %>% 
  dplyr::select(-mean_light, -julian) %>% 
  distinct()

byYear_foliageDays <- bySite_foliageDays %>% 
  group_by(year) %>% 
  summarize_at(vars(first_day, last_day), mean) %>% 
  ungroup() %>% 
  # Manually adding 2016 and 2019
  bind_rows(tibble(
    year = c(2016, 2019),
    first_day = c(151, 141), 
    last_day = c(280, 280)
  ))

siteAvg_foliageDays <- byYear_foliageDays %>% 
  summarize_at(vars(first_day, last_day), mean) %>% 
  mutate(site = "NC")

# Bind to data curated for Klinges and Scheffers Am Nat

NC_mitchell_curated[[3]] <- NC_mitchell_curated[[3]] %>% 
  left_join(byYear_foliageDays) %>% 
  
  mutate(foliage = ifelse(julian <= first_day | julian >= last_day, "leaf-off",
                          ifelse(julian > first_day & julian < last_day, "leaf-on", NA))) %>% 
  dplyr::select(-last_day, -first_day)


NC_mitchell_curated[[5]] <- NC_mitchell_curated[[5]] %>% 
  left_join(siteAvg_foliageDays) %>% 
  
  mutate(foliage = ifelse(julian <= first_day | julian >= last_day, "leaf-off",
                          ifelse(julian > first_day & julian < last_day, "leaf-on", NA))) %>% 
  dplyr::select(-last_day, -first_day)

# Bind to tall-form data for Brett
NC_mitchell_data_tall <- NC_mitchell_data_tall %>% 
  left_join(byYear_foliageDays) %>% 
  mutate(foliage = ifelse(julian <= first_day | julian >= last_day, "leaf-off",
                          ifelse(julian > first_day & julian < last_day, "leaf-on", NA))) %>% 
  dplyr::select(datetime, year, julian, hour, minute, site, logger_ID, elevation, micro, temp, 
                light_intensity, foliage, warning_flags)

## 8. Write out data ###########

# Tall data (for Brett)
write_csv(NC_mitchell_data_tall, "data/01_primary/temperate/NC_mt_mitchell/derivative/NC_mitchell_elevationGradient.csv")

write_csv(NC_mitchell_curated[[1]], "data/01_primary/temperate/NC_mt_mitchell/derivative/NC_high_all_years.csv")
write_csv(NC_mitchell_curated[[2]],  "data/01_primary/temperate/NC_mt_mitchell/derivative/NC_low_all_years.csv")
write_csv(NC_mitchell_curated[[3]], "data/01_primary/temperate/NC_mt_mitchell/derivative/NC_tall.csv")
write_csv(NC_mitchell_curated[[4]],  "data/01_primary/temperate/NC_mt_mitchell/derivative/NC_wide.csv")
write_csv(NC_mitchell_curated[[5]], "data/01_primary/temperate/NC_mt_mitchell/derivative/NC_avgyears_tall.csv")
write_csv(NC_mitchell_curated[[6]], "data/01_primary/temperate/NC_mt_mitchell/derivative/NC_avgyears_wide.csv")


