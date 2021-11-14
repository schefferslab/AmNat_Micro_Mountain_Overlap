# David Klinges, dklinges9@gmail.com
# This script holds a function that curates the pre-standardized thermal overlap
#   datasets

## * Curate data for daily mean, min, max ###########
library(tidyverse)
# Write functions to calculate daily mean, min, max
dailymean_avg <- function(df) {
  dailymean <- df %>%
    dplyr::filter(is.na(temp) == FALSE) %>%
    dplyr::group_by(julian) %>%
    dplyr::summarize(mean = mean(temp, na.rm = TRUE))
  return(dailymean)
}

dailymin_avg <- function(df) {
  dailymin <- df %>%
    dplyr::filter(is.na(temp) == FALSE) %>%
    dplyr::group_by(year, julian) %>%
    dplyr::summarize(min = min(temp, na.rm = TRUE)) %>%
    ungroup() %>%
    dplyr::group_by(julian) %>%
    dplyr::summarize(min = mean(min, na.rm = TRUE))
  return(dailymin)
}

dailymax_avg <- function(df) {
  dailymax <- df %>%
    dplyr::filter(is.na(temp) == FALSE) %>%
    dplyr::group_by(year, julian) %>%
    dplyr::summarize(max = max(temp, na.rm = TRUE)) %>%
    ungroup() %>%
    dplyr::group_by(julian) %>%
    dplyr::summarize(max = mean(max, na.rm = TRUE))
  return(dailymax)
}

## ** Curation functions for daily averages allyears ############

# Customized daily functions for all years
dailymean_allyears <- function(df) {
  dailymean <- df %>%
    group_by(year, julian) %>%
    summarize_all(mean, na.rm = TRUE)
  # Need to ungroup so that the grouped variables (year, julian) don't get
  #   carried over with the select + contains
  dailymean <- ungroup(dailymean)
  # If soil data exists...
  if (length(dplyr::select(dailymean, contains("soil"))) > 0) {
    dailymean <- dailymean %>%
      dplyr::rename(soil_mean = soil)
  }
  
  # If surface data exists...
  if (length(dplyr::select(dailymean, contains("surface"))) > 0) {
    dailymean <- dailymean %>%
      dplyr::rename(surface_mean = surface)
  }
  
  # If canopy data exists...
  if (length(dplyr::select(dailymean, contains("canopy"))) > 0) {
    dailymean <- dailymean %>%
      dplyr::rename(canopy_mean = canopy)
  }
  
  return(dailymean)
}

dailymin_allyears <- function(df) {
  dailymin <- df %>%
    group_by(year, julian) %>%
    summarize_all(min, na.rm = TRUE)
  # Need to ungroup so that the grouped variables (year, julian) don't get
  #   carried over with the select + contains
  dailymin <- ungroup(dailymin)
  # If soil data exists...
  if (length(dplyr::select(dailymin, contains("soil"))) > 0) {
      dailymin <- dailymin %>%
        dplyr::rename(soil_min = soil)
    }
  
  # If surface data exists...
  if (length(dplyr::select(dailymin, contains("surface"))) > 0) {
    dailymin <- dailymin %>%
      dplyr::rename(surface_min = surface)
  }
  
  # If canopy data exists...
  if (length(dplyr::select(dailymin, contains("canopy"))) > 0) {
    dailymin <- dailymin %>%
      dplyr::rename(canopy_min = canopy)
  }

  return(dailymin)
}

dailymax_allyears <- function(df) {
  dailymax <- df %>%
    group_by(year, julian) %>%
    summarize_all(max, na.rm = TRUE)
  # Need to ungroup so that the grouped variables (year, julian) don't get
  #   carried over with the select + contains
  dailymax <- ungroup(dailymax)
  
  # If soil data exists...
  if (length(dplyr::select(dailymax, contains("soil"))) > 0) {
    dailymax <- dailymax %>%
      dplyr::rename(soil_max = soil)
  }
  
  # If surface data exists...
  if (length(dplyr::select(dailymax, contains("surface"))) > 0) {
    dailymax <- dailymax %>%
      dplyr::rename(surface_max = surface)
  }
  
  # If canopy data exists...
  if (length(dplyr::select(dailymax, contains("canopy"))) > 0) {
    dailymax <- dailymax %>%
      dplyr::rename(canopy_max = canopy)
  }
  
  return(dailymax)
}

## Data processing function #############

prep_flux_data <- function(low_dataset, high_dataset) {

## Take daily averages ###########
# Low elevation
low_dataset_dailymean <- dailymean_allyears(low_dataset)
low_dataset_dailymax <- dailymax_allyears(low_dataset)
low_dataset_dailymin <- dailymin_allyears(low_dataset)

low_dataset_all_years <- low_dataset_dailymean %>%
  full_join(low_dataset_dailymax) %>%
  full_join(low_dataset_dailymin)

# High elevation
high_dataset_dailymean <- dailymean_allyears(high_dataset)
high_dataset_dailymax <- dailymax_allyears(high_dataset)
high_dataset_dailymin <- dailymin_allyears(high_dataset)

high_dataset_all_years <- high_dataset_dailymean %>%
  full_join(high_dataset_dailymax) %>%
  full_join(high_dataset_dailymin)

## ** Averaged across years ############

# Soil
if (length(dplyr::select(low_dataset, contains("soil"))) > 0) {
  
  # Soil low elevation
  low_dataset_soil_avg_years <- low_dataset %>%
    dplyr::select(year, julian, soil) %>%
    dplyr::rename(temp = soil)
  
  low_dataset_soil_avg_years_dailymean <- dailymean_avg(low_dataset_soil_avg_years)
  low_dataset_soil_avg_years_dailymax <- dailymax_avg(low_dataset_soil_avg_years)
  low_dataset_soil_avg_years_dailymin <- dailymin_avg(low_dataset_soil_avg_years)
  
  low_dataset_soil_avg_years <- low_dataset_soil_avg_years_dailymean %>%
    full_join(low_dataset_soil_avg_years_dailymax) %>%
    full_join(low_dataset_soil_avg_years_dailymin)
  
  # Soil high elevation
  high_dataset_soil_avg_years <- high_dataset %>%
    dplyr::select(year, julian, soil) %>%
    dplyr::rename(temp = soil)
  
  high_dataset_soil_avg_years_dailymean <- dailymean_avg(high_dataset_soil_avg_years)
  high_dataset_soil_avg_years_dailymax <- dailymax_avg(high_dataset_soil_avg_years)
  high_dataset_soil_avg_years_dailymin <- dailymin_avg(high_dataset_soil_avg_years)
  
  high_dataset_soil_avg_years <- high_dataset_soil_avg_years_dailymean %>%
    full_join(high_dataset_soil_avg_years_dailymax) %>%
    full_join(high_dataset_soil_avg_years_dailymin)
}

# Surface
if (length(dplyr::select(low_dataset, contains("surface"))) > 0) {

  # Surface low elevation
  low_dataset_surface_avg_years <- low_dataset %>%
    dplyr::select(year, julian, surface) %>%
    dplyr::rename(temp = surface)
  
  low_dataset_surface_avg_years_dailymean <- dailymean_avg(low_dataset_surface_avg_years)
  low_dataset_surface_avg_years_dailymax <- dailymax_avg(low_dataset_surface_avg_years)
  low_dataset_surface_avg_years_dailymin <- dailymin_avg(low_dataset_surface_avg_years)
  
  low_dataset_surface_avg_years <- low_dataset_surface_avg_years_dailymean %>%
    full_join(low_dataset_surface_avg_years_dailymax) %>%
    full_join(low_dataset_surface_avg_years_dailymin)
  
  # Surface High elevation
  high_dataset_surface_avg_years <- high_dataset %>%
    dplyr::select(year, julian, surface) %>%
    dplyr::rename(temp = surface)
  
  high_dataset_surface_avg_years_dailymean <- dailymean_avg(high_dataset_surface_avg_years)
  high_dataset_surface_avg_years_dailymax <- dailymax_avg(high_dataset_surface_avg_years)
  high_dataset_surface_avg_years_dailymin <- dailymin_avg(high_dataset_surface_avg_years)
  
  high_dataset_surface_avg_years <- high_dataset_surface_avg_years_dailymean %>%
    full_join(high_dataset_surface_avg_years_dailymax) %>%
    full_join(high_dataset_surface_avg_years_dailymin)
}

# Canopy
if (length(dplyr::select(low_dataset, contains("canopy"))) > 0) {

  # Canopy low elevation
  low_dataset_canopy_avg_years <- low_dataset %>%
    dplyr::select(year, julian, canopy) %>%
    dplyr::rename(temp = canopy)
  
  low_dataset_canopy_avg_years_dailymean <- dailymean_avg(low_dataset_canopy_avg_years)
  low_dataset_canopy_avg_years_dailymax <- dailymax_avg(low_dataset_canopy_avg_years)
  low_dataset_canopy_avg_years_dailymin <- dailymin_avg(low_dataset_canopy_avg_years)
  
  low_dataset_canopy_avg_years <- low_dataset_canopy_avg_years_dailymean %>%
    full_join(low_dataset_canopy_avg_years_dailymax) %>%
    full_join(low_dataset_canopy_avg_years_dailymin)
  
  # Canopy High elevation
  high_dataset_canopy_avg_years <- high_dataset %>%
    dplyr::select(year, julian, canopy) %>%
    dplyr::rename(temp = canopy)
  
  high_dataset_canopy_avg_years_dailymean <- dailymean_avg(high_dataset_canopy_avg_years)
  high_dataset_canopy_avg_years_dailymax <- dailymax_avg(high_dataset_canopy_avg_years)
  high_dataset_canopy_avg_years_dailymin <- dailymin_avg(high_dataset_canopy_avg_years)
  
  high_dataset_canopy_avg_years <- high_dataset_canopy_avg_years_dailymean %>%
    full_join(high_dataset_canopy_avg_years_dailymax) %>%
    full_join(high_dataset_canopy_avg_years_dailymin)
}

## Bind together soil, surface, and canopy from different elevations ##############

## * Bind all elevations together, averaged across years tall-form #############

if (length(dplyr::select(low_dataset, contains("soil"))) > 0) {
low_dataset_soil_avg_years_tall <- low_dataset_soil_avg_years %>%
  mutate(elevation = "low") %>%
  mutate(micro = "soil")

high_dataset_soil_avg_years_tall <- high_dataset_soil_avg_years %>%
  mutate(elevation = "high") %>%
  mutate(micro = "soil")
}

if (length(dplyr::select(low_dataset, contains("surface"))) > 0) {
  
low_dataset_surface_avg_years_tall <- low_dataset_surface_avg_years %>%
  mutate(elevation = "low") %>%
  mutate(micro = "surface")

high_dataset_surface_avg_years_tall <- high_dataset_surface_avg_years %>%
  mutate(elevation = "high") %>%
  mutate(micro = "surface")
}

if (length(dplyr::select(low_dataset, contains("canopy"))) > 0) {
  
  low_dataset_canopy_avg_years_tall <- low_dataset_canopy_avg_years %>%
    mutate(elevation = "low") %>%
    mutate(micro = "canopy")
  
  high_dataset_canopy_avg_years_tall <- high_dataset_canopy_avg_years %>%
    mutate(elevation = "high") %>%
    mutate(micro = "canopy")
}


# Assuming that all sites have surface temps
avgyears_tall <- low_dataset_surface_avg_years_tall %>%
  bind_rows(high_dataset_surface_avg_years_tall)

if (length(dplyr::select(low_dataset, contains("soil"))) > 0) {
  avgyears_tall <- avgyears_tall %>%
    bind_rows(low_dataset_soil_avg_years_tall) %>%
    bind_rows(high_dataset_soil_avg_years_tall)
}

if (length(dplyr::select(low_dataset, contains("canopy"))) > 0) {
  avgyears_tall <- avgyears_tall %>%
    bind_rows(low_dataset_canopy_avg_years_tall) %>%
    bind_rows(high_dataset_canopy_avg_years_tall)
}

# avgyearstall_list <- mget(apropos("avg_years_tall"))
# 
# avg_years_tall <- do.call("rbind", avgyearstall_list)
# rownames(avg_years_tall) <- NULL

## * Bind all elevations together, averaged across years wide-form #############

if (length(dplyr::select(low_dataset, contains("soil"))) > 0) {
  low_dataset_soil_avg_years_wide <- low_dataset_soil_avg_years %>%
    dplyr::rename_all(~ (gsub("^", "low_soil_", make.names(names(low_dataset_soil_avg_years))))) %>%
    dplyr::rename(julian = "low_soil_julian")
  
  high_dataset_soil_avg_years_wide <- high_dataset_soil_avg_years %>%
    dplyr::rename_all(~ (gsub("^", "high_soil_", make.names(names(high_dataset_soil_avg_years))))) %>%
    dplyr::rename(julian = "high_soil_julian")
  
}

if (length(dplyr::select(low_dataset, contains("surface"))) > 0) {
  
  low_dataset_surface_avg_years_wide <- low_dataset_surface_avg_years %>%
    dplyr::rename_all(~ (gsub("^", "low_surface_", make.names(names(low_dataset_surface_avg_years))))) %>%
    dplyr::rename(julian = "low_surface_julian")
  
  high_dataset_surface_avg_years_wide <- high_dataset_surface_avg_years %>%
    dplyr::rename_all(~ (gsub("^", "high_surface_", make.names(names(high_dataset_surface_avg_years))))) %>%
    dplyr::rename(julian = "high_surface_julian")
}

if (length(dplyr::select(low_dataset, contains("canopy"))) > 0) {
  
  low_dataset_canopy_avg_years_wide <- low_dataset_canopy_avg_years %>%
    dplyr::rename_all(~ (gsub("^", "low_canopy_", make.names(names(low_dataset_canopy_avg_years))))) %>%
    dplyr::rename(julian = "low_canopy_julian")
  
  high_dataset_canopy_avg_years_wide <- high_dataset_canopy_avg_years %>%
    dplyr::rename_all(~ (gsub("^", "high_canopy_", make.names(names(high_dataset_canopy_avg_years))))) %>%
    dplyr::rename(julian = "high_canopy_julian")
}


# I'm assuming that every site will have surface temps...hopefully so
avg_years_wide <- low_dataset_surface_avg_years_wide %>%
  full_join(high_dataset_surface_avg_years_wide)

if (exists("low_dataset_soil_avg_years_wide")) {
  avg_years_wide <- avg_years_wide %>%
    full_join(low_dataset_soil_avg_years_wide) %>%
    full_join(high_dataset_soil_avg_years_wide)
}

if (exists("low_dataset_canopy_avg_years_wide")) {
  avg_years_wide <- avg_years_wide %>%
    full_join(low_dataset_canopy_avg_years_wide) %>%
    full_join(high_dataset_canopy_avg_years_wide)
}

# avgyearswide_list <- mget(apropos("avg_years_wide"))

# avg_years_wide <- join_all(avgyearswide_list, by = "julian", type = "full")

## * Bind all elevations together, all years tall-form #############

if (length(dplyr::select(low_dataset, contains("soil"))) > 0) {
  
  low_dataset_soil_all_years_tall <- low_dataset_all_years %>%
    dplyr::select(year, julian, soil_mean, soil_max, soil_min) %>%
    dplyr::rename(mean = soil_mean, max = soil_max, min = soil_min) %>%
    mutate(elevation = "low") %>%
    mutate(micro = "soil")
  
  high_dataset_soil_all_years_tall <- high_dataset_all_years %>%
    dplyr::select(year, julian, soil_mean, soil_max, soil_min) %>%
    dplyr::rename(mean = soil_mean, max = soil_max, min = soil_min) %>%
    mutate(elevation = "high") %>%
    mutate(micro = "soil")
}

if (length(dplyr::select(low_dataset, contains("surface"))) > 0) {
  
  low_dataset_surface_all_years_tall <- low_dataset_all_years %>%
    dplyr::select(year, julian, surface_mean, surface_max, surface_min) %>%
    dplyr::rename(mean = surface_mean, max = surface_max, min = surface_min) %>%
    mutate(elevation = "low") %>%
    mutate(micro = "surface")
  
  high_dataset_surface_all_years_tall <- high_dataset_all_years %>%
    dplyr::select(year, julian, surface_mean, surface_max, surface_min) %>%
    dplyr::rename(mean = surface_mean, max = surface_max, min = surface_min) %>%
    mutate(elevation = "high") %>%
    mutate(micro = "surface")
}

if (length(dplyr::select(low_dataset, contains("canopy"))) > 0) {
  
  low_dataset_canopy_all_years_tall <- low_dataset_all_years %>%
    dplyr::select(year, julian, canopy_mean, canopy_max, canopy_min) %>%
    dplyr::rename(mean = canopy_mean, max = canopy_max, min = canopy_min) %>%
    mutate(elevation = "low") %>%
    mutate(micro = "canopy")
  
  high_dataset_canopy_all_years_tall <- high_dataset_all_years %>%
    dplyr::select(year, julian, canopy_mean, canopy_max, canopy_min) %>%
    dplyr::rename(mean = canopy_mean, max = canopy_max, min = canopy_min) %>%
    mutate(elevation = "high") %>%
    mutate(micro = "canopy")
}


# Assuming that all sites have surface temps
allyears_tall <- low_dataset_surface_all_years_tall %>%
  bind_rows(high_dataset_surface_all_years_tall)

if (length(dplyr::select(low_dataset, contains("soil"))) > 0) {
  allyears_tall <- allyears_tall %>%
    bind_rows(low_dataset_soil_all_years_tall) %>%
    bind_rows(high_dataset_soil_all_years_tall)
}

if (length(dplyr::select(low_dataset, contains("canopy"))) > 0) {
  allyears_tall <- allyears_tall %>%
    bind_rows(low_dataset_canopy_all_years_tall) %>%
    bind_rows(high_dataset_canopy_all_years_tall)
}

# allyearstall_list <- mget(apropos("all_years_tall"))
# 
# allyears_tall <- do.call("rbind", allyearstall_list)
# rownames(all_years_tall) <- NULL

## * Bind all elevations together, all years wide-form #############
high_dataset_wide <- high_dataset_all_years %>%
  dplyr::rename_all(~ (gsub("^", "high_", make.names(names(high_dataset_all_years))))) %>%
  dplyr::rename(year = high_year, julian = high_julian)

low_dataset_wide <- low_dataset_all_years %>%
  dplyr::rename_all(~ (gsub("^", "low_", make.names(names(low_dataset_all_years))))) %>%
  dplyr::rename(year = low_year, julian = low_julian)

allyears_wide <- high_dataset_wide %>%
  full_join(low_dataset_wide)

## Spit back out combined, disaggregated data ############

disagg_low <- low_dataset %>% 
  pivot_longer(c(contains("soil"), contains("surface"), contains("canopy")),
               names_to = "micro", values_to = "temp") %>% 
  mutate(elevation = "low")

disagg_high <- low_dataset %>% 
  pivot_longer(c(contains("soil"), contains("surface"), contains("canopy")),
               names_to = "micro", values_to = "temp") %>% 
  mutate(elevation = "high")

disagg_allyear_tall <- bind_rows(disagg_low, disagg_high)

## Export data #################

data_list <- list(high_dataset_all_years, low_dataset_all_years, allyears_tall, 
                  allyears_wide, avgyears_tall, avg_years_wide, disagg_allyear_tall)

return(data_list)

}

