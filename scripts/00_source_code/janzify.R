# David Klinges
# Janzify: create d-scores from min and max daily temps for high and low elevation

library(tidyverse)
library(lubridate)

janzify <- function(data) {
  
  # If the site has soil data, include it
  if("low_soil_min" %in% colnames(data)) {
  
  soil <- data %>%
    # Janzen overlap calculation: d / sqrt(R1 * R2)
    mutate(janzenDscore = (high_soil_max - low_soil_min) / 
             (sqrt((high_soil_max - high_soil_min) * (low_soil_max - low_soil_min)))) %>%
    # Thermal absolute distance
    mutate(TAD = high_soil_max - low_soil_min) %>%
    mutate(kozak_wiens = 0.5 * (TAD / (high_soil_max - high_soil_min) + 
                                   (TAD / (low_soil_max - low_soil_min)) )) %>% 
    # Flag micro
    mutate(micro = "soil") %>% 
    # Remove rows that have no overlap calculations
    filter_at(vars(janzenDscore, TAD), all_vars(!is.na(.))) %>% 
    mutate_at(vars(janzenDscore, TAD), list(~ replace(., is.infinite(.), NA)))
  
  } else {
    soil <- NULL
  }
  
  # If the site has surface data, include it
  if("low_surface_min" %in% colnames(data)) {
    
    surface <- data %>%
      # Janzen overlap calculation: d / sqrt(R1 * R2)
      mutate(janzenDscore = (high_surface_max - low_surface_min) / 
               (sqrt((high_surface_max - high_surface_min) * (low_surface_max - low_surface_min)))) %>%
      # Thermal absolute distance
      mutate(TAD = high_surface_max - low_surface_min) %>%
      mutate(kozak_wiens = 0.5 * (TAD / (high_surface_max - high_surface_min) + 
                                    (TAD / (low_surface_max - low_surface_min)) )) %>% 
      # Flag micro
      mutate(micro = "surface") %>% 
      # Remove rows that have no overlap calculations
      filter_at(vars(janzenDscore, TAD), all_vars(!is.na(.))) %>% 
      mutate_at(vars(janzenDscore, TAD), list(~ replace(., is.infinite(.), NA)))
    
  } else {
    surface <- NULL
  }
  
  # If the site has canopy/canopy data, include it
  if("low_canopy_min" %in% colnames(data)) {
    
    canopy <- data %>%
      # Janzen overlap calculation: d / sqrt(R1 * R2)
      mutate(janzenDscore = (high_canopy_max - low_canopy_min) / 
               (sqrt((high_canopy_max - high_canopy_min) * (low_canopy_max - low_canopy_min)))) %>%
      # Thermal absolute distance
      mutate(TAD = high_canopy_max - low_canopy_min) %>%
      mutate(kozak_wiens = 0.5 * (TAD / (high_canopy_max - high_canopy_min) + 
                                    (TAD / (low_canopy_max - low_canopy_min)) )) %>% 
      # Flag micro
      mutate(micro = "canopy") %>% 
      # Remove rows that have no overlap calculations
      filter_at(vars(janzenDscore, TAD), all_vars(!is.na(.))) %>% 
      mutate_at(vars(janzenDscore, TAD), list(~ replace(., is.infinite(.), NA)))
    
  } else {
    canopy <- NULL
  }
  
  data <- bind_rows(soil, canopy, surface) %>%
    # Remove original low elev and high cols...using "low" and "high" in case I have
    # other min, mean and max cols
    dplyr::select(-contains("low"), -contains("high"))
  
  # Correct for elevation change
  source("scripts/00_source_code/correct_elevChange.R")
  data <- data %>% 
    ungroup() %>% 
    filter(is.finite(janzenDscore)) %>%
    filter(complete.cases(janzenDscore)) %>%
    filter(is.finite(TAD)) %>%
    filter(complete.cases(TAD)) %>%
    filter(complete.cases(kozak_wiens)) %>% 
    filter(is.finite(kozak_wiens)) %>%
    filter(complete.cases(foliage_cover))
  
  data <- data %>% 
    mutate(janzenDscore_elevCorr = correct_elevChange(data, 
                                                      janzenDscore,
                                                      "quadratic",
                                                      beta0 = 0.54,
                                                      beta1 = .00048,
                                                      beta2 = .0005))
  
  
  data <- data %>% 
    mutate(TAD_elevCorr = correct_elevChange(data, 
                                             TAD,
                                             "linear",
                                             beta0 = NA,
                                             beta1 = NA,
                                             beta2 = NA))
  
  # Return data
  return(data)
  
}

janzify_per_month <- function(data) {
  

  # Add month column, but only if there aren't already month calculations
  # E.g. data pulled from Janzen figures, which is just monthly min and max
  data <- data %>% 
    mutate(month = ifelse(is.na(month), lubridate::month(as.POSIXlt(as.Date(julian, format = "%j", 
                        origin = paste0("1.1.", year)), 
                         format="%d/%m/%Y")), month))
  
  # If the site has soil data, include it
  if("low_soil_min" %in% colnames(data)) {
    
    soil <- data 
    
    # Summarize mins
    data_min <- soil %>%
      dplyr::group_by(site, elevation_change, year, month) %>%
      dplyr::summarize_at(vars(low_soil_min, high_soil_min), min, na.rm = TRUE) %>%
      ungroup()
    
    # Summarize means
    data_mean <- soil %>%
      dplyr::group_by(site, elevation_change, year, month) %>%
      dplyr::summarize_at(vars(low_soil_mean, high_soil_mean), mean, na.rm = TRUE) %>%
      ungroup()
    
    # Summarize maxs
    data_max <- soil %>%
      dplyr::group_by(site, elevation_change, year, month) %>%
      dplyr::summarize_at(vars(low_soil_max, high_soil_max), max, na.rm = TRUE) %>%
      ungroup()
    
    # Join min and max
    soil <- data_min %>% 
      full_join(data_max) %>% 
      full_join(data_mean)
    
    soil <- soil %>%
      # Janzen overlap calculation: d / sqrt(R1 * R2)
      mutate(janzenDscore = (high_soil_max - low_soil_min) / 
               (sqrt((high_soil_max - high_soil_min) * (low_soil_max - low_soil_min)))) %>%
      # Thermal absolute distance
      mutate(TAD = high_soil_max - low_soil_min) %>%
      mutate(kozak_wiens = 0.5 * (TAD / (high_soil_max - high_soil_min) + 
                                    (TAD / (low_soil_max - low_soil_min)) )) %>% 
      
      # Flag micro
      mutate(micro = "soil") %>% 
      # Remove rows that have no overlap calculations
      filter_at(vars(janzenDscore, TAD), all_vars(!is.na(.))) %>% 
      mutate_at(vars(janzenDscore, TAD), list(~ replace(., is.infinite(.), NA)))
    
  } else {
    soil <- NULL
  }
  
  # If the site has surface data, include it
  if("low_surface_min" %in% colnames(data)) {
    
    surface <- data 
    
    # Summarize mins
    data_min <- surface %>%
      dplyr::group_by(site, elevation_change, year, month) %>%
      dplyr::summarize_at(vars(low_surface_min, high_surface_min), min, na.rm = TRUE) %>%
      ungroup()
    
    # Summarize means
    data_mean <- surface %>%
      dplyr::group_by(site, elevation_change, year, month) %>%
      dplyr::summarize_at(vars(low_surface_mean, high_surface_mean), mean, na.rm = TRUE) %>%
      ungroup()
    
    # Summarize maxs
    data_max <- surface %>%
      dplyr::group_by(site, elevation_change, year, month) %>%
      dplyr::summarize_at(vars(low_surface_max, high_surface_max), max, na.rm = TRUE) %>%
      ungroup()
    
    # Join min and max
    surface <- data_min %>% 
      full_join(data_max) %>% 
      full_join(data_mean)
    
    surface <- surface %>%
      # Janzen overlap calculation: d / sqrt(R1 * R2)
      mutate(janzenDscore = (high_surface_max - low_surface_min) / 
               (sqrt((high_surface_max - high_surface_min) * (low_surface_max - low_surface_min)))) %>%
      # Thermal absolute distance
      mutate(TAD = high_surface_max - low_surface_min) %>%
      mutate(kozak_wiens = 0.5 * (TAD / (high_surface_max - high_surface_min) + 
                                    (TAD / (low_surface_max - low_surface_min)) )) %>% 
      
      # Flag micro
      mutate(micro = "surface") %>% 
      # Remove rows that have no overlap calculations
      filter_at(vars(janzenDscore, TAD), all_vars(!is.na(.))) %>% 
      mutate_at(vars(janzenDscore, TAD), list(~ replace(., is.infinite(.), NA)))
    
  } else {
    surface <- NULL
  }
  
  # If the site has canopy/canopy data, include it
  if("low_canopy_min" %in% colnames(data)) {
    
    canopy <- data 
    
    # Summarize mins
    data_min <- canopy %>%
      dplyr::group_by(site, elevation_change, year, month) %>%
      dplyr::summarize_at(vars(low_canopy_min, high_canopy_min), min, na.rm = TRUE) %>%
      ungroup()
    
    # Summarize means
    data_mean <- canopy %>%
      dplyr::group_by(site, elevation_change, year, month) %>%
      dplyr::summarize_at(vars(low_canopy_mean, high_canopy_mean), mean, na.rm = TRUE) %>%
      ungroup()
    
    # Summarize maxs
    data_max <- canopy %>%
      dplyr::group_by(site, elevation_change, year, month) %>%
      dplyr::summarize_at(vars(low_canopy_max, high_canopy_max), max, na.rm = TRUE) %>%
      ungroup()
    
    # Join min and max
    canopy <- data_min %>% 
      full_join(data_max) %>% 
      full_join(data_mean)
    
    canopy <- canopy %>%
      # Janzen overlap calculation: d / sqrt(R1 * R2)
      mutate(janzenDscore = (high_canopy_max - low_canopy_min) / 
               (sqrt((high_canopy_max - high_canopy_min) * (low_canopy_max - low_canopy_min)))) %>%
      # Thermal absolute distance
      mutate(TAD = high_canopy_max - low_canopy_min) %>%
      mutate(kozak_wiens = 0.5 * (TAD / (high_canopy_max - high_canopy_min) + 
                                    (TAD / (low_canopy_max - low_canopy_min)) )) %>% 
      
      # Flag micro
      mutate(micro = "canopy") %>% 
      # Remove rows that have no overlap calculations
      filter_at(vars(janzenDscore, TAD), all_vars(!is.na(.))) %>% 
      mutate_at(vars(janzenDscore, TAD), list(~ replace(., is.infinite(.), NA)))
    
  } else {
    canopy <- NULL
  }
  
  data <- bind_rows(soil, canopy, surface) %>%
    # Remove original low elev and high cols...using "low" and "high" in case I have
    # other min, mean and max cols
    dplyr::select(-contains("low"), -contains("high"))
  
  # Correct for elevation change
  
  data <- data %>% 
    ungroup() %>% 
    filter(is.finite(janzenDscore)) %>%
    filter(complete.cases(janzenDscore)) %>%
    filter(is.finite(TAD)) %>%
    filter(complete.cases(TAD)) %>% 
    filter(complete.cases(kozak_wiens)) %>% 
    filter(is.finite(kozak_wiens)) %>%
    filter(complete.cases(month)) %>%
    filter(complete.cases(elevation_change))
  
  data <- data %>% 
    mutate(janzenDscore_elevCorr = correct_elevChange(data, 
                                                      janzenDscore,
                                                      "quadratic",
                                                      beta0 = 0.54,
                                                      beta1 = .00048,
                                                      beta2 = .0005))
  
  
  data <- data %>% 
    mutate(TAD_elevCorr = correct_elevChange(data, 
                                             TAD,
                                             "linear",
                                             beta0 = NA,
                                             beta1 = NA,
                                             beta2 = NA))
  
  # Return data
  return(data)
  
}

janzify_per_temporalRez <- function(data) {
  
  # annual mean maximum and minimum for the ith year of the higher elevational regime and
  # R2i is the equivalent value for the lower elevational regim
  # data <- na.omit(data)
  
  # If the site has soil data, include it
  if("low_soil_min" %in% colnames(data)) {
    
    soil <- data 
    
    # This process is split into min, mean and max out of necessity, and split
    # into high and low for speed 
    
    # Summarize mins
    data_low_min <- soil %>%
      dplyr::select(-contains("surface"), -contains("canopy"), -contains("mean"),
             -contains("max")) %>% 
      dplyr::group_by(site, foliage, snow, interval) %>%
      dplyr::summarize(low_soil_min = min(low_soil_min, na.rm = TRUE)) %>%
      dplyr::filter(complete.cases(low_soil_min)) %>% 
      dplyr::filter(is.finite(low_soil_min)) %>% 
      ungroup()
    
    data_high_min <- soil %>%
      dplyr::select(-contains("surface"), -contains("canopy"), -contains("mean"),
             -contains("max")) %>% 
      dplyr::group_by(site, foliage, snow, interval) %>%
      dplyr::summarize(high_soil_min = min(high_soil_min, na.rm = TRUE)) %>% 
      dplyr::filter(complete.cases(high_soil_min)) %>% 
      dplyr::filter(is.finite(high_soil_min)) %>% 
      ungroup()
    
    # Summarize means
    data_low_mean <- soil %>%
      dplyr::select(-contains("surface"), -contains("canopy"), -contains("min"),
             -contains("max")) %>% 
      dplyr::group_by(site, foliage, snow, interval) %>%
      dplyr::summarize(low_soil_mean = mean(low_soil_mean, na.rm = TRUE)) %>% 
      dplyr::filter(complete.cases(low_soil_mean)) %>% 
      dplyr::filter(is.finite(low_soil_mean)) %>% 
      ungroup()
    
    data_high_mean <- soil %>%
      dplyr::select(-contains("surface"), -contains("canopy"), -contains("min"),
             -contains("max")) %>% 
      dplyr::group_by(site, foliage, snow, interval) %>%
      dplyr::summarize(high_soil_mean = mean(high_soil_mean, na.rm = TRUE)) %>% 
      dplyr::filter(complete.cases(high_soil_mean)) %>% 
      dplyr::filter(is.finite(high_soil_mean)) %>% 
      ungroup()
    
    # Summarize maxs
    data_low_max <- soil %>%
      dplyr::select(-contains("surface"), -contains("canopy"), -contains("min"),
             -contains("mean")) %>% 
      dplyr::group_by(site, foliage, snow, interval) %>%
      dplyr::summarize(low_soil_max = max(low_soil_max, na.rm = TRUE)) %>% 
      dplyr::filter(complete.cases(low_soil_max)) %>%
      dplyr::filter(is.finite(low_soil_max)) %>% 
      ungroup()
    
    data_high_max <- soil %>%
      dplyr::select(-contains("surface"), -contains("canopy"), -contains("min"),
             -contains("mean")) %>% 
      dplyr::group_by(site, foliage, snow, interval) %>%
      dplyr::summarize(high_soil_max = max(high_soil_max, na.rm = TRUE)) %>% 
      dplyr::filter(complete.cases(high_soil_max)) %>%
      dplyr::filter(is.finite(high_soil_max)) %>% 
      ungroup()
    
    # Join min and max
    soil <- data_low_min %>% 
      full_join(data_high_min) %>% 
      full_join(data_low_max) %>% 
      full_join(data_high_max) %>% 
      full_join(data_low_mean) %>% 
      full_join(data_high_mean)
    
    soil <- soil %>%
      group_by(site) %>% 
      # Janzen overlap calculation: d / sqrt(R1 * R2)
      mutate(janzenDscore = (high_soil_max - low_soil_min) / 
               (sqrt((high_soil_max - high_soil_min) * (low_soil_max - low_soil_min)))) %>%
      # Thermal absolute distance
      mutate(TAD = high_soil_max - low_soil_min) %>%
      # Flag micro
      mutate(micro = "soil")
    
  } else {
    soil <- NULL
  }
  
  # If the site has surface data, include it
  if("low_surface_min" %in% colnames(data)) {
    
    surface <- data 
    
    # This process is split into min, mean and max out of necessity, and split
    # into high and low for speed 
    
    # Summarize mins
    data_low_min <- surface %>%
      dplyr::select(-contains("soil"), -contains("canopy"), -contains("mean"),
                    -contains("max")) %>% 
      dplyr::group_by(site, foliage, snow, interval) %>%
      dplyr::summarize(low_surface_min = min(low_surface_min, na.rm = TRUE)) %>%
      dplyr::filter(complete.cases(low_surface_min)) %>% 
      dplyr::filter(is.finite(low_surface_min)) %>% 
      ungroup()
    
    data_high_min <- surface %>%
      dplyr::select(-contains("soil"), -contains("canopy"), -contains("mean"),
                    -contains("max")) %>% 
      dplyr::group_by(site, foliage, snow, interval) %>%
      dplyr::summarize(high_surface_min = min(high_surface_min, na.rm = TRUE)) %>% 
      dplyr::filter(complete.cases(high_surface_min)) %>% 
      dplyr::filter(is.finite(high_surface_min)) %>% 
      ungroup()
    
    # Summarize means
    data_low_mean <- surface %>%
      dplyr::select(-contains("soil"), -contains("canopy"), -contains("min"),
                    -contains("max")) %>% 
      dplyr::group_by(site, foliage, snow, interval) %>%
      dplyr::summarize(low_surface_mean = mean(low_surface_mean, na.rm = TRUE)) %>% 
      dplyr::filter(complete.cases(low_surface_mean)) %>% 
      dplyr::filter(is.finite(low_surface_mean)) %>% 
      ungroup()
    
    data_high_mean <- surface %>%
      dplyr::select(-contains("soil"), -contains("canopy"), -contains("min"),
                    -contains("max")) %>% 
      dplyr::group_by(site, foliage, snow, interval) %>%
      dplyr::summarize(high_surface_mean = mean(high_surface_mean, na.rm = TRUE)) %>% 
      dplyr::filter(complete.cases(high_surface_mean)) %>% 
      dplyr::filter(is.finite(high_surface_mean)) %>% 
      ungroup()
    
    # Summarize maxs
    data_low_max <- surface %>%
      dplyr::select(-contains("soil"), -contains("canopy"), -contains("min"),
                    -contains("mean")) %>% 
      dplyr::group_by(site, foliage, snow, interval) %>%
      dplyr::summarize(low_surface_max = max(low_surface_max, na.rm = TRUE)) %>% 
      dplyr::filter(complete.cases(low_surface_max)) %>%
      dplyr::filter(is.finite(low_surface_max)) %>% 
      ungroup()
    
    data_high_max <- surface %>%
      dplyr::select(-contains("soil"), -contains("canopy"), -contains("min"),
                    -contains("mean")) %>% 
      dplyr::group_by(site, foliage, snow, interval) %>%
      dplyr::summarize(high_surface_max = max(high_surface_max, na.rm = TRUE)) %>% 
      dplyr::filter(complete.cases(high_surface_max)) %>%
      dplyr::filter(is.finite(high_surface_max)) %>% 
      ungroup()
    
    # Join min and max
    surface <- data_low_min %>% 
      full_join(data_high_min) %>% 
      full_join(data_low_max) %>% 
      full_join(data_high_max) %>% 
      full_join(data_low_mean) %>% 
      full_join(data_high_mean)
    
    
    surface <- surface %>%
      group_by(site) %>% 
      # Janzen overlap calculation: d / sqrt(R1 * R2)
      mutate(janzenDscore = (high_surface_max - low_surface_min) / 
               (sqrt((high_surface_max - high_surface_min) * (low_surface_max - low_surface_min)))) %>%
      # Thermal absolute distance
      mutate(TAD = high_surface_max - low_surface_min) %>%
      # Flag micro
      mutate(micro = "surface")
    
    
  } else {
    surface <- NULL
  }
  
  # If the site has canopy/canopy data, include it
  if("low_canopy_min" %in% colnames(data)) {
    
    canopy <- data 
    
    # This process is split into min, mean and max out of necessity, and split
    # into high and low for speed 
    
    # Summarize mins
    data_low_min <- canopy %>%
      dplyr::select(-contains("soil"), -contains("surface"), -contains("mean"),
                    -contains("max")) %>% 
      dplyr::filter(complete.cases(low_canopy_min)) %>% 
      dplyr::group_by(site, foliage, snow, interval) %>%
      dplyr::summarize(low_canopy_min = min(low_canopy_min, na.rm = TRUE)) %>%
      dplyr::filter(is.finite(low_canopy_min)) %>% 
      ungroup()
    
    data_high_min <- canopy %>%
      dplyr::select(-contains("soil"), -contains("surface"), -contains("mean"),
                    -contains("max")) %>% 
      dplyr::group_by(site, foliage, snow, interval) %>%
      dplyr::summarize(high_canopy_min = min(high_canopy_min, na.rm = TRUE)) %>% 
      dplyr::filter(complete.cases(high_canopy_min)) %>% 
      dplyr::filter(is.finite(high_canopy_min)) %>% 
      ungroup()
    
    # Summarize means
    data_low_mean <- canopy %>%
      dplyr::select(-contains("soil"), -contains("surface"), -contains("min"),
                    -contains("max")) %>% 
      dplyr::group_by(site, foliage, snow, interval) %>%
      dplyr::summarize(low_canopy_mean = mean(low_canopy_mean, na.rm = TRUE)) %>% 
      dplyr::filter(complete.cases(low_canopy_mean)) %>% 
      dplyr::filter(is.finite(low_canopy_mean)) %>% 
      ungroup()
    
    data_high_mean <- canopy %>%
      dplyr::select(-contains("soil"), -contains("surface"), -contains("min"),
                    -contains("max")) %>% 
      dplyr::group_by(site, foliage, snow, interval) %>%
      dplyr::summarize(high_canopy_mean = mean(high_canopy_mean, na.rm = TRUE)) %>% 
      dplyr::filter(complete.cases(high_canopy_mean)) %>% 
      dplyr::filter(is.finite(high_canopy_mean)) %>% 
      ungroup()
    
    # Summarize maxs
    data_low_max <- canopy %>%
      dplyr::select(-contains("soil"), -contains("surface"), -contains("min"),
                    -contains("mean")) %>% 
      dplyr::group_by(site, foliage, snow, interval) %>%
      dplyr::summarize(low_canopy_max = max(low_canopy_max, na.rm = TRUE)) %>% 
      dplyr::filter(complete.cases(low_canopy_max)) %>%
      dplyr::filter(is.finite(low_canopy_max)) %>% 
      ungroup()
    
    data_high_max <- canopy %>%
      dplyr::select(-contains("soil"), -contains("surface"), -contains("min"),
                    -contains("mean")) %>% 
      dplyr::group_by(site, foliage, snow, interval) %>%
      dplyr::summarize(high_canopy_max = max(high_canopy_max, na.rm = TRUE)) %>% 
      dplyr::filter(complete.cases(high_canopy_max)) %>%
      dplyr::filter(is.finite(high_canopy_max)) %>% 
      ungroup()
    
    # Join min and max
    canopy <- data_low_min %>% 
      full_join(data_high_min) %>% 
      full_join(data_low_max) %>% 
      full_join(data_high_max) %>% 
      full_join(data_low_mean) %>% 
      full_join(data_high_mean)
    
    canopy <- canopy %>%
      group_by(site) %>% 
      # Janzen overlap calculation: d / sqrt(R1 * R2)
      mutate(janzenDscore = (high_canopy_max - low_canopy_min) / 
               (sqrt((high_canopy_max - high_canopy_min) * (low_canopy_max - low_canopy_min)))) %>%
      # Thermal absolute distance
      mutate(TAD = high_canopy_max - low_canopy_min) %>%
      # Flag micro
      mutate(micro = "canopy")

  } else {
    canopy <- NULL
  }
  

  data <- bind_rows(soil, canopy, surface) %>%
    dplyr::select(-contains("low"), -contains("high")) %>% 
    # Join back in columns lost by summarizing to the interval
    left_join(
      # attempting to keep julian, snow, and foliage
      dplyr::select(data, -contains("low"), -contains("high"), -interval, 
                    -num_date, -date, -year, -julian, -snow, -foliage) %>% 
        distinct()
    ) %>% 

    # Replace NaN and Inf with NA
    mutate_at(vars(janzenDscore, TAD), list(~ replace(., is.nan(.), NA))) %>% 
    mutate_at(vars(janzenDscore, TAD), list(~ replace(., is.infinite(.), NA))) 
  
  # Correct for elevation change
  
  data <- data %>% 
    ungroup() %>% 
    filter(is.finite(janzenDscore)) %>%
    filter(complete.cases(janzenDscore)) %>%
    filter(is.finite(TAD)) %>%
    filter(complete.cases(TAD)) %>% 
    filter(complete.cases(foliage_cover))
  
  
  data <- data %>% 
    mutate(janzenDscore_elevCorr = correct_elevChange(data, 
                                                      janzenDscore,
                                                      "quadratic",
                                                      beta0 = 0.54,
                                                      beta1 = .00048,
                                                      beta2 = .0005)) %>% 
    
    mutate(TAD_elevCorr = correct_elevChange(data, 
                                             TAD,
                                             "linear",
                                             beta0 = NA,
                                             beta1 = NA,
                                             beta2 = NA))
  
  
  # Return data
  return(data)
  
}

overlappify <- function(data) {
  
  library(overlapping)
  
  data <- data %>% 
    dplyr::select(julian, month, year, site, elevation, elevation_change, 
                  micro, mean, min, max) %>% 
    mutate(month = ifelse(is.na(month), lubridate::month(as.POSIXlt(as.Date(julian, format = "%j", 
                                                                          origin = paste0("1.1.", year)), 
                                                                  format="%d/%m/%Y")), month))
  
  low <- data %>% 
    filter(elevation == "low") %>% 
    dplyr::select(-elevation) %>% 
    pivot_longer(cols = c(min, mean, max), names_to = "param", values_to = "low")
  
  high <- data %>% 
    filter(elevation == "high") %>% 
    dplyr::select(-elevation) %>% 
    pivot_longer(cols = c(min, mean, max), names_to = "param", values_to = "high")
  
  data <- full_join(low, high) %>% 
    dplyr::select(-julian, -param) %>% 
    # Remove NAs and infinites
    filter(complete.cases(low) & is.finite(low)) %>% 
    filter(complete.cases(high) & is.finite(high)) 
  

  out <- data %>% 
    # Because we will lose the grouping cols when we map, let's just duplicate
    # them into new cols
    mutate(site_g = site, micro_g = micro, year_g = year, month_g = month) %>% 
    group_by(site_g, micro_g, year_g, month_g) %>% 
    # The output of a group_map() is a list, which isn't pretty but I got to work. 
    # I tried messing with map_dfc but kept getting
    # the same error: "Index 1 must have length 1, not 7". See here for some 
    # documentation that you could keep playing with but may not be fruitful
    # (I already dug through this and related stackexchanges):
    # https://dplyr.tidyverse.org/reference/group_map.html
    group_map(~ as.data.frame(mutate(.x, kde = overlap(list(.x$high, .x$low))$OV)))
  
  # Now need to bind together elements of the resulting list
  out <- do.call(bind_rows, out)
  
  # Now group by and summarize to the site-micro-year-month combo
  out <- out %>% 
    group_by(site, micro, year, month, elevation_change) %>% 
    summarize(kde = mean(kde, na.rm = TRUE))
  
  # Correct for elevation change
  
  data <- out %>% 
    ungroup() %>% 
    filter(is.finite(kde)) %>%
    filter(complete.cases(kde))
  
  data <- data %>% 
    mutate(kde_elevCorr = correct_elevChange(data, 
                                             kde,
                                             "linear",
                                             beta0 = NA,
                                             beta1 = NA,
                                             beta2 = NA))
  
  # Return data
  return(data)
  
}
