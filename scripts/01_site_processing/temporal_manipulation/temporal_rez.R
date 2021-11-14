# David Klinges
# File creation date: 2019.06.29
# This script curates data in preperation for a temporal resolution x overlap 
# figure and corresponding analyses by generating Janzen d-scores across
# a continuous gradient of intervals

library(tidyverse)
library(lubridate)

temporal_rez <- function(data, max_resolution) {
  ## 1. Organize timeseries date stamps as days since first recording ########
  
  data <- data %>% 
    group_by(site) %>% 
    mutate(date = as_date((julian - 1), origin = paste0(year, "-01-01"))) %>% 
    # Create an attribute corresponding to days since first recording
    mutate(num_date = as.numeric(date - min(date))) %>% 
    ungroup()
  
  ## 2. calculate intervals ############
  
  source("scripts/data_processing/janzify.R")
  
  for (i in 1:max_resolution) {
    
    data_iter <- data %>% 
      group_by(site) %>% 
      # Divide the date by the iterator, so you get the number of times that the 
      # given date fits into the desired interval (which corresponds to the i)
      mutate(interval = num_date / i) %>% 
      # Now round up to the nearest integer above the interval
      mutate(interval = ceiling(interval))
    
    # With the current method of grouping into intervals, oftentimes the number
    # of days in a group is less than it should be. E.g. if the iterator is at 
    # 200 and you only have 365 days, you'll have one group with 200 days and
    # another group with 165 days. We only want groups that have exactly the 
    # number of days as specific by the iterator. Remove the rest of the groups
    
    count_groups <- data_iter %>% 
      # Group by each interval
      group_by(site, interval) %>% 
      # Count the number of rows (each corresponding to a day) in that group
      count() 
    
    data_iter <- data_iter %>% 
      full_join(count_groups) %>% 
      # Remove the groups that have less than `i` observations
      filter(n >= i) %>% 
      dplyr::select(-n) %>% 
      ungroup()
    
    # custom Janzen function picks up on the interval you just generated
    data_iter <- janzify_per_temporalRez(data_iter)
    
    # Add the iterator as a column for the interval over which overlap was 
    # calculated
    data_iter <- data_iter %>% 
      mutate(temporal_rez = i)
    
    if (i == 1) {
      data_out <- data_iter
    } else {
      data_out <- bind_rows(data_out, data_iter)
    }
    
    print(paste0("Completed iteration ", i))
  }
  
return(data_out)
}

