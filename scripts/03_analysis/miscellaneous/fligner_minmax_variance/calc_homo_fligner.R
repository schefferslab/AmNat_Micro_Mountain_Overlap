#David Klinges
#2019.05.05
#This script conducts Fligner's test for homoscedascity, 
# comparing leaf-off vs leaf-on vs non-forest
#does so for daily min + daily max

## Prep workspace #############
library(tidyverse)

# Import data
NC_off <- read_csv("data/01_primary/temperate/NC_mt_mitchell/derivative/NC_off_tall.csv")
NC_on <- read_csv("data/01_primary/temperate/NC_mt_mitchell/derivative/NC_on_tall.csv")

NH_off <- read_csv("data/01_primary/temperate/NH_whites/derivative/NH_hubbardSoilOff_tall.csv",
                   col_types = cols(
                     mean = col_double(),
                     min = col_double(),
                     max= col_double()
                                    ))
NH_on <- read_csv("data/01_primary/temperate/NH_whites/derivative/NH_hubbardSoilOn_tall.csv",
                  col_types = cols(
                    mean = col_double(),
                    min = col_double(),
                    max= col_double()
                  ))

## Prep data ###########

## NC
# Combine leaf off and leaf on
NC_off <- NC_off %>% 
  mutate(foliage = "off")

NC_on <- NC_on %>% 
  mutate(foliage = "on")

NC <- NC_off %>% 
  bind_rows(NC_on) %>% 
  mutate(foliage = as.factor(foliage))

## NH
# Combine leaf off and leaf on
NH_off <- NH_off %>% 
  mutate(foliage = "off")

NH_on <- NH_on %>% 
  mutate(foliage = "on")

NH <- NH_off %>% 
  bind_rows(NH_on) %>% 
  mutate(foliage = as.factor(foliage))

## Build Fligner's test function #########

conduct_fligner <- function(data, site) {
  
# Gather min and max cols
data <- data %>% 
  gather(key = "t", value = "minmax", min, max) %>% 
  select(-mean, -t)

# Note: elev, mic, and yea indexes can't be the same strings as data column headers, 
#   or else filtering the data won't work
for (elev in 1:length(unique(data$elevation))) {
  for (mic in 1:length(unique(data$micro))) {
    for (yea in 1:length(unique(data$year))) {
   print(elev)
    print(mic)
      print(year)

      subset <- data %>% 
      filter(elevation == unique(data$elevation)[elev]) %>% 
      filter(micro == c(unique(data$micro)[mic])) %>% 
      filter(year == unique(data$year)[yea])
     
      
      test_output <- try(fligner.test(minmax ~ foliage, data = subset), silent = FALSE)
      
      # If the output of the test is of class 'htest', rather than class 'try-error'...
      # Meaning, if the flinger test succeeded...
      if (class(test_output) == "htest") {
    
        fligner_output <- test_output
        
    # Create tibble for iteration output
    iteration_output <- tibble(site = site, micro = unique(data$micro)[mic], 
                               year = as.character(unique(data$year)[yea]),
                               elevation = unique(data$elevation)[elev],
                               chi_squared = fligner_output$statistic,
                               p_value = fligner_output$p.value, method = fligner_output$data.name)
    
    # If first iteration...
    if (elev == 1 & mic == 1 & yea == 1) {
      # Create new final output object, a data frame
      total_output <- iteration_output
    }
    # Otherwise bind the iteration's output to our final output data frame
    else {
      total_output <- bind_rows(total_output, iteration_output)
    }
    
      }
      
      # If the output wasn't class htest, meaning something failed along the way...
      else {
        # ...deliver an error message
        message(paste0("CUSTOM ERROR: The fligner test didn't work for year ", 
                       data$year[yea], ". Perhaps the data is NA for this year."))
        
        # And in case it failed on firt iteration...
        if (elev == 1 & mic == 1 & yea == 1) {
          # Create the total_output dataframe as a template
          total_output <- NULL
        }
        
        }
    }
    
    # Once all years have been done individually, complete each year in combination
    # IF there is more than one year of data
    if (length(unique(data$year)) > 1) {
      subset <- data %>% 
        filter(elevation == unique(data$elevation)[elev]) %>% 
        filter(micro == c(unique(data$micro)[mic]))
      
      test_output <- try(fligner.test(minmax ~ foliage, data = subset), silent = FALSE)
      
      # If the output of the test is of class 'htest', rather than class 'try-error'...
      # Meaning, if the flinger test succeeded...
      if (class(test_output) == "htest") {
        
        fligner_output <- test_output
        
        # Create tibble for iteration output
        iteration_output <- tibble(site = site, micro = unique(data$micro)[mic], 
                                   year = "all years",
                                   elevation = unique(data$elevation)[elev],
                                   chi_squared = fligner_output$statistic,
                                   p_value = fligner_output$p.value, method = fligner_output$data.name)
  
        total_output <- bind_rows(total_output, iteration_output)
      }
      
      # If the output wasn't class htest, meaning something failed along the way...
      else {
        # ...deliver an error message
        message(paste0("CUSTOM ERROR: The fligner test didn't work for year ", 
                       data$year[yea], ". Perhaps the data is NA for this year."))
        
      }
    }
  }
  }

  # Return total results
  total_output
}

## Conduct fligner's test on each site ########


nc_fligner <- conduct_fligner(data = NC, site = "NC")
nh_fligner <- conduct_fligner(data = NH, site = "NH")

## Write out results

# Combine results
fligner_results <- nc_fligner %>% 
  bind_rows(nh_fligner)

write_csv(fligner_results, "data/04_analysis/compare_seasonal_variance/fligner_homo_results.csv")

## ZZ retired #########

conduct_fligner_homo <- function(data, leaf_on_start, leaf_on_end,
                                    years, site, micro, final_file_path) {
  
  
  ## Calculate standard deviation, remove outliers ###########
  
  ## ....2f. Calculate Standard deviation and error ########
  
  ## Curate data, calculate Janzen overlap ##########
  
  ## Calculate d scores for soil
  if (length(dplyr::select(data, contains("soil"))) > 0) {  
    soil <- data %>%
      select(contains("soil"), year, julian) %>%
      select(-contains("mean"))
    
    soil <- na.omit(soil)
    
    # Calculate standard deviation and remove outliers (3 * SD)
    SD <- sd(soil$overlap)
    mean <- mean(soil$overlap)
    
    soil <- soil %>%
      filter(overlap < (mean + (3 * SD))) %>%
      filter(overlap > (mean - (3 * SD)))
  }
  
  ## Calculate d scores for surface
  if (length(dplyr::select(data, contains("surface"))) > 0) {  
    surface <- data %>%
      select(contains("surface"), year, julian) %>%
      select(-contains("mean"))
    
    surface <- na.omit(surface)
    
    # Calculate standard deviation and remove outliers (3 * SD)
    SD <- sd(surface$overlap)
    mean <- mean(surface$overlap)
    
    surface <- surface %>%
      filter(overlap < (mean + (3 * SD))) %>%
      filter(overlap > (mean - (3 * SD)))
  }
  
  ## Calculate d scores for canopy
  if (length(dplyr::select(data, contains("canopy"))) > 0) {  
    canopy <- data %>%
      select(contains("canopy"), year, julian) %>%
      select(-contains("mean"))
    
    canopy <- na.omit(canopy)
    
    # Calculate standard deviation and remove outliers (3 * SD)
    SD <- sd(canopy$overlap)
    mean <- mean(canopy$overlap)
    
    canopy <- canopy %>%
      filter(overlap < (mean + (3 * SD))) %>%
      filter(overlap > (mean - (3 * SD)))
  }
  
  ## Fligner's test for homoscedascity, leaf-off vs leaf-on #############
  # Loop through given inputs and conduct Kruskal
  for (i in 1:length(years)) {
    for (j in 1:length(micro)) {
      
      # Call the current desired dataset by combining strings of site and micro
      input_df <- eval(parse(text = (micro[j])))
      
      # Filter to year of interest
      mid_df <- input_df %>%
        filter(year == years[i])
      
      # Conduct kruskal-wallis, save to output df
      # Wrap the kruskal test in a `try` so that if there's an error in the
      #   analysis it won't kick us out of the loop
      test_output <- try(fligner.test(minmax ~ foliage, data = mid_df), silent = FALSE)
      
      # If the output of the test is of class 'htest', rather than class 'try-error'...
      # Meaning, if the kruskal test succeded...
      if (class(test_output) == "htest") {
        # ...save the output and continue forward with the iteration
        kruskal_output <- test_output
        
        # Capture the fligner output, as well as the corresponding site, micro and year
        output <- capture.output(site, micro[j], years[i], kruskal_output)
        
        # Create a tibble from all of these outputs
        iteration_output <- tibble(site = site, micro = micro[j], year = years[i],
                                   chi_squared = kruskal_output$statistic, df = kruskal_output$parameter, 
                                   p_value = kruskal_output$p.value, method = kruskal_output$data.name)
        
        # If first iteration...
        if (i == 1 & j == 1) {
          # Create new final output object, a data frame
          total_output <- iteration_output
        }
        # Otherwise bind the iteration's output to our final output data frame
        else {
          total_output <- bind_rows(total_output, iteration_output)
        }
        
      } 
      # If the output wasn't class htest, meaning something failed along the way...
      else {
        # ...deliver an error message
        message(paste0("CUSTOM ERROR: The fligner test didn't work for year ", 
                       years[i], ". Perhaps the data is NA for this year."))
      }
      
    }
  } 
  
  
  ## Export analysis outputs
  
  write.csv(total_output, final_file_path)
  
  
  # Remove objects that might interfere with later analyses
  rm(test_output, kruskal_output, output, iteration_output, total_output)
}
