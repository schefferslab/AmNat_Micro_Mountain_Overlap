# David Klinges
# this script compares thermal regimes of microhabitats, at the same elevation, 
# seperately for leaf-off and leaf-on


## 1. Workspace prep ###############
library(tidyverse)

mountains_janz <- read_csv("data/03_compiled/mountains_janz_month_avg.csv")
mountains <- read_csv("data/03_compiled/mountains.csv")

## 2. Compare overlap values between micros ########


mountains_janz <- mountains_janz %>%
  mutate(micro = as.factor(micro))

# D-scores
kruskal.test(mountains_janz ~ macro, data = mountains_janz)
# TAD
kruskal.test(TAD ~ macro, data = mountains_janz)

kruskal.test(minmax ~ micro, data = mountains)

## Analysis #############

compare_micros <- function(data) {
  
  data <- data %>% 
    group_by(site)
  
  # Combine min and max into one column
  # Low soil
  low_soil <- data %>%
    filter(elevation == "low", micro == "soil") %>%
    select(year, julian, min, max) %>%
    gather(key = "minmax", "temp", -year, -julian)
  
  # Low surface
  low_surface <- data %>%
    filter(elevation == "low", micro == "surface") %>%
    select(year, julian, min, max) %>%
    gather(key = "minmax", "temp", -year, -julian)
  
  # High soil
  high_soil <- data %>%
    filter(elevation == "high", micro == "soil") %>%
    select(year, julian, min, max) %>%
    gather(key = "minmax", "temp", -year, -julian)
  
  # High surface
  high_surface <- data %>%
    filter(elevation == "high", micro == "surface") %>%
    select(year, julian, min, max) %>%
    gather(key = "minmax", "temp", -year, -julian)
  
  # Designate how many years of data there are
  
  years <- unique(data$year)
  
  # Low elevation, conduct separately for each year
  for (i in 1:length(unique(low_soil$year))) {
    
    # Filter to each year
    subset_soil <- low_soil %>%
      filter(year == years[i])
    subset_soil <- na.omit(subset_soil)
    
    subset_surface <- low_surface %>%
      filter(year == years[i])
    subset_surface <- na.omit(subset_surface)
    
    # Conduct kruskal
    
    test_output <- try(kruskal.test(list(subset_soil$temp, subset_surface$temp), silent = FALSE))
    print(test_output)
    
    if (class(test_output) == "htest") {
      # ...save the output and continue forward with the iteration
      kruskal_output <- test_output
      
      # Create a tibble from all of these outputs
      iteration_output <- tibble(site = site, elevation = "low", year = years[i],
                                 chi_squared = kruskal_output$statistic, df = kruskal_output$parameter, 
                                 p_value = kruskal_output$p.value, method = kruskal_output$data.name)
    } else {
      # If the first iteration fails, just include a blank row
      iteration_output <- tibble(site = NA, elevation = NA, year = NA,
                                 chi_squared = NA, df = NA, 
                                 p_value = NA, method = NA)
    }
      # If first iteration...
      if (i == 1) {
        # Create new final output object, a data frame
        total_output <- iteration_output
      }
      # Otherwise bind the iteration's output to our final output data frame
      else {
        total_output <- bind_rows(total_output, iteration_output)
      }
      
    
  }
  
  # High elevation, conduct separately for each year
  for (i in 1:length(unique(high_soil$year))) {
    
    # Filter to each year
    subset_soil <- high_soil %>%
      filter(year == years[i])
    subset_soil <- na.omit(subset_soil)
    
    subset_surface <- high_surface %>%
      filter(year == years[i])
    subset_surface <- na.omit(subset_surface)
    
    # Conduct kruskal
    
    test_output <- try(kruskal.test(list(subset_soil$temp, subset_surface$temp), silent = FALSE))
    print(test_output)
    
    if (class(test_output) == "htest") {
      # ...save the output and continue forward with the iteration
      kruskal_output <- test_output
      
      # Create a tibble from all of these outputs
      iteration_output <- tibble(site = site, elevation = "high", year = years[i],
                                 chi_squared = kruskal_output$statistic, df = kruskal_output$parameter, 
                                 p_value = kruskal_output$p.value, method = kruskal_output$data.name)
      
      # Want to start with binding to low-elevation outputs
      total_output <- bind_rows(total_output, iteration_output)
    } 
  }
  
  # Now conduct once for all years combined
  # Low elevation
  test_output <- try(kruskal.test(list(low_soil$temp, low_surface$temp), silent = FALSE))
  if (class(test_output) == "htest") {
    # ...save the output and continue forward with the iteration
    kruskal_output <- test_output
    
    # Create a tibble from all of these outputs
    iteration_output <- tibble(site = site, elevation = "low", year = "all years",
                               chi_squared = kruskal_output$statistic, df = kruskal_output$parameter, 
                               p_value = kruskal_output$p.value, method = kruskal_output$data.name)
    
    # Convert year to character to enable string for `all years`
    total_output <- total_output %>%
      mutate(year = as.character(year))
    
    total_output <- bind_rows(total_output, iteration_output)
  } 
  
  # High elevation
  test_output <- try(kruskal.test(list(high_soil$temp, high_surface$temp), silent = FALSE))
  if (class(test_output) == "htest") {
    # ...save the output and continue forward with the iteration
    kruskal_output <- test_output
    
    # Create a tibble from all of these outputs
    iteration_output <- tibble(site = site, elevation = "high", year = "all years",
                               chi_squared = kruskal_output$statistic, df = kruskal_output$parameter, 
                               p_value = kruskal_output$p.value, method = kruskal_output$data.name)
    
    # Convert year to character to enable string for `all years`
    total_output <- total_output %>%
      mutate(year = as.character(year))
    
    total_output <- bind_rows(total_output, iteration_output)
  } 
  return(total_output)
}

output <- compare_micros(mountains)

output <- compare_micros(idaho_raw, "idaho") %>%
  bind_rows(compare_micros(idaho_leafoff, "idaho_leafoff")) %>%
  bind_rows(compare_micros(idaho_leafon, "idaho_leafon")) %>%  
  bind_rows(compare_micros(sonoran_raw, "sonoran")) %>%
  bind_rows(compare_micros(sonoran_leafoff, "sonoran_leafoff")) %>%
  bind_rows(compare_micros(sonoran_leafon, "sonoran_leafon")) %>%  
  bind_rows(compare_micros(NC_leafoff_raw, "NC_leafoff")) %>%
  bind_rows(compare_micros(NC_leafon_raw, "NC_leafon")) %>%
  bind_rows(compare_micros(boulder_raw, "boulder")) %>%
  bind_rows(compare_micros(boulder_leafon, "boulder_leafon")) %>%
  bind_rows(compare_micros(boulder_leafoff, "boulder_leafoff")) %>%
  bind_rows(compare_micros(Mada_raw, "Madagascar")) %>%
  # bind_rows(compare_micros(Phili_raw, "Philippines")) %>%
  bind_rows(compare_micros(Aust_raw, "Australia"))



## Write out data ############

write_csv(output, "data/04_analysis/compare_micros/compare_micros_temperate.csv")

