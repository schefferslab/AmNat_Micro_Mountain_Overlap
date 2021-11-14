# David Klinges
# This functions conducts kruskal wallis tests of d-scores between macrohabitats 
#   (leaf-on forests, leaf-off forests, non-forests)
# D-scores compare thermal distribution of microhabitats at high elevation vs low elevation

## Prep workspace #############
library(tidyverse)

conduct_Kruskal_overlap <- function(data, micro, comparison, final_file_path) {
  
  ## Coerce macro to factor
  data <- data %>% 
    mutate(macro = as.factor(macro))
  
  ## Calculate standard deviation, remove outliers ###########
  
  ## ....2f. Calculate Standard deviation and error ########
  
  ## Curate data, calculate Janzen overlap ##########
  
  ## Calculate d scores for soil
  if (length(dplyr::select(data, contains("soil"))) > 0) {  
    soil <- data %>%
      dplyr::select(contains("soil"), year, julian, macro) %>%
      dplyr::select(-contains("mean"))
    
    soil <- na.omit(soil)
    
    soil <- soil %>%
      # Janzen overlap calculation: d / sqrt(R1 * R2)
      mutate(overlap = (high_soil_max - low_soil_min) / 
               (sqrt((high_soil_max - high_soil_min) * (low_soil_max - low_soil_min)))) %>%
      # 2nd overlap calculation
      mutate(overlap_v2 = high_soil_max - low_soil_min) %>%
      select(year, julian, overlap, overlap_v2, macro)
    
    # Remove NaNs generated from Janzifying
    soil <- na.omit(soil)
    
    # Ensure overlap values are finite
    soil <- soil %>%
      filter(is.finite(overlap))
    
    # Calculate standard deviation and remove outliers (3 * SD)
    SD <- sd(soil$overlap, na.rm = TRUE)
    mean <- mean(soil$overlap, na.rm = TRUE)
    
    soil <- soil %>%
      filter(overlap < (mean + (3 * SD))) %>%
      filter(overlap > (mean - (3 * SD)))
  }
  
  ## Calculate d scores for surface
  if (length(dplyr::select(data, contains("surface"))) > 0) {  
    surface <- data %>%
      select(contains("surface"), year, julian, macro) %>%
      select(-contains("mean"))
    
    surface <- na.omit(surface)
    
    surface <- surface %>%
      # Janzen overlap calculation: d / sqrt(R1 * R2)
      mutate(overlap = (high_surface_max - low_surface_min) / 
               (sqrt((high_surface_max - high_surface_min) * (low_surface_max - low_surface_min)))) %>%
      # 2nd overlap calculation
      mutate(overlap_v2 = high_surface_max - low_surface_min) %>%
      select(year, julian, overlap, overlap_v2, macro)
    
    # Remove NaNs generated from Janzifying
    surface <- na.omit(surface)
    
    # Ensure overlap values are finite
    surface <- surface %>%
      filter(is.finite(overlap))
    
    # Calculate standard deviation and remove outliers (3 * SD)
    SD <- sd(surface$overlap, na.rm = TRUE)
    mean <- mean(surface$overlap, na.rm = TRUE)
    
    surface <- surface %>%
      filter(overlap < (mean + (3 * SD))) %>%
      filter(overlap > (mean - (3 * SD)))
  }
  
  ## Calculate d scores for canopy
  if (length(dplyr::select(data, contains("canopy"))) > 0) {  
    canopy <- data %>%
      select(contains("canopy"), year, julian, macro) %>%
      select(-contains("mean"))
    
    canopy <- na.omit(canopy)
    
    canopy <- canopy %>%
      # Janzen overlap calculation: d / sqrt(R1 * R2)
      mutate(overlap = (high_canopy_max - low_canopy_min) / 
               (sqrt((high_canopy_max - high_canopy_min) * (low_canopy_max - low_canopy_min)))) %>%
      # 2nd overlap calculation
      mutate(overlap_v2 = high_canopy_max - low_canopy_min) %>%
      select(year, julian, overlap, overlap_v2, macro)
    
    # Remove NaNs generated from Janzifying
    canopy <- na.omit(canopy)
    
    # Ensure overlap values are finite
    canopy <- canopy %>%
      filter(is.finite(overlap))
    
    # Calculate standard deviation and remove outliers (3 * SD)
    SD <- sd(canopy$overlap, na.rm = TRUE)
    mean <- mean(canopy$overlap, na.rm = TRUE)
    
    canopy <- canopy %>%
      filter(overlap < (mean + (3 * SD))) %>%
      filter(overlap > (mean - (3 * SD)))
  }
  
  ggplot(soil, aes(overlap)) +
    geom_density(aes(color = macro))
  
  
  ## Krusakl-wallis of overlap values, leaf-off vs leaf-on #############
  # Loop through given inputs and conduct Kruskal
    for (j in 1:length(micro)) {
      
      # Call the current desired dataset by combining strings of site and micro
      input_df <- eval(parse(text = (micro[j])))
      
      # Conduct kruskal-wallis, save to output df
      # Wrap the kruskal test in a `try` so that if there's an error in the
      #   analysis it won't kick us out of the loop
      test_output <- try(kruskal.test(overlap ~ macro, data = input_df), silent = FALSE)
      
      # If the output of the test is of class 'htest', rather than class 'try-error'...
      # Meaning, if the kruskal test succeded...
      if (class(test_output) == "htest") {
        # ...save the output and continue forward with the iteration
        kruskal_output <- test_output
        
        # Capture the kruskal output, as well as the corresponding site, micro and year
        output <- capture.output(comparison, micro[j], kruskal_output)
        
        # Create a tibble from all of these outputs
        iteration_output <- tibble(comparison = comparison, micro = micro[j],
                                   chi_squared = kruskal_output$statistic, df = kruskal_output$parameter, 
                                   p_value = kruskal_output$p.value, method = kruskal_output$data.name)
        
        # If first iteration...
        if (j == 1) {
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
        message(paste0("CUSTOM ERROR: The kruskal test didn't work for micro ", 
                       micro[j], ". Perhaps the data is NA for this micro"))
      }
      
    }
  
  
  ## Export analysis outputs
  
  write_csv(total_output, final_file_path)
  
  
  # Remove objects that might interfere with later analyses
  rm(test_output, kruskal_output, output, iteration_output, total_output)
}
