#David Klinges
#2019.05.05
#This script conducts Fligner's test for homoscedascity, 
# comparing leaf-off vs leaf-on vs non-forest
#does so for daily min + daily max

## Prep workspace #############
library(tidyverse)
library(rlang)

# Import data
mountains_avg <- read_csv("data/03_compiled/mountains_avg.csv")

## Build Fligner's test function #########

# Foliage
conduct_fligner_foliage <- function(data) {
  
  # Gather min and max cols
  data <- data %>% 
    gather(key = "t", value = "minmax", min, max) %>% 
    select(-mean, -t)
  
  # Note: elev, mic, and site_iter indexes can't be the same strings as data column headers, 
  #   or else filtering the data won't work
  for (elev in 1:length(unique(data$elevation))) {
    for (mic in 1:length(unique(data$micro))) {
      for (site_iter in 1:length(unique(data$site))) {
        print(elev)
        print(mic)
        print(site_iter)
        
        subset <- data %>% 
          filter(elevation == unique(data$elevation)[elev]) %>% 
          filter(micro == c(unique(data$micro)[mic])) %>% 
          filter(site == unique(data$site)[site_iter])

        test_output <- try(fligner.test(minmax ~ foliage, data = subset), silent = FALSE)
        
        # If the output of the test is of class 'htest', rather than class 'try-error'...
        # Meaning, if the flinger test succeeded...
        if (class(test_output) == "htest") {
          
          fligner_output <- test_output
          
          # Create tibble for iteration output
          iteration_output <- tibble(site = unique(data$site)[site_iter], 
                                     macro = unique(subset$macro), 
                                     micro = unique(data$micro)[mic], 
                                     elevation = unique(data$elevation)[elev],
                                     chi_squared = fligner_output$statistic,
                                     p_value = fligner_output$p.value, method = fligner_output$data.name)
          
          # If first iteration...
          if (elev == 1 & mic == 1 & site_iter == 1) {
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
          message(paste0("CUSTOM ERROR: The fligner test didn't work for micro ", 
                         data$micro[mic], ". Perhaps the data is NA for this micro"))
          
          # And in case it failed on firt iteration...
          if (elev == 1 & mic == 1 & site_iter == 1) {
            # Create the total_output dataframe as a template
            total_output <- NULL
          }
          
        }
      }
      
    }
  }
  
  # Return total results
  total_output
}

# Micro
conduct_fligner_micro <- function(data) {
  
  # Gather min and max cols
  data <- data %>% 
    gather(key = "t", value = "minmax", min, max) %>% 
    select(-mean, -t)
  
  # Note: elev, and site_iter indexes can't be the same strings as data column headers, 
  #   or else filtering the data won't work
  for (elev in 1:length(unique(data$elevation))) {
      for (site_iter in 1:length(unique(data$site))) {
        print(elev)
        print(site_iter)
        
        subset <- data %>% 
          filter(elevation == unique(data$elevation)[elev]) %>% 
          filter(site == unique(data$site)[site_iter])
        
        test_output <- try(fligner.test(minmax ~ micro, data = subset), silent = FALSE)
        
        # If the output of the test is of class 'htest', rather than class 'try-error'...
        # Meaning, if the flinger test succeeded...
        if (class(test_output) == "htest") {
          
          fligner_output <- test_output
          
          # Create tibble for iteration output
          iteration_output <- tibble(site = unique(data$site)[site_iter], 
                                     macro = unique(subset$macro), 
                                     elevation = unique(data$elevation)[elev],
                                     chi_squared = fligner_output$statistic,
                                     p_value = fligner_output$p.value, method = fligner_output$data.name)
          
          # If first iteration...
          if (elev == 1 & site_iter == 1) {
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
          message(paste0("CUSTOM ERROR: The fligner test didn't work. Perhaps the data is NA for this micro"))
          
          # And in case it failed on firt iteration...
          if (elev == 1 & site_iter == 1) {
            # Create the total_output dataframe as a template
            total_output <- NULL
          }
          
        }
      }
      

  }
  
  # Return total results
  total_output
}

# Macro
conduct_fligner_macro <- function(data) {
  
  # Gather min and max cols
  data <- data %>% 
    gather(key = "t", value = "minmax", min, max) %>% 
    select(-mean, -t)
  
  # Note: elev, and site_iter indexes can't be the same strings as data column headers, 
  #   or else filtering the data won't work
  for (elev in 1:length(unique(data$elevation))) {
    
      print(elev)

      subset <- data %>% 
        filter(elevation == unique(data$elevation)[elev])
      
      test_output <- try(fligner.test(minmax ~ macro, data = subset), silent = FALSE)
      
      # If the output of the test is of class 'htest', rather than class 'try-error'...
      # Meaning, if the flinger test succeeded...
      if (class(test_output) == "htest") {
        
        fligner_output <- test_output
        
        # Create tibble for iteration output
        iteration_output <- tibble(elevation = unique(data$elevation)[elev],
                                   chi_squared = fligner_output$statistic,
                                   p_value = fligner_output$p.value, 
                                   method = fligner_output$data.name)
        
        # If first iteration...
        if (elev == 1) {
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
        message(paste0("CUSTOM ERROR: The fligner test didn't work. Perhaps the data is NA for this micro"))
        
        # And in case it failed on firt iteration...
        if (elev == 1) {
          # Create the total_output dataframe as a template
          total_output <- NULL
        }
        
      }
    }
  
  # Return total results
  total_output
}


## Conduct fligner's test ###############

fligner_foliage <- conduct_fligner_foliage(mountains_avg)

fligner_micro <- conduct_fligner_micro(mountains_avg)

fligner_macro <- conduct_fligner_macro(mountains_avg)

## Write out data #############

## Write out results
write_csv(fligner_foliage, "data/04_analysis/compare_variance/fligner_foliage_results.csv")
write_csv(fligner_micro, "data/04_analysis/compare_variance/fligner_micro_results.csv")
write_csv(fligner_macro, "data/04_analysis/compare_variance/fligner_macro_results.csv")

## Play code ########

test_filter <- function(data, filter_var) {
  
  group_enquo <- as_name(sym(filter_var))
  print(group_enquo)
  # group_enquo <- enquo(filter_var)
  # print(group_enquo)
  # group_enquo <- rlang::quo_text(group_enquo)
  # print(group_enquo)
  # group_enquo <- as.name(group_enquo)
  # print(group_enquo)
  
  data_out <- data %>% 
    filter(sym(filter_var) == "surface")
  
  return(data_out)
  
}
filtered <- test_filter(mountains_avg, "micro")

conduct_fligner <- function(data, grouping_var) {
  
  # Gather min and max cols
  data <- data %>% 
    gather(key = "t", value = "minmax", min, max) %>% 
    select(-mean, -t)
        
        group_enquo <- enquo(grouping_var)
        formula <- glue::glue("minmax ~ {grouping_var}")
        print(formula)
        print(colnames(data))
        col <- glue::glue("{grouping_var}")
        print(col)

        # test_output <- try(fligner.test(formula = formula, data = data), silent = FALSE)
        test_output <- fligner.test(data$minmax, data$col)

        # If the output of the test is of class 'htest', rather than class 'try-error'...
        # Meaning, if the flinger test succeeded...
        if (class(test_output) == "htest") {
          
          fligner_output <- test_output
          
          # Create tibble for iteration output
          output <- paste(fligner_output$statistic, 
                                    p_value = fligner_output$p.value, sep = " ")
        }
        
        # If the output wasn't class htest, meaning something failed along the way...
        else {
          # ...deliver an error message
          message(paste0("CUSTOM ERROR: The fligner test didn't work for micro. Perhaps the data is NA for this micro"))

          
  }
  
  # Return total results
  output
}

mountains_fligner$fligner <- conduct_fligner(data = mountains_avg, grouping_var = "micro")

mountains_fligner <- mountains_avg %>% 
  group_by(site) %>% 
  mutate(fligner = conduct_fligner(data = mountains_avg, grouping_var = micro))

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
