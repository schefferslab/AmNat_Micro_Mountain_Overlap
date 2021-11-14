# David Klinges
# This script conducts Kruskal-Wallis tests of d-scores from 'leaf-on' vs 'leaf-off'
#   for Wyoming Sage brush sites
# D-scores compare thermal distribution of microhabitats at high elevation vs low elevation
# Dependant on objects created in "ameriflux_prep.R"

## Prep workspace #############
library(tidyverse)

# Read in current dataset
CURRENT_DATA <- read_csv("./data/01_primary/temperate/CA_sonoran_desert/derivative/sonoran_desert_wide.csv")

# Set leaf on start and end days of year
LEAF_ON_START <- 150
LEAF_ON_END <- 280

# Set desired years in time series to analyze
YEARS <- c(2009, 2010, 2011, 2012)

# Set current elevation gradient site
SITE <- "socal"

# Set microhabitats of interest
MICRO <- c("soil", "surface")

FINAL_FILE_PATH <- "./data/04_analysis/SoCal_kruskal_outputs2.csv"

## Curate data, calculate Janzen overlap ##########
sage_wide <- na.omit(CURRENT_DATA)

soil <- sage_wide %>%
  # Janzen overlap calculation: d / sqrt(R1 * R2)
  mutate(overlap = (high_soil_max - low_soil_min) / 
  (sqrt((high_soil_max / high_soil_min) * (low_soil_max - low_soil_min)))) %>%
  # 2nd overlap calculation
  mutate(overlap_v2 = high_soil_max - low_soil_min) %>%
  # Define what days are 'leaf on' and 'leaf off' even though there are no leaves
  mutate(foliage = ifelse((julian < LEAF_ON_START | julian > LEAF_ON_END), 0, 1)) %>%
  select(year, julian, overlap, overlap_v2, foliage)

surface <- sage_wide %>%
  # Janzen overlap calculation: d / sqrt(R1 * R2)
  mutate(overlap = (high_surface_max - low_surface_min) / 
           (sqrt((high_surface_max / high_surface_min) * (low_surface_max - low_surface_min)))) %>%
  # 2nd overlap calculation
  mutate(overlap_v2 = high_surface_max - low_surface_min) %>%
  # Define what days are 'leaf on' and 'leaf off' even though there are no leaves
  mutate(foliage = ifelse((julian < LEAF_ON_START | julian > LEAF_ON_END), 0, 1)) %>%
  select(year, julian, overlap, overlap_v2, foliage)

# Create a function that summarizes a data frame to a specific interval of observations
interval_set <- function(df, int, new) {
  out<- data.frame()
  temp <- df
  
  # For 1 to the # of times the interval fits into the length of df, rounded up
  for(w in 1:ceiling(nrow(temp)/int)) { 
    loop <- temp %>%
      slice(1:int) %>% #slice to just the number of rows corresponding to the interval
      mutate(new = w)
    out <- rbind(out, loop)
    temp <- slice(temp, (int+1):nrow(temp))
  }
  out <- out %>%
    group_by(new) %>%
    summarize_all(funs(mean))
  return(out)
}

## Krusakl-wallis of overlap values, leaf-off vs leaf-on #############
# Loop through given inputs and conduct Kruskal
for (i in 1:length(YEARS)) {
  for (j in 1:length(MICRO)) {
    
    # Call the current desired dataset by combining strings of site and micro
    input_df <- eval(parse(text = (MICRO[j])))

    # Filter to year of interest
    mid_df <- input_df %>%
      filter(year == YEARS[i])
  
    # Conduct kruskal-wallis, save to output df
    # Wrap the kruskal test in a `try` so that if there's an error in the
    #   analysis it won't kick us out of the loop
    test_output <- try(kruskal.test(overlap ~ foliage, data = mid_df), silent = FALSE)
    
    # If the output of the test is of class 'htest', rather than class 'try-error'...
    # Meaning, if the kruskal test succeded...
    if (class(test_output) == "htest") {
      # ...save the output
      kruskal_output <- test_output
    }
    # Capture the kruskal output, as well as the corresponding site, micro and year
    output <- capture.output(SITE, MICRO[j], YEARS[i], kruskal_output)
    
    # Create a tibble from all of these outputs
    iteration_output <- tibble(site = SITE, micro = MICRO[j], year = YEARS[i],
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
} 


## Export analysis outputs

write.csv(total_output, FINAL_FILE_PATH)

## Deprecated manual versions #############
## * Compare daily averages ###########
# Conduct kruskal wallis with overlap method 1 data

# 2015
#Compare leaf-off and -on, soil. Overlap method 1
sage_soil_2015_v1 <- kruskal.test(overlap ~ foliage, data = sage_soil_2015)
#Compare leaf-off and -on, soil. Overlap method 2
sage_soil_2015_v2 <- kruskal.test(overlap_v2 ~ foliage, data = sage_soil_2015)

#Compare leaf-off and -on, ground. Overlap method 1
sage_surface_2015_v1 <- kruskal.test(overlap ~ foliage, data = sage_surface_2015)
#Compare leaf-off and -on, ground. Overlap method 2
sage_surface_2015_v2 <- kruskal.test(overlap_v2 ~ foliage, data = sage_surface_2015)

# 2016
#Compare leaf-off and -on, soil. Overlap method 1
kruskal.test(overlap ~ foliage, data = sage_soil_2016)
#Compare leaf-off and -on, soil. Overlap method 2
kruskal.test(overlap_v2 ~ foliage, data = sage_soil_2016)

#Compare leaf-off and -on, ground. Overlap method 1
kruskal.test(overlap ~ foliage, data = sage_surface_2016)
#Compare leaf-off and -on, ground. Overlap method 2
kruskal.test(overlap_v2 ~ foliage, data = sage_surface_2016)

## * Summarize overlap to weekly values ##########
sage_soil_weekly <- interval_set(sage_soil, 7, "week")
sage_surface_weekly <- interval_set(sage_surface, 7, "week")
## * Summarize overlap to monthly values ##########
sage_soil_monthly <- interval_set(sage_soil, 30, "month")
sage_surface_monthly <- interval_set(sage_surface, 30, "month")

## * Compare weekly averages ###########
#Compare leaf-off and -on, soil. Overlap method 1
kruskal.test(overlap ~ foliage, data = sage_soil_weekly)
#Compare leaf-off and -on, soil. Overlap method 2
kruskal.test(overlap_v2 ~ foliage, data = sage_soil_weekly)

#Compare leaf-off and -on, ground. Overlap method 1
kruskal.test(overlap ~ foliage, data = sage_surface_weekly)
#Compare leaf-off and -on, ground. Overlap method 2
kruskal.test(overlap_v2 ~ foliage, data = sage_surface_weekly)

## * Compare monthly averages ##########

soil_overlap_off <- filter(soil_overlap, foliage == 0)
soil_overlap_on <- filter(soil_overlap, foliage == 1)
ground_overlap_off <- filter(ground_overlap, foliage == 0)
ground_overlap_on <- filter(ground_overlap, foliage == 1)
canopy_overlap_off <- filter(canopy_overlap, foliage == 0)
canopy_overlap_on <- filter(canopy_overlap, foliage == 1)


t.test(canopy_overlap_off$overlap, canopy_overlap_on$overlap)

## Sample plots ##############

p <- ggplot(sage_wide, aes(julian, low_surface_mean)) +
  geom_point(color = "red") +
  geom_point(data = sage_wide, aes(julian, high_surface_mean, alpha = "blue"))
p

p <- ggplot(sage_wide, aes(julian, low_surface_min)) +
  geom_point(color = "red") +
  geom_point(data = sage_wide, aes(julian, high_surface_min, alpha = "blue"))
p

p <- ggplot(sage_wide, aes(julian, low_surface_max)) +
  geom_point(color = "red") +
  geom_point(data = sage_wide, aes(julian, high_surface_max, alpha = "blue"))
p
