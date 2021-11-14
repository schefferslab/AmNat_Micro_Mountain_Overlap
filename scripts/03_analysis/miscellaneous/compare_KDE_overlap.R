# David Klinges
# dklinges@ufl.edu
# This script compares the overlap in KDE distributions from different sites


## 1. Prep workspace ########
library(tidyverse)
library(lubridate)
library(overlapping)

# Import data
mountains <- read_csv("data/03_compiled/mountains_avg.csv")
mountains_wide <- read_csv("data/03_compiled/mountains_wide_avg.csv",
                           col_types = cols(
                             high_canopy_mean = col_double(),
                             high_canopy_max = col_double(),
                             high_canopy_min = col_double(),
                             low_canopy_mean = col_double(),
                             low_canopy_max = col_double(),
                             low_canopy_min = col_double()
                           ))

## 2. Data curation ##########

# Create dataframe of just sites and elevation changes, so we can later control
# variance for elevation change
mountains_elevchange <- mountains_wide %>% 
  # Select down columns for ease of joining
  dplyr::select(site, elevation_change) %>% 
  distinct() %>% 
  filter(complete.cases(elevation_change))

mountains <- mountains %>%
  # Combine min, mean, and max cols
  gather(key = "minMeanMax", value = "temp", min, max) %>% 
  # Remove NA values
  filter(complete.cases(temp)) %>% 
  # Select down columns for ease of joining
  dplyr::select(site, macro, micro, foliage, year, julian, temp, elevation) %>% 
  left_join(mountains_elevchange)


# Summarize to monthly values
mountains$date <- as.Date(mountains$julian, format = "%j", origin = paste0("1.1.", mountains$year))
mountains$month <- month(as.POSIXlt(mountains$date, format="%d/%m/%Y"))

## 3. Calc KDE overlap ###########
## ....A. By day ###########
## ......* Calc overlap for each micro for each foliage for each site (see notes) ########

# Must be at site level not macro level, because we're not interested in bunching
# together the thermal regimes of Idaho and New Mexico...or NC and NH. After
# calculating overlap for each site you will summarize to the macro level

# Create a list of site
site_list <- unique(mountains$site)

# For each site...
for (i in 1:length(site_list)) {
  
  site_iter <- mountains %>% 
    filter(site == site_list[i])
  
  # ...for each foliage in that site...
  for (j in 1:length(unique(site_iter$foliage))) {
    
    foliage_iter <- site_iter %>% 
      filter(foliage == unique(site_iter$foliage)[[j]])
    
    # ...and for each micro in that site...
    for (k in 1:length(unique(foliage_iter$micro))) {
      
      micro_iter <- foliage_iter %>% 
        filter(micro == unique(foliage_iter$micro)[[k]])
      
      # Calculate the overlap between high and low
      overlap <- try(overlap(list(na.omit(filter(micro_iter, elevation == "low")$temp), 
                              na.omit(filter(micro_iter, elevation == "high")$temp)), 
                         nbins = 1000, plot = FALSE, partial.plot = FALSE)$OV)
      
      if (class(overlap) == "try-error") {
        print("failed to calculate overlap, saving NA instead")
        overlap <- NA
      }
      
      # Add the overlap value as an attribute
      micro_iter <- micro_iter %>% 
        mutate(overlap = overlap)

      # If first iteration...
      if (i == 1 & j == 1 & k == 1) {
        # Create output dataframe
        KDE_overlaps_daily <- micro_iter
        # Otherwise bind to existing dataframe
      } else {
        KDE_overlaps_daily <- bind_rows(KDE_overlaps_daily, micro_iter)
      }
    }
  }
}

# We only want the overlap values now. There's only one unique overlap value for
# each micro in each macro, so distinct() the dataframe

KDE_overlaps_daily <- KDE_overlaps_daily %>% 
  dplyr::select(macro, site, foliage, micro, overlap, elevation_change) %>% 
  distinct()

# Now that the whole dataframe is bound together, correct for elevation change
# Source for elevation control
source("scripts/00_source_code/correct_elevChange.R")

KDE_overlaps_daily <- KDE_overlaps_daily %>% 
  filter(is.finite(overlap)) %>%
  filter(complete.cases(overlap)) %>%
  filter(is.finite(elevation_change)) %>%
  filter(complete.cases(elevation_change))

KDE_overlaps_daily <- KDE_overlaps_daily %>% 
  # Control for elevation chance, joined in from wide data
  # mutate(overlap_elevCont = overlap * elevation_change / 1000) %>% 
  mutate(overlap_elevCorr = correct_elevChange(KDE_overlaps_daily, 
                                               overlap,
                                               "linear",
                                               beta0 = NA,
                                               beta1 = NA,
                                               beta2 = NA))


## ....B. By month ###########

mountains_monthly <- mountains %>% 
  group_by(macro, site, micro, foliage, elevation_change, elevation, year, month) %>% 
  summarize(temp = mean(temp, na.rm = TRUE)) %>% 
  ungroup()
  
## ......* Calc overlap for each micro for each foliage for each site (see notes) ########

# Must be at site level not macro level, because we're not interested in bunching
# together the thermal regimes of Idaho and New Mexico...or NC and NH. After
# calculating overlap for each site you will summarize to the macro level

# Create a list of site
site_list <- unique(mountains_monthly$site)

# For each site...
for (i in 1:length(site_list)) {
  
  site_iter <- mountains_monthly %>% 
    filter(site == site_list[i])
  
  # ...for each foliage in that site...
  for (j in 1:length(unique(site_iter$foliage))) {
    
    foliage_iter <- site_iter %>% 
      filter(foliage == unique(site_iter$foliage)[[j]])
    
    # ...and for each micro in that site...
    for (k in 1:length(unique(foliage_iter$micro))) {
      
      micro_iter <- foliage_iter %>% 
        filter(micro == unique(foliage_iter$micro)[[k]])
      
      # Calculate the overlap between high and low
      overlap <- try(overlap(list(na.omit(filter(micro_iter, elevation == "low")$temp), 
                              na.omit(filter(micro_iter, elevation == "high")$temp)), 
                         nbins = 1000, plot = FALSE, partial.plot = FALSE)$OV)
      
      if (class(overlap) == "try-error") {
        print("failed to calculate overlap, saving NA instead")
        overlap <- NA
      }
      
      # Add the overlap value as an attribute
      micro_iter <- micro_iter %>% 
        mutate(overlap = overlap)
      
      # If first iteration...
      if (i == 1 & j == 1 & k == 1) {
        # Create output dataframe
        KDE_overlaps_monthly <- micro_iter
        # Otherwise bind to existing dataframe
      } else {
        KDE_overlaps_monthly <- bind_rows(KDE_overlaps_monthly, micro_iter)
      }
    }
  }
}

# We only want the overlap values now. There's only one unique overlap value for
# each micro in each macro, so distinct() the dataframe

KDE_overlaps_monthly <- KDE_overlaps_monthly %>% 
  dplyr::select(macro, site, foliage, micro, overlap, elevation_change) %>% 
  distinct()

# Now that the whole dataframe is bound together, correct for elevation change
# Source for elevation control
KDE_overlaps_monthly <- KDE_overlaps_monthly %>% 
  filter(is.finite(overlap)) %>%
  filter(complete.cases(overlap)) %>%
  filter(is.finite(elevation_change)) %>%
  filter(complete.cases(elevation_change))

KDE_overlaps_monthly <- KDE_overlaps_monthly %>% 
  # Control for elevation chance, joined in from wide data
  # mutate(overlap_elevCont = overlap * elevation_change / 1000) %>% 
  mutate(overlap_elevCorr = correct_elevChange(KDE_overlaps_monthly, 
                                               overlap,
                                               "linear",
                                               beta0 = NA,
                                               beta1 = NA,
                                               beta2 = NA))



## 4. Exclude low-replicate sites/micros (monthly only) ##########

monthly_count <- mountains_monthly %>% 
  group_by(site, micro) %>% 
  count()

KDE_overlaps_monthly_exclude <- KDE_overlaps_monthly %>% 
  left_join(monthly_count) %>% 
  filter(n >= 12) %>% 
  dplyr::select(-n)

## 5. Quick summarization to macro and foliage level #######

KDE_overlaps_daily_summarized <- KDE_overlaps_daily %>% 
  dplyr::select(-site) %>% 
  group_by(macro, foliage, micro) %>% 
  summarize_all(mean, na.rm = TRUE)

KDE_overlaps_monthly_summarized <- KDE_overlaps_monthly %>% 
  dplyr::select(-site) %>% 
  group_by(macro, foliage, micro) %>% 
  summarize_all(mean, na.rm = TRUE)

KDE_overlaps_monthly_exclude_summarized <- KDE_overlaps_monthly_exclude %>% 
  dplyr::select(-site) %>% 
  group_by(macro, foliage, micro) %>% 
  summarize_all(mean, na.rm = TRUE)

## 6. Write out data #######

write_csv(KDE_overlaps_daily, "data/04_analysis/compare_density_distribs/KDE_overlap_daily.csv")
write_csv(KDE_overlaps_monthly, "data/04_analysis/compare_density_distribs/KDE_overlap_monthly.csv")

write_csv(KDE_overlaps_daily_summarized, "data/04_analysis/compare_density_distribs/KDE_overlap_daily_MacroSummarized.csv")
write_csv(KDE_overlaps_monthly_summarized, "data/04_analysis/compare_density_distribs/KDE_overlap_monthly_MacroSummarized.csv")
write_csv(KDE_overlaps_monthly_exclude_summarized, "data/04_analysis/compare_density_distribs/KDE_overlap_monthly_MacroSummarized_lowRepExclude.csv")

# ## RECYCLING BIN ################
# 
# overlapper <- function(x, y, comparison) {
# 
#   for (i in 1:length(micros)) {
#     for (j in 1:length(elevations)) {
#       micro1 <- micros[i]
#       elevation1 <- elevations[j]
#       
#       out <- overlap(list(
#         # Omit NAs, and filter to the appropriate micro and elevation...
#         # Then, extract just the variance column
#         na.omit(filter(x, micro == micro1, elevation == elevation1)$variance),
#         # Do this for the secon dataset
#         na.omit(filter(y, micro == micro1, elevation == elevation1)$variance)), 
#         nbins = 1000, plot = FALSE, partial.plot = FALSE)
#       
#       # Create a tibble from all of these outputs
#       iteration_output <- tibble(comparison = comparison, micro = micro1, elevation = elevation1,
#                                  overlap = out[2], xpoints = out[3])
#       
#       # If first iteration...
#       if (i == 1 & j == 1) {
#         # Create new final output object, a data frame
#         total_output <- iteration_output
#       }
#       # Otherwise bind the iteration's output to our final output data frame
#       else {
#         total_output <- bind_rows(total_output, iteration_output)
#       }
#     }
#   }
# return(total_output)
# }
# 
# temp_leaf_trop <- overlapper(temp_leaf, trop_leaf, "temp leaf vs trop leaf")
# temp_temp <- overlapper(temp_leaf, temp_noleaf, "temp leaf vs temp no leaf")
# temp_noleaf_trop <- overlapper(temp_noleaf, trop_leaf, "temp no leaf vs trop leaf")
# 
# total_overlaps <- temp_leaf_trop %>%
#   bind_rows(temp_temp) %>%
#   bind_rows(temp_noleaf_trop)
# 
# 
# 
# ## Gather min and max ###########
# 
# temp_leaf_soil <- temp_leaf %>%
#   filter(micro == "soil") %>%
#   gather(key = "minmax", value = "soil", min, max)
# temp_leaf_soil <- na.omit(temp_leaf_soil)
# 
# trop_leaf <- trop_leaf %>%
#   filter(micro == "soil") %>%
#   gather(key = "minmax", value = "soil", min, max)
# trop_leaf <- na.omit(trop_leaf)
# 
# overlap(list(temp_leaf_soil$soil, trop_leaf$soil), nbins = 1000, plot = TRUE, partial.plot = FALSE)
# 
# 
# temp_leaf_surface <- temp_leaf %>%
#   filter(micro == "surface") %>%
#   gather(key = "minmax", value = "surface", min, max)
# temp_leaf_surface <- na.omit(temp_leaf_surface)
# 
# trop_leaf <- trop_leaf %>%
#   filter(micro == "surface") %>%
#   gather(key = "minmax", value = "surface", min, max)
# trop_leaf <- na.omit(trop_leaf)
# overlap(list(temp_leaf_surface$surface, trop_leaf$surface), nbins = 1000, plot = TRUE, partial.plot = FALSE)
# 
# ## change test 2 ##########
# 
# 
# 
# gather_custom <- function(data) {
#   data <- data %>%
#     gather(key = "metric", value = "temp", "min", "mean", "max")
#   return(data)
# }
# 
# boulder <- gather_custom(boulder)
# boulder <- gather_custom(boulder)
# boulder <- gather_custom(boulder)
# boulder <- gather_custom(boulder)
# 
# ######## Generate kernal density plots #############
# ######## Tropical ########
# #densities for tropical, all 3 regions combined
# #tropical soil low
# trop_soil_low_density <- density(trop_soil_low$temp)
# plot(trop_soil_low_density)
# 
# #tropical soil up
# trop_soil_up_density <- density(trop_soil_up$temp)
# plot(trop_soil_up_density)
# 
# #tropical ground low
# trop_ground_low_density <- density(trop_ground_low$temp)
# plot(trop_ground_low_density)
# 
# #tropical ground up
# trop_ground_up_density <- density(trop_ground_up$temp)
# plot(trop_ground_up_density)
# 
# #tropical canopy low
# trop_canopy_low_density <- density(trop_canopy_low$temp)
# plot(trop_canopy_low_density)
# 
# #tropical canopy up
# trop_canopy_up_density <- density(trop_canopy_up$temp)
# plot(trop_canopy_up_density)
# 
# ######## Temperate #########
# 
# # Leaf off
# # Idaho sage leaf-off soil low
# boulder <- boulder_raw %>%
#   select(-year, -julian)
# 
# idaho <- idaho_raw %>%
#   select(-year, -julian)
# 
# for (i in 1:length(idaho)) {
#   idaho <- idaho %>%
#     select(i)
#   
#   idaho <- idaho %>%
#     filter_all(all_vars(!is.na(.)))
#   
#   boulder <- boulder %>%
#     filter_all(all_vars(!is.na(.)))
# 
#   
#   
#   idaho[ ,i] <- na.omit(idaho[ ,i])
#   boulder[ ,i] <- na.omit(boulder[ ,i])
#   
# }
# 
# overlap(list(idaho$high_surface_mean, boulder$high_surface_mean), nbins = 1000, plot = TRUE, partial.plot = FALSE)
# 
# sage_soil_high <- na.omit(sage_soil_high)
# 
# overlap(list(sage_soil_low$mean, sage_soil_high$mean), nbins = 1000, plot = TRUE, partial.plot = FALSE)
# 
# 
# NC_off_soil_low_density <- density(NC_off_soil_low$temp) # returns the density data
# plot(NC_off_soil_low_density) # plots the results
# 
# #temperate leaf-off soil up
# NC_off_soil_up_density <- density(NC_off_soil_up$temp) # returns the density data
# plot(NC_off_soil_up_density) # plots the results
# 
# #temperate leaf-off ground low
# NC_off_ground_low_density <- density(NC_off_ground_low$temp) # returns the density data
# plot(NC_off_ground_low_density) # plots the results
# 
# #temperate leaf-off ground up
# NC_off_ground_up_density <- density(NC_off_ground_up$temp) # returns the density data
# plot(NC_off_ground_up_density) # plots the results
# 
# #temperate leaf-off canopy low
# NC_off_canopy_low_density <- density(NC_off_canopy_low$temp) # returns the density data
# plot(NC_off_canopy_low_density) # plots the results
# 
# #temperate leaf-off canopy up
# NC_off_canopy_up_density <- density(NC_off_canopy_up$temp) # returns the density data
# plot(NC_off_canopy_up_density) # plots the results
# 
# #leaf on
# #temperate leaf-on soil low
# NC_on_soil_low_density <- density(NC_on_soil_low$temp) # returns the density data
# plot(NC_on_soil_low_density) # plots the results
# 
# #temperate leaf-on soil up
# NC_on_soil_up_density <- density(NC_on_soil_up$temp) # returns the density data
# plot(NC_on_soil_up_density) # plots the results
# 
# #temperate leaf-on ground low
# NC_on_ground_low_density <- density(NC_on_ground_low$temp) # returns the density data
# plot(NC_on_ground_low_density) # plots the results
# 
# #temperate leaf-on ground up
# NC_on_ground_up_density <- density(NC_on_ground_up$temp) # returns the density data
# plot(NC_on_ground_up_density) # plots the results
# 
# #temperate leaf-on canopy low
# NC_on_canopy_low_density <- density(NC_on_canopy_low$temp) # returns the density data
# plot(NC_on_canopy_low_density) # plots the results
# 
# #temperate leaf-on canopy up
# NC_on_canopy_up_density <- density(NC_on_canopy_up$temp) # returns the density data
# plot(NC_on_canopy_up_density) # plots the results
# 
# ######## Calculate overlap area ############
# 
# ######## leaf off vs leaf on #######
# #Overlap of temperate soil low, leaf off vs leaf on
# overlap(list(NC_off_soil_low$temp, NC_on_soil_low$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)
# 
# #Overlap of temperate soil up, leaf off vs leaf on
# overlap(list(NC_off_soil_up$temp, NC_on_soil_up$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)
# 
# #Overlap of temperate ground low, leaf off vs leaf on
# overlap(list(NC_off_ground_low$temp, NC_on_ground_low$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)
# 
# #Overlap of temperate ground up, leaf off vs leaf on
# overlap(list(NC_off_ground_up$temp, NC_on_ground_up$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)
# 
# #Overlap of temperate canopy low, leaf off vs leaf on
# overlap(list(NC_off_canopy_low$temp, NC_on_canopy_low$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)
# 
# #Overlap of temperate canopoy up, leaf off vs leaf on
# overlap(list(NC_off_canopy_up$temp, NC_on_canopy_up$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)
