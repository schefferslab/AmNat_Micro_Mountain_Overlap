# David Klinges
# File creation date: 2019.05.04
# This script conducts Kruskal-Wallis for overlap across elevation for Portugal sites 


## Prep workspace #############
library(tidyverse)

# Read in current dataset
data <- read_csv("data/01_primary/temperate/PT_serra_estrala/derivative/SerraEstrala_cantaro_zezere_wide.csv")

# Set leaf on start and end days of year
leaf_on_start <- 150
leaf_on_end <- 280

# Set desired years in time series to analyze
years <- c(2000)

# Set current elevation gradient site
site <- "PT serra estrala"

# Set microhabitats of interest
micro <- c("surface")

final_file_path <- "data/04_analysis/compare_overlap_seasonality/SerraEstrala_cantaro_zezere_Kruskal_outputs.csv"

source("./scripts/analysis/compare_overlap_seasonality/conduct_kruskal_overlap.R")
conduct_Kruskal_overlap(data, leaf_on_start, leaf_on_end,
                        years, site, micro, final_file_path)


test <- data %>%
  mutate(minmaxlow = low_surface_max - low_surface_min) %>%
  mutate(minmaxhigh = high_surface_max - high_surface_min)

ggplot(test, aes(julian, minmaxlow)) +
  geom_point(aes(color = "low")) +
  geom_point(data = test, aes(julian, minmaxhigh))

