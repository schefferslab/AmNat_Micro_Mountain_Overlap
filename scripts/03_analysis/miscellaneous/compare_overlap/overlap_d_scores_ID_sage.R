# David Klinges
# File creation date: 2019.03.13
# This script conducts Kruskal-Wallis for overlap across elevation for Idaho 
#   sage brush sites


## Prep workspace #############
library(tidyverse)

# Read in current dataset
data <- read_csv("data/01_primary/temperate/ID_sage_brush/derivative/ID_wide.csv")

# Set leaf on start and end days of year
leaf_on_start <- 150
leaf_on_end <- 280

# Set desired years in time series to analyze
years <- c(2014, 2015, 2017)

# Set current elevation gradient site
site <- "ID sage brush"

# Set microhabitats of interest
micro <- c("soil", "surface")

# Exclude mid elevation temperatures, no need
data <- data %>%
  select(-contains("mid"))

final_file_path <- "data/04_analysis/compare_overlap_seasonality/ID_sage_kruskal_outputs.csv"

source("./scripts/analysis/compare_overlap_seasonality/conduct_kruskal_overlap.R")
conduct_Kruskal_overlap(data, leaf_on_start, leaf_on_end,
                        years, site, micro, final_file_path)


