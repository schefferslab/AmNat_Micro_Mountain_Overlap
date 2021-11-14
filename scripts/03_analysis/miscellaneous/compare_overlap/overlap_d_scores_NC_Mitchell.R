# David Klinges
# File creation date: 2019.05.05
# This script conducts Kruskal-Wallis for overlap across elevation for NC sites 


## Prep workspace #############
library(tidyverse)

# Read in current dataset
off <- read_csv("data/01_primary/temperate/NC_mt_mitchell/derivative/NC_leafoff_wide.csv")
on <- read_csv("data/01_primary/temperate/NC_mt_mitchell/derivative/NC_leafon_wide.csv")

data <- bind_rows(off, on)
# Set leaf on start and end days of year
leaf_on_start <- 150
leaf_on_end <- 280

# Set desired years in time series to analyze
years <- c(2016)

# Set current elevation gradient site
site <- "NC mt mitchell"

# Set microhabitats of interest
micro <- c("soil", "surface", "canopy")

final_file_path <- "data/04_analysis/compare_overlap_seasonality/NC_Kruskal_outputs.csv"

source("./scripts/analysis/compare_overlap_seasonality/conduct_kruskal_overlap.R")
conduct_Kruskal_overlap(data, leaf_on_start, leaf_on_end,
                        years, site, micro, final_file_path)


