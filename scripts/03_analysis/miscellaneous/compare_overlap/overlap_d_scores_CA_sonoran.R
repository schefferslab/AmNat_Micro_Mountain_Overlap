# David Klinges
# This script conducts Kruskal-Wallis tests of d-scores from 'leaf-on' vs 'leaf-off'
#   for Southern California desert sites
# D-scores compare thermal distribution of microhabitats at high elevation vs low elevation
# Dependant on objects created in "ameriflux_prep.R"


## Prep workspace #############
library(tidyverse)

# Read in current dataset
data <- read_csv("./data/01_primary/temperate/CA_sonoran_desert/derivative/sonoran_desert_wide.csv")

# Set leaf on start and end days of year
leaf_on_start <- 150
leaf_on_end <- 280

# Set desired years in time series to analyze
years <- c(2009:2011)

# Set current elevation gradient site
site <- "sonoran"

# Set microhabitats of interest
micro <- c("soil", "surface")

final_file_path <- "./data/04_analysis/compare_overlap_seasonality/socal_kruskal_outputs2.csv"

source("./scripts/analysis/compare_overlap_seasonality/conduct_kruskal_overlap.R")
conduct_Kruskal_overlap(data, leaf_on_start, leaf_on_end,
                        years, site, micro, final_file_path)
