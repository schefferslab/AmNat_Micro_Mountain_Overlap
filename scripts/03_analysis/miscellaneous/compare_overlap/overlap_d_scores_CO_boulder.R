# David Klinges
# File creation date: 2019.03.13
# This script conducts Kruskal-Wallis for overlap across elevation for Berkeley 
#   Angelo Reserve forest sites to prep


## Prep workspace #############
library(tidyverse)

# Read in current dataset
current_data <- read.csv("data/01_primary/temperate/CO_boulder/derivative/Boulder_met1-GLV_wide.csv")

# Set leaf on start and end days of year
leaf_on_start <- 150
leaf_on_end <- 280

# Set desired years in time series to analyze
years <- c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)

# Set current elevation gradient site
site <- "boulder"

# Set microhabitats of interest
micro <- c("soil", "surface")

final_file_path <- "./data/04_analysis/Boulder_kruskal_outputs.csv"

source("./scripts/analysis/compare_overlap_seasonality/conduct_kruskal_overlap.R")
conduct_Kruskal_overlap(current_data, leaf_on_start, leaf_on_end,
                                    years, site, micro, final_file_path)


