# David Klinges
# File creation date: 2019.03.13
# This script conducts Kruskal-Wallis for overlap across elevation for Berkeley 
#   Angelo Reserve forest sites to prep


## Prep workspace #############
library(tidyverse)

# Read in current dataset
current_data <- read.csv("./data/02_derivative/Catalina_wide.csv")

# Set leaf on start and end days of year
leaf_on_start <- 150
leaf_on_end <- 280

# Set desired years in time series to analyze
years <- c(2011:2018)

# Set current elevation gradient site
site <- "catalina"

# Set microhabitats of interest
micro <- c("soil", "air")

final_file_path <- "./data/04_analysis/Catalina_kruskal_outputs.csv"

source("./scripts/analysis/conduct_kruskal_overlap.R")
conduct_Kruskal_overlap(data, leaf_on_start, leaf_on_end,
                                    years, site, micro, final_file_path)


