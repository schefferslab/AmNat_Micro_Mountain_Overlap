# David Klinges
# File creation date: 2019.06.13
# This script extracts elevations from lat/long points from TN Coweeta LTER

## 1. Workspace prep ############

library(rgdal)
library(raster)
library(tidyverse)

dem <- raster("data/01_primary/temperate/TN_coweeta/original/DEM/cwt_dem/sta.adf")
 

# Load in data
hobo_network <- read_csv("data/01_primary/temperate/TN_coweeta/original/hobo_network/1211_1_0.csv")
site_elevations <- read_csv("data/01_primary/temperate/TN_coweeta/original/site_elevations/site_elevations.csv")

plot(dem)

## 2. Extract elevations ##########

# Reproject DEM
dem <- projectRaster(dem, crs = "+proj=longlat +datum=WGS84")

# Determine coordinates of DEM. This is in case any site coordinates do not
# exactly align with DEM coordinates
spts <- rasterToPoints(dem, spatial = TRUE)


site_elevations <- site_elevations %>% 
  mutate(elevation = NA)
# Change coords to those that are in raster
site_elevations$long[3] <- -83.45827
site_elevations$long[4] <- -83.46225

# List out coordinates of sites of interest
for (i in 1:nrow(site_elevations)) {
  elev <- raster::extract(dem, tibble(long = site_elevations$long[i], 
                      lat = site_elevations$lat[i]))
  site_elevations$elevation[i] <- elev
}

## 3. Export data ############

write_csv(site_elevations, "data/01_primary/temperate/TN_coweeta/intermediate/site_elevations.csv")
