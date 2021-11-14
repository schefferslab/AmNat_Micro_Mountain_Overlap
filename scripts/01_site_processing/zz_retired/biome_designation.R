# David Klinges
# 2019-12-16
# This script extracts terrestrial ecoregions from the TNC global biome designations
# Used data in a folder called "terr-ecoregions-TNC" but I deleted from repo
# because not used in project

## 1. Workspace prep ############

library(tidyverse)
library(rgdal)
library(rgeos)
library(sp)

biomes <- readOGR("data/spatial/terr-ecoregions-TNC",
                  layer = "tnc_terr_ecoregions")

coords_raw <- read_csv("figures/tables/site_table.csv")


## 2. Spatial extraction ##########

site_coords <- coords_raw %>% 
  dplyr::select(site_code, elevation, Latitude, Longitude) %>% 
  dplyr::rename(site = site_code, latitude = Latitude, longitude = Longitude)

# Now have a DF of just the coords themselves, required for raster::extract()
coords <- site_coords %>% 
  dplyr::select(longitude, latitude)

coords <- SpatialPoints(coords, 
                     proj4string = CRS("+proj=longlat +datum=WGS84"))

## 3. Extract tree height ##############

site_biomes <- coords %>%
  # Extract out of one of the four quadrats contingent on coordinates
  mutate(biome = gContains(coords, biomes)) %>% 
  left_join(site_coords) %>% 
  distinct()

biomes_raster <- as.raster(biomes)





