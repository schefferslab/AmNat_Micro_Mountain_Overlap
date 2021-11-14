## David Klinges
# Data init: 2019-09-02
# This script processes global tree density (Crowether et al 2015)
# and canopy height (ICESat-1) data
# Code snippets sent by Brunno Oliveira, 2019-09-01 (subject "global habitat complexity")

## 1. Workspace prep ###############

library(tidyverse)
library(raster)

# load tree height
tree_height_NW <- raster("data/vegetation/canopy_height/original/sdat_10023_1_20190902_191159982.asc")
tree_height_NE <- raster("data/vegetation/canopy_height/original/sdat_10023_1_20190902_191339657.asc")
tree_height_SE <- raster("data/vegetation/canopy_height/original/sdat_10023_1_20190902_191423576.asc")
tree_height_SW <- raster("data/vegetation/canopy_height/original/sdat_10023_1_20190902_191431012.asc")

# load tree density
tree_density_raw <- raster("data/vegetation/tree_density/raster/Crowther_Nature_Biome_Revision_01_WGS84_GeoTiff.tif")

# Load site coordinates
coords_raw <- read_csv("figures/tables/site_table.csv")

## 2. Data curation ############

## ....A. Tree height raster ###########

# Overwrite default NA value in tree heights (which were set to -Inf upon import)
NAvalue(tree_height_NW) <- -Inf
NAvalue(tree_height_NE) <- -Inf
NAvalue(tree_height_SE) <- -Inf
NAvalue(tree_height_SW) <- -Inf

# Merge together rasters
# time_allAtOnce <- system.time({
#   tree_height_allAtonce <- merge(tree_height_NW, tree_height_NE, tree_height_SE, tree_height_SW)
# })
# time_allAtOnce
# 
# time_iterated <- system.time({
# tree_height_north <- merge(tree_height_NW, tree_height_NE)
# tree_height_south <- merge(tree_height_SW, tree_height_SE)
# tree_height_iterated <- merge(tree_height_north, tree_height_south)
# })
# time_iterated
# 
# # Write out merged file
# writeRaster(time_allAtOnce, "data/vegetation/canopy_height/global_coverage/global_canopy_height_atonce",
#             format = "GTiff")
# writeRaster(tree_height_iterated, "data/vegetation/canopy_height/global_coverage/global_canopy_height_iterated",
#             format = "GTiff")

## ....B. Prep site coordinates ###########

site_coords <- coords_raw %>% 
  dplyr::select(site_code, elevation, Latitude, Longitude) %>% 
  dplyr::rename(site = site_code, latitude = Latitude, longitude = Longitude)
  
# Now have a DF of just the coords themselves, required for raster::extract()
coords <- site_coords %>% 
  dplyr::select(longitude, latitude)

## 3. Extract tree height ##############

tree_heights <- coords %>%
  # Extract out of one of the four quadrats contingent on coordinates
  mutate(tree_height = ifelse(latitude > 0 & longitude < 0,
    raster::extract(tree_height_NW, coords, method = 'simple'),
    ifelse(latitude > 0 & longitude > 0,
           raster::extract(tree_height_NE, coords, method = 'simple'),
           ifelse(latitude < 0 & longitude < 0,
                  raster::extract(tree_height_SW, coords, method = 'simple'),
                  ifelse(latitude < 0 & longitude > 0,
                         raster::extract(tree_height_SE, coords, method = 'simple'),
                         "failed to extract"
           ))))) %>% 
  left_join(site_coords) %>% 
  distinct()

## 4. Extract tree density ################

tree_density <- coords %>%
  mutate(tree_density = raster::extract(tree_density_raw, coords, method='simple')) %>% 
  left_join(site_coords) %>% 
  distinct()

## 5. Get composite metric of vegetation structure ############

sites_veg <- tree_heights %>% 
  full_join(tree_density) %>% 
  mutate(tree_height = as.double(tree_height)) %>% 
  # Dividing so that weighed equally with height
  mutate(veg_structure = tree_height * (tree_density / 1000)) %>% 
  # If there's any NA values, replace with tree height (which on first run has
  # no NAs), then replace with tree density...janky but point right now is there
  # are some sites with NA tree density, and 0 tree height...which should therefore
  # have 0 veg structure
  mutate(veg_structure = ifelse(is.na(veg_structure), tree_height, veg_structure)) %>% 
  mutate(veg_structure = ifelse(is.na(veg_structure), tree_density, veg_structure))

## 5. Write out files ############

write_csv(tree_heights, "data/vegetation/derivative/tree_height.csv")
write_csv(tree_density, "data/vegetation/derivative/tree_density.csv")
write_csv(sites_veg, "data/vegetation/derivative/veg_structure.csv")



