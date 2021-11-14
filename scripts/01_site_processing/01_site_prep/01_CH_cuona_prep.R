# David Klinges
# 2019-10-14
# This script curates thermal data from the Cuona Mountains, submitted by
# Yiming Hu, for analysis and generating figures

## 1. Workspace prep ############

library(tidyverse)
library(lubridate)
library(readxl)
library(raster)
library(sp)
library(sf)

# Using a mid-elevation set because lower elevations have a much different 
# biome than high elevations
cuona_low_raw <- read_excel("data/01_primary/temperate/CH_Cuona/original/3200.xlsx",
                        skip = 1)

cuona_high_raw <- read_excel("data/01_primary/temperate/CH_Cuona/original/4700.xlsx",
                         skip = 1)

## 2. Data curation #############

cuona_low <- cuona_low_raw %>% 
  dplyr::rename(datetime = "time，GMT +0800", surface = "Tem, °C") %>% 
  mutate(year = year(datetime)) %>% 
  mutate(julian = yday(datetime)) %>% 
  dplyr::select(year, julian, surface)

cuona_high <- cuona_high_raw %>% 
  dplyr::rename(datetime = "time，GMT +0800", surface = "Tem, °C") %>% 
  mutate(year = year(datetime)) %>% 
  mutate(julian = yday(datetime)) %>% 
  dplyr::select(year, julian, surface)


## 3. Snowdepth curation ###########

source("scripts/00_source_code/extract_snowdepth.R")

## ....A. High elevation snowdepth ##########

cuona_high_snow <- extract_snowdepth(x_coord = 5300000, 
                                     y_coord = 5240000,
                                     year = 2018)

cuona_high_snow <- cuona_high_snow %>% 
  mutate(elevation = "high") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

cuona_high_snow_avg <- cuona_high_snow %>% 
  group_by(julian, elevation) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE))

## ....B. Low elevation snowdepth ##########

cuona_low_snow <- extract_snowdepth(x_coord = 5122100.361222, 
                                    y_coord = 5431640.932797,
                                    year = 2018)

cuona_low_snow <- cuona_low_snow %>% 
  mutate(elevation = "low") %>% 
  dplyr::select(year, julian, snowdepth, elevation)

cuona_low_snow_avg <- cuona_low_snow %>% 
  group_by(julian, elevation) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE))

## ....C. Bind together high and low ##########

snowdepth <- cuona_high_snow %>%
  bind_rows(cuona_low_snow)

snowdepth_avg <- cuona_high_snow_avg %>%
  bind_rows(cuona_low_snow_avg)

## 4. Data curation program ###########

source("scripts/00_source_code/data_curation_program.R")
cuona_list <- prep_flux_data(low_dataset = cuona_low,
                                high_dataset = cuona_high)

for (i in 1:length(cuona_list)) {
  
  cuona_list[[i]] <- cuona_list[[i]] %>% 
    mutate(site = "CH_cuona") %>% 
    mutate(macro = "scrub shrub") %>% 
    mutate(foliage = "leaf-off") %>% 
    mutate(foliage_cover = 0) %>% 
    mutate(flora = "NEED TO LOOK UP") %>% 
    mutate(latitude = 27.88)
  
  if (i == 3 | i == 5) {
    
    cuona_list[[i]] <- cuona_list[[i]] %>% 
      mutate(height = dplyr::recode(micro, surface = 1.25)) %>% 
      # Yiming says between 1 and 1.5m above the ground
      mutate(height_notes = "from metadata") %>% 
      # Elevations are averages of the four sensors used at both low and high
      mutate(altitude = dplyr::recode(elevation, "low" = 3200, "high" = 4700))
  }
  # Add snow data
  if (i == 3) {
    # For all years
    cuona_list[[i]] <- cuona_list[[i]] %>% 
      left_join(snowdepth) %>% 
      mutate(snow_source_flag = "measured daily")
  }
  if (i == 5) {
    # For avg years
    cuona_list[[i]] <- cuona_list[[i]] %>% 
      left_join(snowdepth_avg) %>% 
      mutate(snow_source_flag = "measured daily") %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth), 0, 
                                snowdepth))
    
  }
}

## 5. Write out data #########

write_csv(cuona_list[[1]], "./data/01_primary/temperate/CH_cuona//derivative/CH_cuona_high_elev.csv")
write_csv(cuona_list[[2]], "./data/01_primary/temperate/CH_cuona/derivative/CH_cuona_low_elev.csv")
write_csv(cuona_list[[3]], "./data/01_primary/temperate/CH_cuona/derivative/CH_cuona_tall.csv")
write_csv(cuona_list[[4]], "./data/01_primary/temperate/CH_cuona/derivative/CH_cuona_wide.csv")
write_csv(cuona_list[[5]], "./data/01_primary/temperate/CH_cuona/derivative/CH_cuona_avgyears_tall.csv")
write_csv(cuona_list[[6]], "./data/01_primary/temperate/CH_cuona/derivative/CH_cuona_avgyears_wide.csv")


## RECYCLING BIN #########

## Scrapped snowdepth reprojection/extraction/etc #########

# # Re-project snowdepth raster
# proj4string(snowdepth) <- stereo_project
# projection(snowdepth) <- crs("+proj=longlat +datum=WGS84")
# 
# proj4string(a)<-CRS(stere) 
# 
# spatial_coords <- st_multipoint(coords)
# 
# spatial_coords <- sf::st_transform(spatial_coords, crs = 3413) 
# 
# 
# stereo_project <- sp::CRS("+proj=stere +lat_0=90 +lat_ts=60 +lon_0=10 +k=1 +x_0=0 +y_0=0 +a=6371200 +b=6371200 +units=m +no_defs")
# projection(spatial_coords) <- stereo_project
# 
# spatial_coords <- SpatialPoints(coords = coords, proj4string = crs("+proj=longlat +datum=WGS84 +ellps=WGS84"))
# 
# spatial_coords <- spTransform(spatial_coords, CRSobj = crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
# 
# 
# snowdepthtest <- spTransform(snowdepth, crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
# 
# plot(snowdepth)
# points(spatial_coords)
# 
# wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
# snowdepth_wgs84 <- projectRaster(snowdepth, crs = crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"), method = "bilinear")
# 
# latlongpoints <- read_csv("data/spatial/snowdepth/latlongsnowcoords.csv")
# latlongpoints <- latlongpoints %>% 
#   filter_all(complete.cases) %>% 
#   mutate(rownumber = I * J)
# 
# snowdepth_df <- raster::as.data.frame(snowdepth, xy = TRUE)
# 
# snowdepth_df <- snowdepth_df %>% 
#   rownames_to_column("rownumber") %>% 
#   mutate(rownumber = as.double(rownumber))
# 
# joined_test <- latlongpoints %>% 
#   full_join(snowdepth_df)
# 
# cuona_test <- joined_test %>% 
#   filter(lat > 27.7 & lat < 27.8, long > 91.6 & long < 91.7)
# 
# # We want to change to UTM, so that we can designate a buffer in meter units later
# snowdepth_wgs84 <- projectRaster(snowdepth, res = 20000, crs = crs("+proj=longlat +datum=WGS84"))
# 
# st_transform()
# 
# # Subset snowdepth raster to coordinates
# snowdepth_band <- raster::raster(snowdepth, band = 1)
# 
# latlongpoints <- read_csv("data/spatial/snowdepth/latlongsnowcoords.csv")
# latlongpoints <- latlongpoints %>% 
#   filter_all(complete.cases) %>% 
#   mutate(rownumber = I * J)
# 
# crop_extent <- as(extent(6500000, 7500000, -1700000, -400000), 'SpatialPolygons')
# snow_crop <- crop(snowdepth_band_i, crop_extent)
# plot(snow_crop)
# points(c(5431640.932797), c(5122100.361222))
# 
# for (i in 1:snowdepth@file@nbands) {
#   snowdepth_band_i <- raster("data/spatial/snowdepth/cmc_analysis_2018.tif", 
#                              band = i)
#   
#   cuona_coords <- SpatialPoints(coords = data.frame(x = 7050000, y= -1080000), 
#                                 proj4string = crs(proj4string(snowdepth_band_i)))
#   
#   cuona_snowdepth_i <- data.frame(
#     snowdepth = extract(snowdepth_band_i, cuona_coords),
#     julian = i)
#   
#   if (i == 1) {
#     cuoana_snowdepth <- cuona_snowdepth_i
#   } else {
#     cuoana_snowdepth <- bind_rows(cuoana_snowdepth, cuona_snowdepth_i)
#   }
#   # snowdepth_df_i <- raster::as.data.frame(snowdepth_band_i, xy = TRUE)
#   
#   # snowdepth_df_i <- snowdepth_df_i %>% 
#   #   mutate(y_new = -1 * y) %>% 
#   #   arrange(x, y_new) %>% 
#   #   rownames_to_column("rownumber") %>% 
#   #   mutate(rownumber = as.double(rownumber))
#   # 
#   # snow_joined_coords_i <- latlongpoints %>% 
#   #   full_join(snowdepth_df_i)
#   # 
#   # cuona_test <- snow_joined_coords_i %>% 
#   #   filter(lat > 27.7 & lat < 27.8, long > 91.6 & long < 91.7)
#   
# }
# 
#   
#   
#   
