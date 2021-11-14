# David Klinges
# 2020-01-04
# This script extracts site coordinates from a snowdepth raster (daily bands)
# and returns into a data frame

# Not sure why but I could not transform points from WGS84 to the appropriate
# projection (which I believe is WGS84 NSIDC Sea Ice Polar Stereographic North,
# EPSG = 3413, but perhaps not). spTransform says that points are not finite.

# So instead just transform manually here, which appears to yield the correct
# points:
# https://www.pgc.umn.edu/apps/convert/

# snowdepth_band <- raster(paste0("data/spatial/snowdepth/cmc_analysis_", 
#                                   as.character(year), ".tif"))
# 
# snowdepth_bandtest <- raster(paste0("data/spatial/snowdepth/cmc_analysis_2018_newProjection.tif"))
# 
# snowdepth_band <- projectRaster(snowdepth_band, crs = crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"), 
#                                   method = "bilinear")
# writeRaster(snowdepth_band, "data/spatial/snowdepth/cmc_analysis_2018_newProjection.tif")


# x_coord <- 5300000
# y_coord <- 5240000
# snow_crop <- crop(snowdepth_band_i, crop_extent)
# 
# crop_extent <- as(extent(5000000, 5500000, 5000000, 5500000), 'SpatialPolygons')
# snow_crop <- crop(snowdepth_band_i, crop_extent)
# points(c(5310000), c(5240000))

# ## 2018 ################
# snowdepth_project_raster_list <- rep(list(NA),365)
# 
# for (i in 1:365) {
#   
#   snowdepth_band_i <- raster(paste0("data/spatial/snowdepth/cmc_analysis_2018.tif"), 
#                              band = i)
#   
#   snowdepth_band_i <- projectRaster(snowdepth_band_i, crs = crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"), 
#                                     method = "bilinear")
#   
#   snowdepth_project_raster_list[[i]] <- snowdepth_band_i
#   
#   print(paste0("Project raster for day ", i))
#   
# }
# 
# raster_stack <- stack(snowdepth_project_raster_list)
# 
# writeRaster(raster_stack, "data/spatial/snowdepth/derivative/2018/cmc_analysis_2018_newProjection.tif")
# 
# writeRaster(raster_stack, filename = paste0("data/spatial/snowdepth/derivative/2018/", names(raster_stack)), bylayer=TRUE,format="GTiff")
# 
# ## 2000 #########
# 
# snowdepth_project_raster_list2000 <- rep(list(NA),365)
# 
# for (i in 1:365) {
#   
#   snowdepth_band_i <- raster(paste0("data/spatial/snowdepth/cmc_analysis_2000.tif"), 
#                              band = i)
#   
#   snowdepth_band_i <- projectRaster(snowdepth_band_i, crs = crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"), 
#                                     method = "bilinear")
#   
#   snowdepth_project_raster_list2000[[i]] <- snowdepth_band_i
#   
#   print(paste0("Project raster for day ", i))
#   
# }
# 
# raster_stack_2000 <- do.call(merge, snowdepth_project_raster_list2000)
# 
# ## 2001 ##########
# 
# snowdepth_project_raster_list2001 <- rep(list(NA),365)
# 
# for (i in 1:365) {
#   
#   snowdepth_band_i <- raster(paste0("data/spatial/snowdepth/cmc_analysis_2001.tif"), 
#                              band = i)
#   
#   snowdepth_band_i <- projectRaster(snowdepth_band_i, crs = crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"), 
#                                     method = "bilinear")
#   
#   snowdepth_project_raster_list2000[[i]] <- snowdepth_band_i
#   
#   print(paste0("Project raster for day ", i))
#   
# }
# 
# raster_stack_2001 <- do.call(merge, snowdepth_project_raster_list2001)
# 
# ## 2002 #############
# 
# snowdepth_project_raster_list2002 <- rep(list(NA),365)
# 
# for (i in 1:365) {
#   
#   snowdepth_band_i <- raster(paste0("data/spatial/snowdepth/cmc_analysis_2002.tif"), 
#                              band = i)
#   
#   snowdepth_band_i <- projectRaster(snowdepth_band_i, crs = crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"), 
#                                     method = "bilinear")
#   
#   snowdepth_project_raster_list2000[[i]] <- snowdepth_band_i
#   
#   print(paste0("Project raster for day ", i))
#   
# }
# 
# raster_stack_2002 <- do.call(merge, snowdepth_project_raster_list2002)
# 

# ## Exract snowdepth #############
# 
# extract_snowdepth <- function(x_coord, y_coord, year) {
#   
#   
#   for (i in 1:365) {
#     
#     snowdepth_band_i <- raster(paste0("data/spatial/snowdepth/cmc_analysis_", 
#                                       as.character(year), ".tif"), 
#                                band = i)
#     
#     snowdepth_band_i <- projectRaster(snowdepth_band_i, crs = crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"), 
#                                      method = "bilinear")
#     
#     site_coords <- SpatialPoints(coords = data.frame(x = x_coord, y = y_coord), 
#                                   proj4string = crs(proj4string(snowdepth_band_i)))
#     
#     site_snowdepth_i <- data.frame(
#       year = as.double(year),
#       julian = i,
#       snowdepth = extract(snowdepth_band_i, site_coords))
#     
#     if (i == 1) {
#       site_snowdepth <- site_snowdepth_i
#     } else {
#       site_snowdepth <- bind_rows(site_snowdepth, site_snowdepth_i)
#     }
#     
#     print(paste0("...extracted snow depth for day ", i, " of year ", year, ": ",
#                  site_snowdepth_i$snowdepth))
#   }
#   
#   return(site_snowdepth)
# }
# 
# 

## Exract snowdepth 2 #############

extract_snowdepth <- function(x_coord, y_coord, year) {
  
  
  for (i in 1:365) {
    
    snowdepth_band_i <- snowdepth_project_raster_list2000[[i]]
    
    if (class(snowdepth_band_i) == "RasterLayer") {
      
      site_coords <- SpatialPoints(coords = data.frame(x = x_coord, y = y_coord), 
                                   proj4string = crs(proj4string(snowdepth_band_i)))
      
      site_snowdepth_i <- data.frame(
        year = as.double(year),
        julian = i,
        snowdepth = extract(snowdepth_band_i, site_coords))
      
    } else {
      site_snowdepth_i <- data.frame(
        year = as.double(year),
        julian = i,
        snowdepth = NA)
    }

    
    if (i == 1) {
      site_snowdepth <- site_snowdepth_i
    } else {
      site_snowdepth <- bind_rows(site_snowdepth, site_snowdepth_i)
    }
    
    print(paste0("...extracted snow depth for day ", i, " of year ", year, ": ",
                 site_snowdepth_i$snowdepth))
  }
  
  return(site_snowdepth)
}

