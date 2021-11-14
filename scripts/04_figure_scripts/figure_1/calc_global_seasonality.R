## David Klinges
## 2029-09-24
# This script produces a background layer of global seasonality for figure 1


## 1. Workspace prep ####################

library(tidyverse)
library(raster)
library(rgdal)
library(grid)
library(gridExtra)
library(extrafont)

warm <- raster("data/spatial/wc2.0_bio_10m_05.tif")
cold <- raster("data/spatial/wc2.0_bio_10m_06.tif")

sites <- read_csv("figures/tables/site_table.csv")

mountains <- read_csv("data/03_compiled/mountains.csv")

## 2. Data curation ########################

# Raster math
seasonal <- warm - cold

# Generate a subset to experiment plotting with
extent <- as(extent(-16, -7.25, 4, 12.75), 'SpatialPolygons')
crs(extent) <- "+proj=longlat +datum=WGS84 +no_defs"
seasonal_crop <- crop(seasonal, extent)

seasonal_df <- as.data.frame(seasonal, xy = TRUE)
seasonal_crop_df <- as.data.frame(seasonal_crop, xy = TRUE)

# We're not interested in Antarctica
seasonal_df <- seasonal_df %>% 
  filter(y > -58)

# Curate site coordinates
site_coords <- sites %>% 
  dplyr::rename(site = site_code) %>% 
  group_by(site) %>% 
  summarize_at(vars(Longitude, Latitude), ~ mean(., na.rm = TRUE)) %>% 
  dplyr::rename(x = Longitude, y = Latitude)

mountains_summarized <- mountains %>% 
  dplyr::select(site, macro, year) %>% 
  # Determine number of years of data
  group_by(site, macro) %>% 
  distinct() %>% 
  count(name = "n_years")

site_coords <- site_coords %>% 
  full_join(mountains_summarized) %>% 
  # Group macros into forest and non-forest
  mutate(macro = ifelse(macro %in% c("Dense coniferous", "Ponderosa pine", 
                                     "deciduous", "tropical broadleaf", 
                                     "degraded tropical broadleaf"),
                        "forest", "non-forest"))

## Separate points into those that need jitter and those that don't

# Find distances between coords
coords_dist <- dist(site_coords, method = "euclidean")
# Cluster together
coord_clusts <- hclust(coords_dist, method = "average")
# Cut clusters by those that are within 2 degrees of each other
coord_clusts <- cutree(coord_clusts, h = 5)

# Count number of points in each cluster
site_coords <- site_coords %>% 
  mutate(cluster = as.factor(coord_clusts))

cluster_counts <- site_coords %>% 
  group_by(cluster) %>% 
  count()

# For those points in a cluster with more than one point, flag as jitter
site_coords <- site_coords %>% 
  left_join(cluster_counts) %>% 
  mutate(jitter = ifelse(n > 1, "yes", "no"))

## 3. Plot map ###############


plot(warm)
plot(cold)
plot(seasonal)

# Load fonts
loadfonts(device = "win")

windowsFonts()

global_seasonality <- ggplot(seasonal_df, aes(x, y)) +
  geom_raster(aes(fill = layer)) +
  scale_fill_gradientn(colors = c( "#f1f1f1", "#1a1a1a"), 
                       na.value = NA) +
  # scale_fill_gradientn(colors = c("#3399ff", "#B4FFA4", "#cc3300"), 
  #                      na.value="white") +
  # Plot points that don't need jitter
  geom_point(data = filter(site_coords, jitter == "no"), aes(x, y, size = n_years)) +
  # Plot points that do need jitter
  geom_jitter(data = filter(site_coords, jitter == "yes"), aes(x, y, size = n_years), 
              width = 1, height = 1) +
  # guides(size = FALSE) +
  labs(fill = "Annual\nSeasonality",
       size = "Number\nof Years") +
  theme_minimal() +
  theme(text = element_text(size = 16, family="Verdana"),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        # panel.border = element_rect(color = "#4d4d4d", size = 2),
        legend.position="right",
        legend.box = "vertical") +
  ylab(NULL) +
  xlab(NULL)

global_seasonality_legend <- grid.arrange(global_seasonality, bottom = grid.text(
  "Annual Seasonality", 
  gp = gpar(fontsize = 16)))

## 4. Write out figure ##############

writeRaster(seasonal, filename = "data/spatial/derivative/seasonality.tiff")

ggsave(plot = global_seasonality, filename = "data/spatial/derivative/seasonality_global.png",
       width = 9.4333, height = 5.66)

ggsave(plot = global_seasonality_legend, filename = "figures/figure_1/seasonality_global.png",
       width = 13.5, height = 5.66)



