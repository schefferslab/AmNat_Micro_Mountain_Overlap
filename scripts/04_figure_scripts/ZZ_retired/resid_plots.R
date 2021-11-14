

## 1. Workspace prep ##############
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(ggnewscale)
library(viridis)
library(grid)
library(gridExtra)
library(e1071)
library(ggdendro)
library(scales)
library(ggpmisc)

mountains_monthly <- read_csv("data/03_compiled/mountains_overlap_monthly_avg.csv",
                              col_types = cols(
                                # year = col_double(),
                                elevation_change = col_double()
                              ))

mountains_daily <- read_csv("data/03_compiled/mountains_overlap_daily_avg.csv",
                            col_types = cols(
                              # year = col_double(),
                              elevation_change = col_double()
                            ))

mountains <- mountains_monthly

## 2. Data curation ##############
## ....A. Add factor flags ######

# dplyr::recode factors to capitalize
mountains <- mountains %>% 
  filter(complete.cases(kde))

## ....A. Add factor flags ######

mountains <- mountains %>%
  mutate(macro = tools::toTitleCase(macro))

macro_factors <- c("Tropical Broadleaf", "Ponderosa Pine", "Dense Coniferous", 
                   "Deciduous", "Oil Palm", "Meadow Near Forest", "Developed", "Alpine Meadow", 
                   "Scrub Shrub", "Hot Desert")

## ....B. Take abs value of latitude #############

mountains <- mountains %>% 
  mutate(latitude_abs = abs(latitude))

## ....C. Rescale overlap ########

mountains <- mountains %>% 
  mutate(kde_elevCorr_rescale = scales::rescale(kde_elevCorr, to = c(0, 1))) %>% 
  mutate(kde_rescale = scales::rescale(kde, to = c(0, 1)))


## Residuals ###############

ggplot(mountains, aes(elevation_change, kde)) +
  geom_point()

elevVeg_lm <- lm(kde ~ elevation_change + veg_structure, data = mountains)
elevVegHeight_lm <- lm(elevVeg_lm$residuals ~ height, data = mountains)
elevVegHeightSnow_lm <- lm(elevVegHeight_lm$residuals ~ snowdepth, data = mountains)
elevVegHeightSnow_lm <- lm(kde_elevCorr ~ latitude + snowdepth + veg_structure + height, data = mountains)


mountains_plot1 <- mountains %>% 
  mutate(kde_elev_fol_resid = elevVeg_lm$residuals) %>% 
  mutate(kde_elev_fol_height_resid = elevVegHeight_lm$residuals) %>% 
  mutate(kde_elev_fol_height_snow_resid = elevVegHeightSnow_lm$residuals) %>%
  mutate(is_forest = ifelse(macro %in% c("Tropical Broadleaf", "Ponderosa Pine",
                                         "Dense Coniferous", "Deciduous", 
                                         "Degraded Tropical Broadleaf"), "Forest",
                            "Non-forest")) %>% 
  group_by(site, micro) %>% 
  mutate(kde_mean = mean(kde_elev_fol_resid, na.rm = TRUE))
  mutate(kde_mean = mean(kde_elev_fol_height_snow_resid, na.rm = TRUE))

ggplot(mountains_plot1, aes(abs(latitude), kde_elev_fol_resid)) +
  geom_point(aes(color = micro)) +
  facet_wrap(~micro)

##  ################

elevLat_lm <- lm(kde_elevCorr ~ latitude, data = mountains)
elevLatSnow_lm <- lm(elevLat_lm$residuals ~ snowdepth, data = mountains)
elevLatSnowFol_lm <- lm(elevLatSnow_lm$residuals ~ veg_structure, data = mountains)

elevLatSnowFol_lm <- lm(kde_elevCorr ~ latitude + snowdepth + veg_structure, data = mountains)

mountains_plot2 <- mountains %>% 
  mutate(kde_elev_lat_resid = elevLat_lm$residuals) %>% 
  mutate(kde_elev_lat_snow_fol_resid = elevLatSnowFol_lm$residuals) %>% 
  group_by(site, micro) %>% 
  mutate(kde_mean = mean(kde_elev_lat_snow_fol_resid, na.rm = TRUE))
  
ggplot(mountains_plot2, aes(snowdepth, kde_elev_lat_resid)) +
    geom_point(aes(color = micro))


# ggplot(mountains_plot2, aes(veg_structure, kde_elev_lat_resid)) +
#   geom_point(aes(color = foliage_cover))

ggplot(mountains_plot2, aes(height, kde_elev_lat_snow_fol_resid)) +
  geom_point(aes(color = micro)) +
  facet_wrap(~micro)

ggplot(filter(mountains_plot1, micro == "soil"), aes(abs(latitude), kde_elev_fol_height_snow_resid)) +
  geom_point(aes(color = micro)) +
  facet_wrap(~micro)

