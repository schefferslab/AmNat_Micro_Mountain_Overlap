# David Klinges
# Script init: 2019-09-12
# This script generates plots of latitude vs TAD overlap

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

mountains <- mountains %>% 
  filter(complete.cases(kde))

# dplyr::recode factors to capitalize
mountains <- mountains %>%
  mutate(micro = dplyr::recode(micro, "soil" = "B. Soil", "surface" = "C. Surface",
                        "canopy" = "D. Canopy")) %>% 
  mutate(macro = tools::toTitleCase(macro))

macro_factors <- c("Tropical Broadleaf", "Ponderosa Pine", "Dense Coniferous", 
                   "Deciduous", "Oil Palm", "Meadow Near Forest", "Developed", "Alpine Meadow", 
                   "Scrub Shrub", "Hot Desert")

micro_factors <- c("B. Soil", "C. Surface", "D. Canopy")

seasonal_factors <- c("leaf-on and snow", "leaf-on and no snow",
                      "leaf-off and snow", "leaf-off and no snow")

mountains <- mountains %>%
  mutate(micro = factor(micro, levels = micro_factors)) %>% 
  mutate(seasonal_attributes = paste(foliage, snow_presence, sep = " and ")) %>% 
  mutate(seasonal_attributes = factor(seasonal_attributes, levels = seasonal_factors))

mountains <- mountains %>% 
  mutate(is_forest = ifelse(macro %in% c("Tropical Broadleaf", "Ponderosa Pine",
                                         "Dense Coniferous", "Deciduous", 
                                         "Degraded Tropical Broadleaf"), "Forest",
                            "Non-forest"))

## ....B. Take residuals of mesogeographic parameters #############

mountains <- mountains %>% 
  mutate(veg_foliage = ifelse(foliage_cover > 0, foliage_cover * veg_structure,
                              veg_structure))
  
elevVegSnowHeight_lm <- lm(kde ~ elevation_change + veg_foliage + snowdepth + height, data = mountains)
elevVegSnowLat_lm <- lm(kde ~ elevation_change + veg_foliage + snowdepth + latitude, data = mountains)
elevVeg_lm <- lm(kde ~ elevation_change + veg_foliage, data = mountains)
elevSnow_lm <- lm(kde ~ elevation_change + snowdepth, data = mountains)
elevSnowLat_lm <- lm(kde ~ elevation_change + snowdepth + latitude, data = mountains)
elevLat_lm <- lm(kde ~ elevation_change + latitude, data = mountains)

## ....C. Rescaling ########

mountains <- mountains %>% 
  mutate(kde_resids_rescale = scales::rescale( elevVegSnowHeight_lm$residuals, to = c(0, 1))) %>% 
  mutate(kde_resids_forHeightPlot_rescale = scales::rescale(elevVegSnowLat_lm$residuals, to = c(0, 1))) %>% 
  mutate(kde_rescale = scales::rescale(kde, to = c(0, 1))) %>% 
  mutate(kde_elevCorr_rescale = scales::rescale(kde_elevCorr, to = c(0, 1))) %>% 
  mutate(kde_elevVeg_rescale = scales::rescale(elevVeg_lm$residuals, to = c(0, 1))) %>% 
  mutate(kde_elevSnow_rescale = scales::rescale(elevSnow_lm$residuals, to = c(0, 1))) %>% 
  mutate(kde_elevSnowLat_rescale = scales::rescale(elevSnowLat_lm$residuals, to = c(0, 1))) %>% 
  mutate(kde_elevLat_rescale = scales::rescale(elevLat_lm$residuals, to = c(0, 1))) %>% 
  mutate(veg_structure = scales::rescale(veg_structure, to = c(0, 1)))

## 3. Plot data, TAD #########
## ....A. TAD: regression plots with smooth, one color ########

TAD_oneColor <- ggplot(data = mountains, aes(x = latitude_abs, y = TAD_elevCorr)) +
  geom_point(aes(x = latitude_abs, y = TAD_elevCorr), 
             size = 2.2, alpha = 0.9) +
  geom_smooth(method = "lm", color = "black") +
  # scale_color_manual(values = c("Soil" = "#a285fa",
  #                               "Surface" = "#9dcb9a",
  #                               "Canopy" = "#d98c8e")) +
  coord_cartesian(xlim = c(-2, 70), ylim = c(-20, 30), expand = FALSE) +
  theme_bw(base_size = 14) +
  # labs(x = "Vegetation Structure Index") +
  xlab(NULL) +
  ylab(NULL) +
  # Add equation and r-squared as text
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, label.x = 0.5) + 
  theme(legend.position = "none",
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        plot.title = element_text(size = 18, hjust =  0.5))

## ....B. TAD: regression plots, micro panels micro colors ########

TAD_micro_means <- mountains %>% 
  group_by(micro) %>% 
  summarize(TAD_mean = mean(TAD_elevCorr, na.rm = TRUE))

TAD_byMicro_microColor <- ggplot(data = mountains, aes(x = latitude_abs, y = TAD_elevCorr)) +
  geom_point(aes(x = latitude_abs, y = TAD_elevCorr, color = micro), alpha = 0.9,
            size = 2.2) +
  geom_smooth(method = "lm", aes(color = micro)) +
  scale_color_manual(values = c("B. Soil" = "#a285fa",
                                "C. Surface" = "#9dcb9a",
                                "D. Canopy" = "#d98c8e")) +
  coord_cartesian(xlim = c(-2, 70), ylim = c(-20, 30), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
        axis.text.x = element_text(size=8)) +
  # labs(x = "Vegetation Structure Index") +
  xlab(NULL) +
  ylab(NULL) +
  # geom_text(data = TAD_micro_means,
  #           aes(x = 25, y = 22.5,
  #               label = paste("mean overlap: \n",
  #                             round(TAD_mean, digits = 2), sep = " "))) +
  # Add equation and r-squared as text
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, label.x = 0.5) + 
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 18),
        strip.background = element_blank()) +
  facet_wrap(~micro)

## ....C. Combine two panels ########

TAD_two_panel_microColor <- grid.arrange(TAD_oneColor, TAD_byMicro_microColor, nrow = 2, 
                                     left = grid.text(
                                       "Thermal Overlap", 
                                       gp = gpar(fontsize = 20), rot = 90),
                                     bottom = grid.text(
                                       "Latitude", 
                                       gp = gpar(fontsize = 20)))

## 3. Plot data, KDE, vertically orientated #########
## ....A. KDE: regression plots with smooth, one color ########

KDE_oneColor <- ggplot(data = mountains, aes(x = abs(latitude), y = kde_elevCorr_rescale)) +
  geom_point(aes(x = abs(latitude), y = kde_elevCorr_rescale), 
             size = 2.2, alpha = 0.9) +
  geom_smooth(method = "lm", color = "black") +
  # scale_color_manual(values = c("Soil" = "#a285fa",
  #                               "Surface" = "#9dcb9a",
  #                               "Canopy" = "#d98c8e")) +
  coord_cartesian(xlim = c(-2, 70), ylim = c(0, 1), expand = FALSE) +
  theme_bw(base_size = 14) +
  labs(y = "Thermal Overlap") +
  labs(title = "A. All Vertical Microhabitats") +
  # labs(x = "Vegetation Structure Index") +
  xlab(NULL) +
  # Add equation and r-squared as text
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label..)), 
               parse = TRUE, size = 3.5, label.x = 0.5) + 
  theme(legend.position = "none",
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        plot.title = element_text(size = 18, hjust =  0.5))

## ....B. KDE: regression plots, micro panels micro colors ########

KDE_micro_means <- mountains %>% 
  group_by(micro) %>% 
  summarize(KDE_mean = mean(kde_elevCorr, na.rm = TRUE))

KDE_byMicro_microColor <- ggplot(data = mountains, aes(x = abs(latitude), y = kde_resids_rescale)) +
  geom_point(aes(x = abs(latitude), y = kde_resids_rescale, color = micro), 
             size = 2.2, alpha = 0.9) +
  geom_smooth(method = "lm", aes(color = micro)) +
  scale_color_manual(values = c("B. Soil" = "#a285fa",
                                "C. Surface" = "#9dcb9a",
                                "D. Canopy" = "#d98c8e")) +
  coord_cartesian(xlim = c(-2, 70), ylim = c(0, 1), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.text.x = element_text(size=8)) +
  labs(y = "Residuals") +
  # labs(x = "Vegetation Structure Index") +
  xlab(NULL) +
  # geom_text(data = KDE_micro_means,
  #           aes(x = 25, y = 22.5,
  #               label = paste("mean overlap: \n",
  #                             round(KDE_mean, digits = 2), sep = " "))) +
  # Add equation and r-squared as text
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label..)), 
               parse = TRUE, size = 3.5, label.x = 0.1) + 
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 18),
        strip.background = element_blank()) +
  facet_wrap(~micro, nrow = 1)

## ....C. Combine two panels ########

KDE_two_panel_microColor <- grid.arrange(KDE_oneColor, KDE_byMicro_microColor, 
                                         nrow = 2,
                                     left = grid.text(
                                       "Thermal Overlap", 
                                       gp = gpar(fontsize = 20), rot = 90),
                                     bottom = grid.text(
                                       "Latitude", 
                                       gp = gpar(fontsize = 20)))

## 4. Plot data, KDE #########
## ....A. KDE: regression plots with smooth, one color ########

KDE_oneColor <- ggplot(data = mountains, aes(x = abs(latitude), y = kde_elevCorr_rescale)) +
  geom_point(aes(x = abs(latitude), y = kde_elevCorr_rescale), 
             size = 2.2, alpha = 0.9) +
  geom_smooth(method = "lm", color = "black") +
  # scale_color_manual(values = c("Soil" = "#a285fa",
  #                               "Surface" = "#9dcb9a",
  #                               "Canopy" = "#d98c8e")) +
  coord_cartesian(xlim = c(-2, 70), ylim = c(-.2, 1.2), expand = FALSE) +
  theme_bw(base_size = 14) +
  labs(y = "Thermal Overlap") +
  labs(title = "A. Latitude across all Vertical Microhabitats") +
  # labs(x = "Vegetation Structure Index") +
  ylab(NULL) +
  xlab(NULL) +
  # Add equation and r-squared as text
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),                
               parse = TRUE, size = 4, label.x = 0.5) + 
  theme(legend.position = "none",
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        plot.title = element_text(size = 18, hjust =  0.5))


## ....B. KDE: regression plots: meso: snow, foliage, elevation change ########

KDE_snow <- ggplot(data = mountains, aes(x = snowdepth, y = kde_elevCorr_rescale)) +
  geom_point(aes(x = snowdepth, y = kde_elevCorr_rescale, color = snowdepth), 
             size = 2.2, alpha = 0.9) +
  geom_smooth(method = "lm", color = "#000099", fill = "#000099") +
  scale_color_gradient2(midpoint = 2.5, low = "#e6f7f6", mid = "#00ccff",
                        high = "#000099", space = "Lab" ) +
  # scale_color_manual(values = c("Soil" = "#a285fa",
  #                               "Surface" = "#9dcb9a",
  #                               "Canopy" = "#d98c8e")) +
  coord_cartesian(xlim = c(-2, 90), ylim = c(-.2, 1.2), expand = FALSE) +
  theme_bw(base_size = 14) +
  # labs(color = "Foliage") +
  labs(title = "B. Snowdepth") +
  # labs(x = "Vegetation Structure Index") +
  xlab(NULL) +
  ylab(NULL) +
  # Add equation and r-squared as text
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label..)), 
               parse = TRUE, size = 3.5, label.x = 0.5) + 
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(size = 14, hjust =  0.5))

mountains <- mountains %>% 
  mutate(veg_foliage = veg_structure * foliage_cover)

KDE_foliage <- ggplot(data = mountains, aes(x = veg_foliage, y = kde_elevCorr_rescale)) +
  geom_point(aes(x = veg_foliage, y = kde_elevCorr_rescale, color = foliage_cover), 
             size = 2.2, alpha = 0.9) +
  scale_color_gradient(low = "#e08626", high = "#307233") +
  geom_smooth(method = "lm", color = "#307233", fill = "#307233") +
  # scale_color_manual(values = c("Soil" = "#a285fa",
  #                               "Surface" = "#9dcb9a",
  #                               "Canopy" = "#d98c8e")) +
  coord_cartesian(xlim = c(-.2, 1.2), ylim = c(-.2, 1.2), expand = FALSE) +
  theme_bw(base_size = 14) +
  # labs(color = "Foliage") +
  labs(title = "C. Foliage Structure") +
  # labs(x = "Vegetation Structure Index") +
  xlab(NULL) +
  ylab(NULL) +
  # Add equation and r-squared as text
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label..)), 
               parse = TRUE, size = 3.5, label.x = 0.9) + 
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(size = 14, hjust =  0.5))

KDE_elevchange <- ggplot(data = mountains, aes(x = elevation_change, y = kde_rescale)) +
  geom_point(aes(x = elevation_change, y = kde_rescale, color = elevation_change), 
             size = 2.2, alpha = 0.9) +
  scale_color_gradient(low = "#cc9900", high = "#663300") +
  geom_smooth(method = "lm", color = "#663300", fill = "#663300") +
  # scale_color_manual(values = c("Soil" = "#a285fa",
  #                               "Surface" = "#9dcb9a",
  #                               "Canopy" = "#d98c8e")) +
  coord_cartesian(xlim = c(-10, 3300), ylim = c(-.2, 1.2), expand = FALSE) +
  theme_bw(base_size = 14) +
  # labs(color = "Foliage") +
  labs(title = expression(paste("D. ", Delta, " Elevation"))) +
  # labs(x = "Vegetation Structure Index") +
  xlab(NULL) +
  ylab(NULL) +
  # Add equation and r-squared as text
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label..)), 
               parse = TRUE, size = 3.5, label.x = 0.5) + 
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(size = 14, hjust =  0.5))

## ....C. KDE: regression plots, micro panels micro colors ########

KDE_micro_means <- mountains %>% 
  group_by(micro) %>% 
  summarize(KDE_mean = mean(kde_elevCorr, na.rm = TRUE))

# dplyr::recode factors to capitalize
mountains <- mountains %>%
  mutate(micro = dplyr::recode(micro, "B. Soil" = "E. Soil", "C. Surface" = "F. Surface",
                               "D. Canopy" = "G. Canopy")) %>% 
  mutate(macro = tools::toTitleCase(macro))

micro_factors <- c("E. Soil", "F. Surface", "G. Canopy")

mountains <- mountains %>%
  mutate(micro = factor(micro, levels = micro_factors)) 

KDE_byMicro_microColor <- ggplot(data = mountains, aes(x = abs(latitude), y = kde_elevCorr_rescale)) +
  geom_point(aes(x = abs(latitude), y = kde_elevCorr_rescale, color = micro), 
             size = 2.2, alpha = 0.9) +
  geom_smooth(method = "lm", aes(color = micro, fill = micro)) +
  scale_color_manual(values = c("E. Soil" = "#a285fa",
                                "F. Surface" = "#9dcb9a",
                                "G. Canopy" = "#d98c8e")) +
  scale_fill_manual(values = c("E. Soil" = "#a285fa",
                                "F. Surface" = "#9dcb9a",
                                "G. Canopy" = "#d98c8e")) +
  coord_cartesian(xlim = c(-2, 70), ylim = c(-.2, 1.2), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.text.x = element_text(size=8)) +
  labs(y = "") +
  xlab(NULL) +
  # Add equation and r-squared as text
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., sep = "~~~")),                
               parse = TRUE, size = 3.5, label.x = 0.5) + 
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 18),
        strip.background = element_blank()) +
  facet_wrap(~micro, nrow = 1)

## ....D. KDE: regression plots, micro panels, overlap ~ height ########

KDE_micro_means <- mountains %>%
  group_by(micro) %>%
  summarize(KDE_mean = mean(kde_resids_forHeightPlot_rescale, na.rm = TRUE))
  
forest <- mountains %>% 
  filter(is_forest == "Forest")

nonforest <- mountains %>% 
  filter(is_forest == "Non-forest")

test <- mutate(mountains, microforest = paste(micro, is_forest, sep = "_"))

ggplot(data = test, aes(microforest, kde_elevCorr_rescale)) +
  geom_boxplot() +
  geom_jitter()

ggplot(data = test, aes(height, kde_elevCorr_rescale)) +
  geom_point() +
  facet_wrap(~micro+is_forest)


KDE_soil <- ggplot(data = filter(mountains, micro == "B. Soil"),
                          aes(x = height, y = kde_elevSnow_rescale)) +
  geom_point(aes(x = height, y = kde_elevSnow_rescale, color = micro),
             size = 2.2, alpha = 0.9) +
  geom_smooth(method = "lm", aes(color = micro)) +
  scale_color_manual(values = c("B. Soil" = "#a285fa",
                                "C. Surface" = "#9dcb9a",
                                "D. Canopy" = "#d98c8e")) +
  coord_cartesian(ylim = c(0, 1), xlim = c(-.15, 0), expand = FALSE) +
  theme_bw(base_size = 14) +
  labs(title = "Soil") +
  theme(legend.position = "none",
        axis.text.x = element_text(size=8)) +
  xlab(NULL) +
  ylab(NULL) +
  # Add equation and r-squared as text
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, size = 3.5, label.x = 0.5) +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 18),
        strip.background = element_blank())

KDE_surface <- ggplot(data = filter(mountains, micro == "C. Surface"),
                             aes(x = height, y = kde_elevCorr_rescale)) +
  geom_point(aes(x = height, y = kde_elevCorr_rescale, color = micro),
             size = 2.2, alpha = 0.9) +
  geom_smooth(method = "lm", aes(color = micro)) +
  scale_color_manual(values = c("B. Soil" = "#a285fa",
                                "C. Surface" = "#9dcb9a",
                                "D. Canopy" = "#d98c8e")) +
  coord_cartesian(xlim = c(0.8, 3.2), ylim = c(0, 1), expand = FALSE) +
  theme_bw(base_size = 14) +
  labs(title = "Surface") +
  theme(legend.position = "none",
        axis.text.x = element_text(size=8)) +
  xlab(NULL) +
  ylab(NULL) +
  # Add equation and r-squared as text
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, size = 3.5, label.x = 0.5) +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 18),
        strip.background = element_blank())


KDE_canopy <- ggplot(data = filter(mountains, micro == "D. Canopy"),
                            aes(x = height, y = kde_elevCorr_rescale)) +
  geom_point(aes(x = height, y = kde_elevCorr_rescale, color = micro),
             size = 2.2, alpha = 0.9) +
  geom_smooth(method = "lm", aes(color = micro)) +
  scale_color_manual(values = c("B. Soil" = "#a285fa",
                                "C. Surface" = "#9dcb9a",
                                "D. Canopy" = "#d98c8e")) +
  coord_cartesian(xlim = c(11, 22), ylim = c(0, 1), expand = FALSE) +
  theme_bw(base_size = 14) +
  labs(title = "Canopy") +
  theme(legend.position = "none",
        axis.text.x = element_text(size=8)) +
  xlab(NULL) +
  ylab(NULL) +
  # Add equation and r-squared as text
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, size = 3.5, label.x = 0.5) +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 18),
        strip.background = element_blank())

## micro facet plots ############

KDE_soil_forest <- ggplot(data = filter(forest, micro == "B. Soil"),
                          aes(x = height, y = kde_elevCorr_rescale)) +
  geom_point(aes(x = height, y = kde_elevCorr_rescale, color = micro),
             size = 2.2, alpha = 0.9) +
  geom_smooth(method = "lm", aes(color = micro)) +
  scale_color_manual(values = c("B. Soil" = "#a285fa",
                                "C. Surface" = "#9dcb9a",
                                "D. Canopy" = "#d98c8e")) +
  coord_cartesian(ylim = c(0, 1), xlim = c(-.15, 0), expand = FALSE) +
  theme_bw(base_size = 14) +
  labs(title = "Soil") +
  theme(legend.position = "none",
        axis.text.x = element_text(size=8)) +
  xlab(NULL) +
  ylab(NULL) +
  # Add equation and r-squared as text
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, size = 3.5, label.x = 0.5) +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 18),
        strip.background = element_blank())

KDE_surface_forest <- ggplot(data = filter(forest, micro == "C. Surface"),
                             aes(x = height, y = kde_elevCorr_rescale)) +
  geom_point(aes(x = height, y = kde_elevCorr_rescale, color = micro),
             size = 2.2, alpha = 0.9) +
  geom_smooth(method = "lm", aes(color = micro)) +
  scale_color_manual(values = c("B. Soil" = "#a285fa",
                                "C. Surface" = "#9dcb9a",
                                "D. Canopy" = "#d98c8e")) +
  coord_cartesian(xlim = c(0.8, 3.2), ylim = c(0, 1), expand = FALSE) +
  theme_bw(base_size = 14) +
  labs(title = "Surface") +
  theme(legend.position = "none",
        axis.text.x = element_text(size=8)) +
  xlab(NULL) +
  ylab(NULL) +
  # Add equation and r-squared as text
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, size = 3.5, label.x = 0.5) +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 18),
        strip.background = element_blank())


KDE_canopy_forest <- ggplot(data = filter(forest, micro == "D. Canopy"),
                            aes(x = height, y = kde_elevCorr_rescale)) +
  geom_point(aes(x = height, y = kde_elevCorr_rescale, color = micro),
             size = 2.2, alpha = 0.9) +
  geom_smooth(method = "lm", aes(color = micro)) +
  scale_color_manual(values = c("B. Soil" = "#a285fa",
                                "C. Surface" = "#9dcb9a",
                                "D. Canopy" = "#d98c8e")) +
  coord_cartesian(xlim = c(11, 22), ylim = c(0, 1), expand = FALSE) +
  theme_bw(base_size = 14) +
  labs(title = "Canopy") +
  theme(legend.position = "none",
        axis.text.x = element_text(size=8)) +
  xlab(NULL) +
  ylab(NULL) +
  # Add equation and r-squared as text
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, size = 3.5, label.x = 0.5) +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 18),
        strip.background = element_blank())

KDE_soil_nonforest <- ggplot(data = filter(nonforest, micro == "B. Soil"),
                   aes(x = height, y = kde_elevCorr_rescale)) +
  geom_point(aes(x = height, y = kde_elevCorr_rescale, color = micro),
             size = 2.2, alpha = 0.9) +
  geom_smooth(method = "lm", aes(color = micro)) +
  scale_color_manual(values = c("B. Soil" = "#a285fa",
                                "C. Surface" = "#9dcb9a",
                                "D. Canopy" = "#d98c8e")) +
  coord_cartesian(ylim = c(0, 1), xlim = c(-.15, 0), expand = FALSE) +
  theme_bw(base_size = 14) +
  labs(title = "Soil") +
  theme(legend.position = "none",
        axis.text.x = element_text(size=8)) +
  xlab(NULL) +
  ylab(NULL) +
  # Add equation and r-squared as text
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, size = 3.5, label.x = 0.5) +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 18),
        strip.background = element_blank())

KDE_surface_nonforest <- ggplot(data = filter(nonforest, micro == "C. Surface"),
                   aes(x = height, y = kde_elevCorr_rescale)) +
  geom_point(aes(x = height, y = kde_elevCorr_rescale, color = micro),
             size = 2.2, alpha = 0.9) +
  geom_smooth(method = "lm", aes(color = micro)) +
  scale_color_manual(values = c("B. Soil" = "#a285fa",
                                "C. Surface" = "#9dcb9a",
                                "D. Canopy" = "#d98c8e")) +
  coord_cartesian(xlim = c(0.8, 3.2), ylim = c(0, 1), expand = FALSE) +
  theme_bw(base_size = 14) +
  labs(title = "Surface") +
  theme(legend.position = "none",
        axis.text.x = element_text(size=8)) +
  xlab(NULL) +
  ylab(NULL) +
  # Add equation and r-squared as text
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, size = 3.5, label.x = 0.5) +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 18),
        strip.background = element_blank())

test <- mutate(mountains, height = scales::rescale(height, to = c(0, 1)))
KDE_micro <- ggplot(data = test,
                     aes(x = height, y = kde_elevCorr_rescale)) +
  geom_point(aes(x = height, y = kde_elevCorr_rescale, color = micro),
             size = 2.2, alpha = 0.9) +
  # geom_smooth(method = "lm", aes(color = micro)) +
  scale_color_manual(values = c("B. Soil" = "#a285fa",
                                "C. Surface" = "#9dcb9a",
                                "D. Canopy" = "#d98c8e")) +
  coord_cartesian(xlim = c(-.2, 1), ylim = c(0, 1), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.text.x = element_text(size=8)) +
  xlab(NULL) +
  ylab(NULL) +
  # Add equation and r-squared as text
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, size = 3.5, label.x = 0.5) +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 18),
        strip.background = element_blank())

## ###########

mountains <- mountains %>% 
  mutate(is_forest = ifelse(macro %in% c("Tropical Broadleaf", "Ponderosa Pine",
                                         "Dense Coniferous", "Deciduous", 
                                         "Degraded Tropical Broadleaf"), "Forest",
                            "Non-forest")) %>% 
  group_by(site, micro) %>% 
  mutate(kde_mean = mean(kde, na.rm = TRUE)) %>% 
  mutate(kde_elevCorr_mean  = mean(kde_elevCorr, na.rm = TRUE)) %>% 
  mutate(janzenDscore_elevCorr_mean  = mean(janzenDscore_elevCorr, na.rm = TRUE)) %>% 
  ungroup()

ggplot(mountains, aes(elevation_change, janzenDscore)) +
  geom_point()

ggplot(data = mountains, 
                   aes(x = abs(latitude), y = kde_elevCorr_mean)) +
  geom_point(aes(x = abs(latitude), y = kde_elevCorr_mean, color = is_forest), 
             size = 2.2, alpha = 0.9) +
  # geom_smooth(method = "lm", aes(color = is_forest)) +
  facet_wrap(~micro)
  scale_color_manual(values = c("D. Soil" = "#a285fa",
                                "C. Surface" = "#9dcb9a",
                                "B. Canopy" = "#d98c8e")) +
  coord_cartesian(ylim = c(0, 1), expand = FALSE) +
  theme_bw(base_size = 14) +
  labs(title = "E. Soil") +
  theme(legend.position = "none",
        axis.text.x = element_text(size=8)) +
  xlab(NULL) +
  ylab(NULL) +
  # Add equation and r-squared as text
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               parse = TRUE, size = 3.5, label.x = 0.5) +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        strip.text = element_text(size = 18),
        strip.background = element_blank())

## ....C. Combine panels ########

KDE_micros <- grid.arrange(KDE_soil, KDE_surface, KDE_canopy, nrow = 1,
    
                           bottom = grid.text(
                             "Depth Belowground/Height Aboveground", 
                             gp = gpar(fontsize = 20)))
  
KDE_micros <- grid.arrange(KDE_soil_forest, KDE_surface_forest, KDE_canopy_forest, 
                             KDE_soil_nonforest, KDE_surface_nonforest, KDE_canopy_nonforest, 
                             nrow = 2,
                             left = grid.text(
                               "Thermal Overlap (Residuals)", 
                               gp = gpar(fontsize = 20), rot = 90),
                             bottom = grid.text(
                               "Depth Belowground/Height Aboveground", 
                               gp = gpar(fontsize = 20)))
  
  
KDE_mesos <- grid.arrange(KDE_snow, KDE_foliage, KDE_elevchange, ncol = 1)

KDE_four_panel_microColor <- grid.arrange(KDE_oneColor,
                                          KDE_byMicro_microColor, nrow = 2, 
                                          
                                          bottom = grid.text(
                                            "Latitude", 
                                            gp = gpar(fontsize = 20)))

seven_panels <- grid.arrange(KDE_four_panel_microColor, KDE_mesos, ncol = 2, 
                             widths = c(3, 1.5),
                             left = grid.text(
                               "Thermal Overlap", 
                               gp = gpar(fontsize = 20), rot = 90))




KDE_seven_panel_microColor <- grid.arrange(KDE_oneColor, KDE_mesos,
                                          KDE_micro, nrow = 3, 
                                          left = grid.text(
                                            "Thermal Overlap", 
                                            gp = gpar(fontsize = 20), rot = 90),
                                          bottom = grid.text(
                                            "Height of Sensor", 
                                            gp = gpar(fontsize = 20)))

## 4. Write out plots ##########

ggsave(plot = TAD_byMicro_microColor, "figures/latitude/latitude_TAD_byMicro_microColor.png")

ggsave(plot = TAD_two_panel_microColor, 
       "figures/latitude/latitude_TAD_twopanel_microColor.png",
       width = 9.58, height = 7.2)

ggsave(plot = KDE_four_panel_microColor, 
       "figures/latitude/latitude_KDE_fourpanel_microColor.png",
       width = 9.58, height = 7.2)

ggsave(plot = seven_panels, 
       "figures/latitude/seven_panels.png",
       width = 9.58, height = 7.2)

ggsave(plot = KDE_four_panel_microColor, 
       "figures/latitude/KDE_four_panel_microColor.png",
       width = 9.58, height = 7.2)

ggsave(plot = KDE_micros, 
      filename = "figures/latitude/KDE_micros_by_height",
       width = 12, height = 7.2)






