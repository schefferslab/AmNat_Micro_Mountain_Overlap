# David Klinges
# 2019-09-16
# This script generates plots to compare Janzen's hypothesis (overlap ~ latitude)
# to our nested hypthesis (overlap ~ micro and foliage)

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

mountains <- read_csv("data/03_compiled/mountains_overlap_monthly_avg.csv",
                      col_types = cols(
                        # year = col_double(),
                        elevation_change = col_double()
                      ))

KDE_overlap <- read_csv("data/04_analysis/compare_density_distribs/KDE_overlap_monthly.csv")

## 2. Curate data ########

## Join KDE overlap ###############

mountains <- mountains %>% 
  left_join(KDE_overlap)

## Rescale vegetation structure ###################

mountains <- mountains %>% 
  mutate(veg_structure = scales::rescale(veg_structure, to = c(0, 1)))

## ....A. Add factor flags ######

# Recode factors to capitalize
mountains <- mountains %>%
  mutate(micro = tools::toTitleCase(micro)) %>% 
  mutate(macro = tools::toTitleCase(macro))

macro_factors <- c("Tropical Broadleaf", "Ponderosa Pine", "Dense Coniferous", 
                   "Deciduous", "Oil Palm", "Meadow Near Forest", "Developed", "Alpine Meadow", 
                   "Scrub Shrub", "Hot Desert")

micro_factors <- c("Canopy", "Surface", "Soil")

seasonal_factors <- c("leaf-on and snow", "leaf-on and no snow",
                      "leaf-off and snow", "leaf-off and no snow")

mountains <- mountains %>%
  mutate(micro = factor(micro, levels = micro_factors)) %>% 
  mutate(seasonal_attributes = paste(foliage, snow_presence, sep = " and ")) %>% 
  mutate(seasonal_attributes = factor(seasonal_attributes, levels = seasonal_factors))


## ....B. Remove biomes hard to fit in this framework ########

mountains <- mountains %>% 
  # Removing meadows near forest, which are recorded as having high 
  # veg structure due to low resolution of canopy height/tree density raters
  filter(macro != "meadow near forest")


## ....C. Calculate mean overlap values ############


TAD_micro_means <- mountains %>% 
  group_by(micro) %>% 
  summarize(TAD_mean = mean(TAD_elevCorr))

dscore_micro_means <- mountains %>% 
  group_by(micro) %>% 
  summarize(JanzenDscore_mean = mean(janzenDscore_elevCorr))

KDE_micro_means <- mountains %>% 
  group_by(micro) %>% 
  summarize(overlap_mean = mean(overlap_elevCorr))

micro_means <- TAD_micro_means %>% 
  full_join(dscore_micro_means) %>% 
  full_join(KDE_micro_means)


## 3. Plot data #########
## ....A. TAD ~ latitude, foliage colors ########

TAD_foliageColor <- ggplot(data = mountains, aes(x = abs(latitude), y = TAD_elevCorr)) +
  scale_color_manual(values = c("leaf-on" = "#307233",
                                "leaf-off" = "#e08626")) +
  geom_point(aes(x = latitude, y = TAD_elevCorr, color = foliage), 
             size = 2.2, alpha = 0.7) +
  # scale_color_viridis(discrete = TRUE, option = "C") +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = c(0, 70), ylim = c(-20, 30), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.text.x = element_text(size=8)) +
  labs(color = "Foliage") +
  # labs(x = "Vegetation Structure Index") +
  xlab("Latitude") +
  ylab("Thermal Absolute Overlap") +
  theme(strip.text.x = element_text(size = 15))

## ....B. TAD ~ veg_structure, micro panels foliage colors ########

TAD_byMicro <- ggplot(data = mountains, aes(x = veg_structure, y = TAD_elevCorr)) +
  scale_color_manual(values = c("leaf-on" = "#307233",
                                "leaf-off" = "#e08626")) +
  geom_point(aes(x = veg_structure, y = TAD_elevCorr, color = foliage), size = 2.2, alpha = 0.7) +
  # scale_color_viridis(discrete = TRUE, option = "C") +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = c(-0.1, 1.1), ylim = c(-20, 30), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(size=8)) +
  labs(color = "Foliage") +
  # labs(x = "Vegetation Structure Index") +
  xlab("Vegetation Structure") +
  ylab("Thermal Absolute Overlap") +
  geom_text(data = micro_means,
            aes(x = 0.6, y = 22.5,
                label = paste("mean overlap: \n",
                              round(TAD_mean, digits = 2), sep = " "))) +
  theme(axis.text.x = element_text(size = 12),
        strip.text.x = element_text(size = 20)) +
  facet_wrap(~micro) 

## ....E. Combine two panels ########


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(TAD_foliageColor)

TAD_onepanel <- TAD_foliageColor +
  ylab(NULL) +
  theme(legend.position = "none")

TAD_byMicro_onepanel <- TAD_byMicro +
  ylab(NULL) +
  theme(legend.position = "none")

two_panel <- grid.arrange(TAD_onepanel, TAD_byMicro_onepanel, nrow = 2)
two_panel_legend <- grid.arrange(two_panel, legend, ncol = 2, 
                                 widths = c(5, 1),
                                 left = grid.text(
                                   "Thermal Absolute Overlap", 
                                   gp = gpar(fontsize = 20), rot = 90))
## 4. Write out plots ##########

ggsave(plot = TAD_foliageColor, "figures/latitude/latitude_TAD_foliageColor.png")
ggsave(plot = TAD_byMicro_foliageColor, "figures/latitude/latitude_TAD_byMicro_foliageColor.png")

ggsave(plot = TAD_microColor, "figures/latitude/latitude_TAD_microColor.png")
ggsave(plot = TAD_byMicro_microColor, "figures/latitude/latitude_TAD_byMicro_microColor.png")

ggsave(plot = two_panel_foliageColor, "figures/latitude/latitude_TAD_twopanel_foliageColor.png")
ggsave(plot = two_panel_microColor, "figures/latitude/latitude_TAD_twopanel_microColor.png")
