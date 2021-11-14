# David Klinges
# This script generates plot of the magnitude of change in overlap d-score 
#   values for each site

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

mountains <- read_csv("data/03_compiled/mountains_overlap_daily_avg.csv",
                      col_types = cols(
                        # year = col_double(),
                        elevation_change = col_double()
                      ))

# Graphing params
dscore_data <- mountains %>% 
  # Set y axis to plot
  rename(overlap_axis = TAD_elevCorr)

Y_AXIS_RANGE <- c(-25, 25)
Y_AXIS_LABEL <- "Thermal Absolute Distance"

## 2. Curate data #############

## ....A. Change order of data flags ############

# Change levels of factors
# Ordering sites and macros from densest foliage to no foliage
site_factors <- c("OR", "CH_conif", "PH", "CR_basham", "CR_northwest", "CR_southwest",
                  "CO_PrimaryForest", "AU", "CO_OldSecondary", "NM_soil", "NM_surface",
                  "AZ_cata", "NC", "CH_decid", "TN", "MD", "NH", "MY_SAFE", "ID", "AZ", 
                  "SE", "NH_nonforest", "CH_open", "CA", "PT", "CO", "AK_nabesna", 
                  "AK_ameriflux")

macro_factors <- c("tropical\nbroadleaf", "Ponderosa\npine", "Dense\nconiferous", 
                   "deciduous", "meadow\nnear\nforest", "developed", "alpine\nmeadow", 
                   "scrub\nshrub", "hot\ndesert", "oil\npalm")

foliage_factors <- c("leaf-on", "leaf-off")
seasonal_factors <- c("leaf-on\nand snow", "leaf-on\nand no snow",
                      "leaf-off\nand snow", "leaf-off\nand no snow")


# Convert flags to factor
dscore_data <- dscore_data %>%
  mutate(macro =   gsub('\\s', '\n', macro)) %>% 
  mutate(macro = factor(macro, levels = macro_factors)) %>%
  mutate(site = factor(site, levels = site_factors)) %>% 
  mutate(foliage = factor(foliage, levels = foliage_factors)) %>% 
  mutate(snow_presence = ifelse(is.na(snow_presence), "no snow", snow_presence)) %>% 
  mutate(seasonal_attributes = paste(foliage, snow_presence, sep = "\nand ")) %>% 
  mutate(seasonal_attributes = factor(seasonal_attributes, levels = seasonal_factors))
  
## 3. Create figure ###########

## ....A. Boxplots, by site and by micro ###########

soil <- ggplot(data = filter(dscore_data, micro == "soil"), 
               aes(x = site, y = overlap_axis, fill = macro)) +
  geom_boxplot(aes(x = site, y = overlap_axis), alpha = 0.5, lwd = .5) +
  
  # Number of colors correspond to macros present for soil
  scale_fill_manual(values = c("#307233", "#b0db39", "#b0d000", "#6cba3b", 
                               "#d3c250", "#d3c250", "#d3c250", "#d3c250", "#d3c250")) +
  
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8)) +
  ylab(Y_AXIS_LABEL)

surface <- ggplot(data = filter(dscore_data, micro == "surface"), 
                  aes(x = site, y = overlap_axis, fill = macro)) +
  geom_boxplot(aes(x = site, y = overlap_axis), alpha = 0.5, lwd = .5) +
  
  # Number of colors correspond to macros present for soil
  scale_fill_manual(values = c("#307233", "#b0db39", "#b0d000", "#6cba3b", 
                               "#d3c250", "#d3c250", "#d3c250", "#d3c250", "#d3c250")) +
  
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.x = element_text(size = 8)) +
  labs(fill = "Foliage/Snow")
# scale_x_discrete(limits=c("Snow", "Snow", "Leaf-on", "leaf-off", "no foliage"))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)

canopy <- ggplot(data = filter(dscore_data, micro == "canopy"), 
                 aes(x = site, y = overlap_axis, fill = macro)) +
  geom_boxplot(aes(x = site, y = overlap_axis), alpha = 0.5, lwd = .5) +
  
  # Number of colors correspond to macros present for soil
  scale_fill_manual(values = c("#307233", "#b0db39", "#b0d000", "#6cba3b", 
                               "#d3c250", "#d3c250", "#d3c250", "#d3c250", "#d3c250")) +
  
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10),
        axis.text.x = element_text(size = 8))

plots <- grid.arrange(soil, surface + theme(legend.position = 'none'), canopy,
                      legend, 
                      nrow = 1,
                      bottom = grid.text(
                        "Microhabitat", gp = gpar(fontsize = 25)))

## ....B. Violin plots, by micro and by site ###########

soil <- ggplot(data = filter(dscore_data, micro == "soil"), 
               aes(x = site, y = overlap_axis, fill = seasonal_attributes)) +
  geom_violin(aes(x = site, y = overlap_axis), alpha = 0.5, lwd = .5) +
  geom_jitter(aes(x = site, y = overlap_axis), alpha = 0.3) +
  
  # Number of colors correspond to seasonal_attribute levels present for soil
  scale_fill_manual(values = c("#3ee0ac", "#3e9de0", "#3e9de0",
                               "#307233", "#e08626", "#e5d35e")) +
  
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8)) +
  ylab(Y_AXIS_LABEL)

surface <- ggplot(data = filter(dscore_data, micro == "surface"), 
                  aes(x = site, y = overlap_axis, fill = seasonal_attributes)) +
  geom_violin(aes(x = site, y = overlap_axis), alpha = 0.5, lwd = .5) +
  geom_jitter(aes(x = site, y = overlap_axis), alpha = 0.3) +
  
  # Number of colors correspond to seasonal_attribute levels present for surface
  scale_fill_manual(values = c("#3ee0ac", "#3e9de0", "#3e9de0",
                               "#307233", "#e08626", "#e5d35e")) +
  
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.x = element_text(size = 8)) +
  labs(fill = "Foliage/Snow")
# scale_x_discrete(limits=c("Snow", "Snow", "Leaf-on", "leaf-off", "no foliage"))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)

canopy <- ggplot(data = filter(dscore_data, micro == "canopy"), 
                 aes(x = site, y = overlap_axis, fill = seasonal_attributes)) +
  geom_violin(aes(x = site, y = overlap_axis), alpha = 0.5, lwd = .5) +
  geom_jitter(aes(x = site, y = overlap_axis), alpha = 0.3) +
  
  # Number of colors correspond to seasonal_attribute levels present for canopy
  scale_fill_manual(values = c("#3ee0ac", "#3e9de0", "#3e9de0",
                               "#307233", "#e08626", "#e5d35e")) +
  
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10),
        axis.text.x = element_text(size = 8))

plots <- grid.arrange(soil, surface + theme(legend.position = 'none'), canopy,
                      legend, 
                      nrow = 1,
                      bottom = grid.text(
                        "Microhabitat", gp = gpar(fontsize = 25)))


## ....C. Boxplots, by micro and by macro ###########

soil <- ggplot(data = filter(dscore_data, micro == "soil"), aes(x = macro, y = overlap_axis, fill = factor(macro))) +
  scale_fill_manual(values = c("tropical\nbroadleaf" = "#b0db39", 
                                "Dense\nconiferous" = "#307233",
                                "Ponderosa\npine" = "#6cba3b",
                                "deciduous" = "#725100",
                                "alpine\nmeadow" = "#33CCFF", 
                                "meadow\nnear\nforest" = "#CC99FF",
                                "developed" = "#D3D3D3",
                                "scrub\nshrub" = "#d3c250",
                                "hot\ndesert" = "#FFBD00")) +
  geom_boxplot(aes(x = macro, y = overlap_axis, fill = macro), alpha = 0.7, lwd = .5) +
  # scale_color_viridis(discrete = TRUE, option = "C") +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.text.x=element_text(size=8)) +
  labs(x = "Soil") +
  ylab(Y_AXIS_LABEL)

surface <- ggplot(data = filter(dscore_data, micro == "surface"), 
                  aes(x = macro, y = overlap_axis, fill = factor(macro))) +
  scale_fill_manual(values = c("tropical\nbroadleaf" = "#b0db39", 
                               "Dense\nconiferous" = "#307233",
                               "Ponderosa\npine" = "#6cba3b",
                               "deciduous" = "#725100",
                               "alpine\nmeadow" = "#33CCFF", 
                               "meadow\nnear\nforest" = "#CC99FF",
                               "developed" = "#D3D3D3",
                               "scrub\nshrub" = "#d3c250",
                               "hot\ndesert" = "#FFBD00")) +
  geom_boxplot(aes(x = macro, y = overlap_axis, fill = macro), alpha = 0.7, lwd = .5) +
  scale_color_viridis(discrete = TRUE, option = "C") +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(size=8)) +
  labs(x = "Surface")

canopy <- ggplot(data = filter(dscore_data, micro == "canopy"), aes(x = macro, y = overlap_axis, fill = factor(macro))) +
  scale_fill_manual(values = c("tropical\nbroadleaf" = "#b0db39", 
                               "Dense\nconiferous" = "#307233",
                               "Ponderosa\npine" = "#6cba3b",
                               "deciduous" = "#725100",
                               "alpine\nmeadow" = "#33CCFF", 
                               "meadow\nnear\nforest" = "#CC99FF",
                               "developed" = "#D3D3D3",
                               "scrub\nshrub" = "#d3c250",
                               "hot\ndesert" = "#FFBD00"),
  guide = guide_legend(title = "macro")) +
  geom_boxplot(aes(x = macro, y = overlap_axis, fill = macro), alpha = 0.7, lwd = .5) +
  scale_color_viridis(discrete = TRUE, option = "C", guide = FALSE) +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10),
        axis.text.x=element_text(size=8)) +
  # geom_errorbar(aes(x = macro, y = mean, ymin = mean - SE , ymax = mean + SE), 
  #               data = filter(dscore_data_sum, micro == "canopy"), width = 0.2, color = "black") +
  labs(x = "Canopy")

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)


plots <- grid.arrange(soil, surface + theme(legend.position = 'none'), canopy, legend,
                      nrow = 1,
                      bottom = grid.text(
                        "Microhabitat", gp = gpar(fontsize = 25)))

## ....D. Boxplots, by micro, by snow/foliage ###########

soil <- ggplot(data = filter(dscore_data, micro == "soil"), 
               aes(x = seasonal_attributes, y = overlap_axis, fill = seasonal_attributes)) +
  geom_jitter(aes(x = seasonal_attributes, y = overlap_axis, 
                  color = seasonal_attributes), alpha = 0.1, width = .1) +
  geom_boxplot(aes(x = seasonal_attributes, y = overlap_axis, 
                   fill = seasonal_attributes), alpha = 0.7, lwd = .5,
               outlier.shape = NA) +
  scale_fill_manual(values = c("leaf-on\nand snow" = "#3ee0ac", 
                               "leaf-on\nand no snow" = "#307233",
                               "leaf-off\nand snow" = "#3e9de0",
                               "leaf-off\nand no snow" = "#e08626")) +
  scale_color_manual(values = c("leaf-on\nand snow" = "#3ee0ac", 
                               "leaf-on\nand no snow" = "#307233",
                               "leaf-off\nand snow" = "#3e9de0",
                               "leaf-off\nand no snow" = "#e08626")) +
  
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.text.x=element_text(size=8)) +
  labs(x = "Soil") +
  ylab(Y_AXIS_LABEL)

surface <- ggplot(data = filter(dscore_data, micro == "surface"), 
                  aes(x = seasonal_attributes, y = overlap_axis, fill = seasonal_attributes)) +
  geom_jitter(aes(x = seasonal_attributes, y = overlap_axis, 
                  color = seasonal_attributes), alpha = 0.1, width = .1) +
  
  geom_boxplot(aes(x = seasonal_attributes, y = overlap_axis, 
                   fill = seasonal_attributes), alpha = 0.7, lwd = .5,
               outlier.shape = NA) +
  scale_fill_manual(values = c("leaf-on\nand snow" = "#3ee0ac", 
                               "leaf-on\nand no snow" = "#307233",
                               "leaf-off\nand snow" = "#3e9de0",
                               "leaf-off\nand no snow" = "#e08626"),
                    guide = FALSE) +
  scale_color_manual(values = c("leaf-on\nand snow" = "#3ee0ac", 
                                "leaf-on\nand no snow" = "#307233",
                                "leaf-off\nand snow" = "#3e9de0",
                                "leaf-off\nand no snow" = "#e08626"),
                     labels = c("leaf-on and snow", "leaf-on and no snow",
                                "leaf-off and snow", "leaf-off and no snow")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(size=8),
        legend.title = element_blank()) +
  labs(x = "Surface")

canopy <- ggplot(data = filter(dscore_data, micro == "canopy"), 
                 aes(x = seasonal_attributes, y = overlap_axis, fill = seasonal_attributes)) +
  geom_jitter(aes(x = seasonal_attributes, y = overlap_axis, 
                  color = seasonal_attributes), alpha = 0.1, width = .1) +
    geom_boxplot(aes(x = seasonal_attributes, y = overlap_axis, 
                   fill = seasonal_attributes), alpha = 0.7, lwd = .5,
               outlier.shape = NA) +
  scale_fill_manual(values = c("leaf-on\nand snow" = "#3ee0ac", 
                               "leaf-on\nand no snow" = "#307233",
                               "leaf-off\nand snow" = "#3e9de0",
                               "leaf-off\nand no snow" = "#e08626"),
                    guide = guide_legend(title = "macro")) +  
  scale_color_manual(values = c("leaf-on\nand snow" = "#3ee0ac", 
                                "leaf-on\nand no snow" = "#307233",
                                "leaf-off\nand snow" = "#3e9de0",
                                "leaf-off\nand no snow" = "#e08626")) +
  
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10),
        axis.text.x=element_text(size=8)) +
  # geom_errorbar(aes(x = macro, y = mean, ymin = mean - SE , ymax = mean + SE), 
  #               data = filter(dscore_data_sum, micro == "canopy"), width = 0.2, color = "black") +
  labs(x = "Canopy")

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)


plots <- grid.arrange(soil, surface + theme(legend.position = 'none'), canopy, legend,
                      nrow = 1,
                      bottom = grid.text(
                        "Microhabitat", gp = gpar(fontsize = 25)))


## ....E. Boxplots, by macro, micro, and foliage ###########

soil <- ggplot(data = filter(dscore_data, micro == "soil"), 
               aes(x = foliage, y = overlap_axis, fill = factor(macro))) +
  geom_boxplot(aes(x = macro, y = overlap_axis, fill = foliage), alpha = 0.7, lwd = .5) +
  scale_fill_manual(values = c("#307233", "#e08626", "#d3c250")) +
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 6)) +
  labs(x = "Soil")+
  ylab(Y_AXIS_LABEL)

surface <- ggplot(data = filter(dscore_data, micro == "surface"), 
                  aes(x = foliage, y = overlap_axis, fill = factor(macro))) +
  geom_boxplot(aes(x = macro, y = overlap_axis, fill = foliage), 
               alpha = 0.7, lwd = .5) +
  scale_fill_manual(values = c("#307233", "#e08626", "#d3c250")) +
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.x = element_text(size = 6)) +
  labs(fill = "Foliage/Snow") +
  labs(x = "Surface")

canopy <- ggplot(data = filter(dscore_data, micro == "canopy"), 
                 aes(x = foliage, y = overlap_axis, fill = factor(macro))) +
  geom_boxplot(aes(x = macro, y = overlap_axis, fill = foliage), 
               alpha = 0.7, lwd = .5) +
  scale_fill_manual(values = c("#307233", "#e08626")) +
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10),
        axis.text.x = element_text(size = 6)) +
  labs(x = "Canopy")

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)

plots <- grid.arrange(soil, surface + theme(legend.position = "none"), canopy, 
                      legend,
                      nrow = 1,
                      bottom = grid.text(
                        "Microhabitat", gp = gpar(fontsize = 25)))

plots



## ....F. Boxplots, by macro, micro, foliage, and snow ###########

soil <- ggplot(data = filter(dscore_data, micro == "soil"), 
               aes(x = seasonal_attributes, y = overlap_axis, fill = factor(macro))) +
  geom_boxplot(aes(x = macro, y = overlap_axis, fill = seasonal_attributes), alpha = 0.7, lwd = .5) +
  scale_fill_manual(values = c("#3ee0ac", "#3e9de0", "#307233",
                               "#e08626")) +
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 6)) +
  labs(x = "Soil")+
  ylab(Y_AXIS_LABEL)

surface <- ggplot(data = filter(dscore_data, micro == "surface"), 
                  aes(x = foliage, y = overlap_axis, fill = factor(macro))) +
  geom_boxplot(aes(x = macro, y = overlap_axis, fill = foliage), 
               alpha = 0.7, lwd = .5) +
  scale_fill_manual(values = c("#307233", "#e08626")) +
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.x = element_text(size = 6)) +
  labs(fill = "Foliage/Snow") +
  labs(x = "Surface")

canopy <- ggplot(data = filter(dscore_data, micro == "canopy"), 
                 aes(x = foliage, y = overlap_axis, fill = factor(macro))) +
  geom_boxplot(aes(x = macro, y = overlap_axis, fill = foliage), 
               alpha = 0.7, lwd = .5) +
  scale_fill_manual(values = c("#307233", "#e08626")) +
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10),
        axis.text.x = element_text(size = 6)) +
  labs(x = "Canopy")

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)

plots <- grid.arrange(soil, surface + theme(legend.position = "none"), canopy, 
                      legend,
                      nrow = 1,
                      bottom = grid.text(
                        "Microhabitat", gp = gpar(fontsize = 25)))

plots


## ....G. Boxplots, by foliage #########

soil <- ggplot(data = filter(dscore_data, micro == "soil"), 
               aes(x = foliage, y = overlap_axis, fill = foliage)) +
  geom_boxplot(aes(x = foliage, y = overlap_axis, fill = foliage), alpha = 0.7, lwd = .5) +
  scale_fill_manual(values = c("#307233", "#e08626")) +
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 6)) +
  labs(x = "Soil")+
  ylab(Y_AXIS_LABEL)

surface <- ggplot(data = filter(dscore_data, micro == "surface"), 
                  aes(x = foliage, y = overlap_axis, fill = foliage)) +
  geom_boxplot(aes(x = foliage, y = overlap_axis, fill = foliage), 
               alpha = 0.7, lwd = .5) +
  scale_fill_manual(values = c("#307233", "#e08626")) +
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.x = element_text(size = 6)) +
  labs(fill = "Foliage/Snow") +
  labs(x = "Surface")

canopy <- ggplot(data = filter(dscore_data, micro == "canopy"), 
                 aes(x = foliage, y = overlap_axis, fill = foliage)) +
  geom_boxplot(aes(x = foliage, y = overlap_axis, fill = foliage), 
               alpha = 0.7, lwd = .5) +
  scale_fill_manual(values = c("#307233", "#e08626")) +
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10),
        axis.text.x = element_text(size = 6)) +
  labs(x = "Canopy")

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)

plots <- grid.arrange(soil, surface + theme(legend.position = "none"), canopy, 
                      legend,
                      nrow = 1,
                      bottom = grid.text(
                        "Microhabitat", gp = gpar(fontsize = 25)))

plots

## Write out file ###########

# Output PNG params
OUTPUT_FILE <- "figures/boxplots_and_violins/TAD_byDay_byFoliagebySnow.png"
WIDTH_IN <- 9
HEIGHT_IN <- 5

ggsave(plot = plots, filename = OUTPUT_FILE, width = WIDTH_IN, height = HEIGHT_IN)

## 5. RECYCLING BIN: discarded plot generation ##########
## Quick calculation of average climate isolation #######

# Not really any better spot to put this....

## ....4A. Soil ###########
# temp leaf on
dscore_data %>%
  filter(micro == "soil", site == "NC on") %>%
  summarize(mean(overlap))

# temp leaf off
dscore_data %>%
  filter(micro == "soil", macro == "Leaf off") %>%
  summarize(mean(overlap))

# temp non forest
dscore_data %>%
  filter(micro == "soil", macro == "Non-forest") %>%
  summarize(mean(overlap))

# trop
dscore_data %>%
  filter(micro == "soil", site == "Mada" | site == "Aust") %>%
  summarize(mean(overlap))







dscore_data %>%
  filter(micro == "surface", macro == "Leaf on") %>%
  summarize(mean(overlap))

dscore_data %>%
  filter(micro == "surface", macro == "Leaf off") %>%
  summarize(mean(overlap))


dscore_data %>%
  filter(micro == "canopy", macro == "Leaf on") %>%
  summarize(mean(overlap))

dscore_data %>%
  filter(micro == "canopy", macro == "Leaf off") %>%
  summarize(mean(overlap))

## ....4B. Surface #####
# temp leaf on
dscore_data %>%
  filter(micro == "surface", site == "NC on") %>%
  summarize(mean(overlap))

# temp leaf off
dscore_data %>%
  filter(micro == "surface", macro == "Leaf off") %>%
  summarize(mean(overlap))

# temp non forest
dscore_data %>%
  filter(micro == "surface", macro == "Non-forest") %>%
  summarize(mean(overlap))

# trop
dscore_data %>%
  filter(micro == "surface", site == "Mada" | site == "Aust") %>%
  summarize(mean(overlap))

## ....4C. Canopy ########

# temp leaf on
dscore_data %>%
  filter(micro == "canopy", site == "NC on") %>%
  summarize(mean(overlap))

# temp leaf off
dscore_data %>%
  filter(micro == "canopy", macro == "Leaf off") %>%
  summarize(mean(overlap))

# trop
dscore_data %>%
  filter(micro == "canopy", site == "Mada" | site == "Aust") %>%
  summarize(mean(overlap))


## .... Boxplot separeted by micro and macro, default boxplot uncertainty ###########
ggplot(dscore_data, aes(x = micro, y = overlap, fill = factor(macro))) +
  scale_fill_manual(values = c("#3e9de0", "#307233", "#d3c250"), guide = guide_legend(title = "macro")) +
  geom_boxplot(aes(x = reorder(micro, macro, fun = mean), y = overlap, color = description),alpha = 0.7) +
  #guides(colour = guide_legend(override.aes = list(alpha = .7))) +
  scale_color_viridis(discrete = TRUE, option = "D", guide = guide_legend(title = "Description")) +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  labs(x = "Micromacro", y = "Overlap")

## .... Violin plot overlayed with mean and standard error....but no jitter ###########
ggplot(dscore_data, aes(x = description, y = overlap, fill = factor(macro))) +
  scale_fill_manual(values = c("#3e9de0", "#307233", "#d3c250"), guide = guide_legend(title = "macro")) +
  geom_violin(aes(x = reorder(micro, macro, fun = mean), y = overlap, color = description), alpha = 0.7) +
  scale_color_viridis(discrete = TRUE, option = "D", guide = guide_legend(title = "Description")) +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE) +
  theme_bw(base_size = 14) +
  geom_errorbar(aes(x = reorder(micro, macro, fun = mean), y = mean, ymin = mean - SE , ymax = mean + SE), 
                data = dscore_data_sum, width = 0.2, color = "black") +
  labs(x = "Micromacro", y = "Overlap")


## ....3H. Boxplot with overlayed mean and standard error...but no jitter ###########
ggplot(dscore_data, aes(x = micro, y = overlap, fill = factor(macro))) +
  stat_boxplot(aes(micro, overlap), geom='errorbar', linetype=1, width=0.5) +  #whiskers
  geom_boxplot( aes(micro, overlap),outlier.shape=1) +    
  stat_summary(fun.y=mean, geom="point", size=2) + 
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  coord_cartesian(xlim = NULL, ylim = Y_AXIS_RANGE, expand = FALSE)

leafoff <- filter(dscore_data, site == "N Carolina off", micro == "soil")
leafon <- filter(dscore_data, site == "N Carolina on", micro == "soil")

mean(na.omit(leafoff$overlap))
mean(na.omit(leafon$overlap))


