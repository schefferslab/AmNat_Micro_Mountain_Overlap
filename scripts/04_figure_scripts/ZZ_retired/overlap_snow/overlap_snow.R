# David Klinges
# Date init: 2019-08-24
# This script generates plot of the magnitude of change in overlap d-score 
#   values with snow depth

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
Y_AXIS_LABEL <- "TAD"
# Output PNG params
OUTPUT_FILE <- "figures/boxplots_and_violins/TAD_byFoliage_byMonth.png"
WIDTH_IN <- 9
HEIGHT_IN <- 5

# ggsave(plot = plots, filename = OUTPUT_FILE, width = WIDTH_IN, height = HEIGHT_IN)

## 2. Curate data #############

## ....A. Change order of data flags ############

# Change levels of factors
# Ordering sites and macros from densest foliage to no foliage
site_factors <- c("OR", "CH_conif", "PH", "CR_basham", "CR_northwest", "CR_southwest",
                  "CO_PrimaryForest", "AU", "CO_OldSecondary", "NM_soil", "NM_surface",
                  "AZ_cata", "NC", "CH_decid", "TN", "MD", "NH", "ID", "AZ", 
                  "SE", "NH_nonforest", "CH_open", "CA", "PT", "CO")

macro_factors <- c("tropical\nbroadleaf", "Ponderosa\npine", "Dense\nconiferous", 
                   "deciduous", "meadow\nnear\nforest", "developed", "alpine\nmeadow", 
                   "scrub\nshrub", "hot\ndesert")

foliage_factors <- c("leaf-on", "leaf-off")
seasonal_factors <- c("leaf-on and snow", "leaf-off and snow",
                      "leaf-on and no snow", "leaf-off and no snow")


# Convert flags to factor
dscore_data <- dscore_data %>%
  mutate(macro =   gsub('\\s', '\n', macro)) %>% 
  mutate(macro = factor(macro, levels = macro_factors)) %>%
  mutate(site = factor(site, levels = site_factors)) %>% 
  mutate(foliage = factor(foliage, levels = foliage_factors)) %>% 
  # Change snow depth to presence/absence
  mutate(snow_presence = ifelse(snowdepth > 2, "snow", "no snow")) %>% 
  mutate(snow_presence = ifelse(is.na(snow_presence), "no snow", snow_presence)) %>% 
  mutate(seasonal_attributes = paste(foliage, snow_presence, sep = " and ")) %>% 
  mutate(seasonal_attributes = factor(seasonal_attributes, levels = seasonal_factors))


## ....F. Boxplots, by foliage #########

soil <- ggplot(data = filter(dscore_data, micro == "soil")) +
  geom_point(aes(x = snowdepth, y = overlap_axis, color = macro)) +
  geom_line(aes(x = snowdepth, y = overlap_axis, color = macro)) +
  scale_color_manual(values = c("#3ee0ac", "#3e9de0", "#3e9de0",
                               "#307233", "#e08626", "#e5d35e",
                               "#e5d35e", "#e5d35e", "#e5d35e")) +

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

