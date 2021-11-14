# David Klinges
# File creation date: 2019.07.17
# This script generates plots regressing low vs high mean temp and low min temp
# vs high max temp


## 1. Workspace prep ###########
library(tidyverse)
library(grid)
library(gridExtra)

# Daily
mountains_wide <- read_csv("data/03_compiled/mountains_wide_avg.csv",
                           col_types = cols(
                             low_canopy_min = col_double(),
                             low_canopy_mean = col_double(),
                             low_canopy_max = col_double(),
                             high_canopy_min = col_double(),
                             high_canopy_mean = col_double(),
                             high_canopy_max = col_double()
                           ))

mountains_overlap <- read_csv("data/03_compiled/mountains_overlap_daily_avg.csv")

# Monthly
mountains_wide <- read_csv("data/03_compiled/mountains_wide_avg.csv",
                                   col_types = cols(
                                     low_canopy_min = col_double(),
                                     low_canopy_mean = col_double(),
                                     low_canopy_max = col_double(),
                                     high_canopy_min = col_double(),
                                     high_canopy_mean = col_double(),
                                     high_canopy_max = col_double()
                                   ))
mountains_overlap_monthly <- read_csv("data/03_compiled/mountains_overlap_monthly_avg.csv")


## 2. Curation to prep for plotting #############
## ....A. Produce a 1:1 line #########
line <- tibble(
  x = c(-30:40),
  y = c(-30:40)
)

zero_line <- tibble(
  x = c(-10:25),
  y = rep.int(0, 36)
)
## ....B. Change order of data flags ############

# Change levels of factors
# Ordering sites and macros from densest foliage to no foliage
site_factors <- c("OR", "CH_conif", "PH", "CR_basham", "CR_northwest", "CR_southwest",
                  "CO_PrimaryForest", "AU", "CO_OldSecondary", "NM_soil", "NM_surface",
                  "AZ_cata", "NC", "CH_decid", "TN", "MD", "NH", "ID", "AZ", 
                  "SE", "NH_nonforest", "CH_open", "CA", "PT", "CO")

macro_factors <- c("tropical broadleaf", "Ponderosa pine", "Dense coniferous", 
                   "deciduous", "meadow near forest", "developed", "alpine meadow", 
                   "scrub shrub", "hot desert")

foliage_factors <- c("leaf-on", "leaf-off")
seasonal_factors <- c("leaf-off and no snow", "leaf-on and no snow",
                      "leaf-off and snow", "leaf-on and snow")


# Convert flags to factor
mountains_wide <- mountains_wide %>%
  mutate(macro = factor(macro, levels = macro_factors)) %>%
  mutate(site = factor(site, levels = site_factors)) %>% 
  mutate(foliage = factor(foliage, levels = foliage_factors)) %>% 
  # Change snow depth to presence/absence
  mutate(snow_presence = ifelse(snowdepth > 2, "snow", "no snow")) %>% 
  mutate(snow_presence = ifelse(is.na(snow_presence), "no snow", snow_presence)) %>% 
  mutate(seasonal_attributes = paste(foliage, snow_presence, sep = " and ")) %>% 
  mutate(seasonal_attributes = factor(seasonal_attributes, levels = seasonal_factors))


## ....C. Excluding any sites? #########

# mountains_wide <- mountains_wide %>% 
#   filter(macro != "developed")

## ....D. Control for elevation change, separately for soil, surface, canopy ##########

# Needs to be separate so that you can have rows with no NAs

thermal_cols <- colnames(dplyr::select(mountains_wide, contains("low"), contains("high")))

source("scripts/00_source_code/correct_elevChange.R")

mountains_wide_soil <- mountains_wide %>% 
  dplyr::select(year, julian, macro, foliage, latitude, site, 
                seasonal_attributes, elevation_change, contains("soil")) %>% 
  filter_at(vars(contains("min")), complete.cases) %>% 
  filter_at(vars(contains("min")), is.finite) %>% 
  filter_at(vars(contains("max")), complete.cases) %>% 
  filter_at(vars(contains("max")), is.finite) %>% 
  filter(complete.cases(elevation_change))

mountains_wide_surface <- mountains_wide %>% 
  dplyr::select(year, julian, macro, foliage, latitude, site, 
                seasonal_attributes, elevation_change, contains("surface")) %>% 
  filter_at(vars(contains("min")), complete.cases) %>% 
  filter_at(vars(contains("min")), is.finite) %>% 
  filter_at(vars(contains("max")), complete.cases) %>% 
  filter_at(vars(contains("max")), is.finite) %>% 
  filter(complete.cases(elevation_change))

mountains_wide_canopy <- mountains_wide %>% 
  dplyr::select(year, julian, macro, foliage, latitude, site, 
                seasonal_attributes, elevation_change, contains("canopy")) %>% 
  filter_at(vars(contains("min")), complete.cases) %>% 
  filter_at(vars(contains("min")), is.finite) %>% 
  filter_at(vars(contains("max")), complete.cases) %>% 
  filter_at(vars(contains("max")), is.finite) %>% 
  filter(complete.cases(elevation_change))

# Control for elevation difference
mountains_elevcontrol_soil <- mountains_wide_soil %>% 
  mutate(low_soil_min = correct_elevChange(mountains_wide_soil, 
                                           low_soil_min,
                                           "linear"),
         low_soil_max = correct_elevChange(mountains_wide_soil, 
                                           low_soil_max,
                                           "linear"),
         high_soil_min = correct_elevChange(mountains_wide_soil, 
                                            low_soil_min,
                                            "linear"),
         high_soil_max = correct_elevChange(mountains_wide_soil, 
                                            high_soil_max,
                                            "linear"))

mountains_elevcontrol_surface <- mountains_wide_surface %>% 
  mutate(low_surface_min = correct_elevChange(mountains_wide_surface, 
                                              low_surface_min,
                                              "linear"),
         low_surface_max = correct_elevChange(mountains_wide_surface, 
                                              low_surface_max,
                                              "linear"),
         high_surface_min = correct_elevChange(mountains_wide_surface, 
                                               high_surface_min,
                                               "linear"),
         high_surface_max = correct_elevChange(mountains_wide_surface, 
                                               high_surface_max,
                                               "linear"))

mountains_elevcontrol_canopy <- mountains_wide_canopy %>% 
  mutate(low_canopy_min = correct_elevChange(mountains_wide_canopy, 
                                             low_canopy_min,
                                             "linear"),
         low_canopy_max = correct_elevChange(mountains_wide_canopy, 
                                             low_canopy_max,
                                             "linear"),
         high_canopy_min = correct_elevChange(mountains_wide_canopy, 
                                              high_canopy_min,
                                              "linear"),
         high_canopy_max = correct_elevChange(mountains_wide_canopy, 
                                              high_canopy_max,
                                              "linear"))

## ....E. Join to overlap database #########

overlap_soil <- mountains_overlap %>%
  spread(key = micro, value = TAD_elevCorr) %>% 
  dplyr::select(julian, site, macro, soil)

overlap_soil <- mountains_wide_soil %>% 
  distinct() %>% 
  left_join(overlap_soil) %>% 
  mutate(mean_temp = (low_soil_mean + high_soil_mean)/2) %>% 
  dplyr::select(julian, site, soil, mean_temp, 
                macro, foliage, latitude, seasonal_attributes) %>% 
  filter(complete.cases(soil))

overlap_surface <- mountains_overlap %>%
  spread(key = micro, value = TAD_elevCorr) %>% 
  dplyr::select(julian, site, macro, surface)

overlap_surface <- mountains_wide_surface %>%
  distinct() %>% 
  left_join(overlap_surface) %>% 
  mutate(mean_temp = (low_surface_mean + high_surface_mean)/2) %>% 
  dplyr::select(julian, site, surface, mean_temp, 
                macro, foliage, latitude, seasonal_attributes) %>% 
  filter(complete.cases(surface))

overlap_canopy <- mountains_overlap %>%
  spread(key = micro, value = TAD_elevCorr) %>% 
  dplyr::select(julian, site, macro, canopy)

overlap_canopy <- mountains_wide_canopy %>% 
  distinct() %>% 
  left_join(overlap_canopy) %>% 
  mutate(mean_temp = (low_canopy_mean + high_canopy_mean)/2) %>% 
  dplyr::select(julian, site, canopy, mean_temp, 
                macro, foliage, latitude, seasonal_attributes) %>% 
  filter(complete.cases(canopy))

## 3. Generate plots ##########

## ....A. Low mean vs high mean ############

## ...... * by site ##############

soil <- ggplot(mountains_elevcontrol, aes(low_soil_mean, high_soil_mean)) +
  geom_point(aes(color = site, shape = macro)) +
  geom_line(data = line, aes(x,y)) +
  labs(title = "Soil") +
  ylab("Mean temp at high elevation * change in elevation / 1000") +
  xlab("Mean temp at low elevation * change in elevation / 1000") +
  theme_bw() +
  ylim(c(-20, 45)) +
  xlim(c(-20, 50)) +   
  theme(legend.position = "none",
        axis.title.x=element_blank())


surface <- ggplot(mountains_elevcontrol, aes(low_surface_mean, high_surface_mean)) +
  geom_point(aes(color = site, shape = macro)) +
  geom_line(data = line, aes(x,y)) +
  labs(title = "Surface") +
  ylab("Mean temp at high elevation * change in elevation / 1000") +
  xlab("Mean temp at low elevation * change in elevation / 1000") +
  theme_bw() +
  ylim(c(-20, 45)) +
  xlim(c(-20, 45))


canopy <- ggplot(mountains_elevcontrol, aes(low_canopy_mean, high_canopy_mean)) +
  geom_point(aes(color = site, shape = macro)) +
  geom_line(data = line, aes(x,y)) +
  labs(title = "Canopy") +
  ylab("Mean temp at high elevation * change in elevation / 1000") +
  xlab("Mean temp at low elevation * change in elevation / 1000") +
  theme_bw() +
  ylim(c(-20, 45)) +
  xlim(c(-20, 50)) +   
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 8))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)

surface <- surface +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 8))

plot <- grid.arrange(soil, surface, canopy, legend, nrow = 1,
             bottom = grid.text("Mean temp at low elevation * change in elevation / 1000"))

ggsave(plot = plot,
       filename = "figures/lowVShigh_regression/lowMean_high_Mean_bysite.png", 
       width = 10, height = 5)


## ...... * by macro ##############

soil <- ggplot(mountains_elevcontrol, aes(low_soil_mean, high_soil_mean)) +
  geom_point(aes(color = macro)) +
  geom_line(data = line, aes(x,y)) +
  labs(title = "Soil") +
  ylab("Mean temp at high elevation * change in elevation / 1000") +
  xlab("Mean temp at low elevation * change in elevation / 1000") +
  theme_bw() +
  ylim(c(-20, 45)) +
  xlim(c(-20, 50)) +   
  theme(legend.position = "none",
        axis.title.x=element_blank())


surface <- ggplot(mountains_elevcontrol, aes(low_surface_mean, high_surface_mean)) +
  geom_point(aes(color = macro)) +
  geom_line(data = line, aes(x,y)) +
  labs(title = "Surface") +
  ylab("Mean temp at high elevation * change in elevation / 1000") +
  xlab("Mean temp at low elevation * change in elevation / 1000") +
  theme_bw() +
  ylim(c(-20, 45)) +
  xlim(c(-20, 45))


canopy <- ggplot(mountains_elevcontrol, aes(low_canopy_mean, high_canopy_mean)) +
  geom_point(aes(color = macro)) +
  geom_line(data = line, aes(x,y)) +
  labs(title = "Canopy") +
  ylab("Mean temp at high elevation * change in elevation / 1000") +
  xlab("Mean temp at low elevation * change in elevation / 1000") +
  theme_bw() +
  ylim(c(-20, 45)) +
  xlim(c(-20, 50)) +   
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 8))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)

surface <- surface +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 8))

plot <- grid.arrange(soil, surface, canopy, legend, nrow = 1,
                     bottom = grid.text("Mean temp at low elevation * change in elevation / 1000"))

ggsave(plot = plot,
       filename = "figures/lowVShigh_regression/lowMean_high_Mean_bymacro.png", 
       width = 10, height = 5)
## ....B. Low min vs high max ############

## ......* by site #########
soil <- ggplot(mountains_elevcontrol, aes(low_soil_min, high_soil_max)) +
  geom_point(aes(color = site, shape = macro)) +
  geom_line(data = line, aes(x,y)) +
  labs(title = "Soil") +
  ylab("Mean temp at high elevation * change in elevation / 1000") +
  xlab("Mean temp at low elevation * change in elevation / 1000") +
  theme_bw() +
  ylim(c(-20, 45)) +
  xlim(c(-20, 50)) +   
  theme(legend.position = "none",
        axis.title.x=element_blank())


surface <- ggplot(mountains_elevcontrol, aes(low_surface_min, high_surface_max)) +
  geom_point(aes(color = site, shape = macro)) +
  geom_line(data = line, aes(x,y)) +
  labs(title = "Surface") +
  ylab("Mean temp at high elevation * change in elevation / 1000") +
  xlab("Mean temp at low elevation * change in elevation / 1000") +
  theme_bw() +
  ylim(c(-20, 45)) +
  xlim(c(-20, 45))


canopy <- ggplot(mountains_elevcontrol, aes(low_canopy_min, high_canopy_max)) +
  geom_point(aes(color = site, shape = macro)) +
  geom_line(data = line, aes(x,y)) +
  labs(title = "Canopy") +
  ylab("Mean temp at high elevation * change in elevation / 1000") +
  xlab("Mean temp at low elevation * change in elevation / 1000") +
  theme_bw() +
  ylim(c(-20, 45)) +
  xlim(c(-20, 50)) +   
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 8))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)

surface <- surface +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 8))

plot <- grid.arrange(soil, surface, canopy, legend, nrow = 1,
             bottom = grid.text("Mean temp at low elevation * change in elevation / 1000"))

ggsave(plot = plot,
       filename = "figures/lowVShigh_regression/lowMin_highMax_bysites.png", 
       width = 10, height = 5)

## ......* by macro ##########

soil <- ggplot(mountains_elevcontrol_soil, aes(low_soil_min, high_soil_max)) +
  geom_point(aes(color = macro)) +
  geom_line(data = line, aes(x,y)) +
  labs(title = "Soil") +
  ylab("Mean temp at high elevation * change in elevation / 1000") +
  xlab("Mean temp at low elevation * change in elevation / 1000") +
  theme_bw() +
  ylim(c(-20, 45)) +
  xlim(c(-20, 50)) +   
  theme(legend.position = "none",
        axis.title.x=element_blank())


surface <- ggplot(mountains_elevcontrol_surface, aes(low_surface_min, high_surface_max)) +
  geom_point(aes(color = macro)) +
  geom_line(data = line, aes(x,y)) +
  labs(title = "Surface") +
  ylab("Mean temp at high elevation * change in elevation / 1000") +
  xlab("Mean temp at low elevation * change in elevation / 1000") +
  theme_bw() +
  ylim(c(-20, 45)) +
  xlim(c(-20, 45))


canopy <- ggplot(mountains_elevcontrol_canopy, aes(low_canopy_min, high_canopy_max)) +
  geom_point(aes(color = macro)) +
  geom_line(data = line, aes(x,y)) +
  labs(title = "Canopy") +
  ylab("Mean temp at high elevation * change in elevation / 1000") +
  xlab("Mean temp at low elevation * change in elevation / 1000") +
  theme_bw() +
  ylim(c(-20, 45)) +
  xlim(c(-20, 50)) +   
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 8))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)

surface <- surface +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 8))

plot <- grid.arrange(soil, surface, canopy, legend, nrow = 1,
                     bottom = grid.text("Mean temp at low elevation * change in elevation / 1000"))

ggsave(plot = plot,
       filename = "figures/lowVShigh_regression/lowMin_highMax_bymacro.png", 
       width = 10, height = 5)

## ......* by foliage and snow ##########

soil <- ggplot(mountains_elevcontrol_soil, 
               aes(low_soil_min, high_soil_max)) +
  geom_point(aes(color = seasonal_attributes), alpha = 0.2) +
  scale_color_manual(values = c("leaf-on and snow" = "#3ee0ac", 
                                "leaf-on and no snow" = "#307233",
                                "leaf-off and snow" = "#3e9de0",
                                "leaf-off and no snow" = "#e08626")) +
  
  geom_line(data = line, aes(x,y)) +
  labs(title = "Soil") +
  ylab("Max temp at high elevation") +
  xlab("Min temp at low elevation") +
  theme_bw() +
  ylim(c(-20, 30)) +
  xlim(c(-20, 25)) +   
  theme(legend.position = "none",
        axis.title.x=element_blank())


surface <- ggplot(mountains_elevcontrol_surface, 
                  aes(low_surface_min, high_surface_max)) +
  geom_point(aes(color = seasonal_attributes), alpha = 0.2) +
  scale_color_manual(values = c("leaf-on and snow" = "#3ee0ac", 
                                "leaf-on and no snow" = "#307233",
                                "leaf-off and snow" = "#3e9de0",
                                "leaf-off and no snow" = "#e08626")) +
  
  geom_line(data = line, aes(x,y)) +
  labs(title = "Surface") +
  ylab("Max temp at high elevation") +
  xlab("Min temp at low elevation") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  ylim(c(-20, 30)) +
  xlim(c(-20, 25))

canopy <- ggplot(mountains_elevcontrol_canopy, 
                 aes(low_canopy_min, high_canopy_max)) +
  geom_point(aes(color = seasonal_attributes), alpha = 0.2) +
  scale_color_manual(values = c("leaf-on and snow" = "#3ee0ac", 
                                "leaf-on and no snow" = "#307233",
                                "leaf-off and snow" = "#3e9de0",
                                "leaf-off and no snow" = "#e08626")) +
  
  geom_line(data = line, aes(x,y)) +
  labs(title = "Canopy") +
  ylab("Max temp at high elevation") +
  xlab("Min temp at low elevation") +
  theme_bw() +
  ylim(c(-20, 30)) +
  xlim(c(-20, 25)) +   
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 8))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)

surface <- surface +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 8))

plot <- grid.arrange(soil, surface, canopy, legend, nrow = 1,
                     bottom = grid.text("Min temp at low elevation"))

ggsave(plot = plot,
       filename = "figures/lowVShigh_regression/lowMin_highMax_byFoliageSnow.png", 
       width = 10, height = 5)

## ....C. Mean temp vs overlap #######
## ......* by macro ##########

soil <- ggplot(overlap_soil, 
               aes(mean_temp, soil)) +
  geom_point(aes(color = macro), alpha = 0.2) +
  scale_color_manual(values = c("tropical broadleaf" = "#b0db39", 
                                "Dense coniferous" = "#307233",
                                "Ponderosa pine" = "#6cba3b",
                                "deciduous" = "#725100",
                                "alpine meadow" = "#33CCFF", 
                                "meadow near forest" = "#CC99FF",
                                "developed" = "#D3D3D3",
                                "scrub shrub" = "#d3c250",
                                "hot desert" = "#FFBD00")) +
  
  geom_line(data = zero_line, aes(x,y)) +
  labs(title = "Soil") +
  ylab("Thermal Absolute Distance") +
  xlab("Mean temperature") +
  theme_bw() +
  ylim(c(-15, 10)) +
  xlim(c(-10, 25)) +   
  theme(legend.position = "none",
        axis.title.x=element_blank())


surface <- ggplot(overlap_surface, 
                  aes(mean_temp, surface)) +
  geom_point(aes(color = macro), alpha = 0.2) +
  scale_color_manual(values = c("tropical broadleaf" = "#b0db39", 
                                "Dense coniferous" = "#307233",
                                "Ponderosa pine" = "#6cba3b",
                                "deciduous" = "#725100",
                                "alpine meadow" = "#33CCFF", 
                                "meadow near forest" = "#CC99FF",
                                "developed" = "#D3D3D3",
                                "scrub shrub" = "#d3c250",
                                "hot desert" = "#FFBD00")) +
  geom_line(data = zero_line, aes(x,y)) +
  labs(title = "Surface") +
  ylab("Thermal Absolute Distance") +
  xlab("Mean temperature") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  ylim(c(-15, 10)) +
  xlim(c(-10, 25))

canopy <- ggplot(overlap_canopy, 
                 aes(mean_temp, canopy)) +
  geom_point(aes(color = macro), alpha = 0.2) +
  scale_color_manual(values = c("tropical broadleaf" = "#b0db39", 
                                "Dense coniferous" = "#307233",
                                "Ponderosa pine" = "#6cba3b",
                                "deciduous" = "#725100",
                                "alpine meadow" = "#33CCFF", 
                                "developed" = "#D3D3D3",
                                "meadow near forest" = "#CC99FF",
                                "scrub shrub" = "#d3c250",
                                "hot desert" = "#FFBD00")) +
  
  geom_line(data = zero_line, aes(x,y)) +
  labs(title = "Canopy") +
  ylab("Thermal Absolute Distance") +
  xlab("Mean temperature") +
  theme_bw() +
  ylim(c(-15, 10)) +
  xlim(c(-10, 25)) +     
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 8))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)

surface <- surface +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 8))

plot <- grid.arrange(soil, surface, canopy, legend, nrow = 1,
                     bottom = grid.text("Mean Temperature"))

ggsave(plot = plot,
       filename = "figures/lowVShigh_regression/meanTemp_Overlap_byMacro.png", 
       width = 10, height = 5)

## ......* by foliage and snow ##########

soil <- ggplot(overlap_soil, 
               aes(mean_temp, soil)) +
  geom_point(aes(color = seasonal_attributes, 
                 fill = seasonal_attributes, shape = seasonal_attributes), 
             alpha = .2) +
  scale_color_manual(values = c("leaf-on and snow" = "#3ee0ac", 
                                "leaf-on and no snow" = "#307233",
                                "leaf-off and snow" = "#3e9de0",
                                "leaf-off and no snow" = "#e08626")) +
  scale_fill_manual(values = c("leaf-on and snow" = "#3ee0ac", 
                                "leaf-on and no snow" = "#307233",
                                "leaf-off and snow" = "#3e9de0",
                                "leaf-off and no snow" = "#e08626")) +
  scale_shape_manual(values = c("leaf-on and snow" = 16, 
                               "leaf-on and no snow" = 24,
                               "leaf-off and snow" = 16,
                               "leaf-off and no snow" = 24)) +
  
  geom_line(data = zero_line, aes(x,y)) +
  labs(title = "Soil") +
  ylab("Thermal Absolute Distance") +
  xlab("Mean temperature") +
  theme_bw() +
  ylim(c(-15, 10)) +
  xlim(c(-10, 25)) +   
  theme(legend.position = "none",
        axis.title.x=element_blank())


surface <- ggplot(overlap_surface, 
               aes(mean_temp, surface)) +
  geom_point(aes(color = seasonal_attributes, 
                 fill = seasonal_attributes, shape = seasonal_attributes), 
             alpha = .2) +
  scale_color_manual(values = c("leaf-on and snow" = "#3ee0ac", 
                                "leaf-on and no snow" = "#307233",
                                "leaf-off and snow" = "#3e9de0",
                                "leaf-off and no snow" = "#e08626")) +
  scale_fill_manual(values = c("leaf-on and snow" = "#3ee0ac", 
                               "leaf-on and no snow" = "#307233",
                               "leaf-off and snow" = "#3e9de0",
                               "leaf-off and no snow" = "#e08626")) +
  scale_shape_manual(values = c("leaf-on and snow" = 16, 
                                "leaf-on and no snow" = 24,
                                "leaf-off and snow" = 16,
                                "leaf-off and no snow" = 24)) +
  
  geom_line(data = zero_line, aes(x,y)) +
  labs(title = "Surface") +
  ylab("Thermal Absolute Distance") +
  xlab("Mean temperature") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  ylim(c(-15, 10)) +
  xlim(c(-10, 25))

canopy <- ggplot(overlap_canopy, 
               aes(mean_temp, canopy)) +
  geom_point(aes(color = seasonal_attributes, 
                 fill = seasonal_attributes, shape = seasonal_attributes), 
             alpha = .2) +
  scale_color_manual(values = c("leaf-on and snow" = "#3ee0ac", 
                                "leaf-on and no snow" = "#307233",
                                "leaf-off and snow" = "#3e9de0",
                                "leaf-off and no snow" = "#e08626")) +
  scale_fill_manual(values = c("leaf-on and snow" = "#3ee0ac", 
                               "leaf-on and no snow" = "#307233",
                               "leaf-off and snow" = "#3e9de0",
                               "leaf-off and no snow" = "#e08626")) +
  scale_shape_manual(values = c("leaf-on and snow" = 16, 
                                "leaf-on and no snow" = 24,
                                "leaf-off and snow" = 16,
                                "leaf-off and no snow" = 24)) +
  
  geom_line(data = zero_line, aes(x,y)) +
  labs(title = "Canopy") +
  ylab("Thermal Absolute Distance") +
  xlab("Mean temperature") +
  theme_bw() +
  ylim(c(-15, 10)) +
  xlim(c(-10, 25)) +     
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 8))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)

surface <- surface +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 8))

plot <- grid.arrange(soil, surface, canopy, legend, nrow = 1,
                     bottom = grid.text("Mean Temperature"))

ggsave(plot = plot,
       filename = "figures/lowVShigh_regression/meanTemp_Overlap_byFoliageSnow.png", 
       width = 10, height = 5)







## ....D. Latitude vs overlap #######
## ......* by macro ##########

soil <- ggplot(overlap_soil, 
               aes(abs(latitude), soil)) +
  geom_jitter(aes(color = macro), 
              alpha = .2, width = 3.5, height = 0) +
  scale_color_manual(values = c("tropical broadleaf" = "#b0db39", 
                                "Dense coniferous" = "#307233",
                                "Ponderosa pine" = "#6cba3b",
                                "deciduous" = "#725100",
                                "alpine meadow" = "#33CCFF", 
                                "meadow near forest" = "#CC99FF",
                                "developed" = "#D3D3D3",
                                "scrub shrub" = "#d3c250",
                                "hot desert" = "#FFBD00")) +
  
  geom_line(data = zero_line, aes(x,y)) +
  labs(title = "Soil") +
  ylab("Thermal Absolute Distance") +
  xlab("Latitude") +
  theme_bw() +
  ylim(c(-15, 10)) +
  xlim(c(0, 60)) +   
  theme(legend.position = "none",
        axis.title.x=element_blank())


surface <- ggplot(overlap_surface, 
                  aes(abs(latitude), surface)) +
  geom_jitter(aes(color = macro), 
              alpha = .2, width = 3.5, height = 0) +
  scale_color_manual(values = c("tropical broadleaf" = "#b0db39", 
                                "Dense coniferous" = "#307233",
                                "Ponderosa pine" = "#6cba3b",
                                "deciduous" = "#725100",
                                "alpine meadow" = "#33CCFF", 
                                "meadow near forest" = "#CC99FF",
                                "developed" = "#D3D3D3",
                                "scrub shrub" = "#d3c250",
                                "hot desert" = "#FFBD00")) +
  geom_line(data = zero_line, aes(x,y)) +
  labs(title = "Surface") +
  ylab("Thermal Absolute Distance") +
  xlab("Latitude") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  ylim(c(-15, 10)) +
  xlim(c(0, 60))

canopy <- ggplot(overlap_canopy, 
                 aes(abs(latitude), canopy)) +
  geom_jitter(aes(color = macro), 
              alpha = .2, width = 3.5, height = 0) +
  scale_color_manual(values = c("tropical broadleaf" = "#b0db39", 
                                "Dense coniferous" = "#307233",
                                "Ponderosa pine" = "#6cba3b",
                                "deciduous" = "#725100",
                                "alpine meadow" = "#33CCFF", 
                                "developed" = "#D3D3D3",
                                "meadow near forest" = "#CC99FF",
                                "scrub shrub" = "#d3c250",
                                "hot desert" = "#FFBD00")) +
  
  geom_line(data = zero_line, aes(x,y)) +
  labs(title = "Canopy") +
  ylab("Thermal Absolute Distance") +
  xlab("Latitude") +
  theme_bw() +
  ylim(c(-15, 10)) +
  xlim(c(0, 60)) +     
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 8))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)

surface <- surface +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 8))

plot <- grid.arrange(soil, surface, canopy, legend, nrow = 1,
                     bottom = grid.text("Latitude"))

ggsave(plot = plot,
       filename = "figures/lowVShigh_regression/latitude_Overlap_byMacro_Daily.png", 
       width = 10, height = 5)

## ......* by foliage and snow ##########

soil <- ggplot(overlap_soil, 
               aes(abs(latitude), soil)) +
  geom_jitter(aes(color = seasonal_attributes, 
                 fill = seasonal_attributes, shape = seasonal_attributes), 
             alpha = .2, width = 2, height = 0) +
  scale_color_manual(values = c("leaf-on and snow" = "#3ee0ac", 
                                "leaf-on and no snow" = "#307233",
                                "leaf-off and snow" = "#3e9de0",
                                "leaf-off and no snow" = "#e08626")) +
  scale_fill_manual(values = c("leaf-on and snow" = "#3ee0ac", 
                               "leaf-on and no snow" = "#307233",
                               "leaf-off and snow" = "#3e9de0",
                               "leaf-off and no snow" = "#e08626")) +
  scale_shape_manual(values = c("leaf-on and snow" = 16, 
                                "leaf-on and no snow" = 24,
                                "leaf-off and snow" = 16,
                                "leaf-off and no snow" = 24)) +
  
  geom_line(data = zero_line, aes(x,y)) +
  labs(title = "Soil") +
  ylab("Thermal Absolute Distance") +
  xlab("Latitude") +
  theme_bw() +
  ylim(c(-15, 10)) +
  xlim(c(0, 60)) +   
  theme(legend.position = "none",
        axis.title.x=element_blank())


surface <- ggplot(overlap_surface, 
                  aes(abs(latitude), surface)) +
  geom_jitter(aes(color = seasonal_attributes, 
                 fill = seasonal_attributes, shape = seasonal_attributes), 
             alpha = .2, width = 2, height = 0) +
  scale_color_manual(values = c("leaf-on and snow" = "#3ee0ac", 
                                "leaf-on and no snow" = "#307233",
                                "leaf-off and snow" = "#3e9de0",
                                "leaf-off and no snow" = "#e08626")) +
  scale_fill_manual(values = c("leaf-on and snow" = "#3ee0ac", 
                               "leaf-on and no snow" = "#307233",
                               "leaf-off and snow" = "#3e9de0",
                               "leaf-off and no snow" = "#e08626")) +
  scale_shape_manual(values = c("leaf-on and snow" = 16, 
                                "leaf-on and no snow" = 24,
                                "leaf-off and snow" = 16,
                                "leaf-off and no snow" = 24)) +
  
  geom_line(data = zero_line, aes(x,y)) +
  labs(title = "Surface") +
  ylab("Thermal Absolute Distance") +
  xlab("Latitude") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  ylim(c(-15, 10)) +
  xlim(c(0, 60))

canopy <- ggplot(overlap_canopy, 
                 aes(abs(latitude), canopy)) +
  geom_jitter(aes(color = seasonal_attributes, 
                 fill = seasonal_attributes, shape = seasonal_attributes), 
             alpha = .2, width = 2, height = 0) +
  scale_color_manual(values = c("leaf-on and snow" = "#3ee0ac", 
                                "leaf-on and no snow" = "#307233",
                                "leaf-off and snow" = "#3e9de0",
                                "leaf-off and no snow" = "#e08626")) +
  scale_fill_manual(values = c("leaf-on and snow" = "#3ee0ac", 
                               "leaf-on and no snow" = "#307233",
                               "leaf-off and snow" = "#3e9de0",
                               "leaf-off and no snow" = "#e08626")) +
  scale_shape_manual(values = c("leaf-on and snow" = 16, 
                                "leaf-on and no snow" = 24,
                                "leaf-off and snow" = 16,
                                "leaf-off and no snow" = 24)) +
  
  geom_line(data = zero_line, aes(x,y)) +
  labs(title = "Canopy") +
  ylab("Thermal Absolute Distance") +
  xlab("Latitude") +
  theme_bw() +
  ylim(c(-15, 10)) +
  xlim(c(0, 60)) +     
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 8))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)

surface <- surface +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 8))

plot <- grid.arrange(soil, surface, canopy, legend, nrow = 1,
                     bottom = grid.text("Latitude"))

ggsave(plot = plot,
       filename = "figures/lowVShigh_regression/latitude_Overlap_byFoliageSnow_Daily.png", 
       width = 10, height = 5)








## ###############






