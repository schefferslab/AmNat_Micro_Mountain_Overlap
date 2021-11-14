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

mountains <- read_csv("data/03_compiled/mountains_janz_daily_avg.csv",
                                 col_types = cols(
                                   year = col_double(),
                                   elevation_change = col_double()
                                 ))
dscore_data <- mountains

## 2. Curate data #############

## ....A. Change order of data flags ############

# Change levels of factors
site_factors <- c("OR", "PH", "AU", "NM", "NC", "TN", "MD", "NH", "ID", "AZ", 
                  "SE", "PT", "CO")
macro_factors <- c("coniferous", "evergreen", "deciduous", "non-forest")
foliage_factors <- c("leaf-on", "leaf-off", "no foliage")
seasonal_factors <- c("leaf-on and snowpack", "leaf-off and snowpack",
                      "no foliage and snowpack","leaf-on and no snow",  
                      "leaf-off and no snow", "no foliage and no snow")

# Convert flags to factor
dscore_data <- dscore_data %>%
  mutate(macro = factor(macro, levels = macro_factors)) %>%
  mutate(site = factor(site, levels = site_factors)) %>% 
  mutate(foliage = factor(foliage, levels = foliage_factors)) %>% 
# Combine foliage and snow attributes 
  mutate(seasonal_attributes = paste0(foliage, " and ", snow)) %>% 
  mutate(seasonal_attributes = factor(seasonal_attributes, levels = seasonal_factors))

## ....B. Calculate Standard deviation and error ########
dscore_data_naomit <- dscore_data %>% 
  filter(!is.na(overlap))

dscore_data_sum <- dscore_data_naomit %>%
  dplyr::group_by(macro, micro, site, elevation_change) %>%
  dplyr::summarize(mean = mean(overlap)) 

dscore_data_sum <- dscore_data_naomit %>%
  dplyr::group_by(macro, site, micro) %>%
  dplyr::summarize(SD = sd(overlap)) %>%
  dplyr::left_join(dscore_data_sum)

dscore_data_sum <- dscore_data_naomit %>%
  dplyr::group_by(macro, site, micro) %>%
  dplyr::summarize(SE = sd(overlap)/sqrt(length(overlap))) %>%
  dplyr::left_join(dscore_data_sum) %>%
  ungroup()

## ....C. Remove outliers (95%) ############

dscore_data <- dscore_data %>%
  left_join(dscore_data_sum)

dscore_data <- dscore_data %>%
  group_by(site) %>%
  mutate(overlap = ifelse(overlap > (mean + (3 * SD)) | 
                            overlap < (mean - (3 * SD)) , NA, overlap)) %>%  
  ungroup()

dscore_data <- na.omit(dscore_data)



## 3. Create figure ###########

## ....3A. Boxplot separeted by micro and macro, default boxplot uncertainty ###########
ggplot(dscore_data, aes(x = micro, y = overlap, fill = factor(macro))) +
  scale_fill_manual(values = c("#3e9de0", "#307233", "#d3c250"), guide = guide_legend(title = "macro")) +
  geom_boxplot(aes(x = reorder(micro, macro, fun = mean), y = overlap, color = description),alpha = 0.7) +
  #guides(colour = guide_legend(override.aes = list(alpha = .7))) +
  scale_color_viridis(discrete = TRUE, option = "D", guide = guide_legend(title = "Description")) +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-5, 5), expand = FALSE) +
  theme_bw(base_size = 14) +
  labs(x = "Micromacro", y = "Overlap")

## ....3B. Violin plot overlayed with mean and standard error....but no jitter ###########
ggplot(dscore_data, aes(x = description, y = overlap, fill = factor(macro))) +
  scale_fill_manual(values = c("#3e9de0", "#307233", "#d3c250"), guide = guide_legend(title = "macro")) +
  geom_violin(aes(x = reorder(micro, macro, fun = mean), y = overlap, color = description), alpha = 0.7) +
  scale_color_viridis(discrete = TRUE, option = "D", guide = guide_legend(title = "Description")) +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-5, 5), expand = FALSE) +
  theme_bw(base_size = 14) +
  geom_errorbar(aes(x = reorder(micro, macro, fun = mean), y = mean, ymin = mean - SE , ymax = mean + SE), 
                data = dscore_data_sum, width = 0.2, color = "black") +
  labs(x = "Micromacro", y = "Overlap")

## ....3C. Violin plots, 3x panels separated by micro ###########

soil <- ggplot(data = filter(dscore_data, micro == "soil"), 
               aes(x = site, y = overlap, fill = factor(macro))) +
  scale_fill_manual(values = c("#307233", "#3e9de0", "#d3c250", "#b0db39", "#812dc6")) +
  geom_violin(aes(x = site, y = overlap), alpha = 0.7, lwd = 1.5, trim = FALSE) +
  # new_scale_color() +
  # scale_fill_manual(values = c("#5f7ba8", "#5f7ba8", "#ed9d2d", "#b21798")) +
  scale_color_viridis(discrete = TRUE, option = "C") +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-5, 5), expand = FALSE) +
  theme_bw(base_size = 14) +
  
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8)) +
  geom_errorbar(aes(x = site, y = mean, ymin = mean - SE , ymax = mean + SE),
                data = filter(dscore_data_sum, micro == "soil"), width = 0.2, 
                color = "black") + labs(x = "Soil")

surface <- ggplot(data = filter(dscore_data, micro == "surface"), 
                  aes(x = site, y = overlap, fill = factor(macro))) +
  scale_fill_manual(values = c("#307233", "#3e9de0", "#d3c250", "#b0db39", "#812dc6"), 
                    guide = guide_legend(title = "macro")) +
  geom_violin(aes(x = site, y = overlap), alpha = 0.7, lwd = 1.5, trim = FALSE) +
  # scale_color_viridis(discrete = TRUE, option = "C", 
  #                     guide = guide_legend(title = "Description")) +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-5, 5), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.x = element_text(size = 8)) +
  geom_errorbar(aes(x = site, y = mean, ymin = mean - SE , ymax = mean + SE),
                data = filter(dscore_data_sum, micro == "surface"), 
                width = 0.2, color = "black") + labs(x = "surface")

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)

canopy <- ggplot(data = filter(dscore_data, micro == "canopy"), 
                 aes(x = site, y = overlap, fill = factor(macro))) +
  scale_fill_manual(values = c("#307233", "#3e9de0", "#d3c250", "#b0db39", "#812dc6"), 
                    guide = guide_legend(title = "macro")) +
  geom_violin(aes(x = site, y = overlap), alpha = 0.7, lwd = 1.5, trim = FALSE) +
  scale_color_viridis(discrete = TRUE, option = "C", 
                      guide = guide_legend(title = "Description")) +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-5, 5), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10),
        axis.text.x = element_text(size = 8)) +
  geom_errorbar(aes(x = site, y = mean, ymin = mean - SE , ymax = mean + SE),
                data = filter(dscore_data_sum, micro == "canopy"), 
                width = 0.2, color = "black") + labs(x = "Canopy")

# lay <- rbind(c(1, 2, 3, 3))
# grid.arrange(grobs = list(soil, surface + theme(legend.position = 'none'), canopy, legend), layout_matrix = lay)


plots <- grid.arrange(soil, surface + theme(legend.position = 'none'), canopy,
                      legend, 
                      nrow = 1,
                      bottom = grid.text(
                        "Microhabtat", gp = gpar(fontsize = 25)))

plots

## ....3D. Boxplots, 3x panels separated by micro ###########

soil <- ggplot(data = filter(dscore_data, micro == "soil"), 
               aes(x = site, y = overlap, fill = seasonal_attributes)) +
  geom_boxplot(aes(x = site, y = overlap), alpha = 0.5, lwd = .5) +
  
  # geom_boxplot(data = filter(dscore_data, micro == "soil", 
  #                            snow == "no snow" & (foliage == "no foliage" |
  #   foliage == "leaf-off")), aes(x = site, y = overlap), alpha = 0.5, fill = "#e08626") +
  # 
  # geom_boxplot(data = filter(dscore_data, micro == "soil", snow == "snowpack"),
  #              aes(x = site, y = overlap), alpha = 0.5, fill = "#3e9de0") +
 
  # Number of colors correspond to seasonal_attribute levels present for soil
  scale_fill_manual(values = c("#3ee0ac", "#3e9de0", "#3e9de0",
                               "#307233", "#e08626", "#e5d35e")) +
  
  coord_cartesian(xlim = NULL, ylim = c(-15, 10), expand = FALSE) +
  theme_bw(base_size = 14) +
  
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8))

surface <- ggplot(data = filter(dscore_data, micro == "surface"), 
                  aes(x = site, y = overlap, fill = seasonal_attributes)) +
  geom_boxplot(aes(x = site, y = overlap), alpha = 0.5, lwd = .5) +
  
  # geom_boxplot(data = filter(dscore_data, micro == "surface", 
  #                            snow == "no snow" & (foliage == "no foliage" |
  #   foliage == "leaf-off")), aes(x = site, y = overlap), alpha = 0.5, fill = "#e08626") +
  # 
  # geom_boxplot(data = filter(dscore_data, micro == "surface", snow == "snowpack"),
  #              aes(x = site, y = overlap), alpha = 0.5, fill = "#3e9de0") +
  
  # Number of colors correspond to seasonal_attribute levels present for surface
  scale_fill_manual(values = c("#3ee0ac", "#3e9de0", "#307233", 
                               "#e08626", "#e5d35e")) +
  
  coord_cartesian(xlim = NULL, ylim = c(-15, 10), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(
    axis.title.y=element_blank(),
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

canopy <- ggplot(data = filter(dscore_data, micro == "canopy"), 
                 aes(x = site, y = overlap, fill = seasonal_attributes)) +
  geom_boxplot(aes(x = site, y = overlap), alpha = 0.5, lwd = .5) +
  
  # geom_boxplot(data = filter(dscore_data, micro == "canopy", 
  #                            snow == "no snow" & (foliage == "no foliage" |
  #   foliage == "leaf-off")), aes(x = site, y = overlap), alpha = 0.5, fill = "#e08626") +
  # 
  # geom_boxplot(data = filter(dscore_data, micro == "canopy", snow == "snowpack"),
  #              aes(x = site, y = overlap), alpha = 0.5, fill = "#3e9de0") +
  
  # Number of colors correspond to seasonal_attribute levels present for canopy
  scale_fill_manual(values = c("#307233", "#e08626")) +
  
  coord_cartesian(xlim = NULL, ylim = c(-15, 10), expand = FALSE) +
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
                        "Microhabtat", gp = gpar(fontsize = 25)))

plots


## ....3E. Violin plots, grouped by macro and by micro ###########

grouped_stats <- dscore_data %>%
  group_by(micro, macro) %>%
  summarize(mean = mean(mean), SE = mean(SE)) %>%
  ungroup()

soil <- ggplot(data = filter(dscore_data, micro == "soil"), 
       aes(x = macro, y = overlap, fill = factor(seasonal_attributes))) +
  geom_violin(aes(x = macro, y = overlap, fill = seasonal_attributes), alpha = 0.7, lwd = 1.5) +
  
  # Number of colors correspond to seasonal_attribute levels present for soil
  scale_fill_manual(values = c("#3e9de0", "#3e9de0", "#3e9de0", 
                               "#307233", "#e08626", "#e08626")) +
  
  coord_cartesian(xlim = NULL, ylim = c(-20, 5), expand = FALSE) +
  theme_bw(base_size = 14) +
  
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8)) +
  labs(x = "Soil")

surface <- ggplot(data = filter(dscore_data, micro == "surface"), 
                  aes(x = macro, y = overlap, fill = factor(seasonal_attributes))) +
  geom_violin(aes(x = macro, y = overlap, fill = seasonal_attributes), alpha = 0.7, lwd = 1.5) +
  
  # Number of colors correspond to seasonal_attribute levels present for surface
  scale_fill_manual(values = c("#3e9de0", "#3e9de0", "#307233", 
                               "#e08626", "#e08626")) +
  
  coord_cartesian(xlim = NULL, ylim = c(-20, 5), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 8)) +  
  labs(x = "Surface")


canopy <- ggplot(data = filter(dscore_data, micro == "canopy"), 
                 aes(x = macro, y = overlap, fill = factor(seasonal_attributes))) +
  geom_violin(aes(x = macro, y = overlap, fill = seasonal_attributes), alpha = 0.7, lwd = 1.5) +
  
  # Number of colors correspond to seasonal_attribute levels present for canopy
  scale_fill_manual(values = c("#307233", "#e08626")) +
  
  coord_cartesian(xlim = NULL, ylim = c(-20, 5), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10)) +
  labs(x = "Canopy")

# lay <- rbind(c(1, 2, 3, 3))
# grid.arrange(grobs = list(soil, surface, canopy), layout_matrix = lay)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)


plots <- grid.arrange(soil, surface, canopy, legend,
                      nrow = 1,
                      bottom = grid.text(
                        "Microhabtat", gp = gpar(fontsize = 25)))

plots


## ....3F. Boxplot plots, grouped by macro and by micro ###########

soil <- ggplot(data = filter(dscore_data, micro == "soil"), 
               aes(x = macro, y = overlap, fill = factor(seasonal_attributes))) +
  geom_boxplot(aes(x = macro, y = overlap), alpha = 0.5, lwd = .5) +
  
  # Number of colors correspond to seasonal_attribute levels present for soil
  scale_fill_manual(values = c("#3ee0ac", "#3e9de0", "#3e9de0",
                               "#307233", "#e08626", "#e5d35e")) +
  
  coord_cartesian(xlim = NULL, ylim = c(-15, 2.5), expand = FALSE) +
  theme_bw(base_size = 14) +
  
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8)) +
  labs(x = "Soil")

surface <- ggplot(data = filter(dscore_data, micro == "surface"), 
                  aes(x = macro, y = overlap, fill = factor(seasonal_attributes))) +
  geom_boxplot(aes(x = macro, y = overlap), alpha = 0.5, lwd = .5) +
  
  # Number of colors correspond to seasonal_attribute levels present for surface
  scale_fill_manual(values = c("#3ee0ac", "#3e9de0", "#307233", 
                               "#e08626", "#e5d35e")) +
  
  coord_cartesian(xlim = NULL, ylim = c(-15, 2.5), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.x = element_text(size = 8)) +
  labs(fill = "Foliage/Snow")
  labs(x = "Surface")

canopy <- ggplot(data = filter(dscore_data, micro == "canopy"), 
                 aes(x = macro, y = overlap, fill = factor(seasonal_attributes))) +
  geom_boxplot(aes(x = macro, y = overlap), alpha = 0.5, lwd = .5) +
  
  # Number of colors correspond to seasonal_attribute levels present for canopy
  scale_fill_manual(values = c("#307233", "#e08626")) +
  
  coord_cartesian(xlim = NULL, ylim = c(-15, 2.5), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10)) +
  labs(x = "Canopy")

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
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size = 8))


plots <- grid.arrange(soil, surface, canopy, legend,
                      nrow = 1,
                      bottom = grid.text(
                        "Microhabtat", gp = gpar(fontsize = 25)))

plots




## ....3G. Plot overlap by elevation difference ########

soil <- ggplot(data = filter(dscore_data, micro == "soil"), aes(x = elevation_change, y = overlap, fill = factor(macro))) +
  scale_fill_manual(values = c("#3e9de0", "#307233", "#d3c250")) +
  geom_violin(aes(x = as.factor(elevation_change), y = overlap, color = description), alpha = 0.7, lwd = 1.5) +
  scale_color_viridis(discrete = TRUE, option = "C") +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = NULL, ylim = c(-5, 5), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  geom_errorbar(aes(x = elevation_change, y = mean, ymin = mean - SE , ymax = mean + SE), 
                data = filter(dscore_data_sum, micro == "soil"), width = 0.2, color = "black") +
  labs(x = "Soil")

## ....3H. Boxplot with overlayed mean and standard error...but no jitter ###########
ggplot(dscore_data, aes(x = micro, y = overlap, fill = factor(macro))) +
  stat_boxplot(aes(micro, overlap), geom='errorbar', linetype=1, width=0.5) +  #whiskers
  geom_boxplot( aes(micro, overlap),outlier.shape=1) +    
  stat_summary(fun.y=mean, geom="point", size=2) + 
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  coord_cartesian(xlim = NULL, ylim = c(-5, 5), expand = FALSE)

leafoff <- filter(dscore_data, site == "N Carolina off", micro == "soil")
leafon <- filter(dscore_data, site == "N Carolina on", micro == "soil")

mean(na.omit(leafoff$overlap))
mean(na.omit(leafon$overlap))


## 4. Quick calculation of average climate isolation #######

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
