# David Klinges
# dklinges@ufl.edu

# This script generates the thermal distributions for select sites

## 1. Workspace prep ############

library(tidyverse)
library(RColorBrewer)
library(ggnewscale)
library(viridis)
library(grid)
library(gridExtra)
library(e1071)
library(cowplot)

## Import wide data ##########

mountains_wide <- read_csv("data/03_compiled/mountains_wide_avg.csv",
                           col_types = cols(
                             low_canopy_min = col_double(),
                             low_canopy_mean = col_double(),
                             low_canopy_max = col_double(),
                             high_canopy_min = col_double(),
                             high_canopy_mean = col_double(),
                             high_canopy_max = col_double()
                           ))

mountains <- read_csv("data/03_compiled/mountains.csv")

canopy <- filter(mountains, micro == "canopy")

canopy %>% 
  group_by(site) %>% 
  count()

## Select few sites 
# mountains_wide <- mountains_wide %>% 
#   filter(site %in% list("NC", "CO", "AU"))

## Curate ####################

# Subset to just sites of interest
mountains_wide <- mountains_wide %>% 
  filter(site %in% c("AU", "NC"))

mountains_gather <- mountains_wide %>% 
  group_by(site, foliage, snow_presence) %>% 
  gather(key = "minmax", value = "low_soil", low_soil_min, low_soil_mean, low_soil_max) %>%
  gather(key = "minmax", value = "high_soil", high_soil_min, high_soil_mean, high_soil_max) %>%
  gather(key = "minmax", value = "low_surface", low_surface_min, low_surface_mean, low_surface_max) %>%
  gather(key = "minmax", value = "high_surface", high_surface_min, high_surface_mean, high_surface_max) %>%
  gather(key = "minmax", value = "low_canopy", low_canopy_min, low_canopy_mean, low_canopy_max) %>%
  gather(key = "minmax", value = "high_canopy", high_canopy_min, high_canopy_mean, high_canopy_max)

mountains_gather_centered <- mountains_gather %>% 
  group_by(site, foliage) %>% 
  mutate(low_soil = (low_soil + high_soil)/2 - low_soil) %>% 
  mutate(high_soil = (low_soil + high_soil)/2 - high_soil) %>% 
  mutate(low_surface = (low_surface + high_surface)/2 - low_surface) %>% 
  mutate(high_surface = (low_surface + high_surface)/2 - high_surface) %>% 
  mutate(low_canopy = (low_canopy + high_canopy)/2 - low_canopy) %>% 
  mutate(high_canopy = (low_canopy + high_canopy)/2 - high_canopy)  %>% 
  ungroup()

low_centered <- mountains_gather_centered %>% 
  dplyr::select(-contains("high")) %>% 
  mutate(elevation = "low")

high_centered <- mountains_gather_centered %>% 
  dplyr::select(-contains("low")) %>% 
  mutate(elevation = "high")

mountains_gather_centered <- low_centered %>% 
  bind_rows(high_centered)

low <- mountains_gather %>% 
  dplyr::select(-contains("high")) %>% 
  mutate(elevation = "low")

high <- mountains_gather %>% 
  dplyr::select(-contains("low")) %>% 
  mutate(elevation = "high")

mountains_gather <- low %>% 
  bind_rows(high)

## Lump all micros together 

mountains_gather_allMicro <- mountains_gather %>% 
  gather(micro, temp_low, low_soil, low_surface, low_canopy)

mountains_gather_allMicro <- mountains_gather_allMicro %>% 
  gather(micro, temp_high, high_soil, high_surface, high_canopy)

mountains_gather_allMicro <- mountains_gather_allMicro %>% 
  ungroup() %>% 
  dplyr::select(temp_low, temp_high, micro, site, foliage, elevation)

## 3. Generate plots ###########


## ....A. Color coded by foliage and micro ##########
## Australia ########

AU_allMicros <- ggplot(data = filter(mountains_gather_allMicro, site == "AU"), 
                        aes(temp_low)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE)  +
  geom_density(data = filter(mountains_gather_allMicro,
                             site == "AU"), 
               aes(temp_high, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "light grey",
                               "high" = "#574a4a")) +
  coord_cartesian(xlim = c(-20, 60), ylim = c(0.00, 0.5), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Tropical Wet Forest") +
  
  theme(legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

AU_soil <- ggplot(data = filter(mountains_gather, site == "AU", 
                                  micro == "soil"), 
                    aes(low_soil)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "AU", 
                             micro == "soil"), 
               aes(high_soil, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "purple",
                               "high" = "blue")) +
  coord_cartesian(xlim = c(-20, 60), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Soil") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

AU_surface <- ggplot(data = filter(mountains_gather, site == "AU", 
                                micro == "surface"), 
                  aes(low_surface)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "AU", 
                             micro == "surface"), 
               aes(high_surface, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "light green",
                               "high" = "dark green")) +
  coord_cartesian(xlim = c(-20, 60), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Surface") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

AU_canopy <- ggplot(data = filter(mountains_gather, site == "AU", 
                                micro == "canopy"), 
                  aes(low_canopy)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "AU", 
                             micro == "canopy"), 
               aes(high_canopy, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "red",
                               "high" = "dark red")) +
  coord_cartesian(xlim = c(-20, 60), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Canopy") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

AU_with_insets <- ggdraw() +
  draw_plot(AU_allMicros) +
  draw_plot(AU_soil, x = 0.06, y = .45, width = .25, height = .38) +
  draw_plot(AU_surface, x = 0.335, y = .45, width = .25, height = .38) +
  draw_plot(AU_canopy, x = 0.61, y = .45, width = .25, height = .38)

## North Carolina #########

NC_allMicros <- ggplot(data = filter(mountains_gather_allMicro, site == "NC"), 
                       aes(temp_low)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE)  +
  geom_density(data = filter(mountains_gather_allMicro,
                             site == "NC"), 
               aes(temp_high, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "light grey",
                               "high" = "#574a4a")) +
  coord_cartesian(xlim = c(-10, 30), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Temperate Deciduous Forest") +
  annotate("text", x = 0, y = .32, label = "Foliage present", size = 4.3) +
  annotate("text", x = 23, y = .32, label = "Foliage absent", size = 4.3) +
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(1,-0.5,1,1), "cm"),
        plot.title = element_text(size = 18, 
                                  margin=margin(0,0,30,0)),)

NC_leafon <- ggplot(data = filter(mountains_gather_allMicro, 
                                  site == "NC", foliage == "leaf-on"), 
                    aes(temp_low)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather_allMicro, site == "NC", 
                             foliage == "leaf-on"), 
               aes(temp_high, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "#22b9c7",
                               "high" = "#2c8196")) +
  coord_cartesian(xlim = c(-10, 30), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Foliage present") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

NC_leafoff <- ggplot(data = filter(mountains_gather_allMicro, 
                                   site == "NC", foliage == "leaf-off"), 
                    aes(temp_low)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather_allMicro, 
                             site == "NC", foliage == "leaf-off"), 
               aes(temp_high, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "#ebb931",
                               "high" = "dark orange")) +
  coord_cartesian(xlim = c(-10, 30), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Foliage absent") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

NC_leafon_soil <- ggplot(data = filter(mountains_gather, site == "NC", 
                                       foliage == "leaf-on", micro == "soil"), 
                  aes(low_soil)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "NC", 
                             foliage == "leaf-on", micro == "soil"), 
               aes(high_soil, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "purple",
                               "high" = "blue")) +
  coord_cartesian(xlim = c(-10, 30), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Foliage present\nSoil") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

NC_leafon_surface <- ggplot(data = filter(mountains_gather, site == "NC", 
                                  foliage == "leaf-on", micro == "surface"), 
                     aes(low_surface)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "NC", 
                             foliage == "leaf-on", micro == "surface"), 
               aes(high_surface, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "light green",
                               "high" = "dark green")) +
  coord_cartesian(xlim = c(-10, 30), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Surface") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

NC_leafon_canopy <- ggplot(data = filter(mountains_gather, site == "NC", 
                                 foliage == "leaf-on", micro == "canopy"), 
                    aes(low_canopy)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "NC", 
                             foliage == "leaf-on", micro == "canopy"), 
               aes(high_canopy, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "red",
                               "high" = "dark red")) +
  coord_cartesian(xlim = c(-10, 30), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Canopy") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

NC_leafoff_soil <- ggplot(data = filter(mountains_gather, site == "NC", 
                                        foliage == "leaf-off", micro == "soil"), 
                         aes(low_soil)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "NC", 
                             foliage == "leaf-off", micro == "soil"), 
               aes(high_soil, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "purple",
                               "high" = "blue")) +
  coord_cartesian(xlim = c(-10, 30), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Foliage absent\nSoil") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

NC_leafoff_surface <- ggplot(data = filter(mountains_gather, site == "NC", 
                                  foliage == "leaf-off", micro == "surface"), 
                            aes(low_surface)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "NC", 
                             foliage == "leaf-off", micro == "surface"), 
               aes(high_surface, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "light green",
                               "high" = "dark green")) +
  coord_cartesian(xlim = c(-10, 30), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Surface") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

NC_leafoff_canopy <- ggplot(data = filter(mountains_gather, site == "NC", 
                                foliage == "leaf-off", micro == "canopy"), 
                           aes(low_canopy)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "NC", 
                             foliage == "leaf-off", micro == "canopy"), 
               aes(high_canopy, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "red",
                               "high" = "dark red")) +
  coord_cartesian(xlim = c(-10, 30), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Canopy") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

NC_with_insets <- ggdraw() +
  draw_plot(NC_allMicros) +
  draw_plot(NC_leafon, x = 0.05, y = .35, width = .4, height = .3) +
  draw_plot(NC_leafoff, x = 0.47, y = .35, width = .4, height = .3) +
  draw_plot(NC_leafon_soil, x = 0.05, y = .65, width = .15, height = .28) +
  draw_plot(NC_leafon_surface, x = 0.17, y = .65, width = .15, height = .28) +
  draw_plot(NC_leafon_canopy, x = 0.32, y = .65, width = .15, height = .28) +
  draw_plot(NC_leafoff_soil, x = 0.47, y = .65, width = .15, height = .28) +
  draw_plot(NC_leafoff_surface, x = 0.6, y = .65, width = .15, height = .28) +
  draw_plot(NC_leafoff_canopy, x = 0.74, y = .65, width = .15, height = .28)

# Extract legend 
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

soil_legend <- g_legend(tropical_soil)

tropical_soil <- tropical_soil +
  theme(legend.position = "none")

soil_plots <- grid.arrange(tropical_soil, NC_with_insets_soil, ID_with_insets_soil,
                           nrow = 3, top = grid.text(
                             "Soil", 
                             gp = gpar(fontsize = 20)))

soil_plots_with_legend <- grid.arrange(soil_plots, soil_legend, nrow = 1,
                                       widths = c(7, 1),
                                       top = grid.text(
                                         "Soil Temperatures", gp = gpar(fontsize = 16)))


## Arrange ################

margin = theme(plot.margin = unit(c(.3), "cm"))
grid.arrange(grobs = lapply(pl, "+", margin))

NC_AU <- grid.arrange(AU_with_insets, NC_with_insets, nrow = 2,
                      padding = 0.25)

ggsave(plot = NC_with_insets, filename = "figures/figure_6/NC_nested.png")
ggsave(plot = AU_with_insets, filename = "figures/figure_6/AU_nested.png")
ggsave(plot = NC_AU, filename = "figures/figure_6/NC_Au_nested.png",
       width = 10.1, height = 8.3)


## ....B. Color coded by amount of overlap #########
## Australia ########

AU_allMicros <- ggplot(data = filter(mountains_gather_allMicro, site == "AU"), 
                       aes(temp_low)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE)  +
  geom_density(data = filter(mountains_gather_allMicro,
                             site == "AU"), 
               aes(temp_high, fill = elevation),
               alpha=.33, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "#c7411c",
                               "high" = "#1f8abf")) +
  coord_cartesian(xlim = c(-10, 50), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  # labs(title = "Tropical Wet Forest") +
  theme(legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 18, 
                                  margin = margin(30,0,30,0)))

AU_soil <- ggplot(data = filter(mountains_gather, site == "AU", 
                                micro == "soil"), 
                  aes(low_soil)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "AU", 
                             micro == "soil"), 
               aes(high_soil, fill = elevation),
               alpha=.4, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "#c7411c",
                               "high" = "#1f8abf")) +
  coord_cartesian(xlim = c(-10, 50), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Aa. Soil") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

AU_surface <- ggplot(data = filter(mountains_gather, site == "AU", 
                                   micro == "surface"), 
                     aes(low_surface)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "AU", 
                             micro == "surface"), 
               aes(high_surface, fill = elevation),
               alpha=.3, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "#c7411c",
                               "high" = "#1f8abf")) +
  coord_cartesian(xlim = c(-10, 50), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Ab. Surface") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

AU_canopy <- ggplot(data = filter(mountains_gather, site == "AU", 
                                  micro == "canopy"), 
                    aes(low_canopy)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "AU", 
                             micro == "canopy"), 
               aes(high_canopy, fill = elevation),
               alpha=.3, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "#c7411c",
                               "high" = "#1f8abf")) +
  coord_cartesian(xlim = c(-10, 50), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Ac. Canopy") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

AU_with_insets <- ggdraw() +
  draw_plot(AU_allMicros) +
  draw_plot(AU_soil, x = 0.06, y = .5, width = .25, height = .38) +
  draw_plot(AU_surface, x = 0.335, y = .5, width = .25, height = .38) +
  draw_plot(AU_canopy, x = 0.61, y = .5, width = .25, height = .38)

## North Carolina #########

NC_allMicros <- ggplot(data = filter(mountains_gather_allMicro, site == "NC"), 
                       aes(temp_low)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha = .2, 
               show.legend = TRUE)  +
  geom_density(data = filter(mountains_gather_allMicro,
                             site == "NC"), 
               aes(temp_high, fill = elevation),
               alpha=.25, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "#c7411c",
                               "high" = "#1f8abf")) +
  coord_cartesian(xlim = c(-10, 50), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  # labs(title = "Temperate Deciduous Forest") +

  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 18, 
                                  margin = margin(30,0,30,0)))

NC_leafon <- ggplot(data = filter(mountains_gather_allMicro, 
                                  site == "NC", foliage == "leaf-on"), 
                    aes(temp_low)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather_allMicro, site == "NC", 
                             foliage == "leaf-on"), 
               aes(temp_high, fill = elevation),
               alpha=.35, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "#c7411c",
                               "high" = "#1f8abf")) +
  coord_cartesian(xlim = c(-10, 50), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Bd. All microhabitats") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

NC_leafoff <- ggplot(data = filter(mountains_gather_allMicro, 
                                   site == "NC", foliage == "leaf-off"), 
                     aes(temp_low)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather_allMicro, 
                             site == "NC", foliage == "leaf-off"), 
               aes(temp_high, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "#c7411c",
                               "high" = "#1f8abf")) +
  coord_cartesian(xlim = c(-10, 50), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Bh. All microhabitats") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

NC_leafon_soil <- ggplot(data = filter(mountains_gather, site == "NC", 
                                       foliage == "leaf-on", micro == "soil"), 
                         aes(low_soil)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "NC", 
                             foliage == "leaf-on", micro == "soil"), 
               aes(high_soil, fill = elevation),
               alpha=.5, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "#c7411c",
                               "high" = "#1f8abf")) +
  coord_cartesian(xlim = c(-10, 50), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Ba. Soil") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

NC_leafon_surface <- ggplot(data = filter(mountains_gather, site == "NC", 
                                          foliage == "leaf-on", micro == "surface"), 
                            aes(low_surface)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "NC", 
                             foliage == "leaf-on", micro == "surface"), 
               aes(high_surface, fill = elevation),
               alpha=.35, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "#c7411c",
                               "high" = "#1f8abf")) +
  coord_cartesian(xlim = c(-10, 50), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Bb. Surface") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

NC_leafon_canopy <- ggplot(data = filter(mountains_gather, site == "NC", 
                                         foliage == "leaf-on", micro == "canopy"), 
                           aes(low_canopy)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "NC", 
                             foliage == "leaf-on", micro == "canopy"), 
               aes(high_canopy, fill = elevation),
               alpha=.3, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "#c7411c",
                               "high" = "#1f8abf")) +
  coord_cartesian(xlim = c(-10, 50), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Bc. Canopy") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

NC_leafoff_soil <- ggplot(data = filter(mountains_gather, site == "NC", 
                                        foliage == "leaf-off", micro == "soil"), 
                          aes(low_soil)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "NC", 
                             foliage == "leaf-off", micro == "soil"), 
               aes(high_soil, fill = elevation),
               alpha=.27, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "#c7411c",
                               "high" = "#1f8abf")) +
  coord_cartesian(xlim = c(-10, 50), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Be. Soil") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

NC_leafoff_surface <- ggplot(data = filter(mountains_gather, site == "NC", 
                                           foliage == "leaf-off", micro == "surface"), 
                             aes(low_surface)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "NC", 
                             foliage == "leaf-off", micro == "surface"), 
               aes(high_surface, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "#c7411c",
                               "high" = "#1f8abf")) +
  coord_cartesian(xlim = c(-10, 50), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Bf. Surface") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

NC_leafoff_canopy <- ggplot(data = filter(mountains_gather, site == "NC", 
                                          foliage == "leaf-off", micro == "canopy"), 
                            aes(low_canopy)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "NC", 
                             foliage == "leaf-off", micro == "canopy"), 
               aes(high_canopy, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "#c7411c",
                               "high" = "#1f8abf")) +
  coord_cartesian(xlim = c(-10, 50), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Bg. Canopy") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

NC_with_insets <- ggdraw() +
  draw_plot(NC_allMicros) +
  draw_plot(NC_leafon, x = 0.05, y = .35, width = .4, height = .3) +
  draw_plot(NC_leafoff, x = 0.47, y = .35, width = .4, height = .3) +
  draw_plot(NC_leafon_soil, x = 0.05, y = .65, width = .15, height = .28) +
  draw_plot(NC_leafon_surface, x = 0.17, y = .65, width = .15, height = .28) +
  draw_plot(NC_leafon_canopy, x = 0.32, y = .65, width = .15, height = .28) +
  draw_plot(NC_leafoff_soil, x = 0.47, y = .65, width = .15, height = .28) +
  draw_plot(NC_leafoff_surface, x = 0.6, y = .65, width = .15, height = .28) +
  draw_plot(NC_leafoff_canopy, x = 0.74, y = .65, width = .15, height = .28)

# Extract legend 
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

soil_legend <- g_legend(tropical_soil)

tropical_soil <- tropical_soil +
  theme(legend.position = "none")

soil_plots <- grid.arrange(tropical_soil, NC_with_insets_soil, ID_with_insets_soil,
                           nrow = 3, top = grid.text(
                             "Soil", 
                             gp = gpar(fontsize = 20)))

soil_plots_with_legend <- grid.arrange(soil_plots, soil_legend, nrow = 1,
                                       widths = c(7, 1),
                                       top = grid.text(
                                         "Soil Temperatures", gp = gpar(fontsize = 16)))


## Arrange ################

NC_AU <- grid.arrange(AU_with_insets, NC_with_insets, nrow = 2)

ggsave(plot = NC_with_insets, filename = "figures/figure_6/NC_nested.png")
ggsave(plot = AU_with_insets, filename = "figures/figure_6/AU_nested.png")
ggsave(plot = NC_AU, filename = "figures/figure_6/NC_Au_nested.png",
       width = 10.1, height = 8.3)

ggsave(plot = NC_allMicros, filename = "figures/figure_6/nc.png")

AU_plots <- grid.arrange(AU_allMicros, AU_soils, ID_with_insets_soil,
                         nrow = 3, top = grid.text(
                           "Soil", 
                           gp = gpar(fontsize = 20)))




## Oregon #########

OR_allMicros <- ggplot(data = filter(mountains_gather_allMicro, site == "OR"), 
                       aes(temp_low)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE)  +
  geom_density(data = filter(mountains_gather_allMicro,
                             site == "OR"), 
               aes(temp_high, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "purple",
                               "high" = "blue")) +
  coord_cartesian(xlim = c(-20, 60), ylim = c(0.00, 0.5), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Temperate Old-Growth Coniferous") +
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

OR_soil <- ggplot(data = filter(mountains_gather, site == "OR", 
                                micro == "soil"), 
                  aes(low_soil)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "OR", 
                             micro == "soil"), 
               aes(high_soil, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "purple",
                               "high" = "blue")) +
  coord_cartesian(xlim = c(-20, 60), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Soil") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

OR_surface <- ggplot(data = filter(mountains_gather, site == "OR", 
                                   micro == "surface"), 
                     aes(low_surface)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "OR", 
                             micro == "surface"), 
               aes(high_surface, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "purple",
                               "high" = "blue")) +
  coord_cartesian(xlim = c(-20, 60), ylim = c(0.00, 0.3), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "surface") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")


OR_with_insets <- ggdraw() +
  draw_plot(OR_allMicros) +
  draw_plot(OR_soil, x = 0.1, y = .45, width = .25, height = .25) +
  draw_plot(OR_surface, x = 0.375, y = .45, width = .25, height = .25)


## Arrange ################

NC_AU <- grid.arrange(AU_with_insets, NC_with_insets, nrow = 2)

ggsave(plot = NC_with_insets, filename = "figures/figure_6/NC_nested.png")
ggsave(plot = AU_with_insets, filename = "figures/figure_6/AU_nested.png")
ggsave(plot = NC_AU, filename = "figures/figure_6/NC_Au_nested.png",
       width = 10.1, height = 8.3)

AU_plots <- grid.arrange(AU_allMicros, AU_soils, ID_with_insets_soil,
                           nrow = 3, top = grid.text(
                             "Soil", 
                             gp = gpar(fontsize = 20)))


## Plot ####################
## 1. Soil panels ############
## ....Tropics ################

tropical_soil <- ggplot(data = filter(mountains_gather, site == "AU"), 
       aes(low_soil)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE)  +
  geom_density(data = filter(mountains_gather,
                             site == "AU"), 
               aes(high_soil, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "purple",
                               "high" = "blue")) +
  coord_cartesian(xlim = c(-5, 25), ylim = c(0.00, 0.5), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Tropical Wet Forest") +
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

## ....NC plots #################

NC_allyear <- ggplot(data = filter(mountains_gather, site == "NC"), aes(low_soil)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE)  +
  geom_density(data = filter(mountains_gather, site == "NC"), 
               aes(high_soil, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "purple",
                               "high" = "blue")) +
  coord_cartesian(xlim = c(-5, 25), ylim = c(0.00, 0.5), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Temperate Deciduous Forest") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")

NC_leafon <- ggplot(data = filter(mountains_gather, site == "NC", foliage == "leaf-on"), 
       aes(low_soil)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "NC", foliage == "leaf-on"), 
               aes(high_soil, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "purple",
                               "high" = "blue")) +
  coord_cartesian(xlim = c(-5, 25), ylim = c(0.00, 0.5), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Foliage present") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

NC_leafoff <- ggplot(data = filter(mountains_gather, site == "NC", foliage == "leaf-off"), 
       aes(low_soil)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE)  +
  geom_density(data = filter(mountains_gather, site == "NC", foliage == "leaf-off"), 
               aes(high_soil, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "purple",
                               "high" = "blue")) +
  coord_cartesian(xlim = c(-5, 25), ylim = c(0.00, 0.5), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Foliage absent") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

NC_with_insets_soil <- ggdraw() +
  draw_plot(NC_allyear) +
  draw_plot(NC_leafoff, x = 0.12, y = .45, width = .4, height = .4) +
  draw_plot(NC_leafon, x = 0.55, y = .45, width = .4, height = .4)

## ....Idaho soil plots #################

# Snow on plot
ID_allyear <- ggplot(data = filter(mountains_gather, site == "CO")) +
  geom_density(aes(low_soil, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "CO"), 
               aes(high_soil, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "purple",
                               "high" = "blue")) +
  coord_cartesian(xlim = c(-5, 25), ylim = c(0.00, 0.5), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Temperate Scrub Shrub") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")

ID_snow <- ggplot(data = filter(mountains_gather, site == "CO", 
                     snow_presence == "snow")) +
  geom_density(aes(low_soil, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  
  geom_density(data = filter(mountains_gather, site == "CO", 
                             snow_presence == "snow"), 
               aes(high_soil, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "purple",
                               "high" = "blue")) +
  coord_cartesian(xlim = c(-5, 25), ylim = c(0.00, 0.5), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Snow present") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")
  
ID_nosnow <- ggplot(data = filter(mountains_gather, site == "CO", 
                                snow_presence == "no snow")) +
  geom_density(aes(low_soil, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  
  geom_density(data = filter(mountains_gather, site == "CO", 
                             snow_presence == "no snow"), 
               aes(high_soil, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "purple",
                               "high" = "blue")) +
  coord_cartesian(xlim = c(-5, 25), ylim = c(0.00, 0.5), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Snow absent") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

ID_with_insets_soil <- ggdraw() +
  draw_plot(ID_allyear) +
  draw_plot(ID_nosnow, x = 0.12, y = .45, width = .4, height = .4) +
  draw_plot(ID_snow, x = 0.55, y = .45, width = .4, height = .4)



## Arrange ################

# Extract legend 
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

soil_legend <- g_legend(tropical_soil)

tropical_soil <- tropical_soil +
  theme(legend.position = "none")

soil_plots <- grid.arrange(tropical_soil, NC_with_insets_soil, ID_with_insets_soil,
                      nrow = 3, top = grid.text(
                        "Soil", 
                        gp = gpar(fontsize = 20)))

soil_plots_with_legend <- grid.arrange(soil_plots, soil_legend, nrow = 1,
                      widths = c(7, 1),
                      top = grid.text(
                        "Soil Temperatures", gp = gpar(fontsize = 16)))

ggsave(plot = soil_plots_with_legend, "figures/figure_6/select_sites_soil.png",
       width = 5.4, height = 9.2)


## 2. Surface panels ############
## ....Tropics ################

tropical_surface <- ggplot(data = filter(mountains_gather, site == "AU"), 
                        aes(low_surface)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE)  +
  geom_density(data = filter(mountains_gather,
                             site == "AU"), 
               aes(high_surface, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "light green",
                               "high" = "dark green")) +
  coord_cartesian(xlim = c(-12, 32), ylim = c(0.00, 0.5), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Tropical Wet Forest") +
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

## ....NC plots #################

NC_allyear <- ggplot(data = filter(mountains_gather, site == "NC"), aes(low_surface)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE)  +
  geom_density(data = filter(mountains_gather, site == "NC"), 
               aes(high_surface, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "light green",
                               "high" = "dark green")) +
  coord_cartesian(xlim = c(-12, 32), ylim = c(0.00, 0.5), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Temperate Deciduous Forest") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")

NC_leafon <- ggplot(data = filter(mountains_gather, site == "NC", foliage == "leaf-on"), 
                    aes(low_surface)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE)  +
  geom_density(data = filter(mountains_gather, site == "NC", foliage == "leaf-on"), 
               aes(high_surface, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "light green",
                               "high" = "dark green")) +
  coord_cartesian(xlim = c(-12, 32), ylim = c(0.00, 0.5), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Foliage present") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

NC_leafoff <- ggplot(data = filter(mountains_gather, site == "NC", foliage == "leaf-off"), 
                     aes(low_surface)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE)  +
  geom_density(data = filter(mountains_gather, site == "NC", foliage == "leaf-off"), 
               aes(high_surface, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "light green",
                               "high" = "dark green")) +
  coord_cartesian(xlim = c(-12, 32), ylim = c(0.00, 0.5), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Foliage absent") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

NC_with_insets_surface <- ggdraw() +
  draw_plot(NC_allyear) +
  draw_plot(NC_leafoff, x = 0.12, y = .45, width = .4, height = .4) +
  draw_plot(NC_leafon, x = 0.55, y = .45, width = .4, height = .4)

## ....Idaho surface plots #################

# Snow on plot
ID_allyear <- ggplot(data = filter(mountains_gather, site == "CO")) +
  geom_density(aes(low_surface, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  geom_density(data = filter(mountains_gather, site == "CO"), 
               aes(high_surface, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "light green",
                               "high" = "dark green")) +
  coord_cartesian(xlim = c(-12, 32), ylim = c(0.00, 0.5), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Temperate Scrub Shrub") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")

ID_snow <- ggplot(data = filter(mountains_gather, site == "CO", 
                                snow_presence == "snow")) +
  geom_density(aes(low_surface, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  
  geom_density(data = filter(mountains_gather, site == "CO", 
                             snow_presence == "snow"), 
               aes(high_surface, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "light green",
                               "high" = "dark green")) +
  coord_cartesian(xlim = c(-12, 32), ylim = c(0.00, 0.5), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Snow present") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

ID_nosnow <- ggplot(data = filter(mountains_gather, site == "CO", 
                                  snow_presence == "no snow")) +
  geom_density(aes(low_surface, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  
  geom_density(data = filter(mountains_gather, site == "CO", 
                             snow_presence == "no snow"), 
               aes(high_surface, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "light green",
                               "high" = "dark green")) +
  coord_cartesian(xlim = c(-12, 32), ylim = c(0.00, 0.5), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Snow absent") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

ID_with_insets_surface <- ggdraw() +
  draw_plot(ID_allyear) +
  draw_plot(ID_nosnow, x = 0.12, y = .45, width = .4, height = .4) +
  draw_plot(ID_snow, x = 0.55, y = .45, width = .4, height = .4)



## Arrange ################

# Extract legend 
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

surface_legend <- g_legend(tropical_surface)

tropical_surface <- tropical_surface +
  theme(legend.position = "none")

surface_plots <- grid.arrange(tropical_surface, NC_with_insets_surface, 
                              ID_with_insets_surface,
                      nrow = 3, top = grid.text(
                        "Surface", 
                        gp = gpar(fontsize = 20)))

surface_plots_with_legend <- grid.arrange(surface_plots, surface_legend, 
                                          nrow = 1,
                                  widths = c(7, 1),
                                  top = grid.text(
                                    "Surface Temperatures", 
                                    gp = gpar(fontsize = 1)))

ggsave(plot = surface_plots_with_legend, "figures/figure_6/select_sites_surface.png",
       width = 5.4, height = 9.2)



## #. Canopy panels ############
## ....Tropics ################

tropical_canopy <- ggplot(data = filter(mountains_gather, site == "AU"), 
                           aes(low_canopy)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE)  +
  geom_density(data = filter(mountains_gather,
                             site == "AU"), 
               aes(high_canopy, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "red",
                               "high" = "dark red")) +
  coord_cartesian(xlim = c(-12, 32), ylim = c(0.00, 0.5), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Tropical Wet Forest") +
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

## ....NC plots #################

NC_allyear <- ggplot(data = filter(mountains_gather, site == "NC"), aes(low_canopy)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE)  +
  geom_density(data = filter(mountains_gather, site == "NC"), 
               aes(high_canopy, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "red",
                               "high" = "dark red")) +
  coord_cartesian(xlim = c(-12, 32), ylim = c(0.00, 0.5), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Deciduous Temperate Forest") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")

NC_leafon <- ggplot(data = filter(mountains_gather, site == "NC", foliage == "leaf-on"), 
                    aes(low_canopy)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE)  +
  geom_density(data = filter(mountains_gather, site == "NC", foliage == "leaf-on"), 
               aes(high_canopy, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "red",
                               "high" = "dark red")) +
  coord_cartesian(xlim = c(-12, 32), ylim = c(0.00, 0.5), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Foliage present") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

NC_leafoff <- ggplot(data = filter(mountains_gather, site == "NC", foliage == "leaf-off"), 
                     aes(low_canopy)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, 
               show.legend = TRUE)  +
  geom_density(data = filter(mountains_gather, site == "NC", foliage == "leaf-off"), 
               aes(high_canopy, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "red",
                               "high" = "dark red")) +
  coord_cartesian(xlim = c(-12, 32), ylim = c(0.00, 0.5), expand = TRUE) +
  theme_classic(base_size = 12) +
  labs(title = "Foliage absent") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size=3),
        title = element_text(size = 8, face="bold"),
        legend.position = "none")

NC_with_insets_canopy <- ggdraw() +
  draw_plot(NC_allyear) +
  draw_plot(NC_leafoff, x = 0.12, y = .45, width = .4, height = .4) +
  draw_plot(NC_leafon, x = 0.55, y = .45, width = .4, height = .4)

## Arrange ################

# Extract legend 
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

canopy_legend <- g_legend(tropical_canopy)

tropical_canopy <- tropical_canopy +
  theme(legend.position = "none")

canopy_plots <- grid.arrange(tropical_canopy, NC_with_insets_canopy,
                      nrow = 2, top = grid.text(
                        "Canopy", 
                        gp = gpar(fontsize = 20)))

canopy_plots_with_legend <- grid.arrange(canopy_plots, canopy_legend, nrow = 1,
                                  widths = c(7, 1),
                                  top = grid.text(
                                    "Canopy Temperatures", gp = gpar(fontsize = 1)))

ggsave(plot = canopy_plots_with_legend, "figures/figure_6/select_sites_canopy.png",
       width = 5.4, height = 9.2)



## Arrange soil, surface, canopy ##########

all_plots <- grid.arrange(soil_plots, surface_plots, canopy_plots,
                             ncol = 3)

all_legends <- grid.arrange(soil_legend, surface_legend, canopy_legend, ncol = 1)

all_plots_with_legends <- grid.arrange(all_plots, 
                                       all_legends, nrow = 1,
                                         widths = c(12, 1))

ggsave(plot = all_plots_with_legends, "figures/figure_6/select_sites_allMicros.png",
       width = 14.8, height = 8.7)

## ....Boulder plots #################

ggplot(data = filter(mountains_gather_centered, site == "CO"), aes(low_soil)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, show.legend = TRUE)  +
  geom_density(data = filter(mountains_gather_centered, site == "CO"), 
               aes(high_soil, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  
  # geom_density(data = filter(mountains_gather_centered, site == "CO"), 
  #              aes(high_surface, fill = elevation),
  #              alpha=.2, show.legend = TRUE) +
  # 
  # geom_density(data = filter(mountains_gather_centered, site == "CO"), 
  #              aes(low_surface, fill = elevation),
  #              alpha=.2, show.legend = TRUE) +
  
  geom_density(data = filter(mountains_gather_centered, site == "CO", snow_presence == "snow"), 
               aes(high_soil, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  
  geom_density(data = filter(mountains_gather_centered, site == "CO", snow_presence == "snow"), 
               aes(low_soil, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  
  scale_fill_manual(values = c("low" = "purple",
                               "high" = "blue")) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.5), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Tropical soil") +
  
  # Add annotation of overlap value
  # annotate("text",
  #          label = as.character((filter(KDE_overlap, 
  #                                       macro == "tropical broadleaf", micro == "soil"))$overlap),
  # x = 30, y = .28) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 3), 
        legend.text = element_text(size = 3),
        legend.key.size = unit(.2,"line"))

