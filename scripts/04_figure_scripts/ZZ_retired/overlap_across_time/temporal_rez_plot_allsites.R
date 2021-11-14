# David Klinges
# File creation date: 2019-07-03
# This script plots temporal rez data

## 1. Workspace prep #########

library(tidyverse)
library(quantreg)
library(grid)
library(gridExtra)

temporal_rez_data <- read_csv("data/03_compiled/temporal_rez/mountains_rez_allyears.csv")

## 2. Data curation ###########

## ....A. Correct for elevation #######
temporal_rez_data <- temporal_rez_data %>% 
  mutate(janzenDscore = janzenDscore * elevation_change  /1000) %>% 
  mutate(TAD = TAD * elevation_change  /1000)

## ....B. Change factors #######3

# Recode factors to capitalize
temporal_rez_data <- temporal_rez_data %>%
  mutate(micro = tools::toTitleCase(micro)) %>% 
  mutate(macro = tools::toTitleCase(macro))

macro_factors <- c("Tropical Broadleaf", "Ponderosa Pine", "Dense Coniferous", 
                   "Deciduous", "Meadow Near Forest", "Developed", "Alpine Meadow", 
                   "Scrub Shrub", "Hot Desert")

micro_factors <- c("Soil", "Surface", "Canopy")

temporal_rez_data <- temporal_rez_data %>%
  mutate(micro = factor(micro, levels = micro_factors)) %>% 
  mutate(macro = factor(macro, levels = macro_factors))

## 3. Plot data ########

## A. Facet by Micro ###########

## .....1. Just soil #########

soil <- ggplot(
  
  ## Non-forest
  ## Winter
  data = filter(temporal_rez_data, micro == "soil"),
  aes(temporal_rez, TAD, color = foliage)) +
  geom_point(alpha = 0.05) +
  
  scale_color_manual(values = c("leaf-on" = "#307233", 
                                "leaf-off" = "#e08626")) +
  
  geom_point(data = filter(temporal_rez_data, micro == "soil"), 
             aes(temporal_rez, TAD,  color = foliage), 
             alpha = 0.05) +
  
  geom_smooth(data = temporal_rez_data, 
             method = "gam", formula = y ~ log(x), color = "black",
             linetype = "dashed", se = TRUE, show.legend = TRUE) +
  
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  labs(title = "Soil") +
  ylab("Thermal Overlap") +
  xlab("Temporal Resolution (Timestep)") +
  theme_bw() +
  ylim(c(-30, 50)) +
  xlim(c(0, 720)) +
  theme(
    # legend.position = "none",
    #     axis.title.x=element_blank()
    )
  
## Facet by just micro ###########
  
all_micros <- ggplot(
  
  ## Non-forest
  ## Winter
  data = temporal_rez_data,
  aes(temporal_rez, TAD, color = foliage)) +
  geom_point(alpha = 0.05) +
  
  geom_point(data = temporal_rez_data, 
             aes(temporal_rez, TAD,  color = foliage), 
             alpha = 0.05) +
  
  scale_color_manual(values = c("leaf-on" = "#307233", 
                                "leaf-off" = "#e08626")) +
  
  geom_smooth(data = temporal_rez_data, 
              method = "gam", formula = y ~ log(x), color = "black",
              linetype = "dashed", se = TRUE, show.legend = TRUE) +
  
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  ylab("Thermal Overlap") +
  xlab("Temporal Resolution (Timestep)") +
  theme_bw() +
  ylim(c(-40, 90)) +
  xlim(c(0, 720)) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
    # legend.position = "none",
    #     axis.title.x=element_blank()
  ) +
  facet_wrap(~micro)

################
  surface <- ggplot(
    
    ## Non-forest
    ## Winter
    data = filter(temporal_rez_data, micro == "surface"),
    aes(temporal_rez, TAD, shape = season, color = macro)) +
    geom_point(alpha = 0.1) +
    
    geom_point(data = filter(temporal_rez_data, micro == "surface"), 
               aes(temporal_rez, TAD, shape = season, color = macro), 
               alpha = 0.1) +
    
    geom_smooth(data = filter(temporal_rez_data, season == "winter"), 
                method = "gam", formula = y ~ log(x), linetype = "dashed",
                se = TRUE, show.legend = TRUE) +
    
    geom_smooth(data = filter(temporal_rez_data, season != "winter"), 
                method = "gam", formula = y ~ log(x),
                se = TRUE, show.legend = TRUE) +
    
    scale_color_manual(values = c("non-forest" = "tan", "evergreen" = "#4fdb5e", 
                                  "coniferous" = '#095411', "deciduous" = '#3399ff')) +
    scale_shape_manual(values = c("summer" = 17, "winter" = 0,
                                  "allyear" = 19)) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    labs(title = "Surface") +
    theme_bw() +
    ylim(c(-40, 90)) +
    xlim(c(0, 365))

  canopy <- ggplot(
    
    ## Non-forest
    ## Winter
    data = filter(temporal_rez_data, micro == "canopy"),
    aes(temporal_rez, TAD, shape = season, color = macro)) +
    geom_point(alpha = 0.1) +
    
    geom_point(data = filter(temporal_rez_data, micro == "canopy"), 
               aes(temporal_rez, TAD, shape = season, color = macro), 
               alpha = 0.1) +
    
    geom_smooth(data = filter(temporal_rez_data, season == "winter"), 
                method = "gam", formula = y ~ log(x), linetype = "dashed",
                se = TRUE, show.legend = TRUE) +
    
    geom_smooth(data = filter(temporal_rez_data, season != "winter"), 
                method = "gam", formula = y ~ log(x),
                se = TRUE, show.legend = TRUE) +
    
    scale_color_manual(values = c("non-forest" = "tan", "evergreen" = "#4fdb5e", 
                                  "coniferous" = '#095411', "deciduous" = '#3399ff')) +
    scale_shape_manual(values = c("summer" = 17, "winter" = 0,
                                  "allyear" = 19)) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    labs(title = "Canopy") +
    theme_bw() +
    ylim(c(-40, 90)) +
    xlim(c(0, 365)) +
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
  
  ggsave(filename = "allsites_TAD.png", 
         plot = grid.arrange(soil, surface, canopy, legend, nrow = 1, 
                             bottom = grid.text(
                               "Temporal resolution (timestep over which overlap is calculated)", gp = gpar(fontsize = 16))))
  

    
    
  
## B. Facet by micro then macro ############
  
  temporal_rez_datat <- temporal_rez_data %>% 
    filter(micro == "soil")
  
canopy <- ggplot(
  ## Plot points ########
    
    data = filter(temporal_rez_data, micro == "canopy"),
    # aes(temporal_rez, TAD, shape = foliage, color = macro)) +
    aes(temporal_rez, TAD, shape = macro, color = foliage, fill = macro)) +
  
  # Winter
    geom_point(data = filter(temporal_rez_data, micro == "canopy", foliage == "winter"),
               alpha = 0.02, size = 1, stroke = ) +
    
    # geom_smooth(data = filter(temporal_rez_data, foliage == "winter"), 
    #             method = "gam", formula = y ~ log(x), linetype = "dashed",
    #             se = TRUE, show.legend = TRUE) +
    # 
    # geom_smooth(data = filter(temporal_rez_data, foliage != "winter"), 
    #             method = "gam", formula = y ~ log(x),
    #             se = TRUE, show.legend = TRUE) +
    facet_wrap(vars(macro), nrow = 1) +
    
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    labs(title = "Canopy") +
    ylab("[(max temp high elev) - (min temp low elev)] * change in elevation / 1000") +
    theme_bw() +
    ylim(c(-40, 90)) +
    xlim(c(0, 365)) +
    # scale_color_manual(values = c("non-forest" = "tan", "evergreen" = "#4fdb5e", 
    #                                 "coniferous" = '#095411', "deciduous" = '#3399ff')) +
    # scale_shape_manual(values = c("summer" = 17, "winter" = 0,
    #                                 "allyear" = 19)) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.y=element_blank())


surface <- ggplot(
  ## Plot points ########
  
  ## Non-forest
  ## Winter
  data = filter(temporal_rez_data, micro == "surface"),
  # aes(temporal_rez, TAD, shape = foliage, color = macro)) +
  aes(temporal_rez, TAD, shape = macro, color = foliage, fill = macro)) +
  geom_point(alpha = 0.02, size = 1) +
  
  # geom_smooth(data = filter(temporal_rez_data, foliage == "winter"), 
  #             method = "gam", formula = y ~ log(x), linetype = "dashed",
  #             se = TRUE, show.legend = TRUE) +
  # 
  # geom_smooth(data = filter(temporal_rez_data, foliage != "winter"), 
  #             method = "gam", formula = y ~ log(x),
  #             se = TRUE, show.legend = TRUE) +
  facet_wrap(vars(macro), nrow = 1) +
  
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  labs(title = "Surface") +
  # ylab("[(max temp high elev) - (min temp low elev)] * change in elevation / 1000") +
  theme_bw() +
  ylim(c(-40, 90)) +
  xlim(c(0, 365)) +
  # scale_color_manual(values = c("non-forest" = "tan", "evergreen" = "#4fdb5e", 
  #                                 "coniferous" = '#095411', "deciduous" = '#3399ff')) +
  # scale_shape_manual(values = c("summer" = 17, "winter" = 0,
  #                                 "allyear" = 19)) +
  theme_bw() +
  theme(axis.title.y=element_blank())


soil <- ggplot(
  ## Plot points ########
  
  ## Non-forest
  ## Winter
  data = filter(temporal_rez_data, micro == "soil"),
  # aes(temporal_rez, TAD, shape = foliage, color = macro)) +
  aes(temporal_rez, TAD, shape = macro, color = foliage, fill = macro)) +
  geom_point(alpha = 0.02, size = 1) +
  
  # geom_smooth(data = filter(temporal_rez_data, foliage == "winter"), 
  #             method = "gam", formula = y ~ log(x), linetype = "dashed",
  #             se = TRUE, show.legend = TRUE) +
  # 
  # geom_smooth(data = filter(temporal_rez_data, foliage != "winter"), 
  #             method = "gam", formula = y ~ log(x),
  #             se = TRUE, show.legend = TRUE) +
  facet_wrap(vars(macro), nrow = 1) +
  
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  labs(title = "Soil") +
  theme_bw() +
  ylim(c(-40, 90)) +

  xlim(c(0, 365)) +
  # scale_color_manual(values = c("non-forest" = "tan", "evergreen" = "#4fdb5e", 
  #                                 "coniferous" = '#095411', "deciduous" = '#3399ff')) +
  # scale_shape_manual(values = c("summer" = 17, "winter" = 0,
  #                                 "allyear" = 19)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.y=element_blank())

    
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(surface)

surface <- surface +
  theme(legend.position = "none")

    ggsave(filename = "allsites_facetTAD.png", 
           plot =     grid.arrange(canopy, surface, soil, legend,
                                   widths = c(4, 1),
                                   ncol = 2,
                                   layout_matrix = rbind(c(1, 4),
                                                         c(2, 4),
                                                         c(3, 4))
           ))
    

    
## 4. Write out plots ############
    
  ggsave(plot = soil, filename = "figures/temporal_overlap/all_sites/soil_temporal_rez_oneSmooth.png")
  ggsave(plot = all_micros, filename = "figures/temporal_overlap/all_sites/all_micros_temporal_rez.png")
    
