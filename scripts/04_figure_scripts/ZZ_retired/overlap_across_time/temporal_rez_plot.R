# David Klinges
# File creation date: 2019-07-03
# This script plots temporal rez data

## 1. Workspace prep #########

library(tidyverse)
library(quantreg)
library(gridExtra)

temporal_rez_data <- read_csv("data/03_compiled/temporal_rez/temporal_rez_data_elevControlled.csv")

mountains <- read_csv("data/03_compiled/mountains_janz_month_avg.csv")
elevation_change <- mountains %>% 
  select(site, elevation_change) %>% 
  distinct()

## Group together macros ################



## 2. Designate sites/seasons of interest ###############

temperate_forest <- temporal_rez_data %>% 
  # filter(macro == "coniferous" | macro == "deciduous") %>% 
  filter(site == "NC")

temperate_nonforest <- temporal_rez_data %>% 
  filter(macro == "non-forest") %>% 
  filter(site %in% list("ID"))

tropical_forest <- temporal_rez_data %>% 
  filter(macro == "evergreen") 

subset <- temporal_rez_data %>% 
  filter(season != "winter")
## 3. Plot data ########

soil <- ggplot(
  ## Plot points ########
  
  ## Non-forest
  ## Winter
  data = filter(subset, micro == "soil"),
  aes(temporal_rez, overlap_v2, shape = season, color = macro)) +
  geom_point(alpha = 0.1) +
  
  geom_point(data = filter(subset, micro == "soil"), 
             aes(temporal_rez, overlap_v2, shape = season, color = macro), 
             alpha = 0.1) +
  
  scale_color_manual(values = c("non-forest" = "tan", "evergreen" = "lightgreen", 
                                "coniferous" = '#ff00ff', "deciduous" = '#3399ff')) +
  scale_shape_manual(values = c("summer" = 17, "winter" = 0,
                                "allyear" = 19)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  labs(title = "All sites soil overlap") +
  ylab("[(max temp high elev) - (min temp low elev)] / change in elevation") +
  xlab("Temporal resolution (timestep over which overlap is calculated)") +
  theme_bw() +
  ylim(c(-0.01, 0.06)) +
  xlim(c(0, 365))
  
  surface <- ggplot(
    ## Plot points ########
    
    ## Non-forest
    ## Winter
    data = filter(subset, micro == "surface"),
    aes(temporal_rez, overlap_v2, shape = season, color = macro)) +
    geom_point(alpha = 0.1) +
    
    geom_point(data = filter(subset, micro == "surface"), 
               aes(temporal_rez, overlap_v2, shape = season, color = macro), 
               alpha = 0.1) +
    
    scale_color_manual(values = c("non-forest" = "tan", "evergreen" = "lightgreen", 
                                  "coniferous" = '#ff00ff', "deciduous" = '#3399ff')) +
    scale_shape_manual(values = c("summer" = 17, "winter" = 0,
                                  "allyear" = 19)) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    labs(title = "All sites surface overlap") +
    ylab("[(max temp high elev) - (min temp low elev)] / change in elevation") +
    xlab("Temporal resolution (timestep over which overlap is calculated)") +
    theme_bw() +
    ylim(c(-0.01, 0.06)) +
    xlim(c(0, 365))

  canopy <- ggplot(
    ## Plot points ########
    
    ## Non-forest
    ## Winter
    data = filter(subset, micro == "canopy"),
    aes(temporal_rez, overlap_v2, shape = season, color = macro)) +
    geom_point(alpha = 0.1) +
    
    geom_point(data = filter(subset, micro == "canopy"), 
               aes(temporal_rez, overlap_v2, shape = season, color = macro), 
               alpha = 0.1) +
    
    scale_color_manual(values = c("non-forest" = "tan", "evergreen" = "lightgreen", 
                                  "coniferous" = '#ff00ff', "deciduous" = '#3399ff')) +
    scale_shape_manual(values = c("summer" = 17, "winter" = 0,
                                  "allyear" = 19)) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    labs(title = "All sites canopy overlap") +
    ylab("[(max temp high elev) - (min temp low elev)] / change in elevation") +
    xlab("Temporal resolution (timestep over which overlap is calculated)") +
    theme_bw() +
    ylim(c(-0.01, 0.06)) +
    xlim(c(0, 365))
  
  grid.arrange(soil, surface, canopy, ncol = 3)
  
  # geom_point(data = filter(temperate_nonforest, season == "winter", 
  # micro == "surface"), aes(temporal_rez, overlap_v2), alpha = 0.05, 
  # color = "DarkCyan") +
  
  ## Summer
  # geom_point(data = filter(temperate_nonforest, season == "summer", 
  # micro == "surface"), aes(temporal_rez, overlap_v2), alpha = 0.05, 
  # color = "DarkGoldenRod") +
  
  # Forest
  ## Winter
  # geom_point(data = filter(temperate_forest, season == "winter", micro == "surface"), 
  #            aes(temporal_rez, overlap_v2), alpha = 0.05) +

  ## Summer
  # geom_point(data = filter(temperate_forest, season == "summer", micro == "surface"), 
  #            alpha = 0.05, color = "DarkGoldenRod") +
  
  geom_point(data = filter(temperate_forest, season == "summer", micro == "soil"),
             aes(temporal_rez, overlap_v2), color = "#85ed8c", alpha = 0.05,
             show.legend = TRUE) +
  
  ## Tropical forest 
  ## Winter
  # geom_point(data = filter(tropical_forest, micro == "surface"), 
  #            aes(temporal_rez, overlap_v2), alpha = 0.05, color = "#99ff33") +
  geom_point(data = filter(tropical_forest, micro == "soil"), 
             aes(temporal_rez, overlap_v2), color = "#663300", alpha = 0.05,
             show.legend = TRUE) +
  
  ## Plot model fits ########
  ## Non-forest
  # geom_smooth(data = filter(temperate_nonforest, season == "winter", micro == "surface"), 
  #           aes(color = "winter surface"), method = "gam", formula = y ~ log(x), 
  #           se = TRUE, show.legend = TRUE) +
  # 
  # geom_smooth(data = filter(temperate_nonforest, season == "winter", micro == "soil"), 
  #             aes(color = "winter soil"), method = "gam", formula = y ~ log(x),
  #             se = TRUE, show.legend = TRUE) +
  # 
  # geom_smooth(data = filter(temperate_nonforest, season == "summer", micro == "surface"), 
  #             aes(color = "summer surface"), method = "gam", formula = y ~ log(x), 
  #             se = TRUE, show.legend = TRUE) +
  # 
  # geom_smooth(data = filter(temperate_nonforest, season == "summer", micro == "soil"), 
  #             aes(color = "summer soil"), method = "gam", formula = y ~ log(x), 
  #             se = TRUE, show.legend = TRUE) +
  # 
  # ## Forest
  # geom_smooth(data = filter(temperate_forest, season == "winter", micro == "surface"), 
  #             aes(color = "winter surface"), method = "gam", formula = y ~ log(x),
  #             se = TRUE, show.legend = TRUE, size = 3) +
  # 
  # geom_smooth(data = filter(temperate_forest, season == "winter", micro == "soil"), 
  #             aes(color = "winter soil"), method = "gam", formula = y ~ log(x),
  #             se = TRUE, show.legend = TRUE, size = 3) +
  # 
  # geom_smooth(data = filter(temperate_forest, season == "summer", micro == "surface"), 
  #             aes(color = "summer surface"), method = "gam", formula = y ~ log(x),
  #             se = TRUE, show.legend = TRUE, size = 3) +
  # 
  # geom_smooth(data = filter(temperate_forest, season == "summer", micro == "soil"), 
  #             aes(color = "summer soil"), method = "gam", formula = y ~ log(x),
  #             se = TRUE, show.legend = TRUE, size = 3) +
  # 
  # ## Tropical forest
  # geom_smooth(data = filter(tropical_forest, micro == "surface"), 
  #             aes(color = "tropical surface"), method = "gam", formula = y ~ log(x),
  #             se = TRUE, show.legend = TRUE, size = 3) +
  # 
  # geom_smooth(data = filter(tropical_forest, micro == "soil"), 
  #             aes(color = "tropical soil"), method = "gam", formula = y ~ log(x),
  #             se = TRUE, show.legend = TRUE, size = 3) +
  
  
  scale_colour_manual(name="legend", values=c("winter surface" = "DarkCyan", 
                                              "winter soil" = "#9fbba2", 
                                              "summer surface" = "DarkGoldenRod",  
                                              "summer soil" = "LightCoral",
                                              "tropical surface" = "#99ff33",
                                              "tropical soil" = "#663300")) +
  
  labs(title = "OR (coniferous forest), AZ (desert), and AU (tropical broadleaf) soil and surface",
       subtitle = "thick lines = forest, thin lines = non-forest") +
  theme_bw() +
  xlim(c(0, 365))
  # ylim(c(-25, 50))

ggsave(filename = "figures/selectSites_temporalRez_overlapV2_controlled.png", 
       plot = plot)





