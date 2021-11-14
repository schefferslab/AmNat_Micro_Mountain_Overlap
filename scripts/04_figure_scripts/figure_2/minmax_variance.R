# David Klinges
# File creation date: 2019.05.05
# This script creates minmax variance figures

## Workspace prep ############

library(tidyverse)
library(grid)
library(gridExtra)

mountains_avg <- read_csv("data/03_compiled/mountains_avg.csv")

mountains <- read_csv("data/03_compiled/mountains.csv")

## Data curation #########

## ....A. Calculate minmax variance ##############

mountains_avg <- mountains_avg %>% 
  # filter out Inf's
  filter(is.finite(min) & is.finite(max)) %>% 
  mutate(minmax = max - min)

mountains <- mountains %>% 
  # filter out Inf's
  filter(is.finite(min) & is.finite(max)) %>% 
  mutate(minmax = max - min)

## ....B. Transform snow ##############

mountains_avg <- mountains_avg %>% 
  # Log transform snow
  mutate(snowdepth_log = log(snowdepth)) %>% 
  # Correct for bad logs 
  mutate(snowdepth_log = ifelse(snowdepth_log < -1000, 0, snowdepth_log))

mountains <- mountains %>% 
  # Log transform snow
  mutate(snowdepth_log = log(snowdepth)) %>% 
  # Correct for bad logs 
  mutate(snowdepth_log = ifelse(snowdepth_log < -1000, 0, snowdepth_log))

## ....C. Curate data flags ###############

micro_factors <- c("Soil", "Surface", "Canopy")

mountains_avg <- mountains_avg %>% 
  mutate(micro = tools::toTitleCase(micro)) %>% 
  mutate(micro = factor(micro, levels = micro_factors))

mountains <- mountains %>% 
  mutate(micro = tools::toTitleCase(micro)) %>% 
  mutate(micro = factor(micro, levels = micro_factors))

## ....D. Subset to desired datasets ###############

decid <- mountains_avg %>% 
  filter(macro == "deciduous")

nonforest <- mountains %>% 
  filter(macro %in% c("scrub shrub", "meadow near forest", "alpine meadow", 
                      "hot desert", "developed"))

## Determine cut-off point for snowy sites
snow_sites <- mountains_avg %>% 
  group_by(site) %>% 
  filter(julian < 90 | julian > 275) %>% 
  summarize(avg_snowdepth = mean(snowdepth))

# Sites with an average of at least 5 cm snowdepth between julian days 275 and 90
snowy <- mountains_avg %>% 
  filter(site %in% c("ID", "OR", "CO", "NH_forest", "NH_nonforest",
                     "NM_canopy", "NM_soil", "SE", "AK_nabesna", "AK_ameriflux",
                     "CH_cuona", "CH_conif", "CH_open"))

freeze <- mountains_avg %>% 
  filter(latitude > 40)

freeze <- mountains %>% 
  filter(latitude > 40)

## ....E. Determine avg first and last leaf and snow days ########

foliage_days <- decid %>% 
  mutate(foliage = as.double(dplyr::recode(foliage, "leaf-off" = 0, "leaf-on" = 1))) %>% 
  group_by(julian) %>% 
  summarize(foliage = mean(foliage)) %>% 
  filter(foliage > 0.1)
  
min(foliage_days$julian)
max(foliage_days$julian)

snowy_days <- snowy %>% 
  mutate(snow_presence = as.double(dplyr::recode(snow_presence, "no snow" = 0, 
                                          "snow" = 1))) %>% 
  group_by(julian) %>% 
  summarize(snow_presence = mean(snow_presence)) %>% 
  filter(snow_presence > 0.5)

max(filter(snowy_days, julian < 200)$julian)
min(filter(snowy_days, julian > 200)$julian)

## 3. Create figure #########

## ....A. Foliage plot ###############

minmaxplot_foliage <- ggplot(decid, aes(julian, minmax)) +
  geom_vline(xintercept = min(foliage_days$julian), color = "dark grey", linetype = "dashed") +
  geom_vline(xintercept = max(foliage_days$julian), color = "dark grey", linetype = "dashed") +
  geom_point(aes(color = foliage_cover), alpha = 0.2) +
  scale_color_gradient(low = "#e08626", high = "#307233") +
  scale_y_continuous(breaks = c(0, 10, 20)) +
  theme_classic(base_size = 20) +
  labs(color = "Foliage Cover") +
  # labs(x = "Vegetation Structure Index") +
  xlab("Day of Year") +
  ylab("Thermal Variance (°C)") +
  ggtitle("A.") +
  # Add equation and r-squared as text
  theme(axis.text.y = element_text(size = 20),
        legend.text = element_text(size = 14), 
        axis.text.x = element_text(size = 20, angle = 45, vjust = .6),
        axis.title.y = element_text(size = 22),
        axis.title.x = element_blank()) +
  
  facet_wrap(~micro)

## ....B. Snowdepth plot ###################

minmaxplot_snowy <- ggplot(filter(snowy, complete.cases(snowdepth_log)), aes(julian, minmax)) +
  geom_vline(xintercept = max(filter(snowy_days, julian < 200)$julian), color = "dark grey", linetype = "dashed") +
  geom_vline(xintercept = min(filter(snowy_days, julian > 200)$julian), color = "dark grey", linetype = "dashed") +
  geom_point(aes(color = snowdepth_log), alpha = 0.2) +

  scale_color_gradient2(midpoint = 2.5, low = "#e6f7f6", mid = "#00ccff",
                        high = "#000099", space = "Lab" ) +
  scale_y_continuous(breaks = c(0, 10, 20, 30)) +
  theme_classic(base_size = 20) +
  labs(color = "Log of Snow\nDepth (cm)") +
  # labs(x = "Vegetation Structure Index") +
  xlab("Day of Year") +
  ylab("Thermal Variance (°C)") +
  ggtitle("B.") +
  # Add equation and r-squared as text
  theme(axis.text.y = element_text(size = 20),
        legend.text = element_text(size = 14), 
        axis.text.x = element_text(size = 20, angle = 45, vjust = .6),
        axis.title.y = element_text(size = 22)) +
  facet_wrap(~micro)

## ....C. Freeze plot ###############

minmaxplot_freeze <- ggplot(freeze, aes(julian, minmax)) +
  geom_vline(xintercept = min(foliage_days$julian), color = "dark grey", linetype = "dashed") +
  geom_vline(xintercept = max(foliage_days$julian), color = "dark grey", linetype = "dashed") +
  geom_point(aes(color = max), alpha = 0.4) +
  # scale_color_gradient2(low = "#000099", mid = "light blue", high = "white", 
  #                       midpoint = 50) +
  scale_color_gradient(low = "#88d0fc", high = "#bd320b") +
  # scale_color_gradientn(colors = c("#000099", "#00ccff", "#66ffff")) +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(size=8)) +
  labs(color = "Maximum Daily Temperature") +
  # labs(x = "Vegetation Structure Index") +
  xlab("Day of Year") +
  ylab("Thermal Variance (°C)") +
  # Add equation and r-squared as text
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 10)) +
  
  facet_wrap(~micro)

## ....D. Arrange plots #########

# Extract legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

foliage_legend <- g_legend(minmaxplot_foliage)
snow_legend <- g_legend(minmaxplot_snowy)

minmaxplot_foliage <- minmaxplot_foliage +
  theme(legend.position = "none")

minmaxplot_snowy <- minmaxplot_snowy +
  theme(legend.position = "none")

two_panel_nolegend <- grid.arrange(minmaxplot_foliage, minmaxplot_snowy,
                                   nrow = 2)

legend <- grid.arrange(foliage_legend, snow_legend, nrow = 2)

two_panel <- grid.arrange(two_panel_nolegend, legend, nrow = 1,
                          widths = c(3, 1))

## 4. Write out plot ###########

ggsave(plot = minmaxplot_snowy, filename = "figures/figure_2/snowy_thermal_variance.png")

ggsave(plot = minmaxplot_foliage, filename = "figures/figure_2/deciduous_thermal_variance.png")

ggsave(plot = two_panel, 
       filename = "figures/figure_2/two_panel_thermal_variance.pdf",
       device = "pdf",
       width = 9.58, height = 9.8)

ggplot(snowy, aes(snowdepth)) +
  geom_histogram(bins = 60)


glimpse(mountains_avg)
ggplot(mountains_avg, aes(julian, minmax)) +
  geom_point(aes(color = latitude), alpha = 0.4) +
  theme_bw() +
  facet_wrap(~micro)


