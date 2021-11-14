## David Klinges
## 2019-09-24
## This script generates a figure to be used as an inset in figure 1

## 1. Workspace prep #####################

library(tidyverse)
library(grid)
library(gridExtra)

mountains <- read_csv("data/03_compiled/mountains_overlap_monthly_avg.csv")

## 2. Data curation #############

# Recode factors to capitalize
micro_factors <- c("Canopy", "Surface", "Soil")

mountains <- mountains %>% 
  mutate(micro = tools::toTitleCase(micro)) %>% 
  mutate(micro = factor(micro, levels = micro_factors)) 

  
  
# Group to site/micro averages
# mountains <- mountains %>% 
#   group_by(site, micro, latitude) %>% 
#   summarize(TAD_elevCorr = mean(TAD_elevCorr, na.rm = TRUE))

## 3. Plot data #################

ggplot(mountains, aes(abs(latitude), TAD_elevCorr)) + 
  geom_point(aes(color = micro)) +
  geom_smooth(aes(color = micro), method = "glm", size = 2) +
  scale_color_manual(values = c("Soil" = "#a285fa",
                                "Surface" = "#9dcb9a",
                                "Canopy" = "#d98c8e")) +
  ylab("Thermal Absolute Overlap") +
  xlab("Latitude") +
  theme_bw() + 
  scale_x_continuous(position = "top") +
  theme(legend.position = "bottom",
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20, angle = 45),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16)) +
  coord_flip()

## 4. Write out data ########################

ggsave("figures/latitude/map_inset_latitude.png",
       width = 3.8, height = 9.5)
