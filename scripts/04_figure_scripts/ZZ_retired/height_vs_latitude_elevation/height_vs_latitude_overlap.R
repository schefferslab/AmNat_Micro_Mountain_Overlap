## David Klinges
## Data init: 2019-09-01
## This script plots change in thermal range/overlap across height and latitude

## 1. Workspace prep ###########

library(tidyverse)
library(grid)
library(gridExtra)

mountains <- read_csv("data/03_compiled/mountains_avg.csv")
mountains_overlap <- read_csv("data/03_compiled/mountains_overlap_monthly_avg.csv")

## 2. Data curation ##########

# Exctact heights from regular database
mountains_height <- mountains %>% 
  dplyr::select(site, height, macro, micro,
                foliage, latitude) %>% 
  distinct()

# Join to overlap database
mountains_overlap <- mountains_overlap %>% 
  dplyr::select(macro, site, year, month, TAD_elevCorr, janzenDscore_elevCorr,
                micro, foliage, snowdepth) %>% 
  left_join(mountains_height)

# Computer thermal range
mountains <- mountains %>% 
  mutate(thermal_range = max - min)

## 3. Plot ###############

height_range <- ggplot(mountains, aes(height, thermal_range)) + 
  geom_point(aes(color = micro))

latitude_range <- ggplot(mountains, aes(abs(latitude), thermal_range)) + 
  geom_point(aes(color = micro))

height_overlap <- ggplot(filter(mountains_overlap, micro != "canopy"), aes(height, TAD_elevCorr)) + 
  geom_point(aes(color = micro))

latitude_overlap <- ggplot(filter(mountains_overlap, micro == "surface"), aes(abs(latitude), TAD_elevCorr)) + 
  geom_point(aes(color = macro))


plots <- grid.arrange(grobs = list(height_range, latitude_range,
                                   height_overlap, latitude_overlap), 
                      ncol = 2)
