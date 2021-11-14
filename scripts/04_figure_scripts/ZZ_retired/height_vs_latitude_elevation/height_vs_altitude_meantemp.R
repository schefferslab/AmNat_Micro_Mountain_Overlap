## David Klinges
## Data init: 2019-09-01
## This script plots change in mean temp across height and elevation

## 1. Workspace prep #########

mountains <- read_csv("data/03_compiled/mountains_avg.csv")
mountains_overlap <- read_csv("data/03_compiled/mountains_overlap_daily_avg.csv",
                              col_types = cols(height = col_double()))

## 2. Data curation ##########

# Exctact heights from regular database
mountains_height <- mountains %>% 
  dplyr::select(site, height, macro, micro,
                foliage) %>% 
  distinct()

# Join to overlap database
mountains_height <- mountains_overlap %>% 
  dplyr::select(macro, site, year, julian, TAD_elevCorr, janzenDscore_elevCorr,
                micro, foliage, snowdepth) %>% 
  left_join(mountains_height)

## Just forest
forest_mountains <- mountains %>% 
  filter(macro %in% list("Dense coniferous", "deciduous", "tropical broadleaf")) %>% 
  dplyr::rename(mean_temp = mean)

## 3. Plot ###########

## Just forest
elevation_temp <- ggplot(forest_mountains, aes(altitude, mean_temp)) +
  geom_point(aes(color = macro))

height_temp <- ggplot(forest_mountains, aes(height, mean_temp)) +
  geom_point(aes(color = macro))

grid.arrange(elevation_temp, height_temp)

## All biomes
elevation_temp <- ggplot(mountains, aes(altitude, mean)) +
  geom_point(aes(color = macro))

height_temp <- ggplot(mountains, aes(height, mean)) +
  geom_point(aes(color = macro))

grid.arrange(elevation_temp, height_temp)
