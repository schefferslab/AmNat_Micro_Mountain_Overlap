# David Klinges
# 2019-10-30
# This script conducts all univariate regression tests reported in the manuscript 
# and supplements

## 1. Workspace prep ############

library(tidyverse)
library(scales)

mountains <- read_csv("data/03_compiled/mountains_overlap_monthly_avg.csv")

mountains_tall <- read_csv("data/03_compiled/mountains_avg.csv")

## 2. Data Curation ##########

## Calculate minmax variance 

mountains_tall <- mountains_tall %>% 
  # filter out Inf's
  filter(is.finite(min) & is.finite(max)) %>% 
  mutate(minmax = max - min)

# Designate types of mountains
univariate_curation <- function(mountains) {
  mountains <- mountains %>% 
    mutate(is_forest = as.factor(ifelse(macro %in% c("alpine meadow", "hot desert", "scrub shrub", "developed",
                                                     "meadow near forest"), "nonforest",
                                        "forest"))) %>% 
    mutate(foliage = as.factor(foliage)) %>% 
    mutate(latitude_group = ifelse(abs(latitude) < 23.2, "tropical", 
                                   ifelse(abs(latitude) > 23.2 & abs(latitude) < 60,
                                          "temperate", "polar"))) %>% 
    
    mutate(veg_foliage = foliage_cover * veg_structure) %>% 
    
    # Center all variables on zero
    # Re-scale all continuous parameters
    mutate_at(vars(latitude, foliage_cover, veg_structure, 
                   veg_foliage, snowdepth, height), 
              ~ scales::rescale(., to = c(0, 1)))
  
}

mountains <- univariate_curation(mountains)
mountains_tall <- univariate_curation(mountains_tall)

mountains_noFoliage <- mountains %>% 
  filter(foliage == "leaf-off")

deciduous <- mountains %>% 
  filter(macro == "deciduous")

mountains_tall_noFoliage <- mountains_tall %>% 
  filter(foliage == "leaf-off")

deciduous_tall <- mountains_tall %>% 
  filter(macro == "deciduous")


## 3. Analysis ###############

# Linear regression of foliage on overlap for deciduous mountains
foliage_regression <- lm(kde_elevCorr ~ veg_foliage, data = mountains)

foliage_regression <- lm(kde_elevCorr ~ veg_foliage, 
                         data = filter(mountains, micro == "soil"))

foliage_regression <- lm(kde_elevCorr ~ veg_foliage, 
                         data = filter(mountains, micro == "surface"))

foliage_regression <- lm(kde_elevCorr ~ veg_foliage, 
                         data = filter(mountains, micro == "canopy"))

# Linear regression: does foliage cover drive thermal variance
foliage_variance_regression <- lm(minmax ~ veg_foliage, data = mountains_tall)

foliage_variance_regression <- lm(minmax ~ veg_foliage, 
                                  data = filter(mountains_tall, micro == "soil"))

foliage_variance_regression <- lm(minmax ~ veg_foliage, 
                                  data = filter(mountains_tall, micro == "surface"))

foliage_variance_regression <- lm(minmax ~ veg_foliage, 
                                  data = filter(mountains_tall, micro == "canopy"))

# Linear regression of latitude on overlap
latitude_regression <- lm(kde_elevCorr ~ abs(latitude), data = mountains)

# Linear regression of latitude on variance
latitude_variance_regression <- lm(minmax ~ abs(latitude), data = mountains_tall)

# Linear regression of elevation change on overlap
elevChange_regression <- lm(kde_elevCorr ~ elevation_change, data = mountains)

# Linear regression of elevation change on variance
elevChange_variance_regression <- lm(minmax ~ elevation_change, data = mountains_tall)

# Linear regression of snowdepth on overlap
snowdepth_regression <- lm(kde_elevCorr ~ snowdepth, data = mountains)

# Linear regression of snowdepth on thermal variance
snowdepth_variance_regression <- lm(minmax ~ snowdepth, data = mountains_tall)


# Remove tropics
snowdepth_regression <- lm(kde_elevCorr ~ snowdepth, data = filter(mountains, 
                                                          latitude_group != "tropical"))

snowdepth_variance_regression <- lm(minmax ~ snowdepth, data = filter(mountains_tall, 
                                                                      latitude_group != "tropical"))


snowdepthSoil_regression <- lm(TAD ~ snowdepth, data = filter(mountains, micro == "soil", abs(latitude) > 23))
snowdepthSurface_regression <- lm(TAD ~ snowdepth, data = filter(mountains, micro == "surface", abs(latitude) > 23))
snowdepthCanopy_regression <- lm(TAD ~ snowdepth, data = filter(mountains, micro == "canopy", abs(latitude) > 23))

snowdepthSoil_variance_regression <- lm(minmax ~ snowdepth, data = filter(mountains_tall, micro == "soil", abs(latitude) > 23))
snowdepthSurface_variance_regression <- lm(minmax ~ snowdepth, data = filter(mountains_tall))
snowdepthCanopy_variance_regression <- lm(minmax ~ snowdepth, data = filter(mountains_tall, micro == "canopy", abs(latitude) > 23))

# Linear regression of micro height on overlap
micro_regression <- lm(kde_elevCorr ~ height, data = mountains)
micro_variance_regression <- lm(minmax ~ height, data = mountains_tall)

# Linear regression of latitude on snowdepth
snowLatitude_regression <- lm(snowdepth ~ abs(latitude), data = mountains)

micro_regression <- lm(TAD_elevCorr ~ height, data = mountains)

## 4. Visualizations ###############

ggplot(mountains, aes(latitude, TAD)) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, label.x = 0.5) + 
  geom_point()

ggplot(mountains, aes(elevation_change, TAD)) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, label.x = 0.5) + 
  geom_point()

ggplot(filter(mountains, abs(latitude) > 23), aes(snowdepth, TAD)) +
  geom_point()

ggplot(filter(mountains, abs(latitude) > 23), aes(snowdepth, TAD)) +
  geom_point(aes(color = micro)) +
  facet_wrap(~micro)

ggplot(filter(mountains_tall, micro == "soil"), aes(snowdepth, minmax)) +
  geom_point(aes(color = micro))

ggplot(mountains_tall, aes(foliage_cover, minmax)) +
  geom_point() +
  facet_wrap(~micro)

ggplot(mountains, aes(elevation_change, janzenDscore)) +
  geom_point(aes(color = foliage_cover)) +
  facet_wrap(~micro)





