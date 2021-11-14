# David Klinges
# 2019-10-30
# This script conducts all kruskal wallis tests reported in the manuscript and
# supplements


# Thermal overlap was higher in non-forested habitats than forested habitats when 
# foliage was present (X^2 = 26.908, P = < 0.001) but not when foliage was absent (x^2 = 1.833, P = 0.1757)?  


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

mountains <- mountains %>% 
  mutate(above_below = ifelse(micro %in% c("canopy", "surface"), "above", "below"))

## 3. Analysis #########

## KDE ###########
## ....A. Foliage ############

# Does foliage status drive overlap
kruskal.test(kde_elevCorr ~ foliage, data = mountains)

# How do forests compare to non-forests
kruskal.test(kde_elevCorr ~ is_forest, data = mountains)

# How do forests (leaf on only) compare to non-forests
kruskal.test(kde_elevCorr ~ is_forest, data = filter(mountains, 
                                                     is_forest == "nonforest" |
                                                       (is_forest == "forest" & foliage == "leaf-on")))

# How do forests (leaf off only) compare to non-forests
kruskal.test(kde_elevCorr ~ is_forest, data = mountains_noFoliage)


# These are for the nested figure asterisks, top panels
kruskal.test(kde_elevCorr ~ is_forest, data = filter(mountains, micro == "soil"))
kruskal.test(kde_elevCorr ~ is_forest, data = filter(mountains, micro == "surface"))
# To confirm that forest can is diff from nonforest surface
kruskal.test(kde_elevCorr ~ is_forest, data = filter(mountains, 
                                                     (micro == "surface" & is_forest == "nonforest" | micro == "canopy" & is_forest == "forest" )))

## ......** Deciduous only #############

# Compare leaf-on to leaf-off for deciduous mountains
kruskal.test(kde_elevCorr ~ foliage, data = deciduous)
# Deciduous, each micro separately
kruskal.test(kde_elevCorr ~ foliage, data = filter(deciduous, micro == "soil"))
kruskal.test(kde_elevCorr ~ foliage, data = filter(deciduous, micro == "surface"))
kruskal.test(kde_elevCorr ~ foliage, data = filter(deciduous, micro == "canopy"))

kruskal.test(kde_elevCorr ~ micro, data = filter(deciduous, foliage == "leaf-on"),
             micro != "canopy")
kruskal.test(kde_elevCorr ~ micro, data = filter(deciduous, foliage == "leaf-on"),
             micro != "soil")
kruskal.test(kde_elevCorr ~ micro, data = filter(deciduous, foliage == "leaf-on"),
             micro != "surface")

kruskal.test(kde_elevCorr ~ micro, data = filter(deciduous, foliage == "leaf-off"),
             micro != "canopy")
kruskal.test(kde_elevCorr ~ micro, data = filter(deciduous, foliage == "leaf-off"),
             micro != "soil")
kruskal.test(kde_elevCorr ~ micro, data = filter(deciduous, foliage == "leaf-off"),
             micro != "surface")

## ....B. Micros ##########

# Compare microhabitats
kruskal.test(kde_elevCorr ~ micro, data = mountains)

# Just two micros at a time
# These are for the nested figure asterisks
kruskal.test(kde_elevCorr ~ micro, data = filter(mountains, micro != "canopy" &
                                                 is_forest == "forest"))
kruskal.test(kde_elevCorr ~ micro, data = filter(mountains, micro != "soil" &
                                                 is_forest == "forest"))
kruskal.test(kde_elevCorr ~ micro, data = filter(mountains, micro != "surface",
                                                 is_forest == "forest"))

kruskal.test(kde_elevCorr ~ micro, data = filter(mountains, is_forest == "nonforest"))

# Compare temperate to tropical mountains
kruskal.test(kde_elevCorr ~ latitude_group, data = filter(mountains, 
                            latitude_group != "polar", foliage == "leaf-on", micro == "canopy"))


kruskal.test(kde_elevCorr ~ latitude_group, data = filter(mountains, 
                                                          latitude_group != "polar", foliage == "leaf-off"))

# Compare microhabitats separately for forests and non-forests
kruskal.test(kde_elevCorr ~ micro, data = filter(mountains, is_forest == "forest"))
kruskal.test(kde_elevCorr ~ micro, data = filter(mountains, is_forest == "nonforest"))
kruskal.test(kde_elevCorr ~ micro, data = filter(mountains, foliage == "leaf-on"))


# Compare aboveground to belowground
kruskal.test(kde_elevCorr ~ above_below, data = mountains)



## TAD ###########
## ....A. Foliage ############

# Does foliage status drive overlap
kruskal.test(TAD ~ foliage, data = mountains)

# How do forests compare to non-forests
kruskal.test(TAD ~ is_forest, data = mountains)

# How do leaf-off forests compare to non-forests
kruskal.test(TAD ~ is_forest, data = mountains_noFoliage)

# Compare leaf-on to leaf-off for deciduous mountains
kruskal.test(TAD ~ foliage, data = deciduous)

# These are for the nested figure asterisks
kruskal.test(TAD ~ is_forest, data = filter(mountains, micro == "soil"))
kruskal.test(TAD ~ is_forest, data = filter(mountains, micro == "surface"))
# To confirm that forest can is diff from nonforest surface
kruskal.test(TAD ~ is_forest, data = filter(mountains, 
    (micro == "surface" & is_forest == "nonforest" | micro == "canopy" & is_forest == "forest" )))

## ....B. Micros ##########

# Compare microhabitats
kruskal.test(TAD ~ micro, data = mountains)

# Just two micros at a time
# These are for the nested figure asterisks
kruskal.test(TAD ~ micro, data = filter(mountains, micro != "canopy" &
                                                   is_forest == "forest"))
kruskal.test(TAD ~ micro, data = filter(mountains, micro != "soil" &
                                                   is_forest == "forest"))
kruskal.test(TAD ~ micro, data = filter(mountains, micro != "surface",
                                                 is_forest == "forest"))

kruskal.test(TAD ~ micro, data = filter(mountains, is_forest == "nonforest"))

# Compare temperate to tropical mountains
kruskal.test(TAD ~ latitude_group, data = filter(mountains, 
                                                          latitude_group != "polar", foliage == "leaf-on", micro == "canopy"))


kruskal.test(TAD ~ latitude_group, data = filter(mountains, 
                                                          latitude_group != "polar", foliage == "leaf-off"))

# Compare microhabitats separately for forests and non-forests
kruskal.test(TAD ~ micro, data = filter(mountains, is_forest == "forest"))
kruskal.test(TAD ~ micro, data = filter(mountains, is_forest == "nonforest"))
kruskal.test(TAD ~ micro, data = filter(mountains, foliage == "leaf-on"))




