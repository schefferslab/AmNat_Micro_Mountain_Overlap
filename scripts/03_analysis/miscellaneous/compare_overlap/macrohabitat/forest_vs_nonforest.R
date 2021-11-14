


# Thermal overlap was higher in non-forested habitats than forested habitats when 
# foliage was present (X^2 = 26.908, P = < 0.001) but not when foliage was absent (x^2 = 1.833, P = 0.1757)?  

## 1. Workspace prep ############

library(tidyverse)


mountains <- read_csv("data/03_compiled/mountains_overlap_monthly_avg.csv")

## 2. Data Curation ##########


mountains <- mountains %>% 
  mutate(is_forest = as.factor(ifelse(macro %in% c("alpine meadow", "hot desert", "scrub shrub", "developed",
                                              "meadow near forest"), "forest",
                                      "nonforest"))) %>% 
  mutate(foliage = as.factor(foliage))

mountains_noFoliage <- mountains %>% 
  filter(foliage == "leaf-off")

## 3. Analysis #########

kruskal.test(TAD_elevCorr ~ foliage, data = mountains)


kruskal.test(TAD_elevCorr ~ is_forest, data = mountains)

kruskal.test(TAD_elevCorr ~ is_forest, data = mountains_noFoliage)




deciduous <- mountains %>% 
  filter(macro == "deciduous")

kruskal.test(TAD_elevCorr ~ foliage, data = deciduous)




