# David Klinges
# this script calculates summary statistics for different overlap metrics

## 1. Workspace prep #########

library(tidyverse) 

mountains_daily <- read_csv("data/03_compiled/mountains_overlap_daily.csv")
mountains_monthly <- read_csv("data/03_compiled/mountains_overlap_monthly_avg.csv")

KDE_daily <- read_csv("data/04_analysis/compare_density_distribs/KDE_overlap_daily.csv")
KDE_monthly <- read_csv("data/04_analysis/compare_density_distribs/KDE_overlap_monthly.csv")

## 2. Find summary stats ##########

## ....A. Janzen D-score ##########

# Daily
daily_Dscore_summarized <- mountains_daily %>% 
  group_by(foliage, micro) %>% 
  summarize(janzenDscore_elevCorr = mean(janzenDscore_elevCorr, na.rm = TRUE)) %>% 
  ungroup() %>% 
  spread(key = foliage, value = janzenDscore_elevCorr) %>% 
  dplyr::rename(leafoff_Dscore = "leaf-off", leafon_Dscore = "leaf-on")

# Monthly
monthly_Dscore_summarized <- mountains_monthly %>% 
  group_by(foliage, micro) %>% 
  summarize(janzenDscore_elevCorr = mean(janzenDscore_elevCorr, na.rm = TRUE)) %>% 
  ungroup() %>% 
  spread(key = foliage, value = janzenDscore_elevCorr) %>% 
  dplyr::rename(leafoff_Dscore = "leaf-off", leafon_Dscore = "leaf-on")

## ....B. TAD ##########
## .......** By biome ##########

# Daily
daily_TAD_byBiome <- mountains_daily %>% 
  group_by(macro) %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE),
            TAD_SD = sd(TAD, na.rm = TRUE)) %>%
  ungroup()

# Monthly
monthly_TAD_byBiome <- mountains_monthly %>% 
  group_by(macro) %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE),
            TAD_SD = sd(TAD, na.rm = TRUE)) %>%
  ungroup()

## .......** By forest vs non-forest ##########

# Monthly
mountains_monthly_forest <- mountains_monthly %>% 
  mutate(is_forest = ifelse(macro %in% c("deciduous", "tropical broadleaf",
                                         "Ponderosa pine", "Dense coniferous",
                                         "degraded tropical broadleaf"),
                            "forest", "non-forest"))

monthly_TAD_forest_nonforest <- mountains_monthly_forest %>% 
  group_by(is_forest, micro) %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE)) %>%
  ungroup()

monthly_TAD_forest <- mountains_monthly_forest %>% 
  filter(is_forest == "forest") %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE)) %>% 
  mutate(micro = "all micros", is_forest = "forest") 

monthly_TAD_nonforest <- mountains_monthly_forest %>% 
  filter(is_forest == "non-forest") %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE)) %>% 
  mutate(micro = "all micros", is_forest = "non-forest") 

monthly_TAD_allBiomes <- mountains_monthly_forest %>% 
  group_by(micro) %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE)) %>% 
  mutate(is_forest = "all biomes")  %>%
  ungroup()

monthly_TAD_allBiomes_allMicros <- mountains_monthly_forest %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE)) %>% 
  mutate(is_forest = "all biomes") %>% 
  mutate(micro = "all micros")

monthly_TAD_summarized <- monthly_TAD_forest_nonforest %>% 
  bind_rows(monthly_TAD_allBiomes) %>% 
  bind_rows(monthly_TAD_forest) %>% 
  bind_rows(monthly_TAD_nonforest) %>% 
  bind_rows(monthly_TAD_allBiomes_allMicros) %>% 
  spread(key = is_forest, value = TAD) %>% 
  dplyr::rename(forest_TAD = "forest", non_forest_TAD = "non-forest") %>% 
  mutate(foliage_buffering_effect = forest_TAD - non_forest_TAD)

## .......** By leaf-on vs leaf-off (just deciduous) ##########

monthly_TAD_forest_nonforest <- mountains_monthly_forest %>% 
  filter(macro == "deciduous") %>% 
  group_by(foliage, micro) %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE)) %>% 
  ungroup()

monthly_TAD_forest <- mountains_monthly_forest %>%
  filter(macro == "deciduous") %>% 
  filter(foliage == "leaf-on") %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE)) %>% 
  mutate(micro = "all micros", foliage = "leaf-on") 

monthly_TAD_nonforest <- mountains_monthly_forest %>%
  filter(macro == "deciduous") %>% 
  filter(foliage == "leaf-off") %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE)) %>% 
  mutate(micro = "all micros", foliage = "leaf-off") 

monthly_TAD_allBiomes <- mountains_monthly_forest %>% 
  group_by(micro) %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE)) %>% 
  mutate(foliage = "all biomes") %>% 
  ungroup()

monthly_TAD_allBiomes_allMicros <- mountains_monthly_forest %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE)) %>% 
  mutate(foliage = "all biomes") %>% 
  mutate(micro = "all micros")

monthly_TAD_summarized <- monthly_TAD_forest_nonforest %>% 
  bind_rows(monthly_TAD_allBiomes) %>% 
  bind_rows(monthly_TAD_forest) %>% 
  bind_rows(monthly_TAD_nonforest) %>% 
  bind_rows(monthly_TAD_allBiomes_allMicros) %>% 
  spread(key = foliage, value = TAD) %>% 
  dplyr::rename(leafoff_TAD = "leaf-off", leafon_TAD = "leaf-on") %>% 
  mutate(foliage_buffering_effect = leafon_TAD - leafoff_TAD)


# Daily
daily_TAD_summarized <- mountains_daily %>% 
  group_by(foliage, micro) %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE)) %>% 
  spread(key = foliage, value = TAD) %>% 
  dplyr::rename(leafoff_TAD = "leaf-off", leafon_TAD = "leaf-on") %>% 
  ungroup()


## .......** By leaf-on vs non-forest ##########

# Monthly
mountains_monthly_forest <- mountains_monthly %>% 
  mutate(is_forest = ifelse(macro %in% c("deciduous", "tropical broadleaf",
                                         "Ponderosa pine", "Dense coniferous",
                                         "degraded tropical broadleaf"),
                            "forest", "non-forest")) %>% 
  filter(is_forest == "non-forest" | (is_forest == "forest" & foliage == "leaf-on"))

monthly_TAD_forest_nonforest <- mountains_monthly_forest %>% 
  group_by(is_forest, micro) %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE)) %>%
  ungroup()

monthly_TAD_forest <- mountains_monthly_forest %>% 
  filter(is_forest == "forest") %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE)) %>% 
  mutate(micro = "all micros", is_forest = "forest") 

monthly_TAD_nonforest <- mountains_monthly_forest %>% 
  filter(is_forest == "non-forest") %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE)) %>% 
  mutate(micro = "all micros", is_forest = "non-forest") 

monthly_TAD_allBiomes <- mountains_monthly_forest %>% 
  group_by(micro) %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE)) %>% 
  mutate(is_forest = "all biomes")  %>%
  ungroup()

monthly_TAD_allBiomes_allMicros <- mountains_monthly_forest %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE)) %>% 
  mutate(is_forest = "all biomes") %>% 
  mutate(micro = "all micros")

monthly_TAD_summarized <- monthly_TAD_forest_nonforest %>% 
  bind_rows(monthly_TAD_allBiomes) %>% 
  bind_rows(monthly_TAD_forest) %>% 
  bind_rows(monthly_TAD_nonforest) %>% 
  bind_rows(monthly_TAD_allBiomes_allMicros) %>% 
  spread(key = is_forest, value = TAD) %>% 
  dplyr::rename(forest_TAD = "forest", non_forest_TAD = "non-forest") %>% 
  mutate(foliage_buffering_effect = forest_TAD - non_forest_TAD)

## .......** By leaf-on vs leaf-off (forest and non-forest) ##########

monthly_TAD_forest_nonforest <- mountains_monthly %>% 
  group_by(foliage, micro) %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE)) %>% 
  ungroup()

monthly_TAD_forest <- mountains_monthly %>% 
  filter(foliage == "leaf-on") %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE)) %>% 
  mutate(micro = "all micros", foliage = "leaf-on") 

monthly_TAD_nonforest <- mountains_monthly %>% 
  filter(foliage == "leaf-off") %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE)) %>% 
  mutate(micro = "all micros", foliage = "leaf-off") 

monthly_TAD_allBiomes <- mountains_monthly %>% 
  group_by(micro) %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE)) %>% 
  mutate(foliage = "all biomes")

monthly_TAD_allBiomes_allMicros <- mountains_monthly %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE)) %>% 
  mutate(foliage = "all biomes") %>% 
  mutate(micro = "all micros")

monthly_TAD_summarized <- monthly_TAD_forest_nonforest %>% 
  bind_rows(monthly_TAD_allBiomes) %>% 
  bind_rows(monthly_TAD_forest) %>% 
  bind_rows(monthly_TAD_nonforest) %>% 
  bind_rows(monthly_TAD_allBiomes_allMicros) %>% 
  spread(key = foliage, value = TAD) %>% 
  dplyr::rename(leafoff_TAD = "leaf-off", leafon_TAD = "leaf-on") %>% 
  mutate(foliage_buffering_effect = leafon_TAD - leafoff_TAD)


# Daily
daily_TAD_summarized <- mountains_daily %>% 
  group_by(foliage, micro) %>% 
  summarize(TAD = mean(TAD, na.rm = TRUE)) %>% 
  spread(key = foliage, value = TAD) %>% 
  dplyr::rename(leafoff_TAD = "leaf-off", leafon_TAD = "leaf-on") %>% 
  ungroup()
  
## ....C. KDE overlaps ##########
  
# Daily
KDE_daily <- KDE_daily %>% 
  group_by(foliage, micro) %>% 
  summarize(KDE_elevCorr = mean(overlap_elevCorr, na.rm = TRUE)) %>% 
  spread(key = foliage, value = KDE_elevCorr) %>% 
  dplyr::rename(leafoff_KDE = "leaf-off", leafon_KDE = "leaf-on") %>% 
  ungroup()
  
# Monthly
KDE_monthly <- KDE_monthly %>% 
  group_by(foliage, micro) %>% 
  summarize(KDE_elevCorr = mean(overlap_elevCorr, na.rm = TRUE)) %>% 
  spread(key = foliage, value = KDE_elevCorr) %>% 
  dplyr::rename(leafoff_KDE = "leaf-off", leafon_KDE = "leaf-on") %>% 
  ungroup()

## 3. Join summarized overlaps ########

summary_daily <- daily_Dscore_summarized %>% 
  left_join(daily_TAD_summarized) %>% 
  left_join(KDE_daily) %>% 
  
  # Summary differences in overlap (due to foliage)
  mutate(Dscore_foliage_diff = (leafoff_Dscore - leafon_Dscore),
         TAD_foliage_diff = (leafoff_TAD - leafon_TAD),
         KDE_foliage_diff = (leafoff_KDE - leafon_KDE)) %>% 
  # No point in finding mean of diffs...Dscores and KDE densities are unitless,
  # only TAD is actual temperature

  # Summary proportion reductions in overlap (due to foliage)
  mutate(Dscore_foliage_reduction = (leafoff_Dscore - leafon_Dscore)/abs(leafoff_Dscore),
         TAD_foliage_reduction = (leafoff_TAD - leafon_TAD)/abs(leafoff_TAD),
         KDE_foliage_reduction = (leafoff_KDE - leafon_KDE)/abs(leafoff_KDE)) %>% 
  # Find mean of all overlap reductions
  mutate(mean_reduction = (Dscore_foliage_reduction + TAD_foliage_reduction + KDE_foliage_reduction)/3)

# Summarize all micros
summary <- summary_daily %>% 
  summarize_all(mean, na.rm = TRUE) %>% 
  mutate(micro = "all micros")

summary_daily <- summary_daily %>% 
  bind_rows(summary)

summary_monthly <- monthly_Dscore_summarized %>% 
  left_join(monthly_TAD_summarized) %>% 
  left_join(KDE_monthly) %>% 
  
  # Summary differences in overlap (due to foliage)
  mutate(Dscore_foliage_diff = (leafoff_Dscore - leafon_Dscore),
         TAD_foliage_diff = (leafoff_TAD - leafon_TAD),
         KDE_foliage_diff = (leafoff_KDE - leafon_KDE)) %>% 
  # No point in finding mean of diffs...Dscores and KDE densities are unitless,
  # only TAD is actual temperature
  
  # Summary proportion reductions in overlap (due to foliage)
  mutate(Dscore_foliage_reduction = (leafoff_Dscore - leafon_Dscore)/abs(leafoff_Dscore),
         TAD_foliage_reduction = (leafoff_TAD - leafon_TAD)/abs(leafoff_TAD),
         KDE_foliage_reduction = (leafoff_KDE - leafon_KDE)/abs(leafoff_KDE)) %>% 
  # Find mean of all overlap methods
  mutate(mean_reduction = (Dscore_foliage_reduction + TAD_foliage_reduction + KDE_foliage_reduction)/3)

# Summarize all micros
summary <- summary_monthly %>% 
  summarize_all(mean) %>% 
  mutate(micro = "all micros")

summary_monthly <- summary_monthly %>% 
  bind_rows(summary)

## 4. Write out data #######

write_csv(summary_daily, "data/04_analysis/summary_stats/overlap_summary_daily.csv")
write_csv(summary_monthly, "data/04_analysis/summary_stats/overlap_summary_monthly.csv")
