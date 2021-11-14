# David Klinges
# 2019-09-13
# This script generates plots of veg structure ~ biome

## 1. Workspace prep ###########

library(tidyverse)
library(ggplot)

mountains <- read_csv("data/03_compiled/mountains_avg.csv")

## 2. Data curation #############

# Recode factors to capitalize
mountains <- mountains %>%
  mutate(micro = tools::toTitleCase(micro)) %>% 
  mutate(macro = tools::toTitleCase(macro)) %>% 
  filter(complete.cases(macro))

# Add break to macro names
mountains <- mountains %>% 
  mutate(macro = gsub(" ", "\n", macro))

## Sort macros by flag
macro_levels <- mountains %>% 
  dplyr::select(macro, veg_structure) %>% 
  group_by(macro) %>% 
  mutate(veg_structure = mean(veg_structure)) %>% 
  arrange(veg_structure) %>% 
  unique()

macro_levels <- macro_levels$macro

mountains <- mountains %>% 
  mutate(macro = factor(macro, levels = macro_levels))

## 3. Plot data #################

ggplot(mountains, aes(macro, veg_structure)) + 
  geom_boxplot() +
  theme_bw() +
  ylab("Vegetation\nStructure") +
  xlab("Biome") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        axis.title.y = element_text(angle = 0, vjust = 0.5))

## 4. Write out plot ###################
ggsave("figures/supplementary/vegStructure_byBiome.png",
       width = 10.6, height = 3.2)
