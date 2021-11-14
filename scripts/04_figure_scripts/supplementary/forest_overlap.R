## David Klinges
## This script plots the difference in overlap between forests and Non-forests


## 1. Workspace prep ##########

library(tidyverse)

mountains <- read_csv("data/03_compiled/mountains_overlap_monthly_avg.csv")

## 2. Data curation ###############

micro_factors <- c("Soil", "Surface", "Canopy")
foliage_factors <- c("Leaf-on", "Leaf-off")
forest_foliage_factors <- c("Forest, leaf-on", "Non-forest", "Forest, leaf-off")

mountains <- mountains %>% 
  mutate(is_forest = as.factor(ifelse(macro %in% c("alpine meadow", "hot desert", "scrub shrub", "developed",
                                                   "meadow near forest"), "Non-forest",
                                      "Forest"))) %>% 
  mutate(micro = dplyr::recode(micro, "soil" = "Soil", "surface" = "Surface",
                             "canopy" = "Canopy")) %>% 
  mutate(micro = factor(micro, levels = micro_factors)) %>% 
  mutate(forest_foliage = paste(is_forest, foliage, sep = "_")) %>% 
  mutate(forest_foliage = dplyr::recode(forest_foliage, "Forest_leaf-on" = "Forest, leaf-on",
                                        "Forest_leaf-off" = "Forest, leaf-off",
                                        "Non-forest_leaf-off" = "Non-forest")) %>% 
  mutate(forest_foliage = factor(forest_foliage, levels = forest_foliage_factors)) %>% 
  mutate(foliage = dplyr::recode(foliage, "leaf-on" = "Leaf-on", "leaf-off" = "Leaf-off")) %>% 
  mutate(foliage = factor(foliage, levels = foliage_factors))

## Find summary stats
mountains_forestMicro_avg <- mountains %>% 
  group_by(is_forest,  micro) %>% 
  summarize_at(vars(TAD, TAD_elevCorr, kde, kde_elevCorr, janzenDscore, janzenDscore_elevCorr), 
               mean, na.rm = TRUE) 

mountains_forestFoliageMicro_avg <- mountains %>% 
  group_by(is_forest, foliage, forest_foliage, micro) %>% 
  summarize_at(vars(TAD, TAD_elevCorr, kde, kde_elevCorr, janzenDscore, janzenDscore_elevCorr), 
               mean, na.rm = TRUE) 

## For decid only
decid_mountains_forestMicro_avg <- mountains %>% 
  filter(macro == "deciduous") %>% 
  group_by(is_forest, foliage, forest_foliage, micro) %>% 
  summarize_at(vars(TAD, TAD_elevCorr, kde, kde_elevCorr, janzenDscore, janzenDscore_elevCorr), 
               mean, na.rm = TRUE) 

mountains_forest_avg <- mountains %>% 
  group_by(is_forest, forest_foliage) %>% 
  summarize_at(vars(TAD, TAD_elevCorr, kde, kde_elevCorr, janzenDscore, janzenDscore_elevCorr), 
               mean, na.rm = TRUE) 

mountains_means <- mountains %>% 
  group_by(micro, is_forest, forest_foliage) %>% 
  summarize(TAD_mean = mean(TAD, na.rm = TRUE)) %>% 
  mutate(label = ifelse(micro == "Soil" & is_forest == "Forest", "A",
                      ifelse(micro == "Surface" & is_forest == "Forest", "B",
                             ifelse(micro == "Canopy" & is_forest == "Forest", "B",
                                    ifelse(micro == "Soil" & is_forest == "Non-forest", "D",
                                           ifelse(micro == "Surface" & is_forest == "Non-forest", "C", NA))))))

## 3. Plot ##############


## ....A. By forest vs non-forest #############

forest_violins <- ggplot(mountains, aes(is_forest, TAD, group = is_forest)) +
  # geom_boxplot(aes(color = is_forest)) +
  geom_violin(aes(color = is_forest)) +
  geom_jitter(aes(color = is_forest), width = .1, alpha = 0.6) +
  geom_point(data = mountains_forestMicro_avg, aes(is_forest, TAD), 
             size = 4, color = "black") +
  # geom_text(data = filter(mountains_means, is_forest == "Forest"), 
  #           aes(x = micro, group = is_forest, y = TAD_mean + .55, label=label), size = 6) +

  scale_color_manual(values = c("Non-forest" = "#e08626",
                                "Forest" = "#307233")) +
  labs(color = "",
       y = "Thermal Absolute Overlap",
       x = "") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        text=element_text(size= 16),
        axis.ticks.x = element_blank()) +
  facet_wrap(~micro)

## ....B. By forest (leaf on only) vs non-forest #############

plot_data <- mountains %>% 
  filter(is_forest == "Non-forest" | forest_foliage == "Forest, leaf-on")

plot_avg <- mountains_forestFoliageMicro_avg %>% 
  filter(is_forest == "Non-forest" | forest_foliage == "Forest, leaf-on")

forest_leafon_violins <- ggplot(plot_data, aes(is_forest, TAD, group = is_forest)) +
  # geom_boxplot(aes(color = is_forest)) +
  geom_violin(aes(color = is_forest)) +
  geom_jitter(aes(color = is_forest), width = .1, alpha = 0.6) +
  geom_point(data = plot_avg, aes(is_forest, TAD), 
             size = 4, color = "black") +
  # geom_text(data = filter(mountains_means, is_forest == "Forest"), 
  #           aes(x = micro, group = is_forest, y = TAD_mean + .55, label=label), size = 6) +
  
  scale_color_manual(values = c("Non-forest" = "#e08626",
                                "Forest" = "#307233")) +
  labs(color = "",
       y = "Thermal Absolute Overlap",
       x = "") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        text=element_text(size= 16),
        axis.ticks.x = element_blank()) +
  facet_wrap(~micro)

## ....C. By forest and foliage #############

forest_foliage_violins <- ggplot(mountains, aes(forest_foliage, TAD, group = forest_foliage)) +
  # geom_boxplot(aes(color = is_forest)) +
  geom_violin(aes(color = forest_foliage)) +
  geom_jitter(aes(color = forest_foliage), width = .1, alpha = 0.6) +
  geom_point(data = mountains_forestFoliageMicro_avg, aes(forest_foliage, TAD), 
             size = 4, color = "black") +
  # geom_text(data = filter(mountains_means, is_forest == "Forest"), 
  #           aes(x = micro, group = is_forest, y = TAD_mean + .55, label=label), size = 6) +
  scale_color_manual(values = c("Forest, leaf-on" = "#307233",
                                "Non-forest" = "#e08626",
                                "Forest, leaf-off" = "#CC9900")) +
  labs(color = "",
       y = "Thermal Absolute Overlap",
       x = "") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        text=element_text(size= 16),
        axis.ticks.x = element_blank()) +
  facet_wrap(~micro)

## ....D. Deciduous only ##########

deciduous_violins <- ggplot(filter(mountains, macro == "deciduous"), 
                            aes(foliage, TAD, group = foliage)) +
  # geom_boxplot(aes(color = is_forest)) +
  geom_violin(aes(color = foliage)) +
  geom_jitter(aes(color = foliage), width = .1, alpha = 0.6) +
  geom_point(data = decid_mountains_forestMicro_avg, 
             aes(foliage, TAD), 
             size = 4, color = "black") +
  # geom_text(data = filter(mountains_means, is_forest == "Forest"), 
  #           aes(x = micro, group = is_forest, y = TAD_mean + .55, label=label), size = 6) +
  scale_color_manual(values = c("Leaf-on" = "#307233",
                                "Leaf-off" = "#CC9900")) +
  labs(color = "",
       y = "Thermal Absolute Overlap",
       x = "") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        text=element_text(size= 16),
        axis.ticks.x = element_blank()) +
  facet_wrap(~micro)

## 4. Arrange plots ##############

forest_violins <- forest_violins +
  labs(title = "A. All Mountains, Forest versus Non-forest") +
  theme(title = element_text(size = 12))

deciduous_violins <- deciduous_violins +
  labs(title = "B. Deciduous Mountains only, Presence of Seasonal Foliage") +
  theme(title = element_text(size = 12))

multipanel <- grid.arrange(forest_violins, deciduous_violins, nrow = 2)

## 5. Write out plots #############

# ggsave(plot = forest_violins, filename = "figures/supplementary/forest_violins.png")
# ggsave(plot = forest_leafon_violins, filename = "figures/supplementary/forest_leafon_violins.png")
# ggsave(plot = forest_foliage_violins, filename = "figures/supplementary/forest_foliage_violins.png")
# ggsave(plot = deciduous_violins, filename = "figures/supplementary/deciduous_violins.png")
ggsave(plot = multipanel, filename = "figures/supplementary/multipanel.png",
       height = 11, width = 8)
