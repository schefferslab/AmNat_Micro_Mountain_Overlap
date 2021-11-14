# David Klinges
# dklinges@ufl.edu

# This script generates the global thermal distributions comparison

## 1. Workspace prep ############

library(tidyverse)
library(RColorBrewer)
library(ggnewscale)
library(viridis)
library(grid)
library(gridExtra)
library(e1071)


## Import wide data ##########

mountains_wide <- read_csv("data/03_compiled/mountains_wide_avg.csv",
                           col_types = cols(
                             low_canopy_min = col_double(),
                             low_canopy_mean = col_double(),
                             low_canopy_max = col_double(),
                             high_canopy_min = col_double(),
                             high_canopy_mean = col_double(),
                             high_canopy_max = col_double()
                             ))

mountains <- read_csv("data/03_compiled/mountains_avg.csv")

# Also import KDE overlapping values
KDE_overlap <- read_csv("data/04_analysis/compare_density_distribs/KDE_overlap_daily_MacroSummarized.csv")

## ....2a. Add macro and foliage flags ###########

# Note: removing tropical sites that aren't forest, because
# then I can caption non-forest as "temperate non-forest". 

mountains_wide <- mountains_wide %>% 
  filter(site != "CR_northwest" & site != "CR_southwest" & site != "MY_SAFE") %>% 

  mutate(macro = ifelse(macro %in% list("scrub shrub", "hot desert", "alpine meadow",
                                        "meadow near forest", "developed"),
                        "non-forest",
                        ifelse(macro %in% list("Dense coniferous", "Ponderosa pine"), 
                               "coniferous", macro))) %>% 
  mutate(macro = ifelse(macro == "deciduous" & foliage == "leaf-off", 
                        "leaf-off", macro)) %>% 
  mutate(macro = ifelse(macro %in% list("coniferous", "deciduous") & foliage == "leaf-on",
                        "leaf-on", macro))

## ....2b. Combine daily min & max for each micro and zone #######

mountains_minmax <- mountains_wide %>% 
  group_by(site) %>% 
  gather(key = "minmax", value = "low_soil", low_soil_min, low_soil_max) %>%
  gather(key = "minmax", value = "high_soil", high_soil_min, high_soil_max) %>%
  gather(key = "minmax", value = "low_surface", low_surface_min, low_surface_max) %>%
  gather(key = "minmax", value = "high_surface", high_surface_min, high_surface_max) %>%
  gather(key = "minmax", value = "low_canopy", low_canopy_min, low_canopy_max) %>%
  gather(key = "minmax", value = "high_canopy", high_canopy_min, high_canopy_max) %>% 
  ungroup() %>% 
  dplyr::select(-contains("mean"))

## ....2c. Combine daily means for each micro and zone #######

mountains_mean <- mountains_wide %>% 
  group_by(site) %>% 
  gather(key = "mean", value = "low_soil", low_soil_mean) %>%
  gather(key = "mean", value = "high_soil", high_soil_mean) %>%
  gather(key = "mean", value = "low_surface", low_surface_mean) %>%
  gather(key = "mean", value = "high_surface", high_surface_mean) %>%
  gather(key = "mean", value = "low_canopy", low_canopy_mean) %>%
  gather(key = "mean", value = "high_canopy", high_canopy_mean) %>% 
  ungroup() %>% 
  dplyr::select(-contains("min"), -contains("max"))

## ....2d. Transform temp: center on mean #########

mountains_minmax_centered <- mountains_minmax %>% 
  group_by(site, foliage) %>% 
  mutate(low_soil = (low_soil + high_soil)/2 - low_soil) %>% 
  mutate(high_soil = (low_soil + high_soil)/2 - high_soil) %>% 
  mutate(low_surface = (low_surface + high_surface)/2 - low_surface) %>% 
  mutate(high_surface = (low_surface + high_surface)/2 - high_surface) %>% 
  mutate(low_canopy = (low_canopy + high_canopy)/2 - low_canopy) %>% 
  mutate(high_canopy = (low_canopy + high_canopy)/2 - high_canopy)  %>% 
  ungroup()

mountains_mean_centered <- mountains_mean %>% 
  group_by(site, foliage) %>% 
  mutate(low_soil = (low_soil + high_soil)/2 - low_soil) %>% 
  mutate(high_soil = (low_soil + high_soil)/2 - high_soil) %>% 
  mutate(low_surface = (low_surface + high_surface)/2 - low_surface) %>% 
  mutate(high_surface = (low_surface + high_surface)/2 - high_surface) %>% 
  mutate(low_canopy = (low_canopy + high_canopy)/2 - low_canopy) %>% 
  mutate(high_canopy = (low_canopy + high_canopy)/2 - high_canopy)  %>% 
  ungroup()

## ....2e. Join data ########

joined_data_minmax <- mountains_minmax 

joined_data_minmax_centered <- mountains_minmax_centered

joined_data_mean_centered <- mountains_mean_centered

# mountains_grouped <- mountains_wide %>% 
#   gather(key = micro, value = "temp_low", contains("low"), -contains("altitude")) %>% 
#   gather(key = micro, value = "temp_high", contains("high"), -contains("altitude")) %>% 
#   separate(micro, into = c("elevation", "micro"), sep = "_") %>% 
#   group_by(macro, micro) %>% 
#   # Calculate overlap
#   # This doesn't seem to work....I guess overlap doesn't work in a group_by,
#   # hence why I for-looped in compare_KDE_overlap.R.
#   # Abandoning this idea for now because this figures might not even make it into
#   # the manuscript
#   mutate(overlap = overlap(c(temp_low, temp_high), plot = FALSE)$OV)

## ....2f. Separate high and low elevation #########

low_minmax_centered <- joined_data_minmax_centered %>% 
  dplyr::select(-contains("high")) %>% 
  mutate(elevation = "low")

high_minmax_centered <- joined_data_minmax_centered %>% 
  dplyr::select(-contains("low")) %>% 
  mutate(elevation = "high")

low_mean_centered <- joined_data_mean_centered %>% 
  dplyr::select(-contains("high")) %>% 
  mutate(elevation = "low")

high_mean_centered <- joined_data_mean_centered %>% 
  dplyr::select(-contains("low")) %>% 
  mutate(elevation = "high")

## ....2g. Curate KDE overlap values ###########

KDE_overlap <- KDE_overlap %>% 
  mutate(overlap_elevCorr = round(overlap_elevCorr, digits = 3)) %>% 
  mutate(overlap = round(overlap, digits = 3)) %>% 
  mutate(macro = ifelse(macro %in% list("scrub shrub", "hot desert", "alpine meadow",
                                        "meadow near forest", "developed"),
                        "non-forest",
                        ifelse(macro %in% list("Dense coniferous", "Ponderosa pine"), 
                               "coniferous", macro))) %>% 
  mutate(macro = ifelse(macro == "deciduous" & foliage == "leaf-off", 
                        "leaf-off", macro)) %>% 
  mutate(macro = ifelse(macro %in% list("coniferous", "deciduous") & foliage == "leaf-on",
                        "leaf-on", macro)) %>% 
  # Now you need to summarize to the new aggregated macro groupings
  group_by(macro, micro, foliage) %>% 
  summarize_at(vars(overlap, overlap_elevCorr), mean)

## ....2h. Lump all m9cros together ###########

## 3. Generate plots ###########

low_minmax_centered_allMicro <- low_minmax_centered %>% 
  gather(micro, temp, low_soil, low_surface, low_canopy)

high_minmax_centered_allMicro <- high_minmax_centered %>% 
  gather(micro, temp, high_soil, high_surface, high_canopy)

## NEW FIGURE ########

tropical_forest_allMicro <- ggplot(data = filter(low_minmax_centered_allMicro, macro == "tropical broadleaf"), 
                                   aes(temp)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, show.legend = TRUE)  +
  geom_density(data = filter(high_minmax_centered_allMicro, macro == "tropical broadleaf"), 
               aes(temp, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "purple",
                               "high" = "blue")) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Tropical soil") +
  
  # Add annotation of overlap value
  annotate("text",
           label = as.character((filter(KDE_overlap, 
                                        macro == "tropical broadleaf", micro == "soil"))$overlap),
           x = 30, y = .28) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 3), 
        legend.text = element_text(size = 3),
        legend.key.size = unit(.2,"line"))

## ....A. Non-transformed temp #########

# Tropical
trop_soil <- ggplot(data = filter(joined_data_minmax, macro == "tropical broadleaf"), aes(low_soil)) +
  geom_density(kernel = "gaussian", fill = "purple", alpha=.2) +
  geom_density(data = filter(joined_data_minmax, macro == "tropical broadleaf"), aes(high_soil),
               fill = "blue", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Tropical soil") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

trop_surface <- ggplot(data = filter(joined_data_minmax, macro == "tropical broadleaf" ), aes(low_surface)) +
  geom_density(kernel = "gaussian", fill = "light green", alpha=.2) +
  geom_density(data = filter(joined_data_minmax, macro == "tropical broadleaf" ), aes(high_surface),
               fill = "dark green", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Tropical surface")  +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

trop_canopy <- ggplot(data = filter(joined_data_minmax, macro == "tropical broadleaf" ), aes(low_canopy)) +
  geom_density(kernel = "gaussian", fill = "red", alpha=.2) +
  geom_density(data = filter(joined_data_minmax, macro == "tropical broadleaf" ), aes(high_canopy),
               fill = "dark red", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Tropical canopy")  +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Temp leaf-on
leafon_soil <- ggplot(data = filter(joined_data_minmax, macro %in% list("coniferous", "deciduous"), 
                                    foliage == "leaf-on"), aes(low_soil)) +
  geom_density(kernel = "gaussian", fill = "purple", alpha=.2) +
  geom_density(data = filter(joined_data_minmax, macro %in% list("coniferous", "deciduous"), 
                             foliage == "leaf-on"), aes(high_soil),
               fill = "blue", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Leaf-on soil")  +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

leafon_surface <- ggplot(data = filter(joined_data_minmax, macro %in% list("coniferous", "deciduous"), 
                                       foliage == "leaf-on"), aes(low_surface)) +
  geom_density(kernel = "gaussian", fill = "light green", alpha=.2) +
  geom_density(data = filter(joined_data_minmax, macro %in% list("coniferous", "deciduous"), 
                             foliage == "leaf-on"), aes(high_surface),
               fill = "dark green", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14)+
  labs(title = "Leaf-on surface")  +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

leafon_canopy <- ggplot(data = filter(joined_data_minmax, macro %in% list("coniferous", "deciduous"), 
                                      foliage == "leaf-on"), aes(low_canopy)) +
  geom_density(kernel = "gaussian", fill = "red", alpha=.2) +
  geom_density(data = filter(joined_data_minmax, macro %in% list("coniferous", "deciduous"), 
                             foliage == "leaf-on"), aes(high_canopy),
               fill = "dark red", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Leaf-on canopy")  +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())



# Temp leaf-off
leafoff_soil <- ggplot(data = filter(joined_data_minmax, macro %in% list("coniferous", "deciduous"), 
                                     foliage == "leaf-off"), aes(low_soil)) +
  geom_density(kernel = "gaussian", fill = "purple", alpha=.2) +
  geom_density(data = filter(joined_data_minmax, macro %in% list("coniferous", "deciduous"), 
                             foliage == "leaf-off"), aes(high_soil),
               fill = "blue", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Leaf-off soil") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

leafoff_surface <- ggplot(data = filter(joined_data_minmax, macro %in% list("coniferous", "deciduous"), 
                                        foliage == "leaf-off"), aes(low_surface)) +
  geom_density(kernel = "gaussian", fill = "light green", alpha=.2) +
  geom_density(data = filter(joined_data_minmax, macro %in% list("coniferous", "deciduous"), 
                             foliage == "leaf-off"), aes(high_surface),
               fill = "dark green", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Leaf-off surface") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

leafoff_canopy <- ggplot(data = filter(joined_data_minmax, macro %in% list("coniferous", "deciduous"), 
                                       foliage == "leaf-off"), aes(low_canopy)) +
  geom_density(kernel = "gaussian", fill = "red", alpha=.2) +
  geom_density(data = filter(joined_data_minmax, macro %in% list("coniferous", "deciduous"), 
                             foliage == "leaf-off"), aes(high_canopy),
               fill = "dark red", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Leaf-off canopy") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())


# Temp non-forest
nonforest_soil <- ggplot(data = filter(joined_data_minmax, macro == "non-forest"), aes(low_soil)) +
  geom_density(kernel = "gaussian", fill = "purple", alpha=.2) +
  geom_density(data = filter(joined_data_minmax, macro == "non-forest"), aes(high_soil),
               fill = "blue", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Non-forest soil") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

nonforest_surface <- ggplot(data = filter(joined_data_minmax, macro == "non-forest"), aes(low_surface)) +
  geom_density(kernel = "gaussian", fill = "light green", alpha=.2) +
  geom_density(data = filter(joined_data_minmax, macro == "non-forest"), aes(high_surface),
               fill = "dark green", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Non-forest surface") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

nonforest_canopy <- ggplot(data = filter(joined_data_minmax, macro == "non-forest"), aes(low_canopy)) +
  geom_density(kernel = "gaussian", fill = "red", alpha=.2) +
  geom_density(data = filter(joined_data_minmax, macro == "non-forest"), aes(high_canopy),
               fill = "dark red", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())


plots <- grid.arrange(trop_soil, trop_surface, trop_canopy,
                      leafon_soil, leafon_surface, leafon_canopy,
                      leafoff_soil, leafoff_surface, leafoff_canopy,
                      nonforest_soil, nonforest_surface, nonforest_canopy,
                      nrow = 4,
                      left = grid.text(
                        "Density Probability", gp = gpar(fontsize = 18), rot = 90),
                      bottom = grid.text(
                        "Temperature", gp = gpar(fontsize = 18)))

plots



## ....B. Transformed temp, minmax #########

# NOTE: tropical plots are set up differently in order to generate legends, which
# are placed in the bottom-right corner. These are then removed at the end.

## ....** Plots #############
## .......** Tropical ############

trop_soil <- ggplot(data = filter(low_minmax_centered, macro == "tropical broadleaf"), aes(low_soil)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, show.legend = TRUE)  +
  geom_density(data = filter(high_minmax_centered, macro == "tropical broadleaf"), 
               aes(high_soil, fill = elevation),
              alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "purple",
                               "high" = "blue")) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Tropical soil") +
  
  # Add annotation of overlap value
  annotate("text",
    label = as.character((filter(KDE_overlap, 
                macro == "tropical broadleaf", micro == "soil"))$overlap),
           x = 30, y = .28) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 3), 
        legend.text = element_text(size = 3),
        legend.key.size = unit(.2,"line"))

trop_surface <- ggplot(data = filter(low_minmax_centered, macro == "tropical broadleaf" ), aes(low_surface)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, show.legend = TRUE) +
  geom_density(data = filter(high_minmax_centered, macro == "tropical broadleaf" ), aes(high_surface, fill = elevation),
               alpha=.2) +
  scale_fill_manual(values = c("low" = "light green",
                               "high" = "dark green")) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Tropical surface")  +
  
  # Add annotation of overlap value
  annotate("text",
           label = as.character((filter(KDE_overlap, 
           macro == "tropical broadleaf", micro == "surface"))$overlap),
           x = 30, y = .28) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(), 
        legend.text = element_text(size = 3),
        legend.key.size = unit(.2,"line"))

trop_canopy <- ggplot(data = filter(low_minmax_centered, macro == "tropical broadleaf" ), aes(low_canopy)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, show.legend = TRUE) +
  geom_density(data = filter(high_minmax_centered, macro == "tropical broadleaf" ), aes(high_canopy, fill = elevation),
               alpha=.2) +
  scale_fill_manual(values = c("low" = "red",
                               "high" = "dark red")) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Tropical canopy")  +
  
  # Add annotation of overlap value
  annotate("text",
           label = as.character((filter(KDE_overlap, 
                                        macro == "tropical broadleaf", micro == "canopy"))$overlap),
           x = 30, y = .28) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(), 
        legend.text = element_text(size = 3),
        legend.key.size = unit(.2,"line"))

## .......** Temp leaf-on ############

leafon_soil <- ggplot(data = filter(joined_data_minmax_centered, 
                      macro == "leaf-on" & foliage == "leaf-on"), aes(low_soil)) +
  geom_density(kernel = "gaussian", fill = "purple", alpha=.2) +
  geom_density(data = filter(joined_data_minmax_centered, 
                             macro == "leaf-on" & foliage == "leaf-on"), aes(high_soil),
               fill = "blue", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Leaf-on soil")  +
  # Add annotation of overlap value
  annotate("text",
           label = as.character((filter(KDE_overlap, 
                                        macro == "leaf-on", micro == "soil"))$overlap),
           x = 30, y = .28) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key.size = unit(.2,"line"))

leafon_surface <- ggplot(data = filter(joined_data_minmax_centered,
                                       macro == "leaf-on" & foliage == "leaf-on"), 
                         aes(low_surface)) +
  geom_density(kernel = "gaussian", fill = "light green", alpha=.2) +
  geom_density(data = filter(joined_data_minmax_centered, 
                             macro == "leaf-on" & 
                             foliage == "leaf-on"), aes(high_surface),
               fill = "dark green", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Leaf-on surface")  +
  
  # Add annotation of overlap value
  annotate("text",
           label = as.character((filter(KDE_overlap, 
                                        macro == "leaf-on", micro == "surface"))$overlap),
           x = 30, y = .28) +
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

leafon_canopy <- ggplot(data = filter(joined_data_minmax_centered,
                                      macro == "leaf-on" &                       foliage == "leaf-on"), aes(low_canopy)) +
  geom_density(kernel = "gaussian", fill = "red", alpha=.2) +
  geom_density(data = filter(joined_data_minmax_centered, 
                             macro == "leaf-on" & 
                             foliage == "leaf-on"), aes(high_canopy),
               fill = "dark red", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Leaf-on canopy")  +
  # Add annotation of overlap value
  annotate("text",
           label = as.character((filter(KDE_overlap, 
                                        macro == "leaf-on", micro == "canopy"))$overlap),
           x = 30, y = .28) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

## .......** Temp leaf-off ############

leafoff_soil <- ggplot(data = filter(joined_data_minmax_centered, 
                                     macro == "leaf-off" & foliage == "leaf-off"), 
                       aes(low_soil)) +
  geom_density(kernel = "gaussian", fill = "purple", alpha=.2) +
  geom_density(data = filter(joined_data_minmax_centered,
                             macro == "leaf-off" & foliage == "leaf-off"), 
               aes(high_soil),
               fill = "blue", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Leaf-off soil") +
  # Add annotation of overlap value
  annotate("text",
           label = as.character((filter(KDE_overlap, 
                                        macro == "leaf-off" & micro == "soil"))$overlap),
           x = 30, y = .28) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

leafoff_surface <- ggplot(data = filter(joined_data_minmax_centered,
                                        macro == "leaf-off" &                                        foliage == "leaf-off"), aes(low_surface)) +
  geom_density(kernel = "gaussian", fill = "light green", alpha=.2) +
  geom_density(data = filter(joined_data_minmax_centered, macro == "leaf-off" & 
                             foliage == "leaf-off"), aes(high_surface),
               fill = "dark green", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Leaf-off surface") +
  # Add annotation of overlap value
  annotate("text",
           label = as.character((filter(KDE_overlap, 
                                        macro == "leaf-off", micro == "surface"))$overlap),
           x = 30, y = .28) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

leafoff_canopy <- ggplot(data = filter(joined_data_minmax_centered,
                                       macro == "leaf-off" &                                       foliage == "leaf-off"), aes(low_canopy)) +
  geom_density(kernel = "gaussian", fill = "red", alpha=.2) +
  geom_density(data = filter(joined_data_minmax_centered, macro == "leaf-off" & 
                             foliage == "leaf-off"), aes(high_canopy),
               fill = "dark red", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Leaf-off canopy") +
  # Add annotation of overlap value
  annotate("text",
           label = as.character((filter(KDE_overlap, 
                                        macro == "leaf-off", micro == "canopy"))$overlap),
           x = 30, y = .28) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

## .......** Temp non-forest ############

nonforest_soil <- ggplot(data = filter(joined_data_minmax_centered, 
                                       macro == "non-forest"), 
                         aes(low_soil)) +
  geom_density(kernel = "gaussian", fill = "purple", alpha=.2) +
  geom_density(data = filter(joined_data_minmax_centered, macro == "non-forest"), 
               aes(high_soil),
               fill = "blue", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Non-forest soil") +
  # Add annotation of overlap value
  annotate("text",
           label = as.character((filter(KDE_overlap, 
                                        macro == "non-forest", micro == "soil"))$overlap),
           x = 30, y = .28) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

nonforest_surface <- ggplot(data = filter(joined_data_minmax_centered, 
                                          macro == "non-forest"), 
                         aes(low_surface)) +
  geom_density(kernel = "gaussian", fill = "light green", alpha=.2) +
  geom_density(data = filter(joined_data_minmax_centered, macro == "non-forest"), aes(high_surface),
               fill = "dark green", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Non-forest surface") +
  # Add annotation of overlap value
  annotate("text",
           label = as.character((filter(KDE_overlap, 
                                        macro == "non-forest", micro == "surface"))$overlap),
           x = 30, y = .28) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

## ....** Arranging #############
# Legends

# Legend extraction function
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

soil_legend <- g_legend(trop_soil)

surface_legend <- g_legend(trop_surface)

canopy_legend <- g_legend(trop_canopy)

trop_soil <- trop_soil +
  theme(legend.position = "none")

trop_surface <- trop_surface +
  theme(legend.position = "none")

trop_canopy <- trop_canopy +
  theme(legend.position = "none")


legends <- grid.arrange(grobs = list(soil_legend, 
                        surface_legend, 
                        canopy_legend),
                        nrow = 3)
plots <- grid.arrange(grobs = list(trop_soil, trop_surface, trop_canopy,
                      leafon_soil, leafon_surface, leafon_canopy,
                      leafoff_soil, leafoff_surface, leafoff_canopy,
                      nonforest_soil, nonforest_surface), 
                      ncol = 3,
                      left = grid.text(
                        "Density Probability", gp = gpar(fontsize = 18), rot = 90),
                      bottom = grid.text(
                        "Temperature", gp = gpar(fontsize = 18)))

plots

## ....C. Transformed temp, mean #########

# NOTE: tropical plots are set up differently in order to generate legends, which
# are placed in the bottom-right corner. These are then removed at the end.

## ....** Plots #############
## .......** Tropical ############
trop_soil <- ggplot(data = filter(low_mean_centered, macro == "tropical broadleaf"), aes(low_soil)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, show.legend = TRUE)  +
  geom_density(data = filter(high_mean_centered, macro == "tropical broadleaf"), 
               aes(high_soil, fill = elevation),
               alpha=.2, show.legend = TRUE) +
  scale_fill_manual(values = c("low" = "purple",
                               "high" = "blue")) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Tropical soil") +
  
  # Add annotation of overlap value
  annotate("text",
           label = as.character((filter(KDE_overlap, 
                                        macro == "tropical broadleaf", micro == "soil"))$overlap),
           x = 30, y = .28) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 3), 
        legend.text = element_text(size = 3),
        legend.key.size = unit(.2,"line"))

trop_surface <- ggplot(data = filter(low_mean_centered, macro == "tropical broadleaf" ), aes(low_surface)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, show.legend = TRUE) +
  geom_density(data = filter(high_mean_centered, macro == "tropical broadleaf" ), aes(high_surface, fill = elevation),
               alpha=.2) +
  scale_fill_manual(values = c("low" = "light green",
                               "high" = "dark green")) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Tropical surface")  +
  
  # Add annotation of overlap value
  annotate("text",
           label = as.character((filter(KDE_overlap, 
                                        macro == "tropical broadleaf", micro == "surface"))$overlap),
           x = 30, y = .28) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(), 
        legend.text = element_text(size = 3),
        legend.key.size = unit(.2,"line"))

trop_canopy <- ggplot(data = filter(low_mean_centered, macro == "tropical broadleaf" ), aes(low_canopy)) +
  geom_density(kernel = "gaussian", aes(fill = elevation), alpha=.2, show.legend = TRUE) +
  geom_density(data = filter(high_mean_centered, macro == "tropical broadleaf" ), aes(high_canopy, fill = elevation),
               alpha=.2) +
  scale_fill_manual(values = c("low" = "red",
                               "high" = "dark red")) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Tropical canopy")  +
  
  # Add annotation of overlap value
  annotate("text",
           label = as.character((filter(KDE_overlap, 
                                        macro == "tropical broadleaf", micro == "canopy"))$overlap),
           x = 30, y = .28) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(), 
        legend.text = element_text(size = 3),
        legend.key.size = unit(.2,"line"))

## .......** Temp leaf-on ############

leafon_soil <- ggplot(data = filter(joined_data_mean_centered, 
                                    macro %in% list("coniferous", "deciduous") & 
                                      foliage == "leaf-on"), aes(low_soil)) +
  geom_density(kernel = "gaussian", fill = "purple", alpha=.2) +
  geom_density(data = filter(joined_data_mean_centered, 
                             macro %in% list("coniferous", "deciduous"), 
                             foliage == "leaf-on"), aes(high_soil),
               fill = "blue", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Leaf-on soil")  +
  # Add annotation of overlap value
  annotate("text",
           label = as.character((filter(KDE_overlap, 
                                        macro == "leaf-on", micro == "soil"))$overlap),
           x = 30, y = .28) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.key.size = unit(.2,"line"))

leafon_surface <- ggplot(data = filter(joined_data_mean_centered,
                                       macro %in% list("coniferous", "deciduous") & 
                                         foliage == "leaf-on"), aes(low_surface)) +
  geom_density(kernel = "gaussian", fill = "light green", alpha=.2) +
  geom_density(data = filter(joined_data_mean_centered, macro %in% list("coniferous", "deciduous"), 
                             foliage == "leaf-on"), aes(high_surface),
               fill = "dark green", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Leaf-on surface")  +
  
  # Add annotation of overlap value
  annotate("text",
           label = as.character((filter(KDE_overlap, 
                                        macro == "leaf-on", micro == "surface"))$overlap),
           x = 30, y = .28) +
  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

leafon_canopy <- ggplot(data = filter(joined_data_mean_centered,
                                      macro %in% list("coniferous", "deciduous") & 
                                        foliage == "leaf-on"), aes(low_canopy)) +
  geom_density(kernel = "gaussian", fill = "red", alpha=.2) +
  geom_density(data = filter(joined_data_mean_centered, macro %in% list("coniferous", "deciduous"), 
                             foliage == "leaf-on"), aes(high_canopy),
               fill = "dark red", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Leaf-on canopy")  +
  # Add annotation of overlap value
  annotate("text",
           label = as.character((filter(KDE_overlap, 
                                        macro == "leaf-on", micro == "canopy"))$overlap),
           x = 30, y = .28) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

## .......** Temp leaf-off ############

leafoff_soil <- ggplot(data = filter(joined_data_mean_centered, 
                                     macro %in% list("coniferous", "deciduous") & 
                                       foliage == "leaf-off"), aes(low_soil)) +
  geom_density(kernel = "gaussian", fill = "purple", alpha=.2) +
  geom_density(data = filter(joined_data_mean_centered, macro %in% list("coniferous", "deciduous") 
                             & foliage == "leaf-off"), aes(high_soil),
               fill = "blue", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Leaf-off soil") +
  # Add annotation of overlap value
  annotate("text",
           label = as.character((filter(KDE_overlap, 
                                        macro == "leaf-off", micro == "soil"))$overlap),
           x = 30, y = .28) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

leafoff_surface <- ggplot(data = filter(joined_data_mean_centered,
                                        macro %in% list("coniferous", "deciduous") &  
                                          foliage == "leaf-off"), aes(low_surface)) +
  geom_density(kernel = "gaussian", fill = "light green", alpha=.2) +
  geom_density(data = filter(joined_data_mean_centered, macro %in% list("Dense coniferous", "Ponderosa pine", 
                                                                          "deciduous"), 
                             foliage == "leaf-off"), aes(high_surface),
               fill = "dark green", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Leaf-off surface") +
  # Add annotation of overlap value
  annotate("text",
           label = as.character((filter(KDE_overlap, 
                                        macro == "leaf-off", micro == "surface"))$overlap),
           x = 30, y = .28) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

leafoff_canopy <- ggplot(data = filter(joined_data_mean_centered,
                                       macro %in% list("coniferous", "deciduous") &  
                                         foliage == "leaf-off"), aes(low_canopy)) +
  geom_density(kernel = "gaussian", fill = "red", alpha=.2) +
  geom_density(data = filter(joined_data_mean_centered, macro %in% list("Dense coniferous", "Ponderosa pine", 
                                                                          "deciduous"), 
                             foliage == "leaf-off"), aes(high_canopy),
               fill = "dark red", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Leaf-off canopy") +
  # Add annotation of overlap value
  annotate("text",
           label = as.character((filter(KDE_overlap, 
                                        macro == "leaf-off", micro == "canopy"))$overlap),
           x = 30, y = .28) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

## .......** Temp non-forest ############

nonforest_soil <- ggplot(data = filter(joined_data_mean_centered, 
                                       macro == "non-forest"), 
                         aes(low_soil)) +
  geom_density(kernel = "gaussian", fill = "purple", alpha=.2) +
  geom_density(data = filter(joined_data_mean_centered, macro == "non-forest"), 
               aes(high_soil),
               fill = "blue", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Non-forest soil") +
  # Add annotation of overlap value
  annotate("text",
           label = as.character((filter(KDE_overlap, 
                                        macro == "non-forest", micro == "soil"))$overlap),
           x = 30, y = .28) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

nonforest_surface <- ggplot(data = filter(joined_data_mean_centered, 
                                          macro == "non-forest"), 
                            aes(low_surface)) +
  geom_density(kernel = "gaussian", fill = "light green", alpha=.2) +
  geom_density(data = filter(joined_data_mean_centered, macro == "non-forest"), aes(high_surface),
               fill = "dark green", alpha=.2) +
  coord_cartesian(xlim = c(-40, 43), ylim = c(0.00, 0.30), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(title = "Non-forest surface") +
  # Add annotation of overlap value
  annotate("text",
           label = as.character((filter(KDE_overlap, 
                                        macro == "non-forest", micro == "surface"))$overlap),
           x = 30, y = .28) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

## ....** Arranging #############
# Legends

# Legend extraction function
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

soil_legend <- g_legend(trop_soil)

surface_legend <- g_legend(trop_surface)

canopy_legend <- g_legend(trop_canopy)

trop_soil <- trop_soil +
  theme(legend.position = "none")

trop_surface <- trop_surface +
  theme(legend.position = "none")

trop_canopy <- trop_canopy +
  theme(legend.position = "none")


legends <- grid.arrange(grobs = list(soil_legend, 
                                     surface_legend, 
                                     canopy_legend),
                        nrow = 3)
plots <- grid.arrange(grobs = list(trop_soil, trop_surface, trop_canopy,
                                   leafon_soil, leafon_surface, leafon_canopy,
                                   leafoff_soil, leafoff_surface, leafoff_canopy,
                                   nonforest_soil, nonforest_surface), 
                      ncol = 3,
                      left = grid.text(
                        "Density Probability", gp = gpar(fontsize = 18), rot = 90),
                      bottom = grid.text(
                        "Temperature", gp = gpar(fontsize = 18)))

plots

## 4. Write out plots ###################

ggsave(plot = plots, 
       filename = "figures/thermal_distribution_comparison/distribution_comparison.png", 
       width = 9, height = 5)


