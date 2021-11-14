# David Klinges
# File creation date: 2019-08-04
# This script plots predictions of the order that micro-macro-foliage combo experience overlap x
# actual overlap data, to visualize how much data lines up with predictions


## 1. Workspace prep ###########

library(tidyverse)
library(gridExtra)

predictions <- read_csv("data/04_analysis/temporal_rez/asymptote_values/microMacroFoliage_predictions.csv")
mountains_monthly <- read_csv("data/03_compiled/mountains_overlap_monthly_avg.csv")
KDE_overlaps <- read_csv("data/04_analysis/compare_density_distribs/KDE_overlap_monthly_MacroSummarized_lowRepExclude.csv")

# First iteration of this figure had asmptote values, but we're not including that
# workflow and analysis for now
# asymptotes <- read_csv("data/04_analysis/temporal_rez/asymptote_values/microMacroFoliage_asympotote_values.csv")

## 2. Data curation ###########

TAD_monthly <- mountains_monthly %>% 
  group_by(micro, macro, foliage) %>% 
  summarize(TAD = mean(TAD_elevCorr)) %>% 
  ungroup() %>% 
  mutate(parameter = paste(micro, macro, foliage, sep = "_")) %>% 
  dplyr::select(parameter, TAD, micro)

dscore_monthly <- mountains_monthly %>% 
  group_by(micro, macro, foliage) %>% 
  summarize(Dscore = mean(janzenDscore_elevCorr)) %>% 
  ungroup() %>% 
  mutate(parameter = paste(micro, macro, foliage, sep = "_")) %>% 
  dplyr::select(parameter, Dscore, micro)

KDE_overlaps <- KDE_overlaps %>% 
  mutate(parameter = paste(micro, macro, foliage, sep = "_")) %>% 
  dplyr::select(parameter, overlap_elevCorr, micro) %>% 
  rename(KDE_overlap = overlap_elevCorr)

predictions <- predictions %>% 
  mutate(parameter = gsub(".lin.", "", parameter))

plot_data <- TAD_monthly %>% 
  left_join(predictions) %>% 
  full_join(dscore_monthly) %>% 
  full_join(KDE_overlaps) %>% 
  filter(parameter != "c0" & parameter != "lrc") %>% 
  mutate(parameter = gsub("_", " ", parameter)) %>% 
  # we're going to code micros as colors in plots, so remove from labels
  mutate(parameter = gsub("soil ", "", parameter)) %>% 
  mutate(parameter = gsub("surface ", "", parameter)) %>% 
  mutate(parameter = gsub("canopy ", "", parameter))
  
## ....B. Data flags ############

plot_data <- plot_data %>%
  mutate(micro = tools::toTitleCase(micro))

micro_factors <- c("Soil", "Surface", "Canopy")

plot_data <- plot_data %>%
  mutate(micro = factor(micro, levels = micro_factors))

## 3. Plot data #########

dscore <- ggplot(plot_data, aes(predictions, Dscore)) +
  geom_point(aes(color = micro), size = 5, alpha = 0.9) +
  scale_color_manual(values = c("Soil" = "#a285fa",
                                "Surface" = "#9dcb9a",
                                "Canopy" = "#d98c8e")) +
  geom_text(aes(label=parameter), vjust = 2, size = 3) +
  coord_cartesian(xlim = c(-0.5, 20.5), ylim = c(-1, 0.5), expand = FALSE) +
  labs(x = "Predictions") +
  ylab("D-score Overlap") +
  theme_bw() +
  theme(legend.position = "none")

tad <- ggplot(plot_data, aes(predictions, TAD)) +
  geom_point(aes(color = micro), size = 5, alpha = 0.9) +
  scale_color_manual(values = c("Soil" = "#a285fa",
                                "Surface" = "#9dcb9a",
                                "Canopy" = "#d98c8e")) +
  geom_text(aes(label=parameter), vjust = 2, size = 3) +
  labs(x = "Predictions") +
  ylab("Thermal Absolute Overlap") +
  coord_cartesian(xlim = c(-0.5, 20.5), ylim = c(-10, 15), expand = FALSE) +
  
  theme_bw() +
  theme(legend.position = "none")
  
KDE_overlaps <- ggplot(plot_data, aes(predictions, KDE_overlap)) +
  geom_point(aes(color = micro), size = 5, alpha = 0.9) +
  scale_color_manual(values = c("Soil" = "#a285fa",
                                "Surface" = "#9dcb9a",
                                "Canopy" = "#d98c8e")) +
  geom_text(aes(label=parameter), vjust = 2, size = 3) +
  labs(x = "Predictions") +
  ylab("KDE-derived Overlap") +
  labs(color = "Microhabitat") +
  coord_cartesian(xlim = c(-0.5, 20.5), ylim = c(-0.4, 0.4), expand = FALSE) +
  theme_bw()

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(KDE_overlaps)

KDE_overlaps <- KDE_overlaps +
  theme(legend.position = "none")
  
plots <- grid.arrange(grobs = list(dscore, tad, KDE_overlaps, legend), 
             layout_matrix = rbind(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4)))

## 4. Write out plots #################

ggsave(plot = plots, scale = 1.1,
       "figures/predictions/predictionsVSoverlap.png")


