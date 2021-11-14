# David Klinges
# Script init: 2019-08-06
# This script generates plots of latitude vs various metrics of overlap
# (KDE_overlap, TAD @ monthly resolution, asymptotes of TAD overlap across
# temporal rez)

## 1. Workspace prep ###########

library(tidyverse)

KDE_overlap <- read_csv("data/04_analysis/compare_density_distribs/KDE_overlap_monthly.csv")
asymptotes <- read_csv("data/04_analysis/temporal_rez/asymptote_values/microMacroFoliage_asympotote_values.csv")
TAD_monthly <- read_csv("data/03_compiled/temporal_rez/temporal_rez_avgyears.csv")

mountains <- read_csv("data/03_compiled/mountains.csv")

## 2. Data curation ##########

KDE_overlap <- KDE_overlap %>% 
  rename(KDE_overlap = overlap)

TAD_monthly <- TAD_monthly %>% 
  filter(temporal_rez == 30) %>% 
  group_by(micro, macro, foliage) %>% 
  summarize(TAD_at_month_timestep = mean(TAD)) %>% 
  ungroup()

asymptotes <- asymptotes %>% 
  mutate(parameter = gsub(".lin.", "", parameter)) %>% 
  filter(parameter != "c0" & parameter != "lrc") %>% 
  separate(parameter, into = c("micro", "macro", "foliage"), sep = "_") %>% 
  rename(asymptote = values)

mountains <- mountains %>% 
  dplyr::select(site, latitude) %>% 
  mutate(latitude = abs(latitude))

plot_data <- KDE_overlap %>% 
  left_join(mountains) %>% 
  left_join(TAD_monthly) %>% 
  left_join(asymptotes) %>% 
  distinct()

## 2. Plot #############

## ....A. Asym + TAD + KDE ################

asymptote <- ggplot(plot_data, aes(latitude, asymptote)) +
  geom_point(aes(color = micro, size = 3)) +
  theme_bw() +
  theme(legend.position = "none")

tad <- ggplot(plot_data, aes(latitude, TAD_at_month_timestep)) +
  geom_point(aes(color = micro, size = 3)) +
  theme_bw() +
  theme(legend.position = "none")

KDE_overlaps <- ggplot(plot_data, aes(latitude, KDE_overlap)) +
  geom_point(aes(color = micro, size = 3)) +
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

plots <- grid.arrange(grobs = list(asymptote, tad, KDE_overlaps, legend), 
                      layout_matrix = rbind(c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4)))

ggsave(plot = plots, "figures/latitude/latitudeVSoverlap.png")



