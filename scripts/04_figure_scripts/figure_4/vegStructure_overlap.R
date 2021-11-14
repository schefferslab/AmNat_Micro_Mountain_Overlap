# David Klinges
# This script generates plots of the magnitude of change in overlap d-score 
#   values across various drivers, as regression plots

## 1. Workspace prep ##############
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(ggnewscale)
library(viridis)
library(grid)
library(gridExtra)
library(e1071)
library(ggdendro)
library(scales)
library(ggpmisc)

mountains <- read_csv("data/03_compiled/mountains_overlap_monthly_avg.csv",
                      col_types = cols(
                        # year = col_double(),
                        elevation_change = col_double()
                      ))

KDE_overlap <- read_csv("data/04_analysis/compare_density_distribs/KDE_overlap_monthly.csv")

## 2. Curate data ########

## Join KDE overlap ###############

overlap_data <- mountains %>% 
  left_join(KDE_overlap)

## Rescaling ###################

overlap_data <- overlap_data %>% 
  # Create an index for veg x foliage
  mutate(veg_foliage = veg_structure * foliage_cover) %>% 
  mutate(veg_structure = scales::rescale(veg_structure, to = c(0, 1))) %>% 
  mutate(veg_foliage = scales::rescale(veg_foliage, to = c(0, 1))) %>% 
  mutate(kde = scales::rescale(kde, to = c(0, 1))) %>% 
  mutate(kde_elevCorr = scales::rescale(kde_elevCorr, to = c(0, 1)))

## ....A. Add factor flags ######

# Recode factors to capitalize
overlap_data <- overlap_data %>%
  mutate(micro = dplyr::recode(micro, "soil" = "D. Soil", "surface" = "C. Surface",
                        "canopy" = "B. Canopy")) %>% 
  mutate(macro = tools::toTitleCase(macro))

macro_factors <- c("Tropical Broadleaf", "Ponderosa Pine", "Dense Coniferous", 
                   "Deciduous", "Meadow Near Forest", "Developed", "Alpine Meadow", 
                   "Scrub Shrub", "Hot Desert")

micro_factors <- c("B. Canopy", "C. Surface", "D. Soil")

seasonal_factors <- c("leaf-on and snow", "leaf-on and no snow",
                      "leaf-off and snow", "leaf-off and no snow")

overlap_data <- overlap_data %>%
  mutate(micro = factor(micro, levels = micro_factors)) %>% 
  mutate(seasonal_attributes = paste(foliage, snow_presence, sep = " and ")) %>% 
  mutate(seasonal_attributes = factor(seasonal_attributes, levels = seasonal_factors))



## ....B. Calculate mean overlap values ############


TAD_micro_means <- overlap_data %>% 
  group_by(micro) %>% 
  summarize(TAD_mean = mean(TAD_elevCorr, na.rm = TRUE))

dscore_micro_means <- overlap_data %>% 
  group_by(micro) %>% 
  summarize(JanzenDscore_mean = mean(janzenDscore_elevCorr, na.rm = TRUE))

KDE_micro_means <- overlap_data %>% 
  group_by(micro) %>% 
  summarize(kde_mean = mean(kde_elevCorr, na.rm = TRUE))

micro_means <- TAD_micro_means %>% 
  full_join(dscore_micro_means) %>% 
  full_join(KDE_micro_means)


## 7. TAD ~ Veg * Foliage ################

## ....A. TAD: regression plots, single panel, foliage colors ########

TAD <- ggplot(data = overlap_data, aes(x = veg_foliage, y = TAD_elevCorr)) +
  scale_color_gradient(low = "#e08626", high = "#307233") +
  geom_point(aes(x = veg_foliage, y = TAD_elevCorr, color = veg_foliage), size = 2.2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "black") +
  # scale_color_viridis(discrete = TRUE, option = "C") +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = c(-0.1, 1.1), ylim = c(-20, 30), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(size=8)) +
  labs(color = "Foliage Cover") +
  labs(title = "A. All Vertical Microhabitats") +
  # labs(x = "Vegetation Structure Index") +
  xlab("Vegetation Structure") +
  ylab("Thermal Overlap") +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 10, angle = 45, margin = margin(t = 5)),
        plot.title = element_text(size = 18, hjust =  0.5))


## ....B. TAD: regression plots, micro panels foliage colors ########

overlap_data <- filter(overlap_data, macro != "Developed")

TAD_byMicro <- ggplot(data = overlap_data, aes(x = veg_foliage, y = TAD_elevCorr)) +
  scale_color_gradient(low = "#e08626", high = "#307233") +
  geom_point(aes(x = veg_foliage, y = TAD_elevCorr, color = veg_foliage), size = 2.2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "black") +
  # scale_color_viridis(discrete = TRUE, option = "C") +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = c(-0.1, 1.1), ylim = c(-20, 30), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(size=8)) +
  labs(color = "Foliage") +
  # labs(x = "Vegetation Structure Index") +
  xlab("Vegetation Structure") +
  ylab("Thermal Overlap") +
  geom_text(data = micro_means,
            aes(x = 0.45, y = 22.5,
                label = paste("mean overlap: \n",
                              round(TAD_mean, digits = 2), sep = " "))) +
  # stat_poly_eq(formula = y ~ x, 
  #              aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
  #              parse = TRUE, size = 3.5, label.x = 0.5) + 
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 10, angle = 45, margin = margin(t = 5)),
        strip.text = element_text(size = 18),
        strip.background = element_blank()) +
  facet_wrap(~micro) 

## ....C. Arrange grobs ######

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(TAD)

TAD_onepanel <- TAD +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.position = "none")

TAD_byMicro_onepanel <- TAD_byMicro +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.position = "none")

two_panel <- grid.arrange(TAD_onepanel, TAD_byMicro_onepanel, nrow = 2,
                          bottom = grid.text(
                            "Vegetation Structure", 
                            gp = gpar(fontsize = 20)))
two_panel_legend <- grid.arrange(two_panel, legend, ncol = 2, 
                                 widths = c(5, 1.5),
                                 left = grid.text(
                                   "Thermal Overlap", 
                                   gp = gpar(fontsize = 20), rot = 90))

## ....D. Write out figure ##################

ggsave(plot = TAD_byMicro, filename = "figures/figure_4/vegFoliage_byMicro_byFoliage_TAD.png",
       height = 7.9, width = 8.2)

ggsave(plot = TAD, filename = "figures/figure_4/vegFoliage_byFoliage_TAD.png",
       height = 7.9, width = 8.2)

ggsave(plot = two_panel_legend, filename = "figures/figure_4/vegFoliage_byFoliage_TAD_twoPanel.png",
       width = 9.58, height = 7.2)



## 7. kde ~ Veg * Foliage ################

## ....A. kde: regression plots, single panel, foliage colors ########

kde_onepanel <- ggplot(data = overlap_data, aes(x = veg_foliage, y = kde_elevCorr)) +
  scale_color_gradient(low = "#e08626", high = "#307233", breaks = c(0, 0.5, 1)) +
  geom_point(aes(x = veg_foliage, y = kde_elevCorr), color = "#307233", 
             size = 2.2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "#307233", fill = "#307233") +
  # scale_color_viridis(discrete = TRUE, option = "C") +
  #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
  coord_cartesian(xlim = c(-0.1, 1.1), ylim = c(-0.2, 1.4), expand = FALSE) +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(size=8)) +
  labs(color = "Foliage Cover") +
  labs(title = "A. All Vertical Microhabitats") +
  # labs(x = "Vegetation Structure Index") +
  xlab(NULL) +
  ylab(NULL) +
  # Add equation and r-squared as text
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, size = 5, label.x = 0.5, label.y = 0.83) + 
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 10, angle = 45, margin = margin(t = 5)),
        plot.title = element_text(size = 18, hjust =  0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        # legend.position = c(0.7, 0.85),
        # legend.direction = "horizontal",
        # legend.key.size = unit(.8, "cm"),
        legend.position = "none")
  # guides(color = guide_colorbar(title.position="top", title.hjust = 0.5, size = 2))

## ....B. kde: regression plots, micro panels foliage colors ########

kde_byMicro <- ggplot(data = overlap_data, aes(x = veg_foliage, y = kde_elevCorr)) +
  # scale_color_gradient(low = "#e08626", high = "#307233") +
  geom_point(aes(x = veg_foliage, y = kde_elevCorr, color = micro), 
             size = 2.2, alpha = 0.7) +
  scale_color_manual(values = c("D. Soil" = "#a285fa",
                                "C. Surface" = "#9dcb9a",
                                "B. Canopy" = "#d98c8e")) +
  geom_smooth(method = "lm", color = "#307233", fill = "#307233") +
  coord_cartesian(xlim = c(-0.1, 1.1), ylim = c(-0.2, 1.2), expand = FALSE) +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_text(size=8)) +
  labs(color = "Foliage") +
  xlab("Vegetation Structure") +
  ylab("Thermal Overlap") +
  # Add equation and r-squared as text
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, size = 3.5, label.x = 0.5, label.y = 0.1) + 
  geom_text(data = micro_means,
            aes(x = 0.45, y = -.8,
                label = paste("mean overlap: ",
                              round(kde_mean, digits = 2), sep = " "))) +
  # stat_poly_eq(formula = y ~ x, 
  #              aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
  #              parse = TRUE, size = 3.5, label.x = 0.5) + 
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 10, angle = 45, margin = margin(t = 5)),
        strip.text = element_text(size = 18),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  facet_wrap(~micro, ncol = 1) 

# ....C. Arrange grobs ######

kde_byMicro_onepanel <- kde_byMicro +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.position = "none")

two_pane_vertical <- grid.arrange(kde_onepanel, kde_byMicro_onepanel, nrow = 1,
                          widths = c(2, 1),
                          left = grid.text(
                            "Thermal Overlap Across Elevation", 
                            gp = gpar(fontsize = 20), rot = 90),
                          bottom = grid.text(
                            "Vegetation Structure", 
                            gp = gpar(fontsize = 20)))

two_panel_legend <- grid.arrange(two_panel, legend, ncol = 2, 
                                 widths = c(5, 1.5),
                                 left = grid.text(
                                   "Thermal Overlap", 
                                   gp = gpar(fontsize = 20), rot = 90))

g <- ggplotGrob(kde_onepanel)

class(ggplot(legend))

legend +
  annotation_custom(
    grob = g,
    xmin = 0,
    xmax = .8,
    ymin = -.5,
    ymax = 0
  ) +
  annotation_custom(
    grob = rectGrob(gp = gpar(fill = "white")),
    xmin = 7.5,
    xmax = Inf,
    ymin = -Inf,
    ymax = 5
  )

g <- ggplotGrob(qplot(1, 1) +
                  theme(plot.background = element_rect(colour = "black")))
qplot(1:10, 1:10) +
  annotation_custom(
    grob = g,
    xmin = 1,
    xmax = 5,
    ymin = 5,
    ymax = 10
  ) +
  annotation_custom(
    grob = rectGrob(gp = gpar(fill = "white")),
    xmin = 7.5,
    xmax = Inf,
    ymin = -Inf,
    ymax = 5
  )

## ....D. Write out figure ##################

ggsave(plot = kde_byMicro, filename = "figures/figure_4/vegFoliage_byMicro_byFoliage_kde.png",
       height = 7.9, width = 8.2)

ggsave(plot = kde, filename = "figures/figure_4/vegFoliage_byFoliage_kde.png",
       height = 7.9, width = 8.2)

ggsave(plot = two_panel_legend, filename = "figures/figure_4/vegFoliage_byFoliage_kde_twoPanel.png",
       width = 9.58, height = 7.2)

ggsave(plot = two_pane_vertical, filename = "figures/figure_4/fig_4.pdf",
       width = 9.58, height = 7.2)

# ## RECYLCLING BIN #######
# 
# ## 5. Just TAD, color by Foliage Cover ################
# 
# ## ....A. TAD: regression plots, single panel, foliage colors ########
# 
# TAD <- ggplot(data = overlap_data, aes(x = veg_structure, y = TAD_elevCorr)) +
#   scale_color_gradient(low = "#e08626", high = "#307233") +
#   geom_point(aes(x = veg_structure, y = TAD_elevCorr, color = veg_foliage), size = 2.2, alpha = 0.7) +
#   geom_smooth(method = "lm", color = "black") +
#   # scale_color_viridis(discrete = TRUE, option = "C") +
#   #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
#   coord_cartesian(xlim = c(-0.1, 1.1), ylim = c(-20, 30), expand = FALSE) +
#   theme_bw(base_size = 14) +
#   theme(axis.text.x = element_text(size=8)) +
#   labs(color = "Foliage Cover") +
#   # labs(x = "Vegetation Structure Index") +
#   xlab("Vegetation Structure") +
#   ylab("Thermal Overlap") +
#   # Add equation and r-squared as text
#   stat_poly_eq(formula = y ~ x, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE, size = 3.5, label.x = 0.5) + 
#   theme(axis.text.y = element_text(size = 15),
#         axis.text.x = element_text(size = 10, angle = 45, margin = margin(t = 5)))
# 
# ## ....B. TAD: regression plots, micro panels foliage colors ########
# 
# TAD_byMicro <- ggplot(data = overlap_data, aes(x = veg_structure, y = TAD_elevCorr)) +
#   scale_color_gradient(low = "#e08626", high = "#307233") +
#   geom_point(aes(x = veg_structure, y = TAD_elevCorr, color = veg_foliage), size = 2.2, alpha = 0.7) +
#   geom_smooth(method = "lm", color = "black") +
#   # scale_color_viridis(discrete = TRUE, option = "C") +
#   #geom_point(aes(shape = factor(site), color = factor(site)), size = 3) +
#   coord_cartesian(xlim = c(-0.1, 1.1), ylim = c(-20, 30), expand = FALSE) +
#   theme_bw(base_size = 14) +
#   theme(axis.text.x = element_text(size=8)) +
#   labs(color = "Foliage") +
#   # labs(x = "Vegetation Structure Index") +
#   xlab("Vegetation Structure") +
#   ylab("Thermal Overlap") +
#   geom_text(data = micro_means,
#             aes(x = 0.45, y = 22.5,
#                 label = paste("mean overlap: ",
#                               round(TAD_mean, digits = 2), sep = " "))) +
#   theme(axis.text.y = element_text(size = 15),
#         axis.text.x = element_text(size = 10, angle = 45, margin = margin(t = 5))) +
#   # Add equation and r-squared as text
#   stat_poly_eq(formula = y ~ x, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#                parse = TRUE, size = 3.5, label.x = 0.5) + 
#   facet_wrap(~micro) 
# 
# ## ....C. Arrange grobs ######
# 
# g_legend<-function(a.gplot){
#   tmp <- ggplot_gtable(ggplot_build(a.gplot))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   legend
# }
# 
# legend <- g_legend(TAD)
# 
# TAD_onepanel <- TAD +
#   ylab(NULL) +
#   xlab(NULL) +
#   theme(legend.position = "none")
# 
# TAD_byMicro_onepanel <- TAD_byMicro +
#   ylab(NULL) +
#   xlab(NULL) +
#   theme(legend.position = "none")
# 
# two_panel <- grid.arrange(TAD_onepanel, TAD_byMicro_onepanel, nrow = 2)
# two_panel_legend <- grid.arrange(two_panel, legend, ncol = 2, 
#                                  widths = c(5, 1),
#                                  left = grid.text(
#                                    "Thermal Overlap", 
#                                    gp = gpar(fontsize = 20), rot = 90),
#                                  bottom = grid.text(
#                                    "Vegetation Structure", 
#                                    gp = gpar(fontsize = 20)))
# 
# ## ....D. Write out figure ##################
# 
# ggsave(plot = TAD_byMicro, filename = "figures/figure_4/vegStructure_byMicro_byFoliage_TAD.png",
#        height = 7.9, width = 8.2)
# 
# ggsave(plot = TAD, filename = "figures/figure_4/vegStructure_byFoliage_TAD.png",
#        height = 7.9, width = 8.2)
# 
# ggsave(plot = two_panel_legend, filename = "figures/figure_4/vegStructure_byFoliage_TAD_twoPanel.png",
#        height = 7.9, width = 8.2)
# 
