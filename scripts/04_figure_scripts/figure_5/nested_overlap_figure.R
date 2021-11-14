# David Klinges
# Script init: 2019-09-12
# This script generates plots of latitude vs TAD overlap

## 1. Workspace prep ##############

## ....A. Load libraries #########
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
library(effects)

## ....B. Read in data #########

mount <- read_csv("data/03_compiled/mountains_overlap_monthly_avg.csv",
                              col_types = cols(
                                # year = col_double(),
                                elevation_change = col_double()
                              ))

## ....C. Read in mixed model results #############

chosen_parameter <- "kde"

# Load in saved model
global_model <- readRDS(file = "data/04_analysis/model_outputs/kde/monthly/all_micros/global_avg_model.rds")
# Need to also read in data that was used for global model
mountains <- readRDS(file = "data/04_analysis/model_outputs/kde/monthly/all_micros/mountains.rds")

models <- list.files(paste0("data/04_analysis/model_outputs/", chosen_parameter, 
                              "/monthly/all_micros/"),
                       pattern = "model_results")
  
models <- lapply(models, FUN = function(x) {
  data <- read_csv(paste0("data/04_analysis/model_outputs/", chosen_parameter, 
                            "/monthly/all_micros/", x))
    
})

models <- do.call(bind_rows, models) %>% 
  filter(model == "global")
  
## 2. Data curation ##############
## ....A. Add factor flags ######

mount <- mount %>% 
  filter(complete.cases(kde))

# dplyr::recode factors to capitalize
mount <- mount %>%
  mutate(micro = dplyr::recode(micro, "soil" = "Soil", "surface" = "Surface",
                               "canopy" = "Canopy")) %>% 
  mutate(macro = tools::toTitleCase(macro))

macro_factors <- c("Tropical Broadleaf", "Ponderosa Pine", "Dense Coniferous", 
                   "Deciduous", "Oil Palm", "Meadow Near Forest", "Developed", "Alpine Meadow", 
                   "Scrub Shrub", "Hot Desert")

micro_factors <- c("Soil", "Surface", "Canopy")

seasonal_factors <- c("leaf-on and snow", "leaf-on and no snow",
                      "leaf-off and snow", "leaf-off and no snow")

mount <- mount %>%
  mutate(micro = factor(micro, levels = micro_factors)) %>% 
  mutate(seasonal_attributes = paste(foliage, snow_presence, sep = " and ")) %>% 
  mutate(seasonal_attributes = factor(seasonal_attributes, levels = seasonal_factors))

mount <- mount %>% 
  mutate(is_forest = ifelse(macro %in% c("Tropical Broadleaf", "Ponderosa Pine",
                                         "Dense Coniferous", "Deciduous", 
                                         "Degraded Tropical Broadleaf"), "Forest",
                            "Non-forest"))

## ....Take residuals of mesogeographic parameters ############


mount <- mount %>% 
  mutate(veg_foliage = ifelse(foliage_cover > 0, foliage_cover * veg_structure,
                              veg_structure)) %>% 
  mutate(veg_foliage = veg_structure * foliage_cover) %>% 
  mutate(veg_foliage = scales::rescale(veg_foliage, to = c(0, 1)))

elevVegSnowHeight_lm <- lm(kde ~ elevation_change + veg_foliage + snowdepth + height, data = mount)
elevVegSnowLat_lm <- lm(kde ~ elevation_change + veg_foliage + snowdepth + latitude, data = mount)
elevVeg_lm <- lm(kde ~ elevation_change + veg_foliage, data = mount)
elevSnow_lm <- lm(kde ~ elevation_change + snowdepth, data = mount)
elevSnowLat_lm <- lm(kde ~ elevation_change + snowdepth + latitude, data = mount)
elevLat_lm <- lm(kde ~ elevation_change + latitude, data = mount)

## ....Rescaling ########

mount <- mount %>% 
  mutate(kde_elevCorr_rescale = scales::rescale(kde_elevCorr, to = c(0, 1)))

## 3. Generate plots #########
## ....A. Macro: KDE ~ latitude regression ########

coef <- models %>%
  filter(rowname == "latitude")

coef_effect <- as.data.frame(effects::Effect(focal.predictors = "latitude", 
                                                 mod = global_model))

# Back-scale to real latitudes
coef_effect$latitude <- c(min(mount$latitude), .2 * max(mount$latitude), .5 * max(mount$latitude),
                          .8 * max(mount$latitude), max(mount$latitude))

kde_latitude <- ggplot() +
  geom_point(data = mount, aes(x = abs(latitude), y = kde_elevCorr_rescale), color = "#cc9900", 
             size = 2.2, alpha = 0.9) +
  geom_line(data=coef_effect, aes(x=latitude, y=fit), color="#cc9900") +
  geom_ribbon(data= coef_effect, aes(x=latitude, ymin=lower, ymax=upper), 
              alpha= 0.3, fill="#cc9900") +
  coord_cartesian(xlim = c(-2, 70), ylim = c(-.2, 1.2), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(y = "",
       x = "Latitude (°)",
       title = "A. Macrogeography") +
  # Add mixed model coefficient estimates
  geom_text(data = coef,
            aes(x = 50, y = 1.1,
                label = paste("beta ==",
                              round(estimate, digits = 2),
                              "* ' '(",  round(`2.5 %`, digits = 2),
                              ", ", round(`97.5 %`, digits = 2), ")")), parse = T) +
  # Re-size plot
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 14),
        plot.title = element_text(size = 20, hjust =  0.5),
        plot.margin = unit(c(.1,5.7,.3,4.3),"cm"))

## ....B. Meso: KDE ~ snow and foliage ########

## ....** Snow #################

coef <- models %>%
  filter(rowname == "snowdepth")

coef_effect <- as.data.frame(effects::Effect(focal.predictors = "snowdepth", 
                                                   mod = global_model))
  
# Back-scale to real snow depths
coef_effect$snowdepth <- c(min(mount$snowdepth), .2 * max(mount$snowdepth), .5 * max(mount$snowdepth),
                            .8 * max(mount$snowdepth), max(mount$snowdepth))

KDE_snow <- ggplot() +
  geom_point(data = mount, aes(x = snowdepth, y = kde_elevCorr_rescale), color = "#00ccff", 
             size = 2.2, alpha = 0.9) +
  geom_line(data=coef_effect, aes(x=snowdepth, y=fit), color="#00ccff") +
  geom_ribbon(data= coef_effect, aes(x=snowdepth, ymin=lower, ymax=upper), 
              alpha= 0.3, fill="#00ccff") +
  coord_cartesian(xlim = c(-2, 90), ylim = c(-.2, 1.2), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(x = "Snow Depth (m)",
       y = NULL) +
  # Add mixed model coefficient estimates
  geom_text(data = coef,
            aes(x = 60, y = 1.1,
                label = paste("beta ==",
                              round(estimate, digits = 2),
                              "* ' '(",  round(`2.5 %`, digits = 2),
                              ", ", round(`97.5 %`, digits = 2), ")")), parse = T) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(size = 14, hjust =  0.5))

## ....** Foliage ###############

coef <- models %>% 
  filter(rowname == "veg_foliage")

coef_effect$veg_foliage <- c(min(mount$veg_foliage), .2 * max(mount$veg_foliage), .5 * max(mount$veg_foliage),
                           .8 * max(mount$veg_foliage), max(mount$veg_foliage))

coef_effect <- as.data.frame(effects::Effect(focal.predictors = "veg_foliage", 
                                             mod = global_model))

KDE_foliage <- ggplot() +
  geom_point(data = mount, aes(x = veg_foliage, y = kde_elevCorr_rescale), color = "#307233", 
             size = 2.2, alpha = 0.9) +
  geom_line(data=coef_effect, aes(x=veg_foliage, y=fit), color="#307233") +
  geom_ribbon(data= coef_effect, aes(x=veg_foliage, ymin=lower, ymax=upper), 
              alpha= 0.3, fill="#307233") +
  coord_cartesian(xlim = c(-.2, 1.2), ylim = c(-.2, 1.2), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(x = "Foliage Structure",
       y = NULL) +
  # Add mixed model coefficient estimates
  geom_text(data = coef,
            aes(x = .7, y = 1.1,
                label = paste("beta ==",
                              round(estimate, digits = 2),
                              "* ' '(",  round(`2.5 %`, digits = 2),
                              ", ", round(`97.5 %`, digits = 2), ")")), parse = T) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(size = 14, hjust =  0.5))

## ....C. Micro: KDE ~ micro height #############

coef <- models %>% 
  filter(rowname == "height")

coef_effect <- as.data.frame(effects::Effect(focal.predictors = "height", 
                                             mod = global_model))

# Recode x values to match log transform
coef_effect$height <- c(min(mount$height), .2 * max(mount$height), .5 * max(mount$height),
                        .8 * max(mount$height), max(mount$height))

# Customized transformation of x-axis: log transform, add 0.12 for deepest depth of soil sensors
# Keeping this one line in here, but NOTE: IF YOU WANT TO RE-USE GO THROUGH
# COMMIT HISTORY TO SEE HOW IT WAS USED
# one_over_trans = function() trans_new("one_over", function(x) log10(x) - 0.12, function(x)  10^(x + 0.12) - 0.12)

micro_plot <- ggplot() +
  geom_jitter(data = mount, aes(height, kde_elevCorr_rescale, color = micro), 
              width = 0.15) +
  # Add mixed model coefficient estimates
  geom_text(data = coef,
            aes(x = 16, y = 1.09,
                label = paste("beta ==",
                             round(estimate, digits = 2),
                             "* ' '(",  round(`2.5 %`, digits = 2),
                             ", ", round(`97.5 %`, digits = 2), ")")), parse = T) +
  geom_line(data=coef_effect, aes(x=height, y=fit), color="#8C8C8C") +
  geom_ribbon(data= coef_effect, aes(x=height, ymin=lower, ymax=upper),
              alpha= 0.3, fill="#8C8C8C") +
  scale_color_manual(values = c("Soil" = "#a285fa",
                                "Surface" = "#9dcb9a",
                                "Canopy" = "#d98c8e")) +
  coord_cartesian(xlim= c(-1, 25), ylim = c(-.2, 1.2), expand = FALSE) +
  theme_classic(base_size = 14) +
  labs(x = "Sensor Height (m)",
       y = "",
       color = "Vertical\nMicrohabitat",
       title = "C. Microgeography") + 
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 14),
        plot.title = element_text(size = 20, hjust =  0.5),
        plot.margin = unit(c(.1,2.05,.3,4.05),"cm"))

# Arrange grobs #################

meso_panels <- grid.arrange(KDE_snow, KDE_foliage, # KDE_elevchange, 
                            nrow = 1,
                            top = grid.text("B. Mesogeography",
                            gp = gpar(fontsize = 20)))

three_panels <- grid.arrange(kde_latitude, meso_panels, micro_plot, nrow = 3, 
                             heights = c(1.8, 1.8, 1.8),
                             left = grid.text(
                               "Thermal Overlap Across Elevation", 
                               gp = gpar(fontsize = 24), rot = 90))



## 4. Write out plots #######

ggsave(plot = three_panels, filename = "figures/figure_5/fig_5.pdf",
       height = 10.6, width = 8.7)

## RECYCLING BIN ############
# 
# ## ....KDE means ###########
# 
# forest <- mount %>% 
#   filter(is_forest == "Forest")
# 
# nonforest <- mount %>% 
#   filter(is_forest == "Non-forest")
# 
# KDE_micro_means <- mount %>% 
#   group_by(is_forest, micro) %>% 
#   summarize(KDE_mean = mean(kde_elevCorr_rescale, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   mutate(label = ifelse(micro == "Soil" & is_forest == "Forest", "A",
#                         ifelse(micro == "Surface" & is_forest == "Forest", "B",
#                                ifelse(micro == "Canopy" & is_forest == "Forest", "B",
#                                       ifelse(micro == "Soil" & is_forest == "Non-forest", "A",
#                                              ifelse(micro == "Surface" & is_forest == "Non-forest", "C", NA))))))
# 
# KDE_micro_means_forest <- forest %>% 
#   group_by(micro) %>% 
#   summarize(KDE_mean = mean(kde_elevCorr_rescale, na.rm = TRUE))
# 
# KDE_micro_means_nonforest <- nonforest %>% 
#   group_by(micro) %>% 
#   summarize(KDE_mean = mean(kde_elevCorr_rescale, na.rm = TRUE))
# 
# ## ....KDE ~ height, forest and non-forest separately ########
# 
# # Build a plot just to take its legend
# null_plot <- ggplot(data = mount, 
#                     aes(x = height, y = kde_elevCorr_rescale)) +
#   geom_point(aes(color = veg_foliage), 
#              size = 2.2, alpha = 0.9) +
#   scale_color_gradient(low = "#e08626", high = "#307233", breaks = c(0, 0.5, 1)) +
#   labs(color = "Vegetation\nStructure") +
#   theme(
#     # legend.position = c(0.7, 0.85),
#     # legend.direction = "horizontal",
#     legend.key.size = unit(.8, "cm")) +
#   guides(color = guide_colorbar(title.position="top", title.hjust = 0.5, size = 2))
# 
# 
# forest_plot <- ggplot(data = filter(mount, is_forest == "Forest"), 
#                       aes(x = height, y = kde_elevCorr_rescale)) +
#   geom_point(aes(x = height, y = kde_elevCorr_rescale, color = veg_foliage), 
#              size = 2.2, alpha = 0.9) +
#   geom_smooth(method = "lm", fill = "#307233", color = "#307233") +
#   scale_color_gradient(low = "#bd8229", high = "#307233", breaks = c(0, 0.5, 1)) +
#   # scale_fill_manual(values = c("Non-forest" = "#e08626", "Forest" = "#307233")) +
#   coord_cartesian(xlim = c(-1, 22), ylim = c(-.05, 1.2), expand = FALSE) +
#   theme_bw(base_size = 14) +
#   labs(y = "",
#        x = "",
#        color = "Vegetation Structure",
#        title = "Forest") +
#   # Add equation and r-squared as text
#   stat_poly_eq(formula = y ~ x, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),                
#                parse = TRUE, size = 4, label.x = 0.5) + 
#   theme(legend.position = "none",
#         axis.text.y = element_text(size = 15),
#         axis.text.x = element_text(size = 15),
#         plot.title = element_text(size = 20, hjust =  0.5),
#         plot.margin = unit(c(.1,.1,0,.1),"cm"))
# 
# nonforest_plot <- ggplot(data = filter(mount, is_forest == "Non-forest"), 
#                          aes(x = height, y = kde_elevCorr_rescale)) +
#   geom_point(aes(x = height, y = kde_elevCorr_rescale, color = veg_foliage), 
#              size = 2.2, alpha = 0.9) +
#   geom_smooth(method = "lm", fill = "#e08626", color = "#e08626") +
#   scale_color_gradient(low = "#e08626", high = "#9a7e2b", breaks = c(0, 0.5, 1)) +
#   # scale_fill_manual(values = c("Non-forest" = "#e08626", "Forest" = "#307233")) +
#   coord_cartesian(xlim = c(-.5, 3.5), ylim = c(-.05, 1.2), expand = FALSE) +
#   theme_bw(base_size = 14) +
#   labs(y = "",
#        x = "",
#        title = "Non-forest") +
#   # Add equation and r-squared as text
#   stat_poly_eq(formula = y ~ x, 
#                aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),                
#                parse = TRUE, size = 4, label.x = 0.5) + 
#   theme(legend.position = "none",
#         axis.text.y = element_text(size = 15),
#         axis.text.x = element_text(size = 15),
#         plot.title = element_text(size = 20, hjust =  0.5),
#         plot.margin = unit(c(.1,.4,0,.1),"cm"))
# 

# ## ....KDE ~ micro, violin plots ##########
# 
# forest_violins <- ggplot(data = forest, 
#                          aes(x = micro, y = kde_elevCorr_rescale)) +
#   geom_violin(aes(fill = micro), alpha = 0.5) + 
#   geom_text(data = filter(KDE_micro_means, is_forest == "Forest"), 
#             aes(x = micro, y = KDE_mean + .55, label=label), size = 5) +
#   geom_jitter(width = .1, alpha = 0.15) +
#   # geom_point(data = KDE_micro_means_forest, aes(micro, KDE_mean), size = 2) +
#   coord_cartesian(ylim = c(-.05, 1.2), expand = FALSE) +
#   scale_fill_manual(values = c("Soil" = "purple",
#                                "Surface" = "light green",
#                                "Canopy" = "red")) +
#   labs(x = "",
#        y = "",
#        fill = "") +
#   theme_classic() +
#   theme(plot.background = element_blank(),
#         axis.line.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.text.y = element_text(size = 15),
#         plot.margin = unit(c(0.1,0,0,.3),"cm"))
# 
# nonforest_violins <- ggplot(data = nonforest, 
#                             aes(x = micro, y = kde_elevCorr_rescale)) +
#   geom_violin(aes(fill = micro), alpha = 0.5) + 
#   geom_text(data = filter(KDE_micro_means, is_forest == "Non-forest"), 
#             aes(x = micro, y = KDE_mean + .45, label=label), size = 5) +
#   geom_jitter(width = .1, alpha = 0.3) +
#   # geom_point(data = KDE_micro_means_nonforest, aes(micro, KDE_mean), size = 2) +
#   coord_cartesian(ylim = c(-.05, 1.2), expand = FALSE) +
#   scale_fill_manual(values = c("Soil" = "purple",
#                                "Surface" = "light green",
#                                "Canopy" = "red")) +
#   
#   labs(x = "",
#        y = "") +
#   theme_classic() +
#   theme(plot.background = element_blank(),
#         axis.line.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.line.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         legend.position = "none",
#         plot.margin = unit(c(0.1,2,0,3),"cm"))
# 

# ## ## ....C. Micro: KDE ~ micro height #############
# ## .... KDE ~ micro height, transformed axis ###############
# 
# coef <- models %>% 
#   filter(rowname == "height")
# 
# # Find 95% CI min and max
# ymin <- intercept$`2.5 %` + (coef$`2.5 %`) * mount$height
# ymax <- intercept$`97.5 %` + (coef$`97.5 %`) * mount$height
# 
# 
# fit<-nls(kde_elevCorr_rescale ~ SSasymp(height, A, lrc, c0), data = mount,
#          control = list(maxiter = 500))
# # 
# # predict <- predictNLS(fit, interval = "confidence")
# 
# # Customized transformation of x-axis: log transform, add 0.12 for deepest depth of soil sensors
# one_over_trans = function() trans_new("one_over", function(x) log10(x) - 0.12, function(x)  10^(x + 0.12) - 0.12)
# 
# mount_trans <- mount %>%
#   mutate(height = height + .12) %>%
#   mutate(log_height = log(height +.12))
# 
# coef_effect <- as.data.frame(effects::Effect(focal.predictors = "height", 
#                                              mod = global_model)) 
# 
# # Recode x values to match log transform
# coef_effect$height <- c(min(mount$height), .2 * max(mount$height), .5 * max(mount$height),
#                         .8 * max(mount$height), max(mount$height))
# 
# micro_plot <- ggplot() +
#   geom_jitter(data = mount, aes(height, kde_elevCorr_rescale, color = micro), 
#               width = 0.15) +
#   # Add mixed model coefficient estimates
#   geom_text(data = coef,
#             aes(x = 16, y = 1.09,
#                 label = paste("beta ==",
#                               round(estimate, digits = 2),
#                               "* ' '(",  round(`2.5 %`, digits = 2),
#                               ", ", round(`97.5 %`, digits = 2), ")")), parse = T) +
#   geom_line(data=coef_effect, aes(x=height, y=fit), color="#8C8C8C") +
#   geom_ribbon(data= coef_effect, aes(x=height, ymin=lower, ymax=upper),
#               alpha= 0.3, fill="#8C8C8C") +
#   # geom_ribbon(aes(ymax = predict(fit)))
#   # geom_smooth(method = "nls", formula = y ~ SSasymp(x, .641, .4218, .4495), color = "#8C8C8C", fill = "#8C8C8C") +
#   # geom_abline(intercept = intercept$estimate, slope = coef$estimate) +
#   # geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.5, fill = "#8C8C8C") +
#   # geom_smooth(method = "lm", color = "#8C8C8C", fill = "#8C8C8C") +
#   # coord_cartesian(xlim = c(-5.2, 3.3), ylim = c(-.2, 1.2), expand = FALSE) +
#   # scale_x_continuous(trans = "one_over", limits = c(-2, 50),
#   #                  breaks = c(0.02, 0.11, .62, 5.12, 30.12)) +
#   scale_color_manual(values = c("Soil" = "#a285fa",
#                                 "Surface" = "#9dcb9a",
#                                 "Canopy" = "#d98c8e")) +
#   coord_cartesian(xlim= c(-1, 25), ylim = c(-.2, 1.2), expand = FALSE) +
#   
#   # coord_cartesian(xlim = c(-5.2, 3.3), ylim = c(-.2, 1.2), expand = FALSE) +
#   theme_classic(base_size = 14) +
#   labs(x = "Sensor Height (m)",
#        # x = expression(paste(italic(ln), "[Sensor Height (m) + 0.12]")),
#        y = "",
#        color = "Vertical\nMicrohabitat",
#        title = "C. Microgeography") + 
#   # Add equation and r-squared as text
#   # stat_poly_eq(formula = y ~ x, 
#   #              aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#   #              parse = TRUE, size = 5, label.x = 0.5, label.y = 0.83) + 
#   # stat_poly_eq(formula = y ~ x,
#   #              aes(label = stat(p.value.label)),
#   #              parse = TRUE, size = 5, label.x = 0.85, label.y = 0.95) +
#   # stat_poly_eq(formula = y ~ x,
#   #   aes(label = paste(..rr.label..)),
#   #   parse = TRUE, size = 5, label.x = 0.5) +
#   theme(axis.text.y = element_text(size = 10),
#         axis.text.x = element_text(size = 10),
#         axis.title.x = element_text(size = 14),
#         plot.title = element_text(size = 20, hjust =  0.5),
#         plot.margin = unit(c(.1,2.05,.3,4.05),"cm"))
# 
# ## ....Arrange grobs #########
# g_legend<-function(a.gplot){
#   tmp <- ggplot_gtable(ggplot_build(a.gplot))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   legend
# }
# 
# violin_legend <- g_legend(forest_violins)
# 
# forest_violins <- forest_violins +
#   theme(legend.position = "none")
# 
# forest_legend <- g_legend(null_plot)
# 
# sep_forest <- grid.arrange(forest_plot, nonforest_plot, forest_legend, nrow = 1,
#                             widths = c(6, 6, 1.5),
#                            bottom = grid.text(
#                              "Sensor Height (m)", 
#                              gp = gpar(fontsize = 16)))
# 
# sep_violins <- grid.arrange(forest_violins, nonforest_violins, violin_legend, nrow = 1,
#                             widths = c(3, 4, 1),
#                             bottom = grid.text(
#                               "Vertical Microhabitat", 
#                               gp = gpar(fontsize = 16)))
# 
# three_panels <- grid.arrange(kde_latitude, sep_forest, sep_violins, nrow = 3, 
#                              heights = c(1.8, 1.8, .9),
#                              left = grid.text(
#                                "Thermal Overlap", 
#                                gp = gpar(fontsize = 20), rot = 90))
# 
# 
# three_panels <- grid.arrange(kde_latitude, forest_nonforest, violins, nrow = 3, 
#                              left = grid.text(
#                                "Thermal Overlap", 
#                                gp = gpar(fontsize = 20), rot = 90))
