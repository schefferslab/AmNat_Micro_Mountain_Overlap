# David Klinges
# 2019-10-16
# This script generates figures for displaying and comparing parameter coefficient
# estimates

## 1. Workspace prep ##########

library(tidyverse)
library(grid)
library(gridExtra)
library(scales)

# models <- read_csv("data/04_analysis/model_outputs/mixed_model_results.csv")

chosen_parameter <- "kde"
parameter <- c("JanzenDscore_positive", "kde", "TAD")
# kde
# JanzenDscore_positive
# TAD
for (i in 1:length(parameter)) {
  models <- list.files(paste0("data/04_analysis/model_outputs/", parameter[i], 
                              "/monthly/all_micros/"),
                       pattern = "model_results")
  
  models <- lapply(models, FUN = function(x) {
    data <- read_csv(paste0("data/04_analysis/model_outputs/", parameter[i], 
                            "/monthly/all_micros/", x))
    
  })
  models <- do.call(bind_rows, models)
  
  soil_model <- read_csv(paste0("data/04_analysis/model_outputs/", parameter[i],
                                "/monthly/soil_only/model_results_global.csv")) %>% 
    mutate(model = "soil")
  
  surface_model <- read_csv(paste0("data/04_analysis/model_outputs/", parameter[i],
                                   "/monthly/surface_only/model_results_global.csv")) %>% 
    mutate(model = "surface")
  
  canopy_model <- read_csv(paste0("data/04_analysis/model_outputs/", parameter[i],
                                  "/monthly/canopy_only/model_results_global.csv")) %>% 
    mutate(model = "canopy")
  
  models <- bind_rows(models, soil_model, surface_model, canopy_model) 
  
  
  models <- models %>% 
    mutate(index = parameter[i])
  
  if (i == 1) {
    models_out <- models
  } else {
  models_out <- bind_rows(models_out, models)
  }
}

models <- models_out %>% 
  filter(index == chosen_parameter)

## 2. Data curation/subsetting #########

# Recode params. Backwards because axes are flipped
param_levels <- c("Microhabitat \nHeight/Depth", "Snow Depth", "Vegetation\nStructure", 
                  "_ Elevation", "Latitude")
micro_levels <- c("Canopy", "Surface", "Soil")

models <- models %>% 
  dplyr::rename(param = rowname, low_95 = `2.5 %`, high_95= `97.5 %`) %>% 
  # Round estimates to two digits
  mutate(estimate = round(estimate, 2)) %>% 
  # Designate significant parameters
  mutate(sig_params = ifelse(low_95 < 0 & high_95 > 0, 
                                  "not significant", "significant")) %>% 
  mutate(param = dplyr::recode(param, "Microhabitat Height/Depth" = 
                          "Microhabitat \nHeight/Depth",
                        "Vegetation Structure" = "Vegetation\nStructure")) %>% 
  mutate(param = dplyr::recode(param, "elevation_change" = "_ Elevation", 
                        "height" = "Microhabitat \nHeight/Depth",
                        "snowdepth" = "Snow Depth", 
                        "veg_foliage" = "Vegetation\nStructure",
                        "latitude" = "Latitude")) %>% 
  mutate(param = factor(param, levels = param_levels)) %>% 
  # Get rid of intercepts and such
  filter(complete.cases(param))

main_models <- models %>% 
  filter(model %in% c("macro", "meso", "micro", "global")) %>% 
  mutate(model = dplyr::recode(model, "macro" = "Macrogeography", 
                        "meso" = "Mesogeography",
                        "micro" = "Microgeography", 
                        "global" = "Global Model"))

global_model <- main_models %>% 
  filter(model == "Global Model")

missing <- !levels(global_model$param)%in% unique(global_model$param)
test <- levels(global_model$param)[missing]

nrow(bind_rows(global_model, .id = "param") %>%
  complete(param = names(global_model)))



if (any(missing)) {
  
  # Pull in values from the model dredge. Scrapped this
  # dredge <- data <- read_csv(paste0("data/04_analysis/model_outputs/", chosen_parameter, 
  #                                   "/monthly/all_micros/dredged_candidate_list.csv"))
  # dredge <- dredge %>% 
  #   dplyr::rename("_ Elevation" = "elevation_change", 
  #                             "Microhabitat \nHeight/Depth" =  "height",
  #                            "Snow Depth" = "snowdepth", 
  #                             "Vegetation\nStructure" =  "veg_foliage",
  #                              "Latitude" = "latitude")
  # global_model$param[missing]
  # (dredge[,test])[1]
  
  for (i in 1:length(test)) {
    global_model[nrow(global_model)+1,] <- NA
    global_model$param[nrow(global_model)] <- test[i]
    global_model$model[nrow(global_model)] <- "Global Model"
    global_model$estimate[nrow(global_model)] <- 0.00012345
  }
}


micros_models <- models %>% 
  filter(model %in% c("soil", "surface", "canopy")) %>% 
  mutate(model = dplyr::recode(model, "soil" = "Soil", 
                        "surface" = "Surface",
                        "canopy" = "Canopy")) %>% 
  mutate(model = factor(model, levels = micro_levels))

## Plot figures ########

## ....A. Macro, meso, micro, global ############

sort <- arrange(main_models, low_95)
min_bound <- sort$low_95[1]
sort <- arrange(main_models, -high_95)
max_bound <- sort$high_95[1]
# max_bound <- sort$high_95[nrow(sort)] + sort$high_95[nrow(sort)] * 0.3

# sort <- arrange(filter(main_models, model != "Global Model"), estimate)
# min_bound <- sort$low_95[1] - 0.1 
# max_bound <- sort$estimate[nrow(sort)] + sort$high_95[nrow(sort)] + 0.1

sub_model_plots <- ggplot(filter(main_models, model != "Global Model"), 
                          aes(param, estimate)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_point(aes(color = sig_params), size = 3) +
  geom_errorbar(aes(ymin = low_95, ymax = high_95, color = sig_params), 
                width = 0, size = 1.7) +
  scale_color_manual(values = c("significant" = "black",
                                "not significant" = "grey")) +
  scale_y_continuous(breaks = c(round(min_bound/2, 1), 0, round(max_bound/2, 1)), 
                     limits = c(min_bound, max_bound)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size=8)) +
  labs(color = "Biome") +
  xlab(NULL) +
  ylab(expression(beta)) +
  scale_x_discrete(labels = c("_ Elevation" = expression(paste(Delta," Elevation")))) +
  ggtitle("A.") +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 14), 
        axis.title.x = element_text(size = 18),
        strip.text.x = element_text(size = 13),
        title = element_text(size = 18),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "#e6f0ff",
                                        colour = "#e6f0ff")) +
  geom_text(aes(label = estimate), nudge_x = 0.3, size = 4.5) +
  coord_flip() +
  facet_grid(~model, scales = "free_x", space = "free_x")

if (chosen_parameter == "JanzenDscore_positive") {
  param_levels <- c(1,2,3,4,5)
  global_model <- global_model %>%
    mutate(param = dplyr::recode(param, "_ Elevation" = 4, 
                                 "Microhabitat \nHeight/Depth" = 1,
                                 "Snow Depth" = 2, 
                                 "Vegetation\nStructure" = 3,
                                 "Latitude" = 5)) %>% 
    mutate(param = factor(param, levels = param_levels))


}
global <- ggplot(global_model, 
                 aes(param, estimate)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_point(data = filter(global_model, estimate == 0.00012345), size = 0) +
  geom_point(data = filter(global_model, estimate != 0.00012345), aes(color = sig_params), size = 3) +
  geom_errorbar(data = global_model, aes(ymin = low_95, ymax = high_95, color = sig_params), 
                width = 0, size = 1.7) +
  scale_x_discrete(breaks = 1:5, labels=c("Microhabitat \nHeight/Depth","Snow Depth",
                                          "Vegetation\nStructure",
                                          expression(paste(Delta," Elevation")),
                                          "Latitude")) +
  scale_color_manual(values = c("significant" = "black",
                                "not significant" = "grey")) +
  scale_y_continuous(breaks = c(round(min_bound/2, 1), 0, round(max_bound/2, 1)), 
                     limits = c(min_bound, max_bound)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size=8)) +
  xlab(NULL) +
  ylab(expression(beta)) +
  ggtitle("B.") +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 18),
        strip.text.x = element_text(size = 13),
        title = element_text(size = 18),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "#fff5e6",
                                        colour = "#fff5e6")) +
  geom_text(data = filter(global_model, estimate != 0.00012345), aes(label = estimate), nudge_x = 0.2, size = 4.5) +
  coord_flip() +
  facet_grid(~model, scales = "free_x", space = "free_x")


model_plots <- grid.arrange(sub_model_plots, global, ncol = 2,
                            widths = c(1.5, 1))

## ....B. Each microhabitat separately ###########

sort <- arrange(micros_models, estimate)
min_bound <- sort$estimate[1] + sort$low_95[1] - 0.1 
max_bound <- sort$estimate[nrow(sort)] + sort$high_95[nrow(sort)] + 0.1

micro_plots <- ggplot(micros_models, aes(param, estimate)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_point(data = filter(micros_models, estimate != 0.00012345), aes(color = sig_params), 
             size = 3) +
  geom_point(data = filter(micros_models, estimate == 0.00012345), aes(color = sig_params), 
             size = 0) +
  geom_errorbar(aes(ymin = low_95, ymax = high_95, color = sig_params), 
                width = 0, size = 1.7) +
  scale_color_manual(values = c("significant" = "black",
                                "not significant" = "grey")) +
  scale_y_continuous(breaks = c(round(min_bound/2, 1), 0, round(max_bound/2, 1)), 
                     limits = c(min_bound, max_bound)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size=8)) +
  labs(color = "Biome") +
  xlab(NULL) +
  ylab(expression(beta)) +
  scale_x_discrete(labels = c("_ Elevation" = expression(paste(Delta," Elevation")))) +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        strip.text.x = element_text(size = 16),
        title = element_text(size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#fff5e6",
                                        colour = "#fff5e6")) +
  geom_text(data = filter(micros_models, estimate != 0.00012345), aes(label = estimate), nudge_x = 0.2, size = 4.5) +
  facet_wrap(~model, nrow = 3) +
  coord_flip()

## 4. Write out plots #########

ggsave(plot = model_plots, 
       filename = paste0("figures/figure_3/LMM_param_estimates_", chosen_parameter, ".pdf"),
       device = "pdf",
       width = 9.58, height = 7.62)
ggsave(plot = micro_plots, 
       filename = paste0("figures/figure_3/LMM_param_estimates_micros_", chosen_parameter, ".png"),
       width = 9.58, height = 9.24)

