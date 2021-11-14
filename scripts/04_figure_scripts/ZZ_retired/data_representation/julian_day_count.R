## David Klinges
## 2020-04-25
## This script produces a supplmental figure demonstrating the temporal and
## spatial representation of loggers

# Whether and how data from different loggers at each site were averaged is also 
# unclear. Presumably the lengths of these time series differ, and not all provide 
# coverage for entire years. Was there any investigation of this as a potential 
# source of bias, given that it could be problematic if seasonal coverage varied 
# systematically with location and / or canopy cover?

# count number of unique days of year and # years at each site. 
# Plot these counts (y axis) across latitude (x axis), colored by vert 
# micro (soil/surface/canopy)

## 1. Workspace prep #################

library(tidyverse)
library(grid)
library(gridExtra)

mountains <- read_csv("data/03_compiled/mountains.csv")
site_recode <- read_csv("data/QA_QC/site_recode.csv")

## 2. Data curation #############

mountains <- mountains %>% 
  # Recode micros to capitalization
  mutate(micro = dplyr::recode(micro, "soil" = "Soil", "surface" = "Surface", 
                               "canopy" = "Canopy"))


# We need to recode the Janzen sites as full year representation
days <- data.frame(
  site = c(rep("CR_northwest", 365), rep("CR_southwest", 365)),
  julian = rep((1:365),2)
)

janzen <- mountains %>% 
  filter(site %in% c("CR_northwest", "CR_southwest")) %>% 
  dplyr::select(site, elevation, micro, latitude, macro) %>% 
  full_join(days) %>% 
  filter(complete.cases(julian)) %>% 
  dplyr::select(site, julian, micro, latitude, macro) %>% 
  distinct()

mountains <- mountains %>% 
  filter(!site %in% c("CR_northwest", "CR_southwest")) %>% 
  full_join(janzen) %>% 
  filter(complete.cases(site)) %>% 
  # Recode site names (using `name` col in ggplot)
  full_join(site_recode) %>% 
  dplyr::select(year, month, julian, site, name, micro, macro, latitude) %>% 
  distinct()

## ....A. Number of unique days and years ###########

# Count number of unique days per site
unique_days <- mountains %>% 
  dplyr::select(site, year, julian, micro) %>% 
  distinct() %>% 
  group_by(site, year, micro) %>% 
  count() %>% 
  dplyr::rename(n_days = n)

unique_year <- mountains %>% 
  dplyr::select(site, year, micro) %>% 
  distinct() %>% 
  group_by(site, micro) %>% 
  count() %>% 
  dplyr::rename(n_years = n)

plot_ready_1 <- mountains %>% 
  dplyr::select(site, year, micro, latitude) %>% 
  distinct() %>% 
  left_join(unique_days) %>% 
  left_join(unique_year) %>% 
  # Arrange so that longest timeseries are plotted first
  arrange(-n_years)

plot_ready_2 <- plot_ready_1 %>% 
  dplyr::select(site, micro, latitude, n_days, n_years) %>% 
  distinct() %>% 
  # Choose max n_days per site
  group_by(site, micro, latitude, n_years) %>% 
  summarize(n_days = max(n_days))

## ....B. Seasonal coverages ###########

seasonal <- mountains %>% 
  # Don't need day 366
  filter(julian != 366) %>%
  mutate(micro = factor(micro, levels = c("Soil", "Surface", "Canopy"))) %>% 
  filter(complete.cases(site) & complete.cases(micro)) %>% 
  distinct()

## 3. Plots ################

## ....A. Number of unique days and years ##############

number_days_plot_1 <- ggplot(plot_ready_1, aes(abs(latitude), n_days)) +
  geom_point(aes(color = micro, size = n_years), alpha = 0.6) +
  scale_color_manual(values = c("Soil" = "#a285fa",
                                "Surface" = "#9dcb9a",
                                "Canopy" = "#d98c8e")) +
  labs(color = "Vertical\nMicrohabitats",
       size = "Number of Years",
       y = "Number of Days",
       x = "Latitude") +
  theme_bw() +
  theme(text = element_text(size = 16))

number_days_plot_2 <- ggplot(plot_ready_2, aes(abs(latitude), n_days)) +
  geom_jitter(aes(color = micro, size = n_years), alpha = 0.6, width = 10) +
  scale_color_manual(values = c("Soil" = "#a285fa",
                                "Surface" = "#9dcb9a",
                                "Canopy" = "#d98c8e")) +
  labs(color = "Vertical\nMicrohabitats",
       size = "Number of Years",
       y = "Number of Days",
       x = "Latitude") +
  theme_bw() +
  theme(text = element_text(size = 16))

## ....B. Seasonal coverages ###########

days_per_site <- ggplot(seasonal, aes(julian, name)) +
  geom_raster(aes(fill = micro)) +
  scale_fill_manual(values = c("Soil" = "#a285fa",
                               "Surface" = "#9dcb9a",
                               "Canopy" = "#d98c8e")) +
  labs(x = "Day of Year",
       y = "Site") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 6),
        legend.position = "none") +
  facet_wrap(~micro)

days_per_macro <- ggplot(seasonal, aes(julian, macro)) +
  geom_raster(aes(fill = micro)) +
  scale_fill_manual(values = c("Soil" = "#a285fa",
                                "Surface" = "#9dcb9a",
                                "Canopy" = "#d98c8e")) +
  labs(x = "Day of Year",
       y = "Biome") +
  theme_classic() +
  facet_wrap(~micro)


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

legend <- g_legend(days_per_macro)

days_per_macro <- days_per_macro +
  theme(legend.position = "none")

days_per <- grid.arrange(days_per_site, days_per_macro)
days_per_legend <- grid.arrange(days_per, legend, widths = c(5, 1))

## ....C. Number of unique years ##############

number_years_plot <- ggplot(plot_ready_1, aes(abs(latitude), n_years)) +
  geom_point(aes(color = micro), size = 3) +
  scale_color_manual(values = c("Soil" = "#a285fa",
                                "Surface" = "#9dcb9a",
                                "Canopy" = "#d98c8e")) +
  labs(color = "Vertical\nMicrohabitats",
       y = "Number of Years",
       x = "Latitude") +
  theme_bw() +
  theme(text = element_text(size = 16))



## 3. Save files ###############

ggsave(plot = number_days_plot_1, filename = "figures/data_representation/number_days_plot_1.png")
ggsave(plot = number_days_plot_2, filename = "figures/data_representation/number_days_plot_2.png")
ggsave(plot = number_years_plot, filename = "figures/data_representation/number_years.png")
ggsave(plot = days_per_legend, filename = "figures/data_representation/days_per_legend.png")


