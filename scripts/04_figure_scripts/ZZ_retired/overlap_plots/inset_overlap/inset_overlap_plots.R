# David Klinges
# This script generates overlap plots that are insets in the Figure 1 global map

## Workspace prep ##############
library(tidyverse)
library(ggplot2)
library(lubridate)
library(grid)
library(gridExtra)

## Import data ############

# # Australia #######
# data_raw <- read_csv("data/01_primary/tropical/Australia/derivative/Aust_tall.csv")
# data_raw <- data_raw %>%
#   filter(micro != "canopy")
# YEAR <- c(1)
# OUTPUT_FILE_PATH <- "figures/overlap_figures/inset_plots/Aust_month_inset.png"
# 
# HEIGHT <- 7.3
# WIDTH <- 10.2
# 
# ## North Carolina #######
# 
# data_raw <- read_csv("data/01_primary/temperate/NC_mt_mitchell/derivative/NC_tall.csv")
# YEAR <- c(2016)
# OUTPUT_FILE_PATH <- "figures/overlap_figures/inset_plots/Mitchell_month_inset.png"
# HEIGHT <- 7.3
# WIDTH <- 14
# 
# ## Colombia ###########
# 
# data_raw <- read_csv("data/01_primary/tropical/Colombia/derivative/CO_pf_1_tall.csv")
# YEAR <- c(2014)
# OUTPUT_FILE_PATH <- "figures/overlap_figures/inset_plots/colom_month_inset.png"
# 
# HEIGHT <- 10.2
# WIDTH <- 7.3
## Boulder #########

data_raw <- read_csv("data/01_primary/temperate/CO_boulder/derivative/Boulder_met1-GLV_tall.csv")
YEAR <- c(2010)
OUTPUT_FILE_PATH <- "figures/overlap_figures/inset_plots/boulder_month_inset.png"

HEIGHT <- 7.3
WIDTH <- 10.2
# ## Switzerland #######
# 
# data_raw <- read_csv("data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_OTB_ISB_tall.csv")
# YEAR <- c(2010)
# OUTPUT_FILE_PATH <- "figures/overlap_figures/inset_plots/Switz_decid_month_inset.png"
# 
# data_raw <- read_csv("data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_VOB_NAB_tall.csv")
# YEAR <- c(2010)
# OUTPUT_FILE_PATH <- "figures/overlap_figures/inset_plots/Switz_open_month_inset.png"
# 
# HEIGHT <- 10.2
# WIDTH <- 7.3
# ## Alaska #######
# 
# nab_raw <- read_csv("data/01_primary/temperate/AK_nabesna/derivative/AK_nabesna_tall.csv")
# YEAR <- c(1996)
# OUTPUT_FILE_PATH <- "figures/overlap_figures/inset_plots/AKnabesna_month_inset.png"
# 
# flux_raw <- read_csv("data/01_primary/arctic/AK_ameriflux/derivative/AK_ameriflux_tall.csv")
# YEAR <- c(1996)
# OUTPUT_FILE_PATH <- "figures/overlap_figures/inset_plots/AKameriflux_month_inset.png"
# 
# # Combine
# data_raw <- nab_raw %>%
#   bind_rows(flux_raw)
# OUTPUT_FILE_PATH <- "figures/overlap_figures/inset_plots/AKcombined_month_inset.png"
# 
# HEIGHT <- 10.2
# WIDTH <- 7.3
## Set hard codes ##############

# Designate elevations of low and high sites
LOW_ELEVATION <- "100"
HIGH_ELEVATION <- "1000"
# Set time range to plot 
# Set file path where figure will be saved

x_axis <- "month"
geom_line_size <- 0.1
text_size <- 30

find_factors <- function(x) {
  x <- as.integer(x)
  div <- seq_len(abs(x))
  factors <- div[x %% div == 0L]
  factors <- list(neg = -factors, pos = factors)
  return(factors)
}

find_factors(365)
breaks <- c(0, 3, 6, 9, 12)
# breaks <- round(seq(min(data$julian) - 1, max(data$julian),
#           length.out = 5))



## Data curation ###########

## ....A. Subset to a year ###########
if (is.null(data_raw$year) == FALSE) {
  
  # Filter to just the years requested
  data <- data_raw %>%
    filter(year %in% YEAR)
} else {
  data <- data_raw
}

## ....B. Exclude NAs and Infs #########

data <- data %>% 
  filter(is.finite(min)) %>% 
  filter(is.finite(max))

## ....C. Group to month #######

data <- data %>% 
  mutate(month = month(as.POSIXlt(as.Date(julian, format = "%j", 
                                                 origin = paste0("1.1.", year)), 
                                         format="%d/%m/%Y"))) %>% 
  group_by(micro, elevation, month) %>% 
  summarize_at(vars(min, max, height), ~ mean(., na.rm = TRUE)) %>% 
  ungroup()

## ....D. Recode factors #########

micro_flags <- c("Soil", "Surface", "Canopy")

data <- data %>% 
  mutate(micro = tools::toTitleCase(micro)) %>% 
  mutate(micro = factor(micro, levels = micro_flags))

## Seasonal Overlap plots #############

## Plot overlap over time ##########
data <- data %>% 
  mutate(elev_micro = paste(elevation, micro, sep = "_"))

Soil <- filter(data, micro == "Soil")
low_data <- filter(data, elevation == "low")
high_data <- filter(data, elevation == "high")

plot <- ggplot(low_data) +
  geom_ribbon(aes(x = month, ymin = min, ymax = max, fill = elev_micro), 
              alpha = 0.5) +
  geom_line(aes(x = month, y = min),
            alpha = 0.5, size = geom_line_size) +
  geom_line(aes(x = month, y = max),
            alpha = 0.5, size = geom_line_size) +
  geom_ribbon(data = high_data, 
              aes(x = month, ymin = min, ymax = max, fill = elev_micro), 
              alpha = 0.5) +
  geom_line(data = high_data, aes(x = month, y = min),
            alpha = 0.5, size = geom_line_size) +
  geom_line(data = high_data, aes(x = month, y = max),
            alpha = 0.5, size = geom_line_size) +
  scale_fill_manual(values = c("low_Soil" = "purple", 
                               "high_Soil" = "blue", 
                                "low_Surface" = "light green", 
                               "high_Surface" = "dark green", 
                                "low_Canopy" = "red", 
                                "high_Canopy" = "dark red")) +
  coord_cartesian(ylim = c(-25, 35), expand = FALSE) +
  scale_x_continuous(breaks = breaks) +
  # scale_y_continuous(limits=c(0, 150))
  theme_classic(base_size = 60) +
  theme(
        # title = element_text(size = text_size),
        axis.title.y = element_text(size = text_size + 6),
        axis.title.x = element_text(size = text_size + 6),
        # axis.text.y = element_text(size = text_size),
        axis.text.x = element_text(size = text_size),
        axis.text.y = element_text(size = text_size),
        strip.text = element_text(size = text_size + 6),
        strip.background = element_blank(),
        legend.position = "none") +
  ylab("Temperature (C)") +
  xlab("Month of Year") +
  facet_wrap(~micro)

plot_title <- grid.arrange(plot)

## Write out file ##########

ggsave(plot = plot_title, filename = OUTPUT_FILE_PATH, width = WIDTH, height = HEIGHT)
