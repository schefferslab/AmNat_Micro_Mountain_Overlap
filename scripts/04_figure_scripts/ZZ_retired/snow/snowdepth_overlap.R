# David Klinges
# 2019-09-16
# This script generates plots overlap overlap ~ snow depth

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

mountains <- read_csv("data/03_compiled/mountains_overlap_monthly_avg.csv",
                      col_types = cols(
                        # year = col_double(),
                        elevation_change = col_double()
                      ))


## 2. Curate data ########

soil <- filter(mountains, micro == "soil")

ggplot(soil, aes(snowdepth, TAD_elevCorr)) +
  geom_point(aes(color = foliage)) +
  theme_bw()
