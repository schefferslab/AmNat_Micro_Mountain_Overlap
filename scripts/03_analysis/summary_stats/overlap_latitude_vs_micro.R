## David Klinges
# 2019-08-20
# This script quickly averages overlap for a few select microhabitats and latitudes


## 1. Workspace prep ###############

library(tidyverse)

mountains <- read_csv("data/03_compiled/mountains_overlap_monthly_avg.csv")

# Compare micros at same latitude
nc <- filter(mountains, site == "NC")
nc_canopy <- filter(nc, micro == "canopy")
nc_soil <- filter(nc, micro == "soil")

canopy_mean <- mean(nc_canopy$TAD_elevCorr) # 10.635
soil_mean <- mean(nc_soil$TAD_elevCorr) # -1.992

# Compare latitudes at same micro
cr <- filter(mountains, site == "CR_southwest")
co <- filter(mountains, site == "CO")
id <- filter(mountains, site == "ID")
fresno <- filter(mountains, site == "CA")
co_surface <- filter(co, micro == "surface")
id_surface <- filter(id, micro == "surface")

co_mean <- mean(co_surface$TAD_elevCorr)
id_mean <- mean(id_surface$TAD_elevCorr)
cr_mean <- mean(cr$TAD_elevCorr) # -2.472717
fresno_mean <- mean(fresno$TAD_elevCorr) # 10.95135

canopy_mean - soil_mean # 12.62684
fresno_mean - cr_mean # 11.6956
co_mean - cr_mean # 2.510652


