## David Klinges
## klinges@si.edu

# This script manipulates budburst data to determine community averages of leaf-on
#   for each site


## 1. Workspace prep ##############

library(tidyverse)
library(lubridate)

# Import data

idaho_raw <- read_csv("data/01_primary/temperate/ID_sage_brush/budburst/budburst_observations.csv")

cali_raw <- read_csv("data/01_primary/temperate/CA_sonoran_desert/budburst/budburst_observations.csv")

boulder_raw <- read_csv("data/01_primary/temperate/CO_boulder/budburst/budburst_observations.csv")

## 2. Data querying and curation ###########


## ....2A. Idaho ##############
idaho_lat <- 43.167545
idaho_long <- -116.713205
idaho <- idaho_raw %>%
  select(state, latitude, longitude, location_name, common_name, species, observation_date, phenophase, comments) %>%
  filter(latitude > idaho_lat - .5 & latitude < idaho_lat + .5) %>%
  filter(abs(longitude) > abs(idaho_long) - 1.0 & abs(longitude) < abs(idaho_long) + 1.0) 

first_leaf <- idaho %>%
  filter(phenophase == "First Leaf (Deciduous Trees/Shrubs)")

bud_burst <- idaho %>%
  filter(phenophase == "Bud Burst (Deciduous Trees/Shrubs)")

leaves_unfold_early <- idaho %>%
  filter(phenophase == "Leaves Unfolding (Early) (Deciduous Trees/Shrubs)")

leaves_unfold_middle <- idaho %>%
  filter(phenophase == "Leaves Unfolding (Middle) (Deciduous Trees/Shrubs)")

leaves_drop_early <- idaho %>%
  filter(phenophase == "Leaves Dropping (Early) (Deciduous Trees/Shrubs)")

leaves_drop_none <- idaho %>%
  filter(phenophase == "Leaves Dropping (None) (Deciduous Trees/Shrubs)")

# Mean bud burst
bud_burst <- mean(yday(as_date(bud_burst$observation_date)))

# Mean first leaf
first_leaf <- mean(yday(as_date(first_leaf$observation_date)))

# Leaf unfolding early
leaves_unfold_early <- mean(yday(as_date(leaves_unfold_early$observation_date)))

# Leaf unfolding middle
leaves_unfold_middle <- mean(yday(as_date(leaves_unfold_middle$observation_date)))

# Leaf dropping early
leaves_drop_early <- mean(yday(as_date(leaves_drop_early$observation_date)))

# Leaf dropping none
leaves_drop_none <- mean(yday(as_date(leaves_drop_none$observation_date)))



idaho_pheno <- tibble(
  site = "Idaho",
  latitude = idaho_lat,
  longitude = idaho_long,
  bud_burst= bud_burst,
  first_leaf = first_leaf,
  leaves_unfold_early = leaves_unfold_early,
  leaves_unfold_middle = leaves_unfold_middle,
  leaves_drop_early = leaves_drop_early,
  leaves_drop_none = leaves_drop_none
)

# Approx. site lat and long
 
## ....2B. Sonoran Desert ###########

sonoran_lat <- 33.651810
sonoran_long <- -116.37214
sonoran <- cali_raw %>%
  select(state, latitude, longitude, location_name, common_name, species, observation_date, phenophase, comments) %>%
  filter(latitude > sonoran_lat - 1.0 & latitude < sonoran_lat + 1.0) %>%
  filter(abs(longitude) > abs(sonoran_long) - 1.0 & abs(longitude) < abs(sonoran_long) + 1.0) 

first_leaf <- sonoran %>%
  filter(phenophase == "First Leaf (Deciduous Trees/Shrubs)")

bud_burst <- sonoran %>%
  filter(phenophase == "Bud Burst (Deciduous Trees/Shrubs)")

# Mean bud burst
bud_burst <- mean(yday(as_date(bud_burst$observation_date)))

# Mean first leaf
first_leaf <- mean(yday(as_date(first_leaf$observation_date)))

sonoran_pheno <- tibble(
  site = "sonoran",
  latitude = sonoran_lat,
  longitude = sonoran_long,
  bud_burst= bud_burst,
  first_leaf = first_leaf
)

## ....2C. Sierra Forest ###########

sierra_lat <- 37.0674
sierra_long <- -119.1951
sierra <- cali_raw %>%
  select(state, latitude, longitude, location_name, common_name, species, observation_date, phenophase, comments) %>%
  filter(latitude > sierra_lat - 1.0 & latitude < sierra_lat + 1.0) %>%
  filter(abs(longitude) > abs(sierra_long) - 1.0 & abs(longitude) < abs(sierra_long) + 1.0) 


bud_burst <- sierra %>%
  filter(phenophase == "Bud Burst (Deciduous Trees/Shrubs)")

first_leaf <- sierra %>%
  filter(phenophase == "First Leaf (Deciduous Trees/Shrubs)")

all_leaves <- sierra %>%
  filter(phenophase == "All Leaves Unfolded (Deciduous Trees/Shrubs)")

# Mean bud burst
bud_burst <- mean(yday(as_date(bud_burst$observation_date)))

# Mean first leaf
first_leaf <- mean(yday(as_date(first_leaf$observation_date)))

# Mean all leaves
all_leaves <- mean(yday(as_date(all_leaves$observation_date)))

sierra_pheno <- tibble(
  site = "sierra",
  latitude = sierra_lat,
  longitude = sierra_long,
  bud_burst= bud_burst,
  first_leaf = first_leaf,
  all_leaves = all_leaves
)




## ....2D. Boulder ###########

boulder_lat <- 40.014
boulder_long <- -105.337
boulder <- boulder_raw %>%
  select(state, latitude, longitude, location_name, common_name, species, observation_date, phenophase, comments) %>%
  filter(latitude > boulder_lat - .5 & latitude < boulder_lat + .5) %>%
  filter(abs(longitude) > abs(boulder_long) - 1.0 & abs(longitude) < abs(boulder_long) + 1.0) 

first_leaf <- boulder %>%
  filter(phenophase == "First Leaf (Deciduous Trees/Shrubs)")

bud_burst <- boulder %>%
  filter(phenophase == "Bud Burst (Deciduous Trees/Shrubs)")

leaves_unfold_early <- boulder %>%
  filter(phenophase == "Leaves Unfolding (Early) (Deciduous Trees/Shrubs)")

leaves_unfold_middle <- boulder %>%
  filter(phenophase == "Leaves Unfolding (Middle) (Deciduous Trees/Shrubs)")

leaves_drop_early <- boulder %>%
  filter(phenophase == "Leaves Dropping (Early) (Deciduous Trees/Shrubs)")

leaves_drop_none <- boulder %>%
  filter(phenophase == "Leaves Dropping (None) (Deciduous Trees/Shrubs)")

# Mean bud burst
bud_burst <- mean(yday(as_date(bud_burst$observation_date)))

# Mean first leaf
first_leaf <- mean(yday(as_date(first_leaf$observation_date)))

# Leaf unfolding early
leaves_unfold_early <- mean(yday(as_date(leaves_unfold_early$observation_date)))

# Leaf unfolding middle
leaves_unfold_middle <- mean(yday(as_date(leaves_unfold_middle$observation_date)))

# Leaf dropping early
leaves_drop_early <- mean(yday(as_date(leaves_drop_early$observation_date)))

# Leaf dropping none
leaves_drop_none <- mean(yday(as_date(leaves_drop_none$observation_date)))



boulder_pheno <- tibble(
  site = "boulder",
  latitude = boulder_lat,
  longitude = boulder_long,
  bud_burst= bud_burst,
  first_leaf = first_leaf,
  leaves_unfold_early = leaves_unfold_early,
  leaves_unfold_middle = leaves_unfold_middle,
  leaves_drop_early = leaves_drop_early,
  leaves_drop_none = leaves_drop_none
)



## 3. Write out data ##############

write_csv(idaho_pheno, "data/01_primary/temperate/ID_sage_brush/budburst/idaho_pheno_dates.csv")
write_csv(sonoran_pheno, "data/01_primary/temperate/CA_sonoran_desert/budburst/sonoran_pheno_dates.csv")
write_csv(sonoran_pheno, "data/01_primary/temperate/CA_sierra/budburst/sonoran_pheno_dates.csv")
write_csv(boulder_pheno, "data/01_primary/temperate/CO_boulder/budburst/boulder_pheno_dates.csv")

