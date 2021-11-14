## David Klinges
# 2019-05-19

# Calculates mean overlap scores for each system, using avg year data


## Workspace prep ########

# Import data

boulder_raw <- read_csv("./data/02_derivative/Boulder_met1-GLV_avgyears_wide.csv")
boulder_raw <- boulder_raw %>%
  mutate_all(as.double) %>%
  mutate(year = 1)

serra_estrala_raw <- read_csv("./data/01_primary/temperate/PT_serra_estrala/derivative/SerraEstrala_cantaro_zezere_avgyears_wide.csv")
serra_estrala_raw <- serra_estrala_raw %>%
  mutate_all(as.double) %>%
  mutate(year = 1)

sonoran_raw <- read_csv("./data/01_primary/temperate/CA_sonoran_desert/derivative/CA_sonoran_desert_avgyears_wide.csv")
sonoran_raw <- sonoran_raw %>%
  mutate_all(as.double) %>%
  mutate(year = 1)

idaho_raw <- read_csv("./data/01_primary/temperate/ID_sage_brush/derivative/ID_avgyears_wide.csv")
idaho_raw <- idaho_raw %>%
  mutate_all(as.double) %>%
  mutate(year = 1)

valles_raw <- read_csv("data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_avgyears_wide.csv")
valles_raw <- valles_raw %>%
  mutate_all(as.double) %>%
  mutate(year = 1)

NC_leafon_raw <- read_csv("./data/01_primary/temperate/NC_mt_mitchell/derivative/NC_leafon_wide.csv")
NC_leafon_raw <- NC_leafon_raw %>%
  mutate_all(as.double)

NC_leafoff_raw <- read_csv("./data/01_primary/temperate/NC_mt_mitchell/derivative/NC_leafoff_wide.csv")
NC_leafoff_raw <- NC_leafoff_raw %>%
  mutate_all(as.double)

Mada_raw <- read_csv("./data/01_primary/tropical/Madagascar/derivative/Mada_wide.csv")
Mada_raw <- Mada_raw %>%
  mutate_all(as.double)

Phili_raw <- read_csv("./data/01_primary/tropical/Philippines/derivative/Phili_wide.csv")
Phili_raw <- Phili_raw %>%
  mutate_all(as.double)

Aust_raw <- read_csv("./data/01_primary/tropical/Australia/derivative/Aust_wide.csv")
Aust_raw <- Aust_raw %>%
  mutate_all(as.double)


## Combine sites ########

# Foliage present

foliage <- nc_on %>%
  bind_rows(valles) %>%
  bind_rows(Mada_raw) %>%
  bind_rows(Aust_raw) %>%
  bind_rows(Phili_raw)

nonfoliage <- nc_off %>%
  bind_rows(boulder_raw) %>%
  bind_rows(sonoran_raw) %>%
  bind_rows(serra_estrala_raw) %>%
  bind_rows(idaho_raw)

# Janzify
source("scripts/data_processing/janzify.R")
foliage <- janzify(foliage, 151, 280)

nonfoliage <- janzify(nonfoliage, 151, 280)


# Remove NAs and Infs
foliage <- na.omit(foliage)
foliage <- foliage %>%
  filter(!is.infinite(overlap))
nonfoliage <- na.omit(nonfoliage)
nonfoliage <- nonfoliage %>%
  filter(!is.infinite(overlap))
## Calc mean overlap #########

# soil off
soiloff <- mean(filter(nonfoliage, micro == "soil")$overlap)
# soil on
soilon <- mean(filter(foliage, micro == "soil")$overlap)

(soiloff - soilon) / soilon 


# surface off
surfaceoff <- mean(filter(nc_off, micro == "surface")$overlap)
# surface on
surfaceon <- mean(filter(nc_on, micro == "surface")$overlap)

(surfaceoff - surfaceon) / surfaceon 

# canopy off
canopyoff <- mean(filter(nc_off, micro == "canopy")$overlap)
# canopy on
canopyon <- mean(filter(nc_on, micro == "canopy")$overlap)

(canopyoff - canopyon) / canopyon 
