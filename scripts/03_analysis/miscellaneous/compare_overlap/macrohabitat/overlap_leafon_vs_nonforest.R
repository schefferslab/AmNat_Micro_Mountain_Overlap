# David Klinges
# File creation date: 2019.03.13
# This script conducts Kruskal-Wallis for overlap across elevation between leaf-on
#   forest and non-forest


## Prep workspace #############
library(tidyverse)

# Import data
boulder <- read_csv("./data/02_derivative/Boulder_met1-GLV_avgyears_wide.csv")
boulder <- boulder %>%
  mutate_all(as.double) %>%
  mutate(year = 1)

serra_estrala <- read_csv("./data/01_primary/temperate/PT_serra_estrala/derivative/SerraEstrala_cantaro_zezere_avgyears_wide.csv")
serra_estrala <- serra_estrala %>%
  mutate_all(as.double) %>%
  mutate(year = 1)

sonoran <- read_csv("./data/01_primary/temperate/CA_sonoran_desert/derivative/CA_sonoran_desert_avgyears_wide.csv")
sonoran <- sonoran %>%
  mutate_all(as.double) %>%
  mutate(year = 1)

idaho <- read_csv("./data/01_primary/temperate/ID_sage_brush/derivative/ID_avgyears_wide.csv")
idaho <- idaho %>%
  mutate_all(as.double) %>%
  mutate(year = 1)

valles <- read_csv("data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_avgyears_wide.csv")
valles <- valles %>%
  mutate_all(as.double) %>%
  mutate(year = 1)

NC_leafon <- read_csv("./data/01_primary/temperate/NC_mt_mitchell/derivative/NC_leafon_wide.csv")
NC_leafon <- NC_leafon %>%
  mutate_all(as.double)

NC_leafoff <- read_csv("./data/01_primary/temperate/NC_mt_mitchell/derivative/NC_leafoff_wide.csv")
NC_leafoff <- NC_leafoff %>%
  mutate_all(as.double)

NH_surface <- read_csv("./data/01_primary/temperate/NH_whites/derivative/NH_hubbardAir_avgyears_wide.csv")
NH_surface <- NH_surface %>%
  mutate_all(as.double) %>% 
  mutate(year = 1)

NH_leafon <- read_csv("./data/01_primary/temperate/NH_whites/derivative/NH_hubbardSoilOn_avgyears_wide.CSV")
NH_leafon <- NH_leafon %>%
  mutate_all(as.double) %>% 
  mutate(year = 1)

NH_leafoff <- read_csv("./data/01_primary/temperate/NH_whites/derivative/NH_hubbardSoilOff_avgyears_wide.csv")
NH_leafoff <- NH_leafoff %>%
  mutate_all(as.double) %>% 
  mutate(year = 1)

Mada <- read_csv("./data/01_primary/tropical/Madagascar/derivative/Mada_wide.csv")
Mada <- Mada %>%
  mutate_all(as.double)

Phili <- read_csv("./data/01_primary/tropical/Philippines/derivative/Phili_wide.csv")
Phili <- Phili %>%
  mutate_all(as.double)

Aust <- read_csv("./data/01_primary/tropical/Australia/derivative/Aust_wide.csv")
Aust <- Aust %>%
  mutate_all(as.double)

## Combine data #########

# Temperate Leaf-on forest
leafon <- NC_leafon %>% 
  bind_rows(NH_leafon) %>% 
  bind_rows(valles) %>% 
  mutate(macro = "leafon")

# Tropical forest
trop <- Mada %>% 
  bind_rows(Phili) %>% 
  bind_rows(Aust) %>% 
  mutate(macro = "trop")

# Temp + trop leaf-on forest
tempTrop <- NC_leafon %>% 
  bind_rows(NH_leafon) %>% 
  bind_rows(valles) %>% 
  bind_rows(Mada) %>% 
  bind_rows(Phili) %>% 
  bind_rows(Aust) %>% 
  mutate(macro = "tempTrop")

# Leaf-off forest
leafoff <- NC_leafoff %>% 
  bind_rows(NH_leafoff) %>% 
  mutate(macro = "leafoff")

# Non-forest
nonforest <- boulder %>% 
  bind_rows(serra_estrala) %>% 
  bind_rows(sonoran) %>% 
  bind_rows(idaho) %>% 
  mutate(macro = "nonforest")

# Combine macros
tempforestVStropforest <- leafon %>% 
  bind_rows(trop)

templeafonVSleafoff <- leafon %>% 
  bind_rows(leafoff)

allleafonVSleafoff <- tempTrop %>% 
  bind_rows(leafoff)

allleafonVSnonforest <- tempTrop %>% 
  bind_rows(nonforest)

templeafonVSnonforest <- leafon %>% 
  bind_rows(nonforest)

leafoffVSnonforest <- leafoff %>% 
  bind_rows(nonforest)

## Set parameters and run analysis ##########
# Set microhabitats of interest
micro <- c("soil", "surface")

# Designate what macros are being compared
comparison <- "idahoVSsonora"

final_file_path <- "data/04_analysis/compare_overlap/macrohabitat/overlap_leafoffVSnonforest.csv"

source("./scripts/analysis/compare_overlap/macrohabitat/kruskal_overlap_macrohabitat.R")
conduct_Kruskal_overlap(data = idahoVSsonora, 
                        micro = micro, 
                        comparison = comparison, 
                        final_file_path = final_file_path)

idahoVSsonora <- idaho %>% 
  mutate(macro = "idaho") %>% 
  bind_rows(mutate(sonoran, macro = "sonoran"))
