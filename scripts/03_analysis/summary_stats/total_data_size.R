# David Klinges
# 2019-12-19
# This script counts the total number of observations in the database and the
# total number of years, disaggregated


## 1. Workspacep prep ##########

library(tidyverse)
library(lubridate)
library(readxl)

mountains <- read_csv("data/03_compiled/mountains.csv")

## Determine durations of sampling #############

## 2. Data curation #############

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
  full_join(janzen) 

n_days <- mountains %>% 
  dplyr::select(site, year, julian) %>% 
  distinct() %>% 
  group_by(site) %>% 
  count()

n_years <- mountains %>% 
  dplyr::select(site, year) %>% 
  distinct() %>% 
  group_by(site) %>% 
  count()

## Determine number of sampling points ###############

## 2. Data curation ##########

# Gather micros

low_sonoran <- low_sonoran %>% 
  gather(micro, temp, c(surface, soil))

high_sonoran <- high_sonoran %>% 
  gather(micro, temp, c(surface, soil))

boulder_low <- boulder_low %>% 
  gather(micro, temp, c(surface, soil))

boulder_glv_high <- boulder_glv_high %>% 
  gather(micro, temp, c(surface, soil))

low_osf_1 <- low_osf_1 %>% 
  gather(micro, temp, c(surface, canopy))

high_osf_1 <- high_osf_1 %>% 
  gather(micro, temp, c(surface, canopy))

low_pf_1 <- low_pf_1 %>% 
  gather(micro, temp, c(surface, canopy))

high_pf_1 <- high_pf_1 %>% 
  gather(micro, temp, c(surface, canopy))

low_pf_2 <- low_pf_2 %>% 
  gather(micro, temp, c(surface, soil))

high_pf_2 <- high_pf_2 %>% 
  gather(micro, temp, c(surface, soil))

low_ysf_1 <- low_ysf_1 %>% 
  gather(micro, temp, c(surface, canopy))
  
high_ysf_1 <- high_ysf_1 %>% 
  gather(micro, temp, c(surface, canopy))

CR_LS_low <- CR_LS_low %>% 
  gather(micro, temp, c(soil, surface, canopy))

CR_2K_high <- CR_2K_high %>% 
  gather(micro, temp, c(soil, surface, canopy))

low_sage <- low_sage %>% 
  gather(micro, temp, c(soil, surface))

high_sage <- high_sage %>% 
  gather(micro, temp, c(soil, surface))
  
NC_mitchell_data_low <- NC_mitchell_data_low %>% 
  gather(micro, temp, c(soil, surface, canopy))

NC_mitchell_data_high <- NC_mitchell_data_high %>% 
  gather(micro, temp, c(soil, surface, canopy))
  
RS89 <- RS89 %>% 
  gather(micro, temp, c(soil, surface))

RS14 <- RS14 %>% 
  gather(micro, temp, c(soil, surface))
  
# note there's two soils here
CWT_205_low <- CWT_205_low %>% 
  gather(micro, temp, c(soil_05, surface))

CWT_SM3_high <- CWT_SM3_high %>% 
  gather(micro, temp, c(soil_05, surface))

## Find total number of observations ############


nrow(cuona_low) +
  nrow(cuona_high) +
  nrow(malaysia_degrad_low) +
  nrow(malaysia_degrad_high) +
  nrow(malaysia_primary_low) +
  nrow(malaysia_primary_high) +
  nrow(low_tok) +
  nrow(high_nabesna) +
  nrow(soil_low) +
  nrow(soil_high) +
  nrow(surface_low) +
  nrow(surface_high) +
  nrow(low_happyValley) +
  nrow(toolik_high_soil) +
  nrow(ecuador_low) +
  nrow(ecuador_high) +
  nrow(usambara_low) +
  nrow(usambara_high) +
  nrow(mada) +
  nrow(aust) +
  nrow(phili) +
  nrow(valles_canopy_low) +
  nrow(valles_canopy_high) +
  nrow(jemez_soil_low) +
  nrow(jemez_soil_high) +
  nrow(low_sonoran) +
  nrow(high_sonoran) +
  nrow(oracle_soil_low) +
  nrow(bigelow_soil_high) +
  nrow(fresno_low) +
  nrow(bishop_high) +
  nrow(VOB_forest_low) +
  nrow(NAB_forest_high) +
  nrow(OTB_forest_low) +
  nrow(ISB_forest_high) +
  nrow(VOF_open_low) +
  nrow(NAF_open_high) +
  nrow(boulder_low) +
  nrow(boulder_glv_high) +
  nrow(low_osf_1) +
  nrow(high_osf_1) +
  nrow(low_pf_1) +
  nrow(high_pf_1) +
  nrow(low_pf_2) +
  nrow(high_pf_2) +
  nrow(low_ysf_1) +
  nrow(high_ysf_1) +
  nrow(CR_LS_low) +
  nrow(CR_2K_high) +
  nrow(puntarenas_low) +
  nrow(naranjo_high) +
  nrow(palma_low) +
  nrow(villa_high) +
  nrow(low_sage) +
  nrow(high_sage) +
  nrow(NC_mitchell_data_low) +
  nrow(NC_mitchell_data_high) +
  nrow(RS89) +
  nrow(RS14) +
  nrow(zezere_low) +
  nrow(cantaro_high) +
  nrow(low_soil_meadow) +
  nrow(high_soil_meadow) +
  nrow(low_soil_salix) +
  nrow(high_soil_salix) +
  nrow(low_soil_SM) +
  nrow(high_soil_SM) +
  nrow(CWT_205_low) +
  nrow(CWT_SM3_high)

# 11775331

## 3. Count number of years ###############

length(unique(cuona_low$year)) + 
  length(unique(cuona_high$year)) + 
  length(unique(malaysia_degrad_low$year)) + 
  length(unique(malaysia_degrad_high$year)) + 
  length(unique(malaysia_primary_low$year)) + 
  length(unique(malaysia_primary_high$year)) + 
  length(unique(low_tok$year)) + 
  length(unique(high_nabesna$year)) + 
  length(unique(soil_low$year)) + 
  length(unique(soil_high$year)) + 
  length(unique(surface_low$year)) + 
  length(unique(surface_high$year)) + 
  length(unique(low_happyValley$year)) + 
  length(unique(toolik_high_soil$year)) + 
  length(unique(ecuador_low$year)) + 
  length(unique(ecuador_high$year)) + 
  length(unique(usambara_low$year)) + 
  length(unique(usambara_high$year)) + 
  length(unique(mada$year)) + 
  length(unique(aust$year)) + 
  length(unique(phili$year)) + 
  length(unique(valles_canopy_low$year)) + 
  length(unique(valles_canopy_high$year)) + 
  length(unique(jemez_soil_low$year)) + 
  length(unique(jemez_soil_high$year)) + 
  length(unique(low_sonoran$year)) +
  length(unique(high_sonoran$year)) +
  length(unique(oracle_soil_low$year)) + 
  length(unique(bigelow_soil_high$year)) + 
  length(unique(fresno_low$year)) + 
  length(unique(bishop_high$year)) + 
  length(unique(VOB_forest_low$year)) + 
  length(unique(NAB_forest_high$year)) + 
  length(unique(OTB_forest_low$year)) + 
  length(unique(ISB_forest_high$year)) + 
  length(unique(VOF_open_low$year)) + 
  length(unique(NAF_open_high$year)) + 
  length(unique(boulder_low$year)) + 
  length(unique(boulder_glv_high$year)) + 
  length(unique(low_osf_1$year)) + 
  length(unique(high_osf_1$year)) + 
  length(unique(low_pf_1$year)) + 
  length(unique(high_pf_1$year)) + 
  length(unique(low_pf_2$year)) + 
  length(unique(high_pf_2$year)) + 
  length(unique(low_ysf_1$year)) + 
  length(unique(high_ysf_1$year)) + 
  length(unique(CR_LS_low$year)) + 
  length(unique(CR_2K_high$year)) + 
  length(unique(puntarenas_low$year)) + 
  length(unique(naranjo_high$year)) + 
  length(unique(palma_low$year)) + 
  length(unique(villa_high$year)) + 
  length(unique(low_sage$year)) + 
  length(unique(high_sage$year)) + 
  length(unique(NC_mitchell_data_low$year)) + 
  length(unique(NC_mitchell_data_high$year)) + 
  length(unique(RS89$year)) + 
  length(unique(RS14$year)) + 
  length(unique(zezere_low$year)) + 
  length(unique(cantaro_high$year)) + 
  length(unique(low_soil_meadow$year)) + 
  length(unique(high_soil_meadow$year)) + 
  length(unique(low_soil_salix$year)) + 
  length(unique(high_soil_salix$year)) + 
  length(unique(low_soil_SM$year)) + 
  length(unique(high_soil_SM$year)) + 
  length(unique(CWT_205_low$year)) + 
  length(unique(CWT_SM3_high$year))

# 524



