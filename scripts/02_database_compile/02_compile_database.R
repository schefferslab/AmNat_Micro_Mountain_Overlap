# David Klinges
# File creation date: 2019.06.02
# This script compiles sites together


## 1. Workspace Prep ########

library(tidyverse)

veg_structure <- read_csv("data/vegetation/derivative/veg_structure.csv")

## Tall #########
## ....A. All years #########
idaho <- read_csv("data/01_primary/temperate/ID_sage_brush/derivative/ID_tall.csv")
oregon <- read_csv("data/01_primary/temperate/OR_andrews/derivative/OR_andrewsFineRes_tall.csv")
sonoran <- read_csv("data/01_primary/temperate/AZ_sonoran_desert/derivative/AZ_sonoran_desert_tall.csv",
                    col_types = cols(snowdepth = col_double()))
AZ_cata <- read_csv("data/01_primary/temperate/AZ_santa_catelina/derivative/AZ_cata_tall.csv")
fresno <- read_csv("data/01_primary/temperate/CA_fresno/derivative/CA_fresno_tall.csv")
boulder <- read_csv("data/01_primary/temperate/CO_boulder/derivative/Boulder_met1-GLV_tall.csv")
mitchell <- read_csv("data/01_primary/temperate/NC_mt_mitchell/derivative/NC_tall.csv",
                     col_types = cols(snowdepth = col_double()))
hubbard_forest <- read_csv("data/01_primary/temperate/NH_whites/derivative/NH_hubbardForest_Soil_tall.csv")
hubbard_nonforest <- read_csv("data/01_primary/temperate/NH_whites/derivative/NH_hubbardNonforest_surface_tall.csv",
                              col_types = cols(snowdepth = col_double()))
coweeta <- read_csv("data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_205_SM3_D5_tall.csv")
valles_canopy <- read_csv("data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_canopy_tall.csv",
                   col_types = cols(snowdepth = col_double()))
valles_soil <- read_csv("data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_soil_tall.csv",
                           col_types = cols(snowdepth = col_double()))
serra <- read_csv("data/01_primary/temperate/PT_serra_estrala/derivative/SerraEstrala_cantaro_zezere_tall.csv")
sweden <- read_csv("data/01_primary/arctic/SE_suorooaivi/derivative/SE_suorooaiviSM_tall.csv")
switz_decid <- read_csv("data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_OTB_ISB_tall.csv",
                        col_types = cols(snowdepth = col_double()))
switz_conif <- read_csv("data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_VOB_NAB_tall.csv",
                        col_types = cols(snowdepth = col_double()))
switz_open <- read_csv("data/01_primary/temperate/CH_LWF/derivative/open/CH_LWF_VOF_NAF_tall.csv",
                       col_types = cols(snowdepth = col_double()))

AK_nabesna <- read_csv("data/01_primary/temperate/AK_nabesna/derivative/AK_nabesna_tall.csv")
AK_ameriflux <- read_csv("data/01_primary/arctic/AK_ameriflux/derivative/AK_ameriflux_tall.csv")
aust <- read_csv("data/01_primary/tropical/Australia/derivative/Aust_tall.csv",
                 col_types = cols(altitude = col_double()))
mada <- read_csv("data/01_primary/tropical/Madagascar/derivative/mada_tall.csv")
phili <- read_csv("data/01_primary/tropical/Philippines/derivative/phili_tall.csv")
costa_basham <- read_csv("data/01_primary/tropical/Costa_Rica/derivative/CR_tall.csv")
cr_northwest <- read_csv("data/01_primary/tropical/CR_janzen/derivative/CR_northwest_tall.csv")
cr_southwest <- read_csv("data/01_primary/tropical/CR_janzen/derivative/CR_southwest_tall.csv")
CO_PrimaryForest_1 <- read_csv("data/01_primary/tropical/Colombia/derivative/CO_pf_1_tall.csv")
CO_PrimaryForest_2 <- read_csv("data/01_primary/tropical/Colombia/derivative/CO_pf_2_tall.csv")
CO_OldSecondary_1 <- read_csv("data/01_primary/tropical/Colombia/derivative/CO_osf_1_tall.csv")
CO_YoungSecondary_1 <- read_csv("data/01_primary/tropical/Colombia/derivative/CO_ysf_1_tall.csv")
MY_SAFE_primary <- read_csv("data/01_primary/tropical/Senior_global/MY_SAFE/derivative/MY_SAFE_primary_tall.csv")
MY_SAFE_degraded <- read_csv("data/01_primary/tropical/Senior_global/MY_SAFE/derivative/MY_SAFE_degraded_tall.csv")
TZ_usambara <- read_csv("data/01_primary/tropical/Tanzania_usambara/derivative/TZ_usambara_tall.csv")
EC_maquipucuna <- read_csv("data/01_primary/tropical/Ecuador/derivative/EC_maquipucuna_tall.csv")
CH_cuona <- read_csv("data/01_primary/temperate/CH_Cuona/derivative/CH_cuona_tall.csv")

## ....B. Avg years #########

idaho_avg <- read_csv("data/01_primary/temperate/ID_sage_brush/derivative/ID_avgyears_tall.csv")
oregon_avg <- read_csv("data/01_primary/temperate/OR_andrews/derivative/OR_andrewsFineRes_avgyears_tall.csv")
sonoran_avg <- read_csv("data/01_primary/temperate/AZ_sonoran_desert/derivative/AZ_sonoran_desert_avgyears_tall.csv")
AZ_cata_avg <- read_csv("data/01_primary/temperate/AZ_santa_catelina/derivative/AZ_cata_avgyears_tall.csv")
fresno_avg <- read_csv("data/01_primary/temperate/CA_fresno/derivative/CA_fresno_avgyears_tall.csv")
boulder_avg <- read_csv("data/01_primary/temperate/CO_boulder/derivative/Boulder_met1-GLV_avgyears_tall.csv")
hubbard_forest_avg <- read_csv("data/01_primary/temperate/NH_whites/derivative/NH_hubbardForest_Soil_avgyears_tall.csv")
hubbard_nonforest_avg <- read_csv("data/01_primary/temperate/NH_whites/derivative/NH_hubbardNonforest_surface_avgyears_tall.csv")
mitchell_avg <- read_csv("data/01_primary/temperate/NC_mt_mitchell/derivative/NC_avgyears_tall.csv")
coweeta_avg <- read_csv("data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_205_SM3_D5_avgyears_tall.csv")
valles_canopy_avg <- read_csv("data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_canopy_avgyears_tall.csv")
valles_soil_avg <- read_csv("data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_soil_avgyears_tall.csv")
serra_avg <- read_csv("data/01_primary/temperate/PT_serra_estrala/derivative/SerraEstrala_cantaro_zezere_avgyears_tall.csv")
sweden_avg <- read_csv("data/01_primary/arctic/SE_suorooaivi/derivative/SE_suorooaiviSM_avgyears_tall.csv")
switz_decid_avg <- read_csv("data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_OTB_ISB_avgyears_tall.csv")
switz_conif_avg <- read_csv("data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_VOB_NAB_avgyears_tall.csv")
switz_open_avg <- read_csv("data/01_primary/temperate/CH_LWF/derivative/open/CH_LWF_VOF_NAF_avgyears_tall.csv")
AK_nabesna_avg <- read_csv("data/01_primary/temperate/AK_nabesna/derivative/AK_nabesna_avgyears_tall.csv")
AK_ameriflux_avg <- read_csv("data/01_primary/arctic/AK_ameriflux/derivative/AK_ameriflux_avgyears_tall.csv")
aust_avg <- read_csv("data/01_primary/tropical/Australia/derivative/Aust_tall.csv")
mada_avg <- read_csv("data/01_primary/tropical/Madagascar/derivative/mada_tall.csv")
phili_avg <- read_csv("data/01_primary/tropical/Philippines/derivative/phili_tall.csv")
costa_basham_avg <- read_csv("data/01_primary/tropical/Costa_Rica/derivative/CR_avgyears_tall.csv")
cr_northwest_avg <- read_csv("data/01_primary/tropical/CR_janzen/derivative/CR_northwest_avgyears_tall.csv")
cr_southwest_avg <- read_csv("data/01_primary/tropical/CR_janzen/derivative/CR_southwest_avgyears_tall.csv")
CO_PrimaryForest_1_avg <- read_csv("data/01_primary/tropical/Colombia/derivative/CO_pf_1_avgyears_tall.csv")
CO_PrimaryForest_2_avg <- read_csv("data/01_primary/tropical/Colombia/derivative/CO_pf_2_avgyears_tall.csv")
CO_OldSecondary_1_avg <- read_csv("data/01_primary/tropical/Colombia/derivative/CO_osf_1_avgyears_tall.csv")
CO_YoungSecondary_1_avg <- read_csv("data/01_primary/tropical/Colombia/derivative/CO_ysf_1_avgyears_tall.csv")
MY_SAFE_primary_avg <- read_csv("data/01_primary/tropical/Senior_global/MY_SAFE/derivative/MY_SAFE_primary_avgyears_tall.csv")
MY_SAFE_degraded_avg <- read_csv("data/01_primary/tropical/Senior_global/MY_SAFE/derivative/MY_SAFE_degraded_avgyears_tall.csv")
TZ_usambara_avg <- read_csv("data/01_primary/tropical/Tanzania_usambara/derivative/TZ_usambara_avgyears_tall.csv")
EC_maquipucuna_avg <- read_csv("data/01_primary/tropical/Ecuador/derivative/EC_maquipucuna_avgyears_tall.csv")
CH_cuona_avg <- read_csv("data/01_primary/temperate/CH_Cuona/derivative/CH_cuona_avgyears_tall.csv")


## Wide #########
## ....A. All years #######
idaho_wide <-read_csv("data/01_primary/temperate/ID_sage_brush/derivative/ID_wide.csv")
oregon_wide <-read_csv("data/01_primary/temperate/OR_andrews/derivative/OR_andrewsFineRes_wide.csv")
sonoran_wide <-read_csv("data/01_primary/temperate/AZ_sonoran_desert/derivative/AZ_sonoran_desert_wide.csv")
AZ_cata_wide <- read_csv("data/01_primary/temperate/AZ_santa_catelina/derivative/AZ_cata_wide.csv")
fresno_wide <- read_csv("data/01_primary/temperate/CA_fresno/derivative/CA_fresno_wide.csv")
boulder_wide <-read_csv("data/01_primary/temperate/CO_boulder/derivative/Boulder_met1-GLV_wide.csv")
mitchell_wide <- read_csv("data/01_primary/temperate/NC_mt_mitchell/derivative/NC_wide.csv")
hubbard_forest_wide <- read_csv("data/01_primary/temperate/NH_whites/derivative/NH_hubbardForest_Soil_wide.csv")
hubbard_nonforest_wide <- read_csv("data/01_primary/temperate/NH_whites/derivative/NH_hubbardNonforest_surface_wide.csv")
coweeta_wide <- read_csv("data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_205_SM3_D5_wide.csv")
valles_canopy_wide <-read_csv("data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_canopy_wide.csv")
valles_soil_wide <-read_csv("data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_soil_wide.csv")
serra_wide <-read_csv("data/01_primary/temperate/PT_serra_estrala/derivative/SerraEstrala_cantaro_zezere_wide.csv")
sweden_wide <- read_csv("data/01_primary/arctic/SE_suorooaivi/derivative/SE_suorooaiviSM_wide.csv")
switz_decid_wide <- read_csv("data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_OTB_ISB_wide.csv")
switz_conif_wide <- read_csv("data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_VOB_NAB_wide.csv")
switz_open_wide <- read_csv("data/01_primary/temperate/CH_LWF/derivative/open/CH_LWF_VOF_NAF_wide.csv")
AK_nabesna_wide <- read_csv("data/01_primary/temperate/AK_nabesna/derivative/AK_nabesna_wide.csv")
AK_ameriflux_wide <- read_csv("data/01_primary/arctic/AK_ameriflux/derivative/AK_ameriflux_wide.csv")
aust_wide <-read_csv("data/01_primary/tropical/Australia/derivative/Aust_wide.csv")
mada_wide <-read_csv("data/01_primary/tropical/Madagascar/derivative/mada_wide.csv")
phili_wide <-read_csv("data/01_primary/tropical/Philippines/derivative/phili_wide.csv")
costa_basham_wide <- read_csv("data/01_primary/tropical/Costa_Rica/derivative/CR_wide.csv")
cr_northwest_wide <- read_csv("data/01_primary/tropical/CR_janzen/derivative/CR_northwest_wide.csv")
cr_southwest_wide <- read_csv("data/01_primary/tropical/CR_janzen/derivative/CR_southwest_wide.csv")
CO_PrimaryForest_1_wide <- read_csv("data/01_primary/tropical/Colombia/derivative/CO_pf_1_wide.csv")
CO_PrimaryForest_2_wide <- read_csv("data/01_primary/tropical/Colombia/derivative/CO_pf_2_wide.csv")
CO_OldSecondary_1_wide <- read_csv("data/01_primary/tropical/Colombia/derivative/CO_osf_1_wide.csv")
CO_YoungSecondary_1_wide <- read_csv("data/01_primary/tropical/Colombia/derivative/CO_ysf_1_wide.csv")
MY_SAFE_primary_wide <- read_csv("data/01_primary/tropical/Senior_global/MY_SAFE/derivative/MY_SAFE_primary_wide.csv")
MY_SAFE_degraded_wide <- read_csv("data/01_primary/tropical/Senior_global/MY_SAFE/derivative/MY_SAFE_degraded_wide.csv")
TZ_usambara_wide <- read_csv("data/01_primary/tropical/Tanzania_usambara/derivative/TZ_usambara_wide.csv")
EC_maquipucuna_wide <- read_csv("data/01_primary/tropical/Ecuador/derivative/EC_maquipucuna_wide.csv")
CH_cuona_wide <- read_csv("data/01_primary/temperate/CH_Cuona/derivative/CH_cuona_wide.csv")


## ....B. Avg years #######
idaho_wide_avg <-read_csv("data/01_primary/temperate/ID_sage_brush/derivative/ID_avgyears_wide.csv")
idaho_wide_avg <-idaho_wide_avg %>% 
  mutate(year = 1)

oregon_wide_avg <-read_csv("data/01_primary/temperate/OR_andrews/derivative/OR_andrewsFineRes_avgyears_wide.csv")
oregon_wide_avg <-oregon_wide_avg %>% 
  mutate(year = 1)

sonoran_wide_avg <-read_csv("data/01_primary/temperate/AZ_sonoran_desert/derivative/AZ_sonoran_desert_avgyears_wide.csv")
sonoran_wide_avg <-sonoran_wide_avg %>% 
  mutate(year = 1)

AZ_cata_wide_avg <- read_csv("data/01_primary/temperate/AZ_santa_catelina/derivative/AZ_cata_avgyears_wide.csv")
AZ_cata_wide_avg <- AZ_cata_wide_avg %>% 
  mutate(year = 1)

fresno_wide_avg <- read_csv("data/01_primary/temperate/CA_fresno/derivative/CA_fresno_avgyears_wide.csv")
fresno_wide_avg <- fresno_wide_avg %>% 
  mutate(year = 1)

boulder_wide_avg <-read_csv("data/01_primary/temperate/CO_boulder/derivative/Boulder_met1-GLV_avgyears_wide.csv")
boulder_wide_avg <-boulder_wide_avg %>% 
  mutate(year = 1)

hubbard_forest_wide_avg <-read_csv("data/01_primary/temperate/NH_whites/derivative/NH_hubbardForest_Soil_avgyears_wide.csv")
hubbard_forest_wide_avg <-hubbard_forest_wide_avg %>% 
  mutate(year = 1)

hubbard_nonforest_wide_avg <- read_csv("data/01_primary/temperate/NH_whites/derivative/NH_hubbardNonforest_surface_avgyears_wide.csv")
hubbard_nonforest_wide_avg <- hubbard_nonforest_wide_avg %>% 
  mutate(year = 1)

mitchell_wide_avg <- read_csv("data/01_primary/temperate/NC_mt_mitchell/derivative/NC_avgyears_wide.csv")
mitchell_wide_avg <- mitchell_wide_avg %>% 
  mutate(year = 1)

coweeta_wide_avg <- read_csv("data/01_primary/temperate/TN_coweeta/derivative/TN_coweeta_soil_205_SM3_D5_avgyears_wide.csv")
coweeta_wide_avg <-coweeta_wide_avg %>% 
  mutate(year = 1)

valles_canopy_wide_avg <-read_csv("data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_canopy_avgyears_wide.csv")
valles_canopy_wide_avg <-valles_canopy_wide_avg %>% 
  mutate(year = 1)

valles_soil_wide_avg <-read_csv("data/01_primary/temperate/NM_valles_caldera/derivative/NM_valles_caldera_soil_avgyears_wide.csv")
valles_soil_wide_avg <-valles_soil_wide_avg %>% 
  mutate(year = 1)

serra_wide_avg <-read_csv("data/01_primary/temperate/PT_serra_estrala/derivative/SerraEstrala_cantaro_zezere_avgyears_wide.csv")
serra_wide_avg <-serra_wide_avg %>% 
  mutate(year = 1)

sweden_wide_avg <- read_csv("data/01_primary/arctic/SE_suorooaivi/derivative/SE_suorooaiviSM_avgyears_wide.csv")
sweden_wide_avg <-sweden_wide_avg %>% 
  mutate(year = 1)

switz_decid_wide_avg <- read_csv("data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_OTB_ISB_avgyears_wide.csv")
switz_decid_wide_avg <- switz_decid_wide_avg %>% 
  mutate(year = 1)

switz_conif_wide_avg <- read_csv("data/01_primary/temperate/CH_LWF/derivative/forest/CH_LWF_VOB_NAB_avgyears_wide.csv")
switz_conif_wide_avg <- switz_conif_wide_avg %>% 
  mutate(year = 1)

switz_open_wide_avg <- read_csv("data/01_primary/temperate/CH_LWF/derivative/open/CH_LWF_VOF_NAF_avgyears_wide.csv")
switz_open_wide_avg <- switz_open_wide_avg %>% 
  mutate(year = 1)

AK_nabesna_wide_avg <- read_csv("data/01_primary/temperate/AK_nabesna/derivative/AK_nabesna_avgyears_wide.csv")
AK_nabesna_wide_avg <- AK_nabesna_wide_avg %>% 
  mutate(year = 1)

AK_ameriflux_wide_avg <- read_csv("data/01_primary/arctic/AK_ameriflux/derivative/AK_ameriflux_avgyears_wide.csv")
AK_ameriflux_wide_avg <- AK_ameriflux_wide_avg %>% 
  mutate(year = 1)

aust_wide_avg <-read_csv("data/01_primary/tropical/Australia/derivative/Aust_wide.csv")
aust_wide_avg <-aust_wide_avg %>% 
  mutate(year = 1)

mada_wide_avg <-read_csv("data/01_primary/tropical/Madagascar/derivative/mada_wide.csv")
mada_wide_avg <-mada_wide_avg %>% 
  mutate(year = 1)

phili_wide_avg <-read_csv("data/01_primary/tropical/Philippines/derivative/phili_wide.csv")
phili_wide_avg <-phili_wide_avg %>% 
  mutate(year = 1)

costa_basham_wide_avg <- read_csv("data/01_primary/tropical/Costa_Rica/derivative/CR_avgyears_wide.csv")
costa_basham_wide_avg <-costa_basham_wide_avg %>% 
  mutate(year = 1)

cr_northwest_wide_avg <- read_csv("data/01_primary/tropical/CR_janzen/derivative/CR_northwest_avgyears_wide.csv")
cr_northwest_wide_avg <-cr_northwest_wide_avg %>% 
  mutate(year = 1)

cr_southwest_wide_avg <- read_csv("data/01_primary/tropical/CR_janzen/derivative/CR_southwest_avgyears_wide.csv")
cr_southwest_wide_avg <-cr_southwest_wide_avg %>% 
  mutate(year = 1)

CO_PrimaryForest_1_wide_avg <- read_csv("data/01_primary/tropical/Colombia/derivative/CO_pf_1_avgyears_wide.csv")
CO_PrimaryForest_1_wide_avg <-CO_PrimaryForest_1_wide_avg %>% 
  mutate(year = 1)

CO_PrimaryForest_2_wide_avg <- read_csv("data/01_primary/tropical/Colombia/derivative/CO_pf_2_avgyears_wide.csv")
CO_PrimaryForest_2_wide_avg <- CO_PrimaryForest_2_wide_avg %>% 
  mutate(year = 1)

CO_OldSecondary_1_wide_avg <- read_csv("data/01_primary/tropical/Colombia/derivative/CO_osf_1_avgyears_wide.csv")
CO_OldSecondary_1_wide_avg <- CO_OldSecondary_1_wide_avg %>% 
  mutate(year = 1)

CO_YoungSecondary_1_wide_avg <- read_csv("data/01_primary/tropical/Colombia/derivative/CO_ysf_1_avgyears_wide.csv")
CO_YoungSecondary_1_wide_avg <- CO_YoungSecondary_1_wide_avg %>% 
  mutate(year = 1)

MY_SAFE_primary_wide_avg <- read_csv("data/01_primary/tropical/Senior_global/MY_SAFE/derivative/MY_SAFE_primary_avgyears_wide.csv")
MY_SAFE_primary_wide_avg <- MY_SAFE_primary_wide_avg %>% 
  mutate(year = 1)

MY_SAFE_degraded_wide_avg <- read_csv("data/01_primary/tropical/Senior_global/MY_SAFE/derivative/MY_SAFE_degraded_avgyears_wide.csv")
MY_SAFE_degraded_wide_avg <- MY_SAFE_degraded_wide_avg %>% 
  mutate(year = 1)

TZ_usambara_wide_avg <- read_csv("data/01_primary/tropical/Tanzania_usambara/derivative/TZ_usambara_avgyears_wide.csv")
TZ_usambara_wide_avg <- TZ_usambara_wide_avg %>% 
  mutate(year = 1)

EC_maquipucuna_wide_avg <- read_csv("data/01_primary/tropical/Ecuador/derivative/EC_maquipucuna_avgyears_wide.csv")
EC_maquipucuna_wide_avg <- EC_maquipucuna_wide_avg %>% 
  mutate(year = 1)

CH_cuona_wide_avg <- read_csv("data/01_primary/temperate/CH_Cuona/derivative/CH_cuona_avgyears_wide.csv")
CH_cuona_wide_avg <- CH_cuona_wide_avg %>% 
  mutate(year = 1)

## 2. Compile database #######
## ....A. Tall ########

mountains <- as_tibble(idaho) %>% 
  bind_rows(as_tibble(oregon)) %>% 
  bind_rows(as_tibble(sonoran)) %>%
  bind_rows(as_tibble(AZ_cata)) %>% 
  bind_rows(as_tibble(fresno)) %>% 
  bind_rows(as_tibble(boulder)) %>% 
  bind_rows(as_tibble(mitchell)) %>%
  bind_rows(as_tibble(hubbard_forest)) %>% 
  bind_rows(as_tibble(hubbard_nonforest)) %>%
  bind_rows(as_tibble(coweeta)) %>% 
  bind_rows(as_tibble(valles_canopy)) %>% 
  bind_rows(as_tibble(valles_soil)) %>% 
  bind_rows(as_tibble(serra)) %>%
  bind_rows(as_tibble(sweden)) %>%
  bind_rows(as_tibble(switz_decid)) %>% 
  bind_rows(as_tibble(switz_conif)) %>% 
  bind_rows(as_tibble(switz_open)) %>%
  bind_rows(as_tibble(AK_nabesna)) %>% 
  bind_rows(as_tibble(aust)) %>% 
  bind_rows(as_tibble(mada)) %>% 
  bind_rows(as_tibble(phili)) %>% 
  bind_rows(as_tibble(costa_basham)) %>% 
  bind_rows(as_tibble(cr_northwest)) %>% 
  bind_rows(as_tibble(cr_southwest)) %>% 
  bind_rows(as_tibble(CO_PrimaryForest_1)) %>%
  bind_rows(as_tibble(CO_PrimaryForest_2)) %>%
  bind_rows(as_tibble(CO_OldSecondary_1)) %>% 
  bind_rows(as_tibble(CO_YoungSecondary_1)) %>% 
  bind_rows(as_tibble(AK_ameriflux)) %>% 
  bind_rows(as_tibble(MY_SAFE_primary)) %>%
  bind_rows(as_tibble(MY_SAFE_degraded)) %>%
  bind_rows(as_tibble(TZ_usambara)) %>% 
  bind_rows(as_tibble(EC_maquipucuna)) %>% 
  bind_rows(as_tibble(CH_cuona)) 


mountains_avg <- as_tibble(idaho_avg) %>% 
  bind_rows(as_tibble(oregon_avg)) %>% 
  bind_rows(as_tibble(sonoran_avg)) %>%
  bind_rows(as_tibble(AZ_cata_avg)) %>% 
  bind_rows(as_tibble(fresno_avg)) %>% 
  bind_rows(as_tibble(boulder_avg)) %>% 
  bind_rows(as_tibble(mitchell_avg)) %>% 
  bind_rows(as_tibble(hubbard_forest_avg)) %>% 
  bind_rows(as_tibble(hubbard_nonforest_avg)) %>% 
  bind_rows(as_tibble(coweeta_avg)) %>% 
  bind_rows(as_tibble(valles_canopy_avg)) %>% 
  bind_rows(as_tibble(valles_soil_avg)) %>% 
  bind_rows(as_tibble(serra_avg)) %>% 
  bind_rows(as_tibble(sweden_avg)) %>% 
  bind_rows(as_tibble(switz_decid_avg)) %>% 
  bind_rows(as_tibble(switz_conif_avg)) %>% 
  bind_rows(as_tibble(switz_open_avg)) %>% 
  bind_rows(as_tibble(AK_nabesna_avg)) %>% 
  bind_rows(as_tibble(aust)) %>% 
  bind_rows(as_tibble(mada_avg)) %>% 
  bind_rows(as_tibble(phili_avg)) %>% 
  bind_rows(as_tibble(costa_basham_avg)) %>% 
  bind_rows(as_tibble(cr_northwest_avg)) %>% 
  bind_rows(as_tibble(cr_southwest_avg)) %>% 
  bind_rows(as_tibble(CO_PrimaryForest_1_avg)) %>%
  bind_rows(as_tibble(CO_PrimaryForest_2_avg)) %>%
  bind_rows(as_tibble(CO_OldSecondary_1_avg)) %>% 
  bind_rows(as_tibble(CO_YoungSecondary_1_avg)) %>% 
  bind_rows(as_tibble(AK_ameriflux_avg)) %>% 
  bind_rows(as_tibble(MY_SAFE_primary_avg)) %>%
  bind_rows(as_tibble(MY_SAFE_degraded_avg)) %>%
  bind_rows(as_tibble(TZ_usambara_avg)) %>% 
  bind_rows(as_tibble(EC_maquipucuna_avg)) %>% 
  bind_rows(as_tibble(CH_cuona_avg)) 


## ....B. Wide ########

mountains_wide <- as_tibble(idaho_wide) %>% 
  bind_rows(as_tibble(oregon_wide)) %>% 
  bind_rows(as_tibble(sonoran_wide)) %>%
  bind_rows(as_tibble(AZ_cata_wide)) %>% 
  bind_rows(as_tibble(fresno_wide)) %>% 
  bind_rows(as_tibble(boulder_wide)) %>% 
  bind_rows(as_tibble(mitchell_wide)) %>% 
  bind_rows(as_tibble(hubbard_forest_wide)) %>% 
  bind_rows(as_tibble(hubbard_nonforest_wide)) %>% 
  bind_rows(as_tibble(coweeta_wide)) %>% 
  bind_rows(as_tibble(valles_canopy_wide)) %>% 
  bind_rows(as_tibble(valles_soil_wide)) %>% 
  bind_rows(as_tibble(serra_wide)) %>% 
  bind_rows(as_tibble(sweden_wide)) %>% 
  bind_rows(as_tibble(switz_decid_wide)) %>% 
  bind_rows(as_tibble(switz_conif_wide)) %>% 
  bind_rows(as_tibble(switz_open_wide)) %>% 
  bind_rows(as_tibble(AK_nabesna_wide)) %>% 
  bind_rows(as_tibble(aust_wide)) %>% 
  bind_rows(as_tibble(mada_wide)) %>% 
  bind_rows(as_tibble(phili_wide)) %>% 
  bind_rows(as_tibble(costa_basham_wide)) %>% 
  bind_rows(as_tibble(cr_northwest_wide)) %>% 
  bind_rows(as_tibble(cr_southwest_wide)) %>% 
  bind_rows(as_tibble(CO_PrimaryForest_1_wide)) %>%
  bind_rows(as_tibble(CO_PrimaryForest_2_wide)) %>%
  bind_rows(as_tibble(CO_OldSecondary_1_wide)) %>% 
  bind_rows(as_tibble(CO_YoungSecondary_1_wide)) %>% 
  bind_rows(as_tibble(AK_ameriflux_wide)) %>% 
  bind_rows(as_tibble(MY_SAFE_primary_wide)) %>%
  bind_rows(as_tibble(MY_SAFE_degraded_wide)) %>%
  bind_rows(as_tibble(TZ_usambara_wide)) %>% 
  bind_rows(as_tibble(EC_maquipucuna_wide)) %>% 
  bind_rows(as_tibble(CH_cuona_wide)) 


mountains_wide_avg <- as_tibble(idaho_wide_avg) %>% 
  bind_rows(as_tibble(oregon_wide_avg)) %>% 
  bind_rows(as_tibble(sonoran_wide_avg)) %>%
  bind_rows(as_tibble(AZ_cata_wide_avg)) %>% 
  bind_rows(as_tibble(fresno_wide_avg)) %>% 
  bind_rows(as_tibble(boulder_wide_avg)) %>% 
  bind_rows(as_tibble(mitchell_wide_avg)) %>% 
  bind_rows(as_tibble(hubbard_forest_wide_avg)) %>% 
  bind_rows(as_tibble(hubbard_nonforest_wide_avg)) %>% 
  bind_rows(as_tibble(coweeta_wide_avg)) %>% 
  bind_rows(as_tibble(valles_canopy_wide_avg)) %>% 
  bind_rows(as_tibble(valles_soil_wide_avg)) %>% 
  bind_rows(as_tibble(serra_wide_avg)) %>% 
  bind_rows(as_tibble(sweden_wide_avg)) %>% 
  bind_rows(as_tibble(switz_decid_wide_avg)) %>% 
  bind_rows(as_tibble(switz_conif_wide_avg)) %>% 
  bind_rows(as_tibble(switz_open_wide_avg)) %>% 
  bind_rows(as_tibble(AK_nabesna_wide_avg)) %>% 
  bind_rows(as_tibble(aust_wide_avg)) %>% 
  bind_rows(as_tibble(mada_wide_avg)) %>% 
  bind_rows(as_tibble(phili_wide_avg)) %>% 
  bind_rows(as_tibble(costa_basham_wide_avg)) %>% 
  bind_rows(as_tibble(cr_northwest_wide_avg)) %>% 
  bind_rows(as_tibble(cr_southwest_wide_avg)) %>% 
  bind_rows(as_tibble(CO_PrimaryForest_1_wide_avg)) %>%
  bind_rows(as_tibble(CO_PrimaryForest_2_wide_avg)) %>%
  bind_rows(as_tibble(CO_OldSecondary_1_wide_avg)) %>% 
  bind_rows(as_tibble(CO_YoungSecondary_1_wide_avg)) %>% 
  bind_rows(as_tibble(AK_ameriflux_wide_avg)) %>% 
  bind_rows(as_tibble(MY_SAFE_primary_wide_avg)) %>%
  bind_rows(as_tibble(MY_SAFE_degraded_wide_avg)) %>%
  bind_rows(as_tibble(TZ_usambara_wide_avg)) %>% 
  bind_rows(as_tibble(EC_maquipucuna_wide_avg)) %>% 
  bind_rows(as_tibble(CH_cuona_wide_avg)) 

## Filtering out deep soil sensors ##########

## It's only Pam's data from Colombia, at 20 cm depth. Next deepest is 12
## Also it's just the soil sensors at that site so can safely remove the site
## without losing other data
mountains <- mountains %>% 
  filter(site != "CO_PrimaryForest_2")
mountains_avg <- mountains_avg %>% 
  filter(site != "CO_PrimaryForest_2")
mountains_wide <- mountains_wide %>% 
  filter(site != "CO_PrimaryForest_2")
mountains_wide_avg <- mountains_wide_avg %>% 
  filter(site != "CO_PrimaryForest_2")

## 3. Calculate and combine elevation change #######

# Prep elevation change
elev_change <- mountains %>% 
  dplyr::select(site, elevation, altitude) %>% 
  distinct() %>% 
  group_by(site) %>% 
  spread(key = elevation, value = altitude) %>% 
  dplyr::rename(altitude_low = low, altitude_high = high) %>% 
  # altitude_high and altitude_low values are in different rows, so some rows
  #   contain NAs. If we're grouped by site and take the max we should get
  #   rid of those.
  mutate(altitude_high = mean(altitude_high, na.rm = TRUE)) %>%
  mutate(altitude_low = mean(altitude_low, na.rm = TRUE)) %>% 
  # # For some reason sometimes -Inf is generated, get rid of those rows
  # filter(is.finite(altitude_high) & is.finite(altitude_low)) %>% 
  mutate(elevation_change = altitude_high - altitude_low)

mountains <- mountains %>% 
  left_join(elev_change)

mountains_avg <- mountains_avg %>% 
  left_join(elev_change)

mountains_wide <- mountains_wide %>% 
  left_join(elev_change)

mountains_wide_avg <- mountains_wide_avg %>% 
  left_join(elev_change)

## 4. Join vegetation structure ############

mountains <- mountains %>%
  dplyr::select(-latitude) %>% 
  full_join(veg_structure)

mountains_avg <- mountains_avg %>%
  dplyr::select(-latitude) %>% 
  full_join(veg_structure)

correct_vegStructure <- function(mountains) {
  
  # Correcting vegetation structure data
  # mountains <- mountains %>% 
  #   
  #   # For "meadows near forest" and "developed" macros, for which I know there 
  #   # isn't forest, reduce the veg_structure of these sites to values comparable 
  #   # to non-forest
  #   mutate(veg_structure = ifelse(macro == "meadow near forest" | macro == "developed", 
  #                                 veg_structure * 0.1, veg_structure)) 
    
  
  mountains
}

mountains <- correct_vegStructure(mountains)
mountains_avg <- correct_vegStructure(mountains_avg)

## 5. Calculate overlap #########

source("scripts/00_source_code/janzify.R")

## ....A. Daily level ########

# Original calc: overlap per day
mountains_janz <- janzify(mountains_wide)
mountains_janz_avg <- janzify(mountains_wide_avg)

# Remove data that is aggregated about daily level
mountains_janz <- mountains_janz %>% 
  filter(complete.cases(julian))

mountains_janz_avg <- mountains_janz_avg %>% 
  filter(complete.cases(julian))

## ....B. Monthly level ########

# Overlap per month, D scores and TAD
mountains_janz_monthly <- janzify_per_month(mountains_wide)
mountains_janz_avg_monthly <- janzify_per_month(mountains_wide_avg)

# Average overlap values for each month of the year
mountains_janz_avg_monthly <- mountains_janz_monthly %>% 
  # Ignoring kozak_wiens because I don't think I use it downstream
  group_by(site, micro, month, elevation_change) %>% 
  summarize_at(vars(janzenDscore, janzenDscore_elevCorr, TAD, TAD_elevCorr),
               mean, na.rm = TRUE) %>% 
  ungroup()

# Overlap per month, KDE
mountains_janz_monthly_kde <- overlappify(mountains)

# Average KDE values for each month of the year
mountains_janz_avg_monthly_kde <- mountains_janz_monthly_kde %>% 
  group_by(site, micro, month, elevation_change) %>% 
  summarize_at(vars(kde, kde_elevCorr),
               mean, na.rm = TRUE) %>% 
  ungroup()

mountains_janz_avg_monthly <- mountains_janz_avg_monthly %>% 
  left_join(mountains_janz_avg_monthly_kde)

## 6. Join other flags to wide data ########

## ....A. Regular database + daily overlap, allyears #############

# Prep the habitat/site flags in the tall data that we want in wide data
wide_ready <- mountains %>% 
  dplyr::select(-month) %>% 
  group_by(site, year, julian, micro) %>% 
  count(site, macro, foliage, flora, snow_source_flag, height_notes) %>% 
  slice(which.max(n)) %>% 
  ungroup() %>% 
  dplyr::select(-n)

# Now do continous attributes 
wide_ready_continous <- mountains %>% 
  dplyr::select(-month) %>% 
  group_by(site, year, julian, micro) %>% 
  summarize_at(vars(snowdepth, latitude, veg_structure, foliage_cover,
                    elevation_change, height), ~ mean(., na.rm = TRUE)) %>% 
  ungroup() %>% 
  # Add a snow presence/absence flag from snowdepth data
  mutate(snow_presence = ifelse(snowdepth > 2, "snow", "no snow"))

wide_ready <- wide_ready %>% 
  left_join(wide_ready_continous)

mountains_janz <- mountains_janz %>% 
  # Removing a few cols that are going to get joined from the wide_ready...
  # keeping them in explodes the dataset
  dplyr::select(site, year, julian, micro, TAD, TAD_elevCorr, janzenDscore,
                janzenDscore_elevCorr) %>%
  full_join(wide_ready) %>% 
  # Filtering out some NAs that pop up
  filter(complete.cases(micro))

# Calc snow presence for regular database too
mountains <- mountains %>% 
  # Add a snow presence/absence flag from snowdepth data
  mutate(snow_presence = ifelse(snowdepth > 2, "snow", "no snow")) 

mountains_avg <- mountains_avg %>% 
  # Add a snow presence/absence flag from snowdepth data
  mutate(snow_presence = ifelse(snowdepth > 2, "snow", "no snow")) 

## ....B. Regular database + daily overlap, avgyears #############

wide_ready_avg <- mountains_avg %>% 
  dplyr::select(-month) %>% 
  group_by(site, julian, micro) %>% 
  count(site, macro, foliage, flora, snow_source_flag, height_notes) %>% 
  slice(which.max(n)) %>% 
  ungroup() %>% 
  dplyr::select(-n)

# Now do continous attributes 
wide_ready_continous_avg <- mountains_avg %>% 
  dplyr::select(-month) %>% 
  group_by(site, julian, micro) %>% 
  summarize_at(vars(snowdepth, latitude, veg_structure, foliage_cover, height,
                    elevation_change), ~ mean(., na.rm = TRUE)) %>% 
  ungroup() %>% 
  # Add a snow presence/absence flag from snowdepth data
  mutate(snow_presence = ifelse(snowdepth > 2, "snow", "no snow")) 

wide_ready_avg <- wide_ready_avg %>% 
  left_join(wide_ready_continous_avg)

mountains_wide_avg <- mountains_wide_avg %>% 
  # Removing a few cols that are going to get joined from the wide_ready...
  # keeping them in explodes the dataset
  dplyr::select(julian, site, year, contains("high_"), contains("low_")) %>% 
  full_join(wide_ready_avg)

mountains_janz_avg <- mountains_janz_avg %>% 
  # Removing a few cols that are going to get joined from the wide_ready...
  # keeping them in explodes the dataset
  dplyr::select(julian, site, year, TAD, micro, janzenDscore, TAD_elevCorr, 
                janzenDscore_elevCorr) %>% 
  full_join(wide_ready_avg) %>% 
  # Filtering out some NAs that pop up
  filter(complete.cases(micro))

## ....C. For monthly overlap, allyears #############

# Wide-ready for monthly
# Prep the habitat/site flags in the tall data that we want in wide data
wide_ready_monthly <- mountains %>% 
  mutate(month = ifelse(is.na(month), 
                        month(as.POSIXlt(as.Date(julian, format = "%j", 
                                                 origin = paste0("1.1.", year)), 
                                         format="%d/%m/%Y")), month)) %>% 
  group_by(site, year, month, micro) %>% 
  count(site, macro, foliage, flora, snow_source_flag, height_notes) %>% 
  slice(which.max(n)) %>% 
  ungroup() %>% 
  dplyr::select(-n)

# Now do continous attributes 
wide_ready_monthly_continous <- mountains %>%
  mutate(month = ifelse(is.na(month), 
                        month(as.POSIXlt(as.Date(julian, format = "%j", 
                                                 origin = paste0("1.1.", year)), 
                                         format="%d/%m/%Y")), month)) %>% 
  group_by(site, year, month, micro) %>% 
  summarize_at(vars(snowdepth, latitude, veg_structure, foliage_cover, height,
                    elevation_change), ~ mean(., na.rm = TRUE)) %>% 
  ungroup() %>% 
  # Add a snow presence/absence flag from snowdepth data
  mutate(snow_presence = ifelse(snowdepth > 2, "snow", "no snow"))

wide_ready_monthly <- wide_ready_monthly %>% 
  left_join(wide_ready_monthly_continous)

mountains_janz_monthly <- mountains_janz_monthly %>% 
  dplyr::select(site, year, month, micro, TAD, janzenDscore, TAD_elevCorr, 
                janzenDscore_elevCorr) %>% 
  full_join(wide_ready_monthly) %>% 
  # Filtering out some NAs that pop up
  filter(complete.cases(micro))

## ....D. For monthly overlap, avgyears #############

wide_ready_monthly_avg <- mountains_avg %>% 
  mutate(month = ifelse(is.na(month), 
                        month(as.POSIXlt(as.Date(julian, format = "%j", 
                                                 origin = paste0("1.1.", year)), 
                                         format="%d/%m/%Y")), month)) %>% 
  group_by(site, month, micro) %>% 
  count(site, macro, foliage, flora, snow_source_flag, height_notes) %>% 
  slice(which.max(n)) %>% 
  ungroup() %>% 
  dplyr::select(-n)

# Now do continous attributes 
wide_ready_monthly_continous_avg <- mountains_avg %>%
  mutate(month = ifelse(is.na(month), 
                        month(as.POSIXlt(as.Date(julian, format = "%j", 
                                                 origin = paste0("1.1.", year)), 
                                         format="%d/%m/%Y")), month)) %>% 
  # Filter down to non-NA so that a single NA does not disrupt average
  # Not including heights here because it the NA omit removes all places that 
  # I don't know their heights
  # filter(complete.cases(snowdepth), complete.cases(latitude),
  #        complete.cases(veg_structure)) %>% 
  group_by(site, month, micro) %>% 
  summarize_at(vars(snowdepth, latitude, veg_structure, foliage_cover,
                    elevation_change, height), ~ mean(., na.rm = TRUE)) %>% 
  ungroup() %>% 
  # Add a snow presence/absence flag from snowdepth data
  mutate(snow_presence = ifelse(snowdepth > 2, "snow", "no snow"))

wide_ready_monthly_avg <- wide_ready_monthly_avg %>% 
  left_join(wide_ready_monthly_continous_avg)

mountains_janz_avg_monthly <- mountains_janz_avg_monthly %>% 
  full_join(wide_ready_monthly_avg) %>% 
  # Filtering out some NAs that pop up
  filter(complete.cases(micro))

## 7. Final curation, removing unnecessary cols ############

mountains_avg <- mountains_avg %>% 
  dplyr::select(-year, -month)

mountains_wide_avg <- mountains_wide_avg %>% 
  dplyr::select(-year)

## 6. Write out data ##########

write_csv(mountains, "data/03_compiled/mountains.csv")
write_csv(mountains_avg, "data/03_compiled/mountains_avg.csv")
write_csv(mountains_wide, "data/03_compiled/mountains_wide.csv")
write_csv(mountains_wide_avg, "data/03_compiled/mountains_wide_avg.csv")
write_csv(mountains_janz, "data/03_compiled/mountains_overlap_daily.csv")
write_csv(mountains_janz_avg, "data/03_compiled/mountains_overlap_daily_avg.csv")
write_csv(mountains_janz_monthly, "data/03_compiled/mountains_overlap_monthly.csv")
write_csv(mountains_janz_avg_monthly, "data/03_compiled/mountains_overlap_monthly_avg.csv")
