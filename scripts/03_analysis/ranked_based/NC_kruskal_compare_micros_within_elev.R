#David Klinges
#this script compares thermal regimes of microhabitats, at the same elevation, 
#seperately for leaf-off and leaf-on



####### Combine soil, ground, and canopy to prep for Kruskal ##########

#leafoff
soil_500_leafoff_withmicro <- soil_500_leafoff %>%
  mutate(micro = "soil")

ground_500_leafoff_withmicro <- ground_500_leafoff %>%
  mutate(micro = "ground")

can_500_leafoff_withmicro <- can_500_leafoff %>%
  mutate(micro = "can")

all_500_leafoff <- rbind(soil_500_leafoff_withmicro, ground_500_leafoff_withmicro, can_500_leafoff_withmicro)

#leafon
soil_500_leafon_withmicro <- soil_500_leafon %>%
  mutate(micro = "soil")

ground_500_leafon_withmicro <- ground_500_leafon %>%
  mutate(micro = "ground")

can_500_leafon_withmicro <- can_500_leafon %>%
  mutate(micro = "can")

all_500_leafon <- rbind(soil_500_leafon_withmicro, ground_500_leafon_withmicro, can_500_leafon_withmicro)

#leafoff
soil_1700_leafoff_withmicro <- soil_1700_leafoff %>%
  mutate(micro = "soil")

ground_1700_leafoff_withmicro <- ground_1700_leafoff %>%
  mutate(micro = "ground")

can_1700_leafoff_withmicro <- can_1700_leafoff %>%
  mutate(micro = "can")

all_1700_leafoff <- rbind(soil_1700_leafoff_withmicro, ground_1700_leafoff_withmicro, can_1700_leafoff_withmicro)

#leafon
soil_1700_leafon_withmicro <- soil_1700_leafon %>%
  mutate(micro = "soil")

ground_1700_leafon_withmicro <- ground_1700_leafon %>%
  mutate(micro = "ground")

can_1700_leafon_withmicro <- can_1700_leafon %>%
  mutate(micro = "can")

all_1700_leafon <- rbind(soil_1700_leafon_withmicro, ground_1700_leafon_withmicro, can_1700_leafon_withmicro)





####### Create daily means, mins, and maxs from combined data #########

#daily mean
all_500_leafoff_mean <- dailymean(all_500_leafoff)
all_500_leafon_mean <- dailymean(all_500_leafon)
all_1700_leafoff_mean <- dailymean(all_1700_leafoff)
all_1700_leafon_mean <- dailymean(all_1700_leafon)

#combined daily min and daily max
all_500_leafoff_minmax <- dailyminmax(all_500_leafoff)
all_500_leafon_minmax <- dailyminmax(all_500_leafon)
all_1700_leafoff_minmax <- dailyminmax(all_1700_leafoff)
all_1700_leafon_minmax <- dailyminmax(all_1700_leafon)

####### ^Didn't actually need to do these steps, but keeping in ########
####### Kruskal-Wallis for all temp ##########

#500 leaf-off
kruskal.test(list(soil_500_leafoff$temp, ground_500_leafoff$temp, can_500_leafoff$temp))

#500 leaf-on
kruskal.test(list(soil_500_leafon$temp, ground_500_leafon$temp, can_500_leafon$temp))

#1700 leaf-off
kruskal.test(list(soil_1700_leafoff$temp, ground_1700_leafoff$temp, can_1700_leafoff$temp))

#1700 leaf-on
kruskal.test(list(soil_1700_leafon$temp, ground_1700_leafon$temp, can_1700_leafon$temp))

####### Kruskal-Wallis for daily mean ##########

#500 leaf-off
kruskal.test(list(soil_500_leafoff_mean$temp, ground_500_leafoff_mean$temp, can_500_leafoff_mean$temp))

#500 leaf-on
kruskal.test(list(soil_500_leafon_mean$temp, ground_500_leafon_mean$temp, can_500_leafon_mean$temp))

#1700 leaf-off
kruskal.test(list(soil_1700_leafoff_mean$temp, ground_1700_leafoff_mean$temp, can_1700_leafoff_mean$temp))

#1700 leaf-on
kruskal.test(list(soil_1700_leafon_mean$temp, ground_1700_leafon_mean$temp, can_1700_leafon_mean$temp))

###### Kruskal-Wallis for combined daily min and daily max #########

#500 leaf-off
kruskal.test(list(soil_500_leafoff_minmax$temp, ground_500_leafoff_minmax$temp, can_500_leafoff_minmax$temp))

#500 leaf-on
kruskal.test(list(soil_500_leafon_minmax$temp, ground_500_leafon_minmax$temp, can_500_leafon_minmax$temp))

#1700 leaf-off
kruskal.test(list(soil_1700_leafoff_minmax$temp, ground_1700_leafoff_minmax$temp, can_1700_leafoff_minmax$temp))

#1700 leaf-on
kruskal.test(list(soil_1700_leafon_minmax$temp, ground_1700_leafon_minmax$temp, can_1700_leafon_minmax$temp))


