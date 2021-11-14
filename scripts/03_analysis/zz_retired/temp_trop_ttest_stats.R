#Klinges and Scheffers 2018
#compare daily average min and max of temperate and tropical data
#this is a helper script that is tasked with just conducting t tests of daily averages.
#It is dependent on objects created in /temp_trop_overlap_stats.



######## Combine tropical data ############

#Tropical Soil low
trop_min_low_soil <- rbind(Mada_min_low_soil_sampled, Aust_min_low_soil_sampled)
trop_max_low_soil <- rbind(Mada_max_low_soil_sampled, Aust_max_low_soil_sampled)

#Tropical Soil up
trop_min_up_soil <- rbind(Mada_min_up_soil_sampled, Aust_min_up_soil_sampled)
trop_max_up_soil <- rbind(Mada_max_up_soil_sampled, Aust_max_up_soil_sampled)

#Tropical Ground low
trop_min_low_ground <- rbind(Mada_min_low_ground_sampled, Phili_min_low_ground_sampled,
                             Aust_min_low_ground_sampled)

trop_max_low_ground <- rbind(Mada_max_low_ground_sampled, Phili_max_low_ground_sampled, 
                             Aust_max_low_ground_sampled)
#Tropical Ground up
trop_min_up_ground <- rbind(Mada_min_up_ground_sampled, Phili_min_up_ground_sampled,
                            Aust_min_up_ground_sampled)

trop_max_up_ground <- rbind(Mada_max_up_ground_sampled, Phili_max_up_ground_sampled, 
                            Aust_max_up_ground_sampled)

#Tropical Canopy low
trop_min_low_canopy <- rbind(Mada_min_low_canopy_sampled, Phili_min_low_canopy_sampled,
                            Aust_min_low_canopy_sampled)

trop_max_low_canopy <- rbind(Mada_max_low_canopy_sampled, Phili_max_low_canopy_sampled, 
                            Aust_max_low_canopy_sampled)
#Tropical Canopy up
trop_min_up_canopy <- rbind(Mada_min_up_canopy_sampled, Phili_min_up_canopy_sampled,
                            Aust_min_up_canopy_sampled)

trop_max_up_canopy <- rbind(Mada_max_up_canopy_sampled, Phili_max_up_canopy_sampled, 
                            Aust_max_up_canopy_sampled)

#Leaf on
#Temperate Leaf-off Soil up
NC_off_soil_up <- rbind(NC_min_off_up_soil_sampled, NC_max_off_up_soil_sampled)

#Temperate Leaf-off Soil low
NC_off_soil_low <- rbind(NC_min_off_low_soil_sampled, NC_max_off_low_soil_sampled)

#Temperate Leaf-off Ground up
NC_off_ground_up <- rbind(NC_min_off_up_ground_sampled, NC_max_off_up_ground_sampled)

#Temperate Leaf-off Ground low
NC_off_ground_low <- rbind(NC_min_off_low_ground_sampled, NC_max_off_low_ground_sampled)

#Temperate Leaf-off Canopy up
NC_off_canopy_up <- rbind(NC_min_off_up_canopy_sampled, NC_max_off_up_canopy_sampled)

#Temperate Leaf-off Canopy low
NC_off_canopy_low <- rbind(NC_min_off_low_canopy_sampled, NC_max_off_low_canopy_sampled)


##Leaf on
#Temperate Leaf-on Soil up
NC_on_soil_up <- rbind(NC_min_on_up_soil_sampled, NC_max_on_up_soil_sampled)

#Temperate Leaf-on Soil low
NC_on_soil_low <- rbind(NC_min_on_low_soil_sampled, NC_max_on_low_soil_sampled)

#Temperate Leaf-on Ground up
NC_on_ground_up <- rbind(NC_min_on_up_ground_sampled, NC_max_on_up_ground_sampled)

#Temperate Leaf-on Ground low
NC_on_ground_low <- rbind(NC_min_on_low_ground_sampled, NC_max_on_low_ground_sampled)

#Temperate Leaf-on Canopy up
NC_on_canopy_up <- rbind(NC_min_on_up_canopy_sampled, NC_max_on_up_canopy_sampled)

#Temperate Leaf-on Canopy low
NC_on_canopy_low <- rbind(NC_min_on_low_canopy_sampled, NC_max_on_low_canopy_sampled)





######## Sample randomly #########
#take a random sample from each microhabitat
#the total sample from tropical regions should equal the sample from NC

#set sample sizes
m <- 80
a <- 30
p <- 36
non <- 120
noff <- 18

#Sample Mada micros
Mada_micros_sampled <- lapply(Mada_micros, function(x) {
  x %>%
    group_by(site) %>%
    sample_n(m, replace = FALSE) %>%
    select(elev, site, micro, temp) #remove julian column, which was causing problems of coercion later on
})
#rename
Mada_min_up_soil_sampled <- Mada_micros_sampled[[1]]
Mada_min_up_ground_sampled <- Mada_micros_sampled[[2]]
Mada_min_up_canopy_sampled <- Mada_micros_sampled[[3]]
Mada_min_low_soil_sampled <- Mada_micros_sampled[[4]]
Mada_min_low_ground_sampled <- Mada_micros_sampled[[5]]
Mada_min_low_canopy_sampled <- Mada_micros_sampled[[6]]
Mada_max_up_soil_sampled <- Mada_micros_sampled[[7]]
Mada_max_up_ground_sampled <- Mada_micros_sampled[[8]]
Mada_max_up_canopy_sampled <- Mada_micros_sampled[[9]]
Mada_max_low_soil_sampled <- Mada_micros_sampled[[10]]
Mada_max_low_ground_sampled <- Mada_micros_sampled[[11]]
Mada_max_low_canopy_sampled <- Mada_micros_sampled[[12]]

#Sample Phili micros
Phili_micros_sampled <- lapply(Phili_micros, function(x) {
  x %>%
    group_by(site) %>%
    sample_n(p, replace = FALSE) %>%
    select(elev, site, micro, temp) #remove julian column, which was causing problems of coercion later on
})
#rename
Phili_min_up_ground_sampled <- Phili_micros_sampled[[1]]
Phili_min_up_canopy_sampled <- Phili_micros_sampled[[2]]
Phili_min_low_ground_sampled <- Phili_micros_sampled[[3]]
Phili_min_low_canopy_sampled <- Phili_micros_sampled[[4]]
Phili_max_up_ground_sampled <- Phili_micros_sampled[[5]]
Phili_max_up_canopy_sampled <- Phili_micros_sampled[[6]]
Phili_max_low_ground_sampled <- Phili_micros_sampled[[7]]
Phili_max_low_canopy_sampled <- Phili_micros_sampled[[8]]

#Sample Aust micros
Aust_micros_sampled <- lapply(Aust_micros, function(x) {
  x %>%
    group_by(site) %>%
    sample_n(a, replace = FALSE) %>%
    select(elev, site, micro, temp) #remove julian column, which was causing problems of coercion later on
})
#rename
Aust_min_up_soil_sampled <- Aust_micros_sampled[[1]]
Aust_min_up_ground_sampled <- Aust_micros_sampled[[2]]
Aust_min_up_canopy_sampled <- Aust_micros_sampled[[3]]
Aust_min_low_soil_sampled <- Aust_micros_sampled[[4]]
Aust_min_low_ground_sampled <- Aust_micros_sampled[[5]]
Aust_min_low_canopy_sampled <- Aust_micros_sampled[[6]]
Aust_max_up_soil_sampled <- Aust_micros_sampled[[7]]
Aust_max_up_ground_sampled <- Aust_micros_sampled[[8]]
Aust_max_up_canopy_sampled <- Aust_micros_sampled[[9]]
Aust_max_low_soil_sampled <- Aust_micros_sampled[[10]]
Aust_max_low_ground_sampled <- Aust_micros_sampled[[11]]
Aust_max_low_canopy_sampled <- Aust_micros_sampled[[12]]

#Sample NC on micros
NC_micros_on_sampled <- lapply(NC_micros_on, function(x) {
  x %>%
    group_by(site) %>%
    sample_n(non, replace = FALSE) %>%
    select(elev, site, micro, temp) #remove julian column, which was causing problems of coercion later on
})
#rename
NC_min_on_up_soil_sampled <- NC_micros_on_sampled[[1]]
NC_min_on_up_ground_sampled <- NC_micros_on_sampled[[2]]
NC_min_on_up_canopy_sampled <- NC_micros_on_sampled[[3]]
NC_min_on_low_soil_sampled <- NC_micros_on_sampled[[4]]
NC_min_on_low_ground_sampled <- NC_micros_on_sampled[[5]]
NC_min_on_low_canopy_sampled <- NC_micros_on_sampled[[6]]
NC_max_on_up_soil_sampled <- NC_micros_on_sampled[[7]]
NC_max_on_up_ground_sampled <- NC_micros_on_sampled[[8]]
NC_max_on_up_canopy_sampled <- NC_micros_on_sampled[[9]]
NC_max_on_low_soil_sampled <- NC_micros_on_sampled[[10]]
NC_max_on_low_ground_sampled <- NC_micros_on_sampled[[11]]
NC_max_on_low_canopy_sampled <- NC_micros_on_sampled[[12]]

#Sample NC off micros
NC_micros_off_sampled <- lapply(NC_micros_off, function(x) {
  x %>%
    group_by(site) %>%
    sample_n(noff, replace = FALSE) %>%
    select(elev, site, micro, temp) #remove julian column, which was causing problems of coercion later on
})
#rename
NC_min_off_up_soil_sampled <- NC_micros_off_sampled[[1]]
NC_min_off_up_ground_sampled <- NC_micros_off_sampled[[2]]
NC_min_off_up_canopy_sampled <- NC_micros_off_sampled[[3]]
NC_min_off_low_soil_sampled <- NC_micros_off_sampled[[4]]
NC_min_off_low_ground_sampled <- NC_micros_off_sampled[[5]]
NC_min_off_low_canopy_sampled <- NC_micros_off_sampled[[6]]
NC_max_off_up_soil_sampled <- NC_micros_off_sampled[[7]]
NC_max_off_up_ground_sampled <- NC_micros_off_sampled[[8]]
NC_max_off_up_canopy_sampled <- NC_micros_off_sampled[[9]]
NC_max_off_low_soil_sampled <- NC_micros_off_sampled[[10]]
NC_max_off_low_ground_sampled <- NC_micros_off_sampled[[11]]
NC_max_off_low_canopy_sampled <- NC_micros_off_sampled[[12]]



######## Perform T-tests ########

#temp off vs trop
t.test(NC_max_on_low_canopy$temp, trop_max_low_canopy$temp)

NC_max_off_low_canopy

Aust_max_low_ground

length(Phili_max_low_ground$temp)

######## Perform KS tests ########

ks.test(NC_max_on_low_canopy$temp, trop_max_low_canopy$temp, alternative= "two.sided", exact = NULL)

ggplot(NC_max_on_low_canopy, aes(temp)) +
       geom_histogram(binwidth = 1)
