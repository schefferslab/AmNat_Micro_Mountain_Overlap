#Klinges and Scheffers 2018
#This script depends upon objects created in trop_main
#compare KDEs of temperate and tropical data
#overlap % calculation derived from https://stats.stackexchange.com/questions/97596/how-to-calculate-overlap-between-empirical-probability-densities?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa


######## Reshape and standardize data ############

#Datasets are of different shape, and need to be standardized
#Because Aust data has only mean and max (no mean), we will convert all temp data to mean and max
#We will also summarize to daily mean, and daily max

#Mada
#daily mean temps for each elevation, site and year
Mada_mean <- Mada %>%
  group_by(elev, site, year, micro, julian) %>%
  summarize(temp = mean(Value))

#Phili
#daily mean temps for each elevation, site and year
Phili_mean <- Phili %>%
  group_by(elev, site, year, micro, julian) %>%
  summarize(temp = mean(Temp))

#Phili micro levels were capitalized, must de-capitalize
if(!exists("Phili_mean_ground")) {
  Phili_mean_ground <- Phili_mean %>%
    filter(micro == "Ground")
  Phili_mean_ground$micro <- "ground"
}

if(!exists("Phili_mean_canopy")) {
  Phili_mean_canopy <- Phili_mean %>%
    filter(micro == "Canopy")
  Phili_mean_canopy$micro <- "canopy"
}

Phili_mean <- rbind(Phili_mean_ground, Phili_mean_canopy)

#Aust
#for Aust, we only have mean and max data. So we have to average them arithmetically
#daily mean temps for each elevation, site and year
Aust_mean <- Aust %>%
  group_by(elev, site, micro, julian) %>%
  summarize(temp = (min + max)/2)

#NC
#daily mean temps for each elevation, site and year
NC_mean <- NC %>%
  group_by(elev, site, year, micro, julian) %>%
  summarize(temp = mean(Value))

#daily max temps for each elevation, site and year
NC_mean <- NC %>%
  group_by(elev, site, year, micro, julian) %>%
  summarize(temp = mean(Value))

######## Separate elevations, microhabitats and seasons ########
######## Separate for Mada ##########
#Seperate Elevations
Mada_mean_up <- filter(Mada_mean, elev == 1150)
Mada_mean_low <- filter(Mada_mean, elev == 550)

#seperate microhabitats
Mada_mean_up_soil <- filter(Mada_mean_up, micro == "soil")
Mada_mean_up_ground <- filter(Mada_mean_up, micro == "ground")
Mada_mean_up_canopy <- filter(Mada_mean_up, micro == "canopy")
Mada_mean_low_soil <- filter(Mada_mean_low, micro == "soil")
Mada_mean_low_ground <- filter(Mada_mean_low, micro == "ground")
Mada_mean_low_canopy <- filter(Mada_mean_low, micro == "canopy")


#make list of tibbles
Mada_micros <- list(Mada_mean_up_soil, Mada_mean_up_ground, Mada_mean_up_canopy,
                    Mada_mean_low_soil, Mada_mean_low_ground, Mada_mean_low_canopy)

######## Separate for Phili #######
#Seperate Elevations
Phili_mean_up <- filter(Phili_mean, elev == 1900)
Phili_mean_low <- filter(Phili_mean, elev == 900)

#seperate microhabitats
#no soil data
Phili_mean_up_ground <- filter(Phili_mean_up, micro == "ground")
Phili_mean_up_canopy <- filter(Phili_mean_up, micro == "canopy")
Phili_mean_low_ground <- filter(Phili_mean_low, micro == "ground")
Phili_mean_low_canopy <- filter(Phili_mean_low, micro == "canopy")

#make list of tibbles
Phili_micros <- list(Phili_mean_up_ground, Phili_mean_up_canopy,
                     Phili_mean_low_ground, Phili_mean_low_canopy)

######## Separate for Aust ######
#Seperate Elevations
Aust_mean_up <- filter(Aust_mean, elev == 10)
Aust_mean_low <- filter(Aust_mean, elev == 1)

#seperate microhabitats
Aust_mean_up_soil <- filter(Aust_mean_up, micro == "soil")
Aust_mean_up_ground <- filter(Aust_mean_up, micro == "ground")
Aust_mean_up_canopy <- filter(Aust_mean_up, micro == "canopy")
Aust_mean_low_soil <- filter(Aust_mean_low, micro == "soil")
Aust_mean_low_ground <- filter(Aust_mean_low, micro == "ground")
Aust_mean_low_canopy <- filter(Aust_mean_low, micro == "canopy")

Aust_micros <- list(Aust_mean_up_soil, Aust_mean_up_ground, Aust_mean_up_canopy,
                    Aust_mean_low_soil, Aust_mean_low_ground, Aust_mean_low_canopy)

######## Separate for NC ######
#Separate Seasons
NC_mean_on <- filter(NC_mean, julian > 150 & julian < 281)
NC_mean_off <- filter(NC_mean, julian < 151 | julian > 280)

#Seperate Elevations
NC_mean_on_up <- filter(NC_mean_on, elev == 1700)
NC_mean_on_low <- filter(NC_mean_on, elev == 500)
NC_mean_off_up <- filter(NC_mean_off, elev == 1700)
NC_mean_off_low <- filter(NC_mean_off, elev == 500)

#Separate Microhabitats
NC_mean_on_up_soil <- filter(NC_mean_on_up, micro == "soil")
NC_mean_on_up_ground <- filter(NC_mean_on_up, micro == "ground")
NC_mean_on_up_canopy <- filter(NC_mean_on_up, micro == "canopy")

NC_mean_on_low_soil <- filter(NC_mean_on_low, micro == "soil")
NC_mean_on_low_ground <- filter(NC_mean_on_low, micro == "ground")
NC_mean_on_low_canopy <- filter(NC_mean_on_low, micro == "canopy")

NC_mean_off_up_soil <- filter(NC_mean_off_up, micro == "soil")
NC_mean_off_up_ground <- filter(NC_mean_off_up, micro == "ground")
NC_mean_off_up_canopy <- filter(NC_mean_off_up, micro == "canopy")

NC_mean_off_low_soil <- filter(NC_mean_off_low, micro == "soil")
NC_mean_off_low_ground <- filter(NC_mean_off_low, micro == "ground")
NC_mean_off_low_canopy <- filter(NC_mean_off_low, micro == "canopy")

#make list of tibbles (for NC, one each of on and off)
NC_micros_on <- list(NC_mean_on_up_soil, NC_mean_on_up_ground, NC_mean_on_up_canopy,
                     NC_mean_on_low_soil, NC_mean_on_low_ground, NC_mean_on_low_canopy)

NC_micros_off <- list(NC_mean_off_up_soil, NC_mean_off_up_ground, NC_mean_off_up_canopy,
                      NC_mean_off_low_soil, NC_mean_off_low_ground, NC_mean_off_low_canopy)

######## Sample randomly (in this case, sample 100%) #########
#take a random sample from each microhabitat
#the total sample from tropical regions should equal the sample from NC

#set sample sizes
m <- 1
a <- 1
p <- 1
non <- 1
noff <- 1

#Sample Mada micros
Mada_micros_sampled <- lapply(Mada_micros, function(x) {
  x %>%
    group_by(site) %>%
    sample_frac(m, replace = FALSE) %>%
    select(elev, site, micro, temp) #remove julian column, which was causing problems of coercion later on
})
#rename
Mada_mean_up_soil_sampled <- Mada_micros_sampled[[1]]
Mada_mean_up_ground_sampled <- Mada_micros_sampled[[2]]
Mada_mean_up_canopy_sampled <- Mada_micros_sampled[[3]]
Mada_mean_low_soil_sampled <- Mada_micros_sampled[[4]]
Mada_mean_low_ground_sampled <- Mada_micros_sampled[[5]]
Mada_mean_low_canopy_sampled <- Mada_micros_sampled[[6]]

#Sample Phili micros
Phili_micros_sampled <- lapply(Phili_micros, function(x) {
  x %>%
    group_by(site) %>%
    sample_frac(p, replace = FALSE) %>%
    select(elev, site, micro, temp) #remove julian column, which was causing problems of coercion later on
})
#rename
Phili_mean_up_ground_sampled <- Phili_micros_sampled[[1]]
Phili_mean_up_canopy_sampled <- Phili_micros_sampled[[2]]
Phili_mean_low_ground_sampled <- Phili_micros_sampled[[3]]
Phili_mean_low_canopy_sampled <- Phili_micros_sampled[[4]]

#Sample Aust micros
Aust_micros_sampled <- lapply(Aust_micros, function(x) {
  x %>%
    group_by(site) %>%
    sample_frac(a, replace = FALSE) %>%
    select(elev, site, micro, temp) #remove julian column, which was causing problems of coercion later on
})
#rename
Aust_mean_up_soil_sampled <- Aust_micros_sampled[[1]]
Aust_mean_up_ground_sampled <- Aust_micros_sampled[[2]]
Aust_mean_up_canopy_sampled <- Aust_micros_sampled[[3]]
Aust_mean_low_soil_sampled <- Aust_micros_sampled[[4]]
Aust_mean_low_ground_sampled <- Aust_micros_sampled[[5]]
Aust_mean_low_canopy_sampled <- Aust_micros_sampled[[6]]

#Sample NC on micros
NC_micros_on_sampled <- lapply(NC_micros_on, function(x) {
  x %>%
    group_by(site) %>%
    sample_frac(non, replace = FALSE) %>%
    select(elev, site, micro, temp) #remove julian column, which was causing problems of coercion later on
})
#rename
NC_mean_on_up_soil_sampled <- NC_micros_on_sampled[[1]]
NC_mean_on_up_ground_sampled <- NC_micros_on_sampled[[2]]
NC_mean_on_up_canopy_sampled <- NC_micros_on_sampled[[3]]
NC_mean_on_low_soil_sampled <- NC_micros_on_sampled[[4]]
NC_mean_on_low_ground_sampled <- NC_micros_on_sampled[[5]]
NC_mean_on_low_canopy_sampled <- NC_micros_on_sampled[[6]]

#Sample NC off micros
NC_micros_off_sampled <- lapply(NC_micros_off, function(x) {
  x %>%
    group_by(site) %>%
    sample_frac(noff, replace = FALSE) %>%
    select(elev, site, micro, temp) #remove julian column, which was causing problems of coercion later on
})
#rename
NC_mean_off_up_soil_sampled <- NC_micros_off_sampled[[1]]
NC_mean_off_up_ground_sampled <- NC_micros_off_sampled[[2]]
NC_mean_off_up_canopy_sampled <- NC_micros_off_sampled[[3]]
NC_mean_off_low_soil_sampled <- NC_micros_off_sampled[[4]]
NC_mean_off_low_ground_sampled <- NC_micros_off_sampled[[5]]
NC_mean_off_low_canopy_sampled <- NC_micros_off_sampled[[6]]

######## Combine tropical regions ############

#Tropical Soil low
trop_soil_low <- rbind(Mada_mean_low_soil_sampled, Aust_mean_low_soil_sampled)

#Tropical Soil up
trop_soil_up <- rbind(Mada_mean_up_soil_sampled, Aust_mean_up_soil_sampled)

#Tropical Ground low
trop_ground_low <- rbind(Mada_mean_low_ground_sampled, Phili_mean_low_ground_sampled, 
                         Aust_mean_low_ground_sampled)

#Tropical Ground up
trop_ground_up <- rbind(Mada_mean_up_ground_sampled, Phili_mean_up_ground_sampled, 
                        Aust_mean_up_ground_sampled)

#Tropical Canopy low
trop_canopy_low <- rbind(Mada_mean_low_canopy_sampled, Phili_mean_low_canopy_sampled, 
                         Aust_mean_low_canopy_sampled)

#Tropical Canopy up
trop_canopy_up <- rbind(Mada_mean_up_canopy_sampled, Phili_mean_up_canopy_sampled, 
                        Aust_mean_up_canopy_sampled)


######## Generate kernal density plots #############
######## Tropical ########
#densities for tropical, all 3 regions combined
#tropical soil low
trop_soil_low_density <- density(trop_soil_low$temp)
plot(trop_soil_low_density)

#tropical soil up
trop_soil_up_density <- density(trop_soil_up$temp)
plot(trop_soil_up_density)

#tropical ground low
trop_ground_low_density <- density(trop_ground_low$temp)
plot(trop_ground_low_density)

#tropical ground up
trop_ground_up_density <- density(trop_ground_up$temp)
plot(trop_ground_up_density)

#tropical canopy low
trop_canopy_low_density <- density(trop_canopy_low$temp)
plot(trop_canopy_low_density)

#tropical canopy up
trop_canopy_up_density <- density(trop_canopy_up$temp)
plot(trop_canopy_up_density)

######## Temperate #########

#leaf off
#temperate leaf-off soil low
NC_mean_off_low_soil_sampled_density <- density(NC_mean_off_low_soil_sampled$temp) # returns the density data
plot(NC_mean_off_low_soil_sampled_density) # plots the results


#temperate leaf-off soil up
NC_mean_off_up_soil_sampled_density <- density(NC_mean_off_up_soil_sampled$temp) # returns the density data
plot(NC_mean_off_up_soil_sampled_density) # plots the results

#temperate leaf-off ground low
NC_mean_off_low_ground_sampled_density <- density(NC_mean_off_low_ground_sampled$temp) # returns the density data
plot(NC_mean_off_low_ground_sampled_density) # plots the results

#temperate leaf-off ground up
NC_mean_off_up_ground_sampled_density <- density(NC_mean_off_up_ground_sampled$temp) # returns the density data
plot(NC_mean_off_up_ground_sampled_density) # plots the results

#temperate leaf-off canopy low
NC_mean_off_low_canopy_sampled_density <- density(NC_mean_off_low_canopy_sampled$temp) # returns the density data
plot(NC_mean_off_low_canopy_sampled_density) # plots the results

#temperate leaf-off canopy up
NC_mean_off_up_canopy_sampled_density <- density(NC_mean_off_up_canopy_sampled$temp) # returns the density data
plot(NC_mean_off_up_canopy_sampled_density) # plots the results

#leaf on
#temperate leaf-on soil low
NC_mean_on_low_soil_sampled_density <- density(NC_mean_on_low_soil_sampled$temp) # returns the density data
plot(NC_mean_on_low_soil_sampled_density) # plots the results

#temperate leaf-on soil up
NC_mean_on_up_soil_sampled_density <- density(NC_mean_on_up_soil_sampled$temp) # returns the density data
plot(NC_mean_on_up_soil_sampled_density) # plots the results

#temperate leaf-on ground low
NC_mean_on_low_ground_sampled_density <- density(NC_mean_on_low_ground_sampled$temp) # returns the density data
plot(NC_mean_on_low_ground_sampled_density) # plots the results

#temperate leaf-on ground up
NC_mean_on_up_ground_sampled_density <- density(NC_mean_on_up_ground_sampled$temp) # returns the density data
plot(NC_mean_on_up_ground_sampled_density) # plots the results

#temperate leaf-on canopy low
NC_mean_on_low_canopy_sampled_density <- density(NC_mean_on_low_canopy_sampled$temp) # returns the density data
plot(NC_mean_on_low_canopy_sampled_density) # plots the results

#temperate leaf-on canopy up
NC_mean_on_up_canopy_sampled_density <- density(NC_mean_on_up_canopy_sampled$temp) # returns the density data
plot(NC_mean_on_up_canopy_sampled_density) # plots the results

######## Calculate overlap area ############
######## leaf on vs trop #######

#Overlap of leaf on soil low vs trop soil low
overlap(list(NC_mean_on_low_soil_sampled$temp, trop_soil_low$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of leaf on soil up vs trop soil up
overlap(list(NC_mean_on_up_soil_sampled$temp, trop_soil_up$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of leaf on ground low vs trop ground low
overlap(list(NC_mean_on_low_ground_sampled$temp, trop_ground_low$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of leaf on ground up vs trop ground up
overlap(list(NC_mean_on_up_ground_sampled$temp, trop_ground_up$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of leaf on canopy low vs trop canopy low
overlap(list(NC_mean_on_low_canopy_sampled$temp, trop_canopy_low$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of leaf on canopy up vs trop canopy up
overlap(list(NC_mean_on_up_canopy_sampled$temp, trop_canopy_up$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)


######## leaf off vs leaf on #######
#Overlap of temperate soil low, leaf off vs leaf on
overlap(list(NC_mean_off_low_soil_sampled$temp, NC_mean_on_low_soil_sampled$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of temperate soil up, leaf off vs leaf on
overlap(list(NC_mean_off_up_soil_sampled$temp, NC_mean_on_up_soil_sampled$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of temperate ground low, leaf off vs leaf on
overlap(list(NC_mean_off_low_ground_sampled$temp, NC_mean_on_low_ground_sampled$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of temperate ground up, leaf off vs leaf on
overlap(list(NC_mean_off_up_ground_sampled$temp, NC_mean_on_up_ground_sampled$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of temperate canopy low, leaf off vs leaf on
overlap(list(NC_mean_off_low_canopy_sampled$temp, NC_mean_on_low_canopy_sampled$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of temperate canopy up, leaf off vs leaf on
overlap(list(NC_mean_off_up_canopy_sampled$temp, NC_mean_on_up_canopy_sampled$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

######## leaf off vs trop #######

#Overlap of leaf off soil low vs trop soil low
overlap(list(NC_mean_off_low_soil_sampled$temp, trop_soil_low$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of leaf off soil up vs trop soil up
overlap(list(NC_mean_off_up_soil_sampled$temp, trop_soil_up$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of leaf off ground low vs trop ground low
overlap(list(NC_mean_off_low_ground_sampled$temp, trop_ground_low$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of leaf off ground up vs trop ground up
overlap(list(NC_mean_off_up_ground_sampled$temp, trop_ground_up$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of leaf off canopy low vs trop canopy low
overlap(list(NC_mean_off_low_canopy_sampled$temp, trop_canopy_low$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of leaf off canopy up vs trop canopy up
overlap(list(NC_mean_off_up_canopy_sampled$temp, trop_canopy_up$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)






