#Klinges and Scheffers 2018
#compare KDEs of temperate and tropical data
#overlap % calculation derived from https://stats.stackexchange.com/questions/97596/how-to-calculate-overlap-between-empirical-probability-densities?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa

#overlapping package https://www.rdocumentation.org/packages/overlapping/versions/1.5.0/topics/overlap
require(overlapping)
require(readr)
require(tidyr)
require(dplyr) 
#require(tidyverse)

##NOTE: line ~70 will cause an intentional bug if first time running code. Remove "if" statements


######## Load and prep data ###########
Mada <- read_csv("data/01_primary/tropical/Madagascar/Madagascar.csv")
Phili <- read_csv("data/01_primary/tropical/Philippines/Philippines.csv")
Aust_canopy <- read_csv("data/01_primary/tropical/Australia/Australia_canopy.csv")
Aust_soil <- read_csv("data/01_primary/tropical/Australia/Australia_soil.csv")
NC <- read_csv("data/01_primary/temperate/NC_mt_mitchell/NorthCarolina.csv")


#prep Mada data
Mada <- filter(Mada, micro != "bnf")

#prep Aust data
Aust_canopy <- select(Aust_canopy, "site_code", "date", "airrange", "site", 
                      "elevation", "micro", "min", "max")
Aust <- rbind(Aust_canopy, Aust_soil)
Aust <- Aust %>%
  rename(elev = elevation) %>%
  select(-site) %>%
  rename(site = site_code) %>%
  rename(julian = date)

#remove NA's
Mada <- na.omit(Mada)
Phili <- na.omit(Phili)
Aust <- na.omit(Aust)
NC <- na.omit(NC)

######## Reshape and standardize min and max data ############

#Datasets are of different shape, and need to be standardized
#Because Aust data has only min and max (no mean), we will convert all temp data to min and max
#We will also summarize to daily min, and daily max

#Mada
#daily min temps for each elevation, site and year
Mada_dailymin <- Mada %>%
  group_by(elev, site, year, micro, julian) %>%
  summarize(temp = min(Value))

#daily max temps for each elevation, site and year
Mada_dailymax <- Mada %>%
  group_by(elev, site, year, micro, julian) %>%
  summarize(temp = max(Value))

#Phili
#daily min temps for each elevation, site and year
Phili_dailymin <- Phili %>%
  group_by(elev, site, year, micro, julian) %>%
  summarize(temp = min(Temp))

#daily max temps for each elevation, site and year
Phili_dailymax <- Phili %>%
  group_by(elev, site, year, micro, julian) %>%
  summarize(temp = max(Temp))

#Phili micro levels were capitalized, must de-capitalize
if(!exists("Phili_dailymin_ground")) {
Phili_dailymin_ground <- Phili_dailymin %>%
  filter(micro == "Ground")
Phili_dailymin_ground$micro <- "ground"
}

if(!exists("Phili_dailymin_canopy")) {
Phili_dailymin_canopy <- Phili_dailymin %>%
  filter(micro == "Canopy")
Phili_dailymin_canopy$micro <- "canopy"
}

Phili_dailymin <- rbind(Phili_dailymin_ground, Phili_dailymin_canopy)

if(!exists("Phili_dailymax_ground")) {
Phili_dailymax_ground <- Phili_dailymax %>%
  filter(micro == "Ground")
Phili_dailymax_ground$micro <- "ground"
}

if(!exists("Phili_dailymax_canopy")) {
Phili_dailymax_canopy <- Phili_dailymax %>%
  filter(micro == "Canopy")
Phili_dailymax_canopy$micro <- "canopy"
}

Phili_dailymax <- rbind(Phili_dailymax_ground, Phili_dailymax_canopy)

#Aust
#for Aust, we only have min and max data. So the data won't actually change in size...
#essentially just removing some columns and renaming others to standardize sets
#daily min temps for each elevation, site and year
Aust_dailymin <- Aust %>%
  group_by(elev, site, micro, julian) %>%
  summarize(temp = min(min))

#daily max temps for each elevation, site and year
Aust_dailymax <- Aust %>%
  group_by(elev, site, micro, julian) %>%
  summarize(temp = max(max))

#NC
#daily min temps for each elevation, site and year
NC_dailymin <- NC %>%
  group_by(elev, site, year, micro, julian) %>%
  summarize(temp = min(Value))

#daily max temps for each elevation, site and year
NC_dailymax <- NC %>%
  group_by(elev, site, year, micro, julian) %>%
  summarize(temp = max(Value))

######## Separate elevations, microhabitats and seasons ########
  ######## Separate for Mada ##########
#Seperate Elevations
Mada_min_up <- filter(Mada_dailymin, elev == 1150)
Mada_min_low <- filter(Mada_dailymin, elev == 550)
Mada_max_up <- filter(Mada_dailymax, elev == 1150)
Mada_max_low <- filter(Mada_dailymax, elev == 550)

#seperate microhabitats
Mada_min_up_soil <- filter(Mada_min_up, micro == "soil")
Mada_min_up_ground <- filter(Mada_min_up, micro == "ground")
Mada_min_up_canopy <- filter(Mada_min_up, micro == "canopy")
Mada_min_low_soil <- filter(Mada_min_low, micro == "soil")
Mada_min_low_ground <- filter(Mada_min_low, micro == "ground")
Mada_min_low_canopy <- filter(Mada_min_low, micro == "canopy")
Mada_max_up_soil <- filter(Mada_max_up, micro == "soil")
Mada_max_up_ground <- filter(Mada_max_up, micro == "ground")
Mada_max_up_canopy <- filter(Mada_max_up, micro == "canopy")
Mada_max_low_soil <- filter(Mada_max_low, micro == "soil")
Mada_max_low_ground <- filter(Mada_max_low, micro == "ground")
Mada_max_low_canopy <- filter(Mada_max_low, micro == "canopy")

#make list of tibbles
Mada_micros <- list(Mada_min_up_soil, Mada_min_up_ground, Mada_min_up_canopy,
                    Mada_min_low_soil, Mada_min_low_ground, Mada_min_low_canopy,
                    Mada_max_up_soil, Mada_max_up_ground, Mada_max_up_canopy, 
                    Mada_max_low_soil, Mada_max_low_ground, Mada_max_low_canopy)

  ######## Separate for Phili #######
#Seperate Elevations
Phili_min_up <- filter(Phili_dailymin, elev == 1900)
Phili_min_low <- filter(Phili_dailymin, elev == 900)
Phili_max_up <- filter(Phili_dailymax, elev == 1900)
Phili_max_low <- filter(Phili_dailymax, elev == 900)



#seperate microhabitats
#no soil data
Phili_min_up_ground <- filter(Phili_min_up, micro == "ground")
Phili_min_up_canopy <- filter(Phili_min_up, micro == "canopy")
Phili_min_low_ground <- filter(Phili_min_low, micro == "ground")
Phili_min_low_canopy <- filter(Phili_min_low, micro == "canopy")
Phili_max_up_ground <- filter(Phili_max_up, micro == "ground")
Phili_max_up_canopy <- filter(Phili_max_up, micro == "canopy")
Phili_max_low_ground <- filter(Phili_max_low, micro == "ground")
Phili_max_low_canopy <- filter(Phili_max_low, micro == "canopy")

#make list of tibbles
Phili_micros <- list(Phili_min_up_ground, Phili_min_up_canopy,
                     Phili_min_low_ground, Phili_min_low_canopy,
                     Phili_max_up_ground, Phili_max_up_canopy, 
                     Phili_max_low_ground, Phili_max_low_canopy)

  ######## Separate for Aust ######
#Seperate Elevations
Aust_min_up <- filter(Aust_dailymin, elev == 10)
Aust_min_low <- filter(Aust_dailymin, elev == 1)
Aust_max_up <- filter(Aust_dailymax, elev == 10)
Aust_max_low <- filter(Aust_dailymax, elev == 1)

#seperate microhabitats
Aust_min_up_soil <- filter(Aust_min_up, micro == "soil")
Aust_min_up_ground <- filter(Aust_min_up, micro == "ground")
Aust_min_up_canopy <- filter(Aust_min_up, micro == "canopy")
Aust_min_low_soil <- filter(Aust_min_low, micro == "soil")
Aust_min_low_ground <- filter(Aust_min_low, micro == "ground")
Aust_min_low_canopy <- filter(Aust_min_low, micro == "canopy")
Aust_max_up_soil <- filter(Aust_max_up, micro == "soil")
Aust_max_up_ground <- filter(Aust_max_up, micro == "ground")
Aust_max_up_canopy <- filter(Aust_max_up, micro == "canopy")
Aust_max_low_soil <- filter(Aust_max_low, micro == "soil")
Aust_max_low_ground <- filter(Aust_max_low, micro == "ground")
Aust_max_low_canopy <- filter(Aust_max_low, micro == "canopy")

Aust_micros <- list(Aust_min_up_soil, Aust_min_up_ground, Aust_min_up_canopy,
                    Aust_min_low_soil, Aust_min_low_ground, Aust_min_low_canopy,
                    Aust_max_up_soil, Aust_max_up_ground, Aust_max_up_canopy, 
                    Aust_max_low_soil, Aust_max_low_ground, Aust_max_low_canopy)

  ######## Separate for NC ######
#Separate Seasons
NC_min_on <- filter(NC_dailymin, julian > 150 & julian < 281)
NC_min_off <- filter(NC_dailymin, julian < 151 | julian > 280)
NC_max_on <- filter(NC_dailymax, julian > 150 & julian < 281)
NC_max_off <- filter(NC_dailymax, julian < 151 | julian > 280)

#Seperate Elevations
NC_min_on_up <- filter(NC_min_on, elev == 1700)
NC_min_on_low <- filter(NC_min_on, elev == 500)
NC_min_off_up <- filter(NC_min_off, elev == 1700)
NC_min_off_low <- filter(NC_min_off, elev == 500)
NC_max_on_up <- filter(NC_max_on, elev == 1700)
NC_max_on_low <- filter(NC_max_on, elev == 500)
NC_max_off_up <- filter(NC_max_off, elev == 1700)
NC_max_off_low <- filter(NC_max_off, elev == 500)

#Separate Microhabitats
NC_min_on_up_soil <- filter(NC_min_on_up, micro == "soil")
NC_min_on_up_ground <- filter(NC_min_on_up, micro == "ground")
NC_min_on_up_canopy <- filter(NC_min_on_up, micro == "canopy")

NC_min_on_low_soil <- filter(NC_min_on_low, micro == "soil")
NC_min_on_low_ground <- filter(NC_min_on_low, micro == "ground")
NC_min_on_low_canopy <- filter(NC_min_on_low, micro == "canopy")

NC_min_off_up_soil <- filter(NC_min_off_up, micro == "soil")
NC_min_off_up_ground <- filter(NC_min_off_up, micro == "ground")
NC_min_off_up_canopy <- filter(NC_min_off_up, micro == "canopy")

NC_min_off_low_soil <- filter(NC_min_off_low, micro == "soil")
NC_min_off_low_ground <- filter(NC_min_off_low, micro == "ground")
NC_min_off_low_canopy <- filter(NC_min_off_low, micro == "canopy")

NC_max_on_up_soil <- filter(NC_max_on_up, micro == "soil")
NC_max_on_up_ground <- filter(NC_max_on_up, micro == "ground")
NC_max_on_up_canopy <- filter(NC_max_on_up, micro == "canopy")

NC_max_on_low_soil <- filter(NC_max_on_low, micro == "soil")
NC_max_on_low_ground <- filter(NC_max_on_low, micro == "ground")
NC_max_on_low_canopy <- filter(NC_max_on_low, micro == "canopy")

NC_max_off_up_soil <- filter(NC_max_off_up, micro == "soil")
NC_max_off_up_ground <- filter(NC_max_off_up, micro == "ground")
NC_max_off_up_canopy <- filter(NC_max_off_up, micro == "canopy")

NC_max_off_low_soil <- filter(NC_max_off_low, micro == "soil")
NC_max_off_low_ground <- filter(NC_max_off_low, micro == "ground")
NC_max_off_low_canopy <- filter(NC_max_off_low, micro == "canopy")



#make list of tibbles (for NC, one each of on and off)
NC_micros_on <- list(NC_min_on_up_soil, NC_min_on_up_ground, NC_min_on_up_canopy,
                     NC_min_on_low_soil, NC_min_on_low_ground, NC_min_on_low_canopy,
                     NC_max_on_up_soil, NC_max_on_up_ground, NC_max_on_up_canopy,
                     NC_max_on_low_soil, NC_max_on_low_ground, NC_max_on_low_canopy)

NC_micros_off <- list(NC_min_off_up_soil, NC_min_off_up_ground, NC_min_off_up_canopy,
                      NC_min_off_low_soil, NC_min_off_low_ground, NC_min_off_low_canopy,
                      NC_max_off_up_soil, NC_max_off_up_ground, NC_max_off_up_canopy,
                      NC_max_off_low_soil, NC_max_off_low_ground, NC_max_off_low_canopy)

######## Sample randomly #########
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
    sample_frac(p, replace = FALSE) %>%
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
    sample_frac(a, replace = FALSE) %>%
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
    sample_frac(non, replace = FALSE) %>%
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
    sample_frac(noff, replace = FALSE) %>%
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

######## Combine min and max data ############

#Tropical Soil low
trop_soil_low <- rbind(Mada_min_low_soil_sampled, Mada_max_low_soil_sampled,
                   Aust_min_low_soil_sampled, Aust_max_low_soil_sampled)

#Tropical Soil up
trop_soil_up <- rbind(Mada_min_up_soil_sampled, Mada_max_up_soil_sampled,
                   Aust_min_up_soil_sampled, Aust_max_up_soil_sampled)

#Tropical Ground low
trop_ground_low <- rbind(Mada_min_low_ground_sampled, Mada_max_low_ground_sampled,
                     Phili_min_low_ground_sampled, Phili_max_low_ground_sampled,
                     Aust_min_low_ground_sampled, Aust_max_low_ground_sampled)

#Tropical Ground up
trop_ground_up <- rbind(Mada_min_up_ground_sampled, Mada_max_up_ground_sampled,
                     Phili_min_up_ground_sampled, Phili_max_up_ground_sampled,
                     Aust_min_up_ground_sampled, Aust_max_up_ground_sampled)

#Tropical Canopy low
trop_canopy_low <- rbind(Mada_min_low_canopy_sampled, Mada_max_low_canopy_sampled,
                     Phili_min_low_canopy_sampled, Phili_max_low_canopy_sampled,
                     Aust_min_low_canopy_sampled, Aust_max_low_canopy_sampled)

#Tropical Canopy up
trop_canopy_up <- rbind(Mada_min_up_canopy_sampled, Mada_max_up_canopy_sampled,
                     Phili_min_up_canopy_sampled, Phili_max_up_canopy_sampled,
                     Aust_min_up_canopy_sampled, Aust_max_up_canopy_sampled)


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
NC_off_soil_low_density <- density(NC_off_soil_low$temp) # returns the density data
plot(NC_off_soil_low_density) # plots the results

#temperate leaf-off soil up
NC_off_soil_up_density <- density(NC_off_soil_up$temp) # returns the density data
plot(NC_off_soil_up_density) # plots the results

#temperate leaf-off ground low
NC_off_ground_low_density <- density(NC_off_ground_low$temp) # returns the density data
plot(NC_off_ground_low_density) # plots the results

#temperate leaf-off ground up
NC_off_ground_up_density <- density(NC_off_ground_up$temp) # returns the density data
plot(NC_off_ground_up_density) # plots the results

#temperate leaf-off canopy low
NC_off_canopy_low_density <- density(NC_off_canopy_low$temp) # returns the density data
plot(NC_off_canopy_low_density) # plots the results

#temperate leaf-off canopy up
NC_off_canopy_up_density <- density(NC_off_canopy_up$temp) # returns the density data
plot(NC_off_canopy_up_density) # plots the results

#leaf on
#temperate leaf-on soil low
NC_on_soil_low_density <- density(NC_on_soil_low$temp) # returns the density data
plot(NC_on_soil_low_density) # plots the results

#temperate leaf-on soil up
NC_on_soil_up_density <- density(NC_on_soil_up$temp) # returns the density data
plot(NC_on_soil_up_density) # plots the results

#temperate leaf-on ground low
NC_on_ground_low_density <- density(NC_on_ground_low$temp) # returns the density data
plot(NC_on_ground_low_density) # plots the results

#temperate leaf-on ground up
NC_on_ground_up_density <- density(NC_on_ground_up$temp) # returns the density data
plot(NC_on_ground_up_density) # plots the results

#temperate leaf-on canopy low
NC_on_canopy_low_density <- density(NC_on_canopy_low$temp) # returns the density data
plot(NC_on_canopy_low_density) # plots the results

#temperate leaf-on canopy up
NC_on_canopy_up_density <- density(NC_on_canopy_up$temp) # returns the density data
plot(NC_on_canopy_up_density) # plots the results

######## Calculate overlap area ############

  ######## leaf off vs leaf on #######
#Overlap of temperate soil low, leaf off vs leaf on
overlap(list(NC_off_soil_low$temp, NC_on_soil_low$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of temperate soil up, leaf off vs leaf on
overlap(list(NC_off_soil_up$temp, NC_on_soil_up$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of temperate ground low, leaf off vs leaf on
overlap(list(NC_off_ground_low$temp, NC_on_ground_low$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of temperate ground up, leaf off vs leaf on
overlap(list(NC_off_ground_up$temp, NC_on_ground_up$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of temperate canopy low, leaf off vs leaf on
overlap(list(NC_off_canopy_low$temp, NC_on_canopy_low$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of temperate canopoy up, leaf off vs leaf on
overlap(list(NC_off_canopy_up$temp, NC_on_canopy_up$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

  ######## leaf on vs trop #######

#Overlap of leaf on soil low vs trop soil low
overlap(list(NC_on_soil_low$temp, trop_soil_low$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of leaf on soil up vs trop soil up
overlap(list(NC_on_soil_up$temp, trop_soil_up$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of leaf on ground low vs trop ground low
overlap(list(NC_on_ground_low$temp, trop_ground_low$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of leaf on ground up vs trop ground up
overlap(list(NC_on_ground_up$temp, trop_ground_up$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of leaf on canopy low vs trop canopy low
overlap(list(NC_on_canopy_low$temp, trop_canopy_low$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of leaf on canopy up vs trop canopy up
overlap(list(NC_on_canopy_up$temp, trop_canopy_up$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

  ######## leaf off vs trop #######

#Overlap of leaf off soil low vs trop soil low
overlap(list(NC_off_soil_low$temp, trop_soil_low$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of leaf off soil up vs trop soil up
overlap(list(NC_off_soil_up$temp, trop_soil_up$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of leaf off ground low vs trop ground low
overlap(list(NC_off_ground_low$temp, trop_ground_low$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of leaf off ground up vs trop ground up
overlap(list(NC_off_ground_up$temp, trop_ground_up$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of leaf off canopy low vs trop canopy low
overlap(list(NC_off_canopy_low$temp, trop_canopy_low$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)

#Overlap of leaf off canopy up vs trop canopy up
overlap(list(NC_off_canopy_up$temp, trop_canopy_up$temp), nbins = 1000, plot = TRUE, partial.plot = FALSE)






