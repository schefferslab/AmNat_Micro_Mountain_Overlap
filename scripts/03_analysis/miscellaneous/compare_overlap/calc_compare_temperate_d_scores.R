#David Klinges
#this script calculates d scores, used to quantify overlap
#d scores are calculated using a method developed by Janzen (1967)

#This method was used in Scheffers and Williams 2018
#d-scores compare thermal distribution of microhabitats at high elevation vs low elevation
#only conducted for temperate data here
#dependant on objects created in "North Carolina Overlap Script_DHK edits"

#Steps:
#Prep data: we need daily min and daily max for each elevation and microhabitat
#Calculate d score using Cohen's method
#Compare summary stats of d scores for leafoff and leafon
#Compare d scores for leafoff and leafon using Kruskl-Wallis


####### Calc daily min and max, seperate object #########

  #leaf off
  soil_500_leafoff_min <- dailymin(soil_500_leafoff)
  ground_500_leafoff_min <- dailymin(ground_500_leafoff)
  can_500_leafoff_min <- dailymin(can_500_leafoff)
  soil_1700_leafoff_min <- dailymin(soil_1700_leafoff)
  ground_1700_leafoff_min <- dailymin(ground_1700_leafoff)
  can_1700_leafoff_min <- dailymin(can_1700_leafoff)
  soil_500_leafoff_max <- dailymax(soil_500_leafoff)
  ground_500_leafoff_max <- dailymax(ground_500_leafoff)
  can_500_leafoff_max <- dailymax(can_500_leafoff)
  soil_1700_leafoff_max <- dailymax(soil_1700_leafoff)
  ground_1700_leafoff_max <- dailymax(ground_1700_leafoff)
  can_1700_leafoff_max <- dailymax(can_1700_leafoff)
  
  #leaf on
  soil_500_leafon_min <- dailymin(soil_500_leafon)
  ground_500_leafon_min <- dailymin(ground_500_leafon)
  can_500_leafon_min <- dailymin(can_500_leafon)
  soil_1700_leafon_min <- dailymin(soil_1700_leafon)
  ground_1700_leafon_min <- dailymin(ground_1700_leafon)
  can_1700_leafon_min <- dailymin(can_1700_leafon)
  soil_500_leafon_max <- dailymax(soil_500_leafon)
  ground_500_leafon_max <- dailymax(ground_500_leafon)
  can_500_leafon_max <- dailymax(can_500_leafon)
  soil_1700_leafon_max <- dailymax(soil_1700_leafon)
  ground_1700_leafon_max <- dailymax(ground_1700_leafon)
  can_1700_leafon_max <- dailymax(can_1700_leafon)
  
####### Calc d scores######################### 

#function to calculate d scores
#inputs are 4 data frames: daily min temp of uplands, daily max temp of uplands, 
#and the same two for lowlands
calc_d_score <- function(upmin, upmax, lowmin, lowmax) {
 out <- (upmax - lowmin)/(sqrt((upmax - upmin)*(lowmax - lowmin)))
}

#calc d scores for leafoff data
#soil
soil_leafoff_d_score <- select(soil_500_leafoff_min, julian)
soil_leafoff_d_score$dscore <- calc_d_score(soil_1700_leafoff_min$temp, soil_1700_leafoff_max$temp, 
                             soil_500_leafoff_min$temp, soil_500_leafoff_max$temp)

#ground
ground_leafoff_d_score <- select(ground_500_leafoff_min, julian)
ground_leafoff_d_score$dscore <- calc_d_score(ground_1700_leafoff_min$temp, ground_1700_leafoff_max$temp, 
                                    ground_500_leafoff_min$temp, ground_500_leafoff_max$temp)

#can
can_leafoff_d_score <- select(can_500_leafoff_min, julian)
can_leafoff_d_score$dscore <- calc_d_score(can_1700_leafoff_min$temp, can_1700_leafoff_max$temp, 
                                      can_500_leafoff_min$temp, can_500_leafoff_max$temp)


#calc d scores for leafon data
#soil
soil_leafon_d_score <- select(soil_500_leafon_min, julian)
soil_leafon_d_score$dscore <- calc_d_score(soil_1700_leafon_min$temp, soil_1700_leafon_max$temp, 
                                    soil_500_leafon_min$temp, soil_500_leafon_max$temp)

#ground
ground_leafon_d_score <- select(ground_500_leafon_min, julian)
ground_leafon_d_score$dscore <- calc_d_score(ground_1700_leafon_min$temp, ground_1700_leafon_max$temp, 
                                      ground_500_leafon_min$temp, ground_500_leafon_max$temp)

#can
can_leafon_d_score <- select(can_500_leafon_min, julian)
can_leafon_d_score$dscore <- calc_d_score(can_1700_leafon_min$temp, can_1700_leafon_max$temp, 
                                   can_500_leafon_min$temp, can_500_leafon_max$temp)


####### Calc summary stats ############

#mean d scores
mean(soil_leafoff_d_score$dscore)
mean(ground_leafoff_d_score$dscore)
mean(can_leafoff_d_score$dscore)
mean(soil_leafon_d_score$dscore)
mean(ground_leafon_d_score$dscore)
mean(can_leafon_d_score$dscore)

#median d scores
median(soil_leafoff_d_score$dscore)
median(ground_leafoff_d_score$dscore)
median(can_leafoff_d_score$dscore)
median(soil_leafon_d_score$dscore)
median(ground_leafon_d_score$dscore)
median(can_leafon_d_score$dscore)

#sd of d scores
sd(soil_leafoff_d_score$dscore)
sd(ground_leafoff_d_score$dscore)
sd(can_leafoff_d_score$dscore)
sd(soil_leafon_d_score$dscore)
sd(ground_leafon_d_score$dscore)
sd(can_leafon_d_score$dscore)

# Magnitude of decrease of overlap due to leaf-on conditions
# Calculate it like percent error
diff_overlap_soil <- abs((mean(soil_leafon_d_score$dscore) - mean(soil_leafoff_d_score$dscore)) / mean(soil_leafoff_d_score$dscore))
diff_overlap_ground <- abs((mean(ground_leafon_d_score$dscore) - mean(ground_leafoff_d_score$dscore)) / mean(ground_leafoff_d_score$dscore))
diff_overlap_can <- abs((mean(can_leafon_d_score$dscore) - mean(can_leafoff_d_score$dscore)) / mean(can_leafoff_d_score$dscore))
# Average magnitude decrease in overlap
mean(c(diff_overlap_soil, diff_overlap_ground, diff_overlap_can))

####### Prep data for Kruskal-Wallis ############

#Combine leafoff and leafon into single object
#necessary for KW, as leafoff and leafon are different lengths

soil_leafoff_d_score <- soil_leafoff_d_score %>%
  mutate(foliage = 0)
ground_leafoff_d_score <- ground_leafoff_d_score %>%
  mutate(foliage = 0)
can_leafoff_d_score <- can_leafoff_d_score %>%
  mutate(foliage = 0)
soil_leafon_d_score <- soil_leafon_d_score %>%
  mutate(foliage = 1)
ground_leafon_d_score <- ground_leafon_d_score %>%
  mutate(foliage = 1)
can_leafon_d_score <- can_leafon_d_score %>%
  mutate(foliage = 1)

soil_d_score <- rbind(soil_leafoff_d_score, soil_leafon_d_score)
ground_d_score <- rbind(ground_leafoff_d_score, ground_leafon_d_score)
can_d_score <- rbind(can_leafoff_d_score, can_leafon_d_score)

####### Kruskal-Wallis tests of d scores, leafoff vs leafon #######
kruskal.test(dscore ~ foliage, data = soil_d_score)
kruskal.test(dscore ~ foliage, data = ground_d_score)
kruskal.test(dscore ~ foliage, data = can_d_score)

##NOTE: I didn't trust this immediately. But then I did the same thing, with random 40% samples
#of leafoff vs leafoff. Those were NOT sig. different...which means that the kruskal.test
#isn't just spitting out significant p values



