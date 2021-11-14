#David Klinges
#this script calculates d scores, used to quantify overlap
#d scores are calculated using a method developed by Janzen (1967)

#This method was used in Scheffers and Williams 2018
#d-scores compare thermal distribution of microhabitats at high elevation vs low elevation
#only conducted for temperate data herwe
#dependant on objects created in "North Carolina Overlap Script_DHK edits"

#Steps:
#Prep data: we need daily min and daily max for each elevation and microhabitat
#Calculate d score using Cohen's method
#Compare summary stats of d scores for leafoff and leafon
#Compare d scores for leafoff and leafon using Kruskl-Wallis


############# README README README

where you left off:
  - for tropical data, low elevation and high elevation datasets are of different length
- you'll need to subset the datasets....but you couldn't get sample_n to work
- so, find another way to randomly subsample your datasets, then use that to calculate d scores
- you'll need to calculate d scores for each of Mada, Phili, and Aust, seperately for each microhabitat.
DON'T LET THIS EAT UP ALL YOUR TIME. It's just for one figure, remember. At least, for now.



####### Subset trop datasets so they are all same length ##########

# Mada
Mada_dailymin_up_soil_sampled <- sample_n(Mada_dailymin_up_soil, 300) %>%
  slice(1:344)
Mada_dailymax_up_soil_sampled <- Mada_dailymax_up_soil %>%
  sample_frac(0.599, replace = FALSE)

# Soil
# randomly sample down to size of _low_ datasets...have to do _frac because sample_n is a broken function


# Ground
trop_dailymin_up_ground_sampled <- trop_dailymin_up_ground %>%
  sample_frac(0.717, replace = FALSE)
trop_dailymax_up_ground_sampled <- trop_dailymax_up_ground %>%
  sample_frac(0.717, replace = FALSE)

# Canopy
trop_dailymin_low_canopy_sampled <- trop_dailymin_low_canopy %>%
  sample_frac(0.987, replace = FALSE)
trop_dailymax_low_canopy_sampled <- trop_dailymax_low_canopy %>%
  sample_frac(0.987, replace = FALSE)


####### Calc d scores######################### 

#function to calculate d scores
#inputs are 4 data frames: daily min temp of uplands, daily max temp of uplands, 
#and the same two for lowlands
calc_d_score <- function(upmin, upmax, lowmin, lowmax) {
  out <- (upmax - lowmin)/(sqrt((upmax - upmin)*(lowmax - lowmin)))
}


# Calculate Mada D scores

#soil
Mada_soil_d_score <- data.frame(matrix(NA, nrow = 430, ncol = 1))
Mada_soil_d_score$dscore <- apply(Mada_soil_d_score, FUN = calc_d_score(Mada_dailymin_up_soil$temp, Mada_dailymax_up_soil$temp, 
                                         Mada_dailymin_low_soil$temp, Mada_dailymax_up_soil$temp))
#ground
trop_ground_d_score <- c()
trop_ground_d_score$dscore <- calc_d_score(trop_dailymin_up_ground_sampled$temp, trop_dailymax_up_ground_sampled$temp, 
                                         trop_dailymin_low_ground$temp, trop_dailymax_low_ground$temp)

#canopy
trop_canopy_d_score <- c()
trop_canopy_d_score$dscore <- calc_d_score(trop_dailymin_up_canopy$temp, trop_dailymax_up_canopy$temp, 
                                           trop_dailymin_low_canopy_sampled$temp, trop_dailymax_low_canopy_sampled$temp)


# Calculate Phili D Scores



# Calculate Aust D scores

