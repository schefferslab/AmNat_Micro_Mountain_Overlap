#David Klinges
#2018.07.04
#This script conducts Fligner's test for homoscedascity, comparing leaf-off vs leaf-on
#does so for ALL temp data, daily mean, and daily min + daily max

## Workspace Prep #############

sage <- read.csv("./data/02_derivative/wyoming_all_elevations_wide.csv")

########## Fligner's, all temp data #############

fligner.test(list(soil_500_leafoff$temp, soil_500_leafon$temp))
fligner.test(list(ground_500_leafoff$temp, ground_500_leafon$temp))
fligner.test(list(can_500_leafoff$temp, can_500_leafon$temp))
fligner.test(list(soil_1700_leafoff$temp, soil_1700_leafon$temp))
fligner.test(list(ground_1700_leafoff$temp, ground_1700_leafon$temp))
fligner.test(list(can_1700_leafoff$temp, can_1700_leafon$temp))

########## Fligner's, daily mean #############

fligner.test(list(soil_500_leafoff_mean$temp, soil_500_leafon_mean$temp))
fligner.test(list(ground_500_leafoff_mean$temp, ground_500_leafon_mean$temp))
fligner.test(list(can_500_leafoff_mean$temp, can_500_leafon_mean$temp))
fligner.test(list(soil_1700_leafoff_mean$temp, soil_1700_leafon_mean$temp))
fligner.test(list(ground_1700_leafoff_mean$temp, ground_1700_leafon_mean$temp))
fligner.test(list(can_1700_leafoff_mean$temp, can_1700_leafon_mean$temp))

########## Fligner's, combined daily min and daily max #############

fligner.test(list(soil_500_leafoff_minmax$temp, soil_500_leafon_minmax$temp))
fligner.test(list(ground_500_leafoff_minmax$temp, ground_500_leafon_minmax$temp))
fligner.test(list(can_500_leafoff_minmax$temp, can_500_leafon_minmax$temp))
fligner.test(list(soil_1700_leafoff_minmax$temp, soil_1700_leafon_minmax$temp))
fligner.test(list(ground_1700_leafoff_minmax$temp, ground_1700_leafon_minmax$temp))
fligner.test(list(can_1700_leafoff_minmax$temp, can_1700_leafon_minmax$temp))

########## Standard deviations of combined daily min and daily max ##########

#elevation seperate
sd(soil_500_leafoff_minmax$temp)
sd(soil_1700_leafoff_minmax$temp)
sd(ground_500_leafoff_minmax$temp)
sd(ground_1700_leafoff_minmax$temp)
sd(can_500_leafoff_minmax$temp)
sd(can_1700_leafoff_minmax$temp)

sd(soil_500_leafon_minmax$temp)
sd(soil_1700_leafon_minmax$temp)
sd(ground_500_leafon_minmax$temp)
sd(ground_1700_leafon_minmax$temp)
sd(can_500_leafon_minmax$temp)
sd(can_1700_leafon_minmax$temp)

#elevation combined (so there are fewer numbers to report in MS main text)
#I don't think it makes much sense to combined elevations, because the SD of that combined data will be artificial.
#So keeping this here, but don't plan on using
soil_leafoff_minmax <- rbind(soil_500_leafoff_minmax, soil_1700_leafoff_minmax)
ground_leafoff_minmax <- rbind(ground_500_leafoff_minmax, ground_1700_leafoff_minmax)
can_leafoff_minmax <- rbind(can_500_leafoff_minmax, can_1700_leafoff_minmax)

soil_leafon_minmax <- rbind(soil_500_leafon_minmax, soil_1700_leafon_minmax)
ground_leafon_minmax <- rbind(ground_500_leafon_minmax, ground_1700_leafon_minmax)
can_leafon_minmax <- rbind(can_500_leafon_minmax, can_1700_leafon_minmax)

sd(soil_leafoff_minmax$temp)
sd(ground_leafoff_minmax$temp)
sd(can_leafoff_minmax$temp)
sd(soil_leafon_minmax$temp)
sd(ground_leafon_minmax$temp)
sd(can_leafon_minmax$temp)

