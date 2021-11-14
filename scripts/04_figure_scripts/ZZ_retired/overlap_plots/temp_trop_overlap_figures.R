#David Klinges
#2018.07.04
#This scripts plots figures of the overlaps curves for AT THE MOMENT, JUST TEMPERATE DATA.
#ADD TROPICAL DATA TO THIS



####### Plot elevation overlap curves, all temp data ############

#soil
ggplot(soil_500_leafoff, aes(temp)) +
  geom_density(kernel = "gaussian", fill = "red", alpha = 0.6) +
  geom_density(data = soil_1700_leafoff, kernel = "gaussian", fill = "blue", alpha = 0.6) +
  coord_cartesian(xlim = c(-30, 40), ylim = c(0, .30)) +
  theme_classic()

ggplot(soil_500_leafon, aes(temp)) +
  geom_density(kernel = "gaussian", fill = "red", alpha = 0.6) +
  geom_density(data = soil_1700_leafon, kernel = "gaussian", fill = "blue", alpha = 0.6) +
  coord_cartesian(xlim = c(-30, 40), ylim = c(0, .30)) +
theme_classic()
  
#ground
ggplot(ground_500_leafoff, aes(temp)) +
  geom_density(kernel = "gaussian", fill = "red", alpha = 0.6) +
  geom_density(data = ground_1700_leafoff, kernel = "gaussian", fill = "blue", alpha = 0.6) +
  coord_cartesian(xlim = c(-30, 40), ylim = c(0, .30)) +
  theme_classic()

ggplot(ground_500_leafon, aes(temp)) +
  geom_density(kernel = "gaussian", fill = "red", alpha = 0.6) +
  geom_density(data = ground_1700_leafon, kernel = "gaussian", fill = "blue", alpha = 0.6) +
  coord_cartesian(xlim = c(-30, 40), ylim = c(0, .30)) +
  theme_classic()

#canopy
ggplot(can_500_leafoff, aes(temp)) +
  geom_density(kernel = "gaussian", fill = "red", alpha = 0.6) +
  geom_density(data = can_1700_leafoff, kernel = "gaussian", fill = "blue", alpha = 0.6) +
  coord_cartesian(xlim = c(-30, 40), ylim = c(0, .30)) +
  theme_classic()

ggplot(can_500_leafon, aes(temp)) +
  geom_density(kernel = "gaussian", fill = "red", alpha = 0.6) +
  geom_density(data = can_1700_leafon, kernel = "gaussian", fill = "blue", alpha = 0.6) +
  coord_cartesian(xlim = c(-30, 40), ylim = c(0, .30)) +
  theme_classic()

####### Plot elevation overlap curves, daily mean ############

#soil
ggplot(soil_500_leafoff_mean, aes(temp)) +
  geom_density(kernel = "gaussian", fill = "red", alpha = 0.6) +
  geom_density(data = soil_1700_leafoff_mean, kernel = "gaussian", fill = "blue", alpha = 0.6) +
  coord_cartesian(xlim = c(-30, 40), ylim = c(0, .30)) +
  theme_classic()

ggplot(soil_500_leafon_mean, aes(temp)) +
  geom_density(kernel = "gaussian", fill = "red", alpha = 0.6) +
  geom_density(data = soil_1700_leafon_mean, kernel = "gaussian", fill = "blue", alpha = 0.6) +
  coord_cartesian(xlim = c(-30, 40), ylim = c(0, .30)) +
  theme_classic()

#ground
ggplot(ground_500_leafoff_mean, aes(temp)) +
  geom_density(kernel = "gaussian", fill = "red", alpha = 0.6) +
  geom_density(data = ground_1700_leafoff_mean, kernel = "gaussian", fill = "blue", alpha = 0.6) +
  coord_cartesian(xlim = c(-30, 40), ylim = c(0, .30)) +
  theme_classic()

ggplot(ground_500_leafon_mean, aes(temp)) +
  geom_density(kernel = "gaussian", fill = "red", alpha = 0.6) +
  geom_density(data = ground_1700_leafon_mean, kernel = "gaussian", fill = "blue", alpha = 0.6) +
  coord_cartesian(xlim = c(-30, 40), ylim = c(0, .30)) +
  theme_classic()

#canopy
ggplot(can_500_leafoff_mean, aes(temp)) +
  geom_density(kernel = "gaussian", fill = "red", alpha = 0.6) +
  geom_density(data = can_1700_leafoff_mean, kernel = "gaussian", fill = "blue", alpha = 0.6) +
  coord_cartesian(xlim = c(-30, 40), ylim = c(0, .30)) +
  theme_classic()

ggplot(can_500_leafon_mean, aes(temp)) +
  geom_density(kernel = "gaussian", fill = "red", alpha = 0.6) +
  geom_density(data = can_1700_leafon_mean, kernel = "gaussian", fill = "blue", alpha = 0.6) +
  coord_cartesian(xlim = c(-30, 40), ylim = c(0, .30)) +
  theme_classic()


####### Plot elevation overlap curves, daily min and max ##########

#soil
ggplot(soil_500_leafoff_minmax, aes(temp)) +
  geom_density(kernel = "gaussian", fill = "red", alpha = 0.6) +
  geom_density(data = soil_1700_leafoff_minmax, kernel = "gaussian", fill = "blue", alpha = 0.6) +
  coord_cartesian(xlim = c(-30, 40), ylim = c(0, .30)) +
  theme_classic()

ggplot(soil_500_leafon_minmax, aes(temp)) +
  geom_density(kernel = "gaussian", fill = "red", alpha = 0.6) +
  geom_density(data = soil_1700_leafon_minmax, kernel = "gaussian", fill = "blue", alpha = 0.6) +
  coord_cartesian(xlim = c(-30, 40), ylim = c(0, .30)) +
  theme_classic()

#ground
ggplot(ground_500_leafoff_minmax, aes(temp)) +
  geom_density(kernel = "gaussian", fill = "red", alpha = 0.6) +
  geom_density(data = ground_1700_leafoff_minmax, kernel = "gaussian", fill = "blue", alpha = 0.6) +
  coord_cartesian(xlim = c(-30, 40), ylim = c(0, .30)) +
  theme_classic()

ggplot(ground_500_leafon_minmax, aes(temp)) +
  geom_density(kernel = "gaussian", fill = "red", alpha = 0.6) +
  geom_density(data = ground_1700_leafon_minmax, kernel = "gaussian", fill = "blue", alpha = 0.6) +
  coord_cartesian(xlim = c(-30, 40), ylim = c(0, .30)) +
  theme_classic()

#canopy
ggplot(can_500_leafoff_minmax, aes(temp)) +
  geom_density(kernel = "gaussian", fill = "red", alpha = 0.6) +
  geom_density(data = can_1700_leafoff_minmax, kernel = "gaussian", fill = "blue", alpha = 0.6) +
  coord_cartesian(xlim = c(-30, 40), ylim = c(0, .30)) +
  theme_classic()

ggplot(can_500_leafon_minmax, aes(temp)) +
  geom_density(kernel = "gaussian", fill = "red", alpha = 0.6) +
  geom_density(data = can_1700_leafon_minmax, kernel = "gaussian", fill = "blue", alpha = 0.6) +
  coord_cartesian(xlim = c(-30, 40), ylim = c(0, .30)) +
  theme_classic()
