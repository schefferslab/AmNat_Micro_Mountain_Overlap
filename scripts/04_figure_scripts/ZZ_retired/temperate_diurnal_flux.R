# Klinges and Scheffers 2018
# This script is used to generate plots of diurnal flux (daily max - daily min)
# This is for the purpose of demonstrating the buffering effect of leaf cover
# This script is dependent on objects created in NC_main.R

require(tidyverse)
require(gridExtra)
require(grid)
require(lattice)

####### Calculate difference between daily min and daily max ########

# leaf-off
soil_500_leafoff_diurnal_flux <- soil_500_leafoff_max %>%
  mutate(temp = (soil_500_leafoff_max$temp - soil_500_leafoff_min$temp))
soil_1700_leafoff_diurnal_flux <- soil_1700_leafoff_max %>%
  mutate(temp = (soil_1700_leafoff_max$temp - soil_1700_leafoff_min$temp))
ground_500_leafoff_diurnal_flux <- ground_500_leafoff_max %>%
  mutate(temp = (ground_500_leafoff_max$temp - ground_500_leafoff_min$temp))
ground_1700_leafoff_diurnal_flux <- ground_1700_leafoff_max %>%
  mutate(temp = (ground_1700_leafoff_max$temp - ground_1700_leafoff_min$temp))
can_500_leafoff_diurnal_flux <- can_500_leafoff_max %>%
  mutate(temp = (can_500_leafoff_max$temp - can_500_leafoff_min$temp))
can_1700_leafoff_diurnal_flux <- can_1700_leafoff_max %>%
  mutate(temp = (can_1700_leafoff_max$temp - can_1700_leafoff_min$temp))

# leaf-on
soil_500_leafon_diurnal_flux <- soil_500_leafon_max %>%
  mutate(temp = (soil_500_leafon_max$temp - soil_500_leafon_min$temp))
soil_1700_leafon_diurnal_flux <- soil_1700_leafon_max %>%
  mutate(temp = (soil_1700_leafon_max$temp - soil_1700_leafon_min$temp))
ground_500_leafon_diurnal_flux <- ground_500_leafon_max %>%
  mutate(temp = (ground_500_leafon_max$temp - ground_500_leafon_min$temp))
ground_1700_leafon_diurnal_flux <- ground_1700_leafon_max %>%
  mutate(temp = (ground_1700_leafon_max$temp - ground_1700_leafon_min$temp))
can_500_leafon_diurnal_flux <- can_500_leafon_max %>%
  mutate(temp = (can_500_leafon_max$temp - can_500_leafon_min$temp))
can_1700_leafon_diurnal_flux <- can_1700_leafon_max %>%
  mutate(temp = (can_1700_leafon_max$temp - can_1700_leafon_min$temp))

mean(soil_500_leafoff_diurnal_flux$temp)
mean(soil_1700_leafoff_diurnal_flux$temp)
mean(ground_500_leafoff_diurnal_flux$temp)
mean(ground_1700_leafoff_diurnal_flux$temp)
mean(can_500_leafoff_diurnal_flux$temp)
mean(can_1700_leafoff_diurnal_flux$temp)

mean(soil_500_leafon_diurnal_flux$temp)
mean(soil_1700_leafon_diurnal_flux$temp)
mean(ground_500_leafon_diurnal_flux$temp)
mean(ground_1700_leafon_diurnal_flux$temp)
mean(can_500_leafon_diurnal_flux$temp)
mean(can_1700_leafon_diurnal_flux$temp)


####### Combine leaf-on and leaf-off #########


soil_500_leafon_diurnal_flux <- soil_500_leafon_diurnal_flux %>%
  mutate(Foliage = "leafon")
soil_1700_leafon_diurnal_flux <- soil_1700_leafon_diurnal_flux %>%
  mutate(Foliage = "leafon")
ground_500_leafon_diurnal_flux <- ground_500_leafon_diurnal_flux %>%
  mutate(Foliage = "leafon")
ground_1700_leafon_diurnal_flux <- ground_1700_leafon_diurnal_flux %>%
  mutate(Foliage = "leafon")
can_500_leafon_diurnal_flux <- can_500_leafon_diurnal_flux %>%
  mutate(Foliage = "leafon")
can_1700_leafon_diurnal_flux <- can_1700_leafon_diurnal_flux %>%
  mutate(Foliage = "leafon")

soil_500_leafoff_diurnal_flux <- soil_500_leafoff_diurnal_flux %>%
  mutate(Foliage = "leafoff")
soil_1700_leafoff_diurnal_flux <- soil_1700_leafoff_diurnal_flux %>%
  mutate(Foliage = "leafoff")
ground_500_leafoff_diurnal_flux <- ground_500_leafoff_diurnal_flux %>%
  mutate(Foliage = "leafoff")
ground_1700_leafoff_diurnal_flux <- ground_1700_leafoff_diurnal_flux %>%
  mutate(Foliage = "leafoff")
can_500_leafoff_diurnal_flux <- can_500_leafoff_diurnal_flux %>%
  mutate(Foliage = "leafoff")
can_1700_leafoff_diurnal_flux <- can_1700_leafoff_diurnal_flux %>%
  mutate(Foliage = "leafoff")

soil_500_diurnal_flux <- rbind(soil_500_leafon_diurnal_flux, soil_500_leafoff_diurnal_flux)
soil_1700_diurnal_flux <- rbind(soil_1700_leafon_diurnal_flux, soil_1700_leafoff_diurnal_flux)
ground_500_diurnal_flux <- rbind(ground_500_leafon_diurnal_flux, ground_500_leafoff_diurnal_flux)
ground_1700_diurnal_flux <- rbind(ground_1700_leafon_diurnal_flux, ground_1700_leafoff_diurnal_flux)
can_500_diurnal_flux <- rbind(can_500_leafon_diurnal_flux, can_500_leafoff_diurnal_flux)
can_1700_diurnal_flux <- rbind(can_1700_leafon_diurnal_flux, can_1700_leafoff_diurnal_flux)





####### Plot #########

soil_500_diurnal_flux_plot <- ggplot(data = soil_500_diurnal_flux, aes(x = julian, y = temp)) + 
  geom_point(aes(color = Foliage), alpha = 1) + 
  geom_smooth(data = soil_500_diurnal_flux, color = "black") +
  coord_cartesian(
    xlim = c(0, 370), ylim = c(0, 30)) +
  theme_classic() +
  xlab("Julian Day") +
  ylab("") +
  ggtitle("Soil Diurnal Flux, Lowlands") +
  theme(text = element_text(size=16))
soil_500_diurnal_flux_plot

soil_1700_diurnal_flux_plot <- ggplot(data = soil_1700_diurnal_flux, aes(x = julian, y = temp)) + 
  geom_point(aes(color = Foliage), alpha = 1) + 
  geom_smooth(data = soil_1700_diurnal_flux, color = "black") +
  coord_cartesian(
    xlim = c(0, 370), ylim = c(0, 30)) +
  theme_classic() +
  xlab("Julian Day") +
  ylab("") +
  ggtitle("Soil Diurnal Flux, Uplands") +
  theme(text = element_text(size=16))
soil_1700_diurnal_flux_plot

ground_500_diurnal_flux_plot <- ggplot(data = ground_500_diurnal_flux, aes(x = julian, y = temp)) + 
  geom_point(aes(color = Foliage), alpha = 1) + 
  geom_smooth(data = ground_500_diurnal_flux, color = "black") +
  coord_cartesian(
    xlim = c(0, 370), ylim = c(0, 30)) +
  theme_classic() +
  xlab("") +
  ylab("Difference in Daily Min and Max") +
  ggtitle("Ground Diurnal Flux, Lowlands") +
  theme(text = element_text(size=16))
ground_500_diurnal_flux_plot

ground_1700_diurnal_flux_plot <- ggplot(data = ground_1700_diurnal_flux, aes(x = julian, y = temp)) + 
  geom_point(aes(color = Foliage), alpha = 1) + 
  geom_smooth(data = ground_1700_diurnal_flux, color = "black") +
  coord_cartesian(
    xlim = c(0, 370), ylim = c(0, 30)) +
  theme_classic() +
  xlab("") +
  ylab("") +
  ggtitle("Ground Diurnal Flux, Uplands") +
  theme(text = element_text(size=16))
ground_1700_diurnal_flux_plot

can_500_diurnal_flux_plot <- ggplot(data = can_500_diurnal_flux, aes(x = julian, y = temp)) + 
  geom_point(aes(color = Foliage), alpha = 1) + 
  geom_smooth(data = can_500_diurnal_flux, color = "black") +
  coord_cartesian(
    xlim = c(0, 370), ylim = c(0, 30)) +
  theme_classic() +
  xlab("") +
  ylab("") +
  ggtitle("Canopy Diurnal Flux, Lowlands") +
  theme(text = element_text(size=16))
can_500_diurnal_flux_plot

can_1700_diurnal_flux_plot <- ggplot(data = can_1700_diurnal_flux, aes(x = julian, y = temp)) + 
  geom_point(aes(color = Foliage), alpha = 1) + 
  geom_smooth(data = can_1700_diurnal_flux, color = "black") +
  coord_cartesian(
    xlim = c(0, 370), ylim = c(0, 30)) +
  theme_classic() +
  xlab("") +
  ylab("") +
  ggtitle("Canopy Diurnal Flux, Uplands") +
  theme(text = element_text(size=16))
can_1700_diurnal_flux_plot

p1 <- can_500_diurnal_flux_plot
p2 <- can_1700_diurnal_flux_plot
p3 <- ground_500_diurnal_flux_plot
p4 <- ground_1700_diurnal_flux_plot
p5 <- soil_500_diurnal_flux_plot
p6 <- soil_1700_diurnal_flux_plot

grid.arrange(p1, p2, p3, p4, p5, p6)


