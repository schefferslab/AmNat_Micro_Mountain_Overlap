# Klinges and Scheffers 2018
# This script is used to generate plots of tropical canopy temp x soil temp, and canopy temp x ground temp
# This is for the purpose of determining the buffering effect of leaf cover in the tropics
# This script is dependent on objects created in trop_main.R

#### NOTE: I abandoned this script, because there's simply too many elevations that are too different from each other.
# I can't combine a 'lowlands' between sea level and 550 m....and doesn't make sense to have like 8 panels. Yet.

# I went through and plotted for Mada lowlands...the trop data is weird. Like, every min or max temp is one of 
# 10 or so values. Don't really want to deal with this anymore, don't know what to think of it but these plots
# weren't for the tropical data anyways

require(tidyverse)
require(gridExtra)
require(grid)
require(lattice)


# Trying just for Mada
######## Combine daily min and max for Mada ##########

# lowlands
Mada_dailymin_low_soil <- Mada_dailymin_low_soil %>%
  mutate(minmax = "min")
Mada_dailymin_low_soil <- Mada_dailymin_low_soil %>%
  mutate(minmax = "min")
Mada_low_soil_minmax <- Mada_dailymin_low_soil %>%
  left_join(Mada_dailymax_low_soil)

Mada_dailymin_low_ground <- Mada_dailymin_low_ground %>%
  mutate(minmax = "min")
Mada_dailymin_low_ground <- Mada_dailymin_low_ground %>%
  mutate(minmax = "min")
Mada_low_ground_minmax <- Mada_dailymin_low_ground %>%
  left_join(Mada_dailymax_low_ground)

Mada_dailymin_low_canopy <- Mada_dailymin_low_canopy %>%
  mutate(minmax = "min")
Mada_dailymin_low_canopy <- Mada_dailymin_low_canopy %>%
  mutate(minmax = "min")
Mada_low_canopy_minmax <- Mada_dailymin_low_canopy %>%
  left_join(Mada_dailymax_low_canopy)

# uplands
Mada_dailymin_up_soil <- Mada_dailymin_up_soil %>%
  mutate(minmax = "min")
Mada_dailymin_up_soil <- Mada_dailymin_up_soil %>%
  mutate(minmax = "min")
Mada_up_soil_minmax <- Mada_dailymin_up_soil %>%
  left_join(Mada_dailymax_up_soil)

Mada_dailymin_up_ground <- Mada_dailymin_up_ground %>%
  mutate(minmax = "min")
Mada_dailymin_up_ground <- Mada_dailymin_up_ground %>%
  mutate(minmax = "min")
Mada_up_ground_minmax <- Mada_dailymin_up_ground %>%
  left_join(Mada_dailymax_up_ground)

Mada_dailymin_up_canopy <- Mada_dailymin_up_canopy %>%
  mutate(minmax = "min")
Mada_dailymin_up_canopy <- Mada_dailymin_up_canopy %>%
  mutate(minmax = "min")
Mada_up_canopy_minmax <- Mada_dailymin_up_canopy %>%
  left_join(Mada_dailymax_up_canopy)


###### Data curation for Mada #########

## Create necessary dataset: independent columns for soil, ground and can temps
## lowlands
arrange(Mada_low_soil_minmax, desc(minmax), julian)
arrange(Mada_low_ground_minmax, desc(minmax), julian)
arrange(Mada_low_canopy_minmax, desc(minmax), julian)

all_Mada_low_minmax <- rbind(Mada_low_soil_minmax, Mada_low_ground_minmax, Mada_low_canopy_minmax)

# There is less soil data than either ground or canopy,
# so sample down

Mada_low_soil_minmax_short <- Mada_low_soil_minmax[sample(nrow(Mada_low_soil_minmax), 344), ]
Mada_low_ground_minmax_short <- Mada_low_ground_minmax[sample(nrow(Mada_low_ground_minmax), 344), ]
Mada_low_canopy_minmax_short <- Mada_low_canopy_minmax[sample(nrow(Mada_low_canopy_minmax), 344), ]

all_Mada_low_minmax_wide <- Mada_low_soil_minmax_short %>%
  select(julian, minmax)
all_Mada_low_minmax_wide <- all_Mada_low_minmax_wide %>%
  cbind(select(Mada_low_soil_minmax_short, temp)) %>%
  rename(soil_temp = temp)
all_Mada_low_minmax_wide <- all_Mada_low_minmax_wide %>%
  cbind(select(Mada_low_ground_minmax_short, temp)) %>%
  rename(ground_temp = temp)
all_Mada_low_minmax_wide <- all_Mada_low_minmax_wide %>%
  cbind(select(Mada_low_canopy_minmax_short, temp)) %>%
  rename(can_temp = temp)


## uplands
arrange(Mada_up_soil_minmax, desc(minmax), julian)
arrange(Mada_up_ground_minmax, desc(minmax), julian)
arrange(Mada_up_canopy_minmax, desc(minmax), julian)

all_Mada_up_minmax <- rbind(Mada_up_soil_minmax, Mada_up_ground_minmax, Mada_up_canopy_minmax)

# There is less ground data than either soil or canopy,
# so sample down

Mada_up_soil_minmax_short <- Mada_up_soil_minmax[sample(nrow(Mada_up_soil_minmax), 261), ]
Mada_up_ground_minmax_short <- Mada_up_ground_minmax[sample(nrow(Mada_up_ground_minmax), 261), ]
Mada_up_canopy_minmax_short <- Mada_up_canopy_minmax[sample(nrow(Mada_up_canopy_minmax), 261), ]

all_Mada_up_minmax_wide <- Mada_up_soil_minmax_short %>%
  select(julian, minmax)
all_Mada_up_minmax_wide <- all_Mada_up_minmax_wide %>%
  cbind(select(Mada_up_soil_minmax_short, temp)) %>%
  rename(soil_temp = temp)
all_Mada_up_minmax_wide <- all_Mada_up_minmax_wide %>%
  cbind(select(Mada_up_ground_minmax_short, temp)) %>%
  rename(ground_temp = temp)
all_Mada_up_minmax_wide <- all_Mada_up_minmax_wide %>%
  cbind(select(Mada_up_canopy_minmax_short, temp)) %>%
  rename(can_temp = temp)

###### Simple difference in temp between canopy and soil/ground for Mada ######

mean(all_Mada_up_minmax_wide$can_temp - all_Mada_up_minmax_wide$soil_temp)
mean(all_Mada_up_minmax_wide$can_temp - all_Mada_up_minmax_wide$ground_temp)

sd(all_Mada_up_minmax_wide$can_temp - all_Mada_up_minmax_wide$soil_temp)
sd(all_Mada_up_minmax_wide$can_temp - all_Mada_up_minmax_wide$ground_temp)

wilcox.test(all_Mada_up_minmax_wide$can_temp, all_Mada_up_minmax_wide$soil_temp, paired = TRUE)
wilcox.test(all_Mada_up_minmax_wide$can_temp, all_Mada_up_minmax_wide$ground_temp, paired = TRUE)


###### Create canopy x soil/ground plots for Mada ######



# 500, canopy x soil

plot_Mada_low_soil <- ggplot(data = all_Mada_low_minmax_wide, aes(x = can_temp, y = soil_temp)) + 
  geom_point(alpha = 0.5) + 
  coord_cartesian(
    xlim = c(0, 40), ylim = c(0, 40)) +
  geom_line(data = line, aes(x, y), linetype = 2) +
  theme_classic() +
  xlab("Canopy Temperature") +
  ylab("Soil Temperature") +
  ggtitle("Canopy x Soil, Lowlands") +
  theme(text = element_text(size=16))
plot_Mada_low_soil







###### Data curation #########

## Create necessary dataset: independent columns for soil, ground and can temps
## lowlands
arrange(trop_low_soil_minmax, desc(minmax), julian)
arrange(trop_low_ground_minmax, desc(minmax), julian)
arrange(trop_low_canopy_minmax, desc(minmax), julian)

all_trop_low_minmax <- rbind(trop_low_soil_minmax, trop_low_ground_minmax, trop_low_canopy_minmax)


# Because there is less canopy data than either soil or ground, we'll need to sample soil and ground data
# to make sure all data frames are the same length, so they can be cbinded together
trop_low_soil_minmax_short <- trop_low_soil_minmax[sample(nrow(trop_low_soil_minmax), 1000), ]
trop_low_ground_minmax_short <- trop_low_ground_minmax[sample(nrow(trop_low_ground_minmax), 1000), ]
trop_low_canopy_minmax_short <- trop_low_canopy_minmax[sample(nrow(trop_low_canopy_minmax), 1000), ]

all_trop_low_minmax_short <- rbind(trop_low_soil_minmax_short, trop_low_ground_minmax_short, trop_low_canopy_minmax_short)


all_trop_low_minmax_wide <- trop_low_soil_minmax_short %>%
  select(julian, minmax)
all_trop_low_minmax_wide <- all_trop_low_minmax_wide %>%
  cbind(select(trop_low_soil_minmax_short, temp)) %>%
  rename(soil_temp = temp)
all_trop_low_minmax_wide <- all_trop_low_minmax_wide %>%
  cbind(select(trop_low_ground_minmax_short, temp)) %>%
  rename(ground_temp = temp)
all_trop_low_minmax_wide <- all_trop_low_minmax_wide %>%
  cbind(select(trop_low_canopy_minmax_short, temp)) %>%
  rename(can_temp = temp)


## uplands
arrange(trop_up_soil_minmax, desc(minmax), julian)
arrange(trop_up_ground_minmax, desc(minmax), julian)
arrange(trop_up_canopy_minmax, desc(minmax), julian)

all_trop_up_minmax <- rbind(trop_up_soil_minmax, trop_up_ground_minmax, trop_up_canopy_minmax)


# Because there is less canopy data than either soil or ground, we'll need to sample soil and ground data
# to make sure all data frames are the same length, so they can be cbinded together
trop_up_soil_minmax_short <- trop_up_soil_minmax[sample(nrow(trop_up_soil_minmax), 1000), ]
trop_up_ground_minmax_short <- trop_up_ground_minmax[sample(nrow(trop_up_ground_minmax), 1000), ]
trop_up_canopy_minmax_short <- trop_up_canopy_minmax[sample(nrow(trop_up_canopy_minmax), 1000), ]

all_trop_up_minmax_short <- rbind(trop_up_soil_minmax_short, trop_up_ground_minmax_short, trop_up_canopy_minmax_short)


all_trop_up_minmax_wide <- trop_up_soil_minmax_short %>%
  select(julian, minmax)
all_trop_up_minmax_wide <- all_trop_up_minmax_wide %>%
  cbind(select(trop_up_soil_minmax_short, temp)) %>%
  rename(soil_temp = temp)
all_trop_up_minmax_wide <- all_trop_up_minmax_wide %>%
  cbind(select(trop_up_ground_minmax_short, temp)) %>%
  rename(ground_temp = temp)
all_trop_up_minmax_wide <- all_trop_up_minmax_wide %>%
  cbind(select(trop_up_canopy_minmax_short, temp)) %>%
  rename(can_temp = temp)



