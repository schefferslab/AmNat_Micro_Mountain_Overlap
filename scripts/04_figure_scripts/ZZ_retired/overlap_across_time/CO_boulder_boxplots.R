# David Klinges
# This script generates overlap plots with different panels for increasing
# temporal representation: day, month, year, 10 years

## 1. Workspace prep ##############
library(tidyverse)
library(scales)
library(gridExtra)

# Import data
data_compiled <- read_csv("data/03_compiled/mountains_wide.csv")
soil_raw <- read_csv("./data/01_primary/temperate/CO_boulder/derivative/Boulder_met1-GLV_wide.csv")
surface_raw <- read_csv("./data/01_primary/temperate/CO_boulder/derivative/Boulder_met1-GLV_wide.csv")
fine_res <- read_csv("./data/01_primary/temperate/OR_andrews/derivative/fineRes/OR_andrews_fineResRS89RS04.csv")

# Figure out what sites have the most years of data
count_years <- data_compiled %>% 
  group_by(site) %>% 
  summarize(n_years =  n_distinct(year))

ggplot(count_years, aes(site, n)) +
  geom_bar(stat = "identity")


# Designate years, months, and days of interest
YEAR <- 2003
JULIAN <- 10
PLOT_TITLE <- "Colorado: Rocky Mountains (non-forest)"

## Soil curation and plots #########

soil_raw <- soil_raw %>% 
  select(-contains("surface"))
## 2. Generate daily, monthly, annual, ten-year timestep datasets #######

## ....A. Daily ##########
daily <- fine_res %>% 
  filter(year == YEAR & julian == JULIAN) %>% 
  group_by(hour) %>% 
  mutate(low_soil_min = min(low_soil, na.rm = TRUE)) %>% 
  mutate(low_soil_max = max(low_soil, na.rm = TRUE)) %>% 
  mutate(high_soil_min = min(high_soil, na.rm = TRUE)) %>% 
  mutate(high_soil_max = max(high_soil, na.rm = TRUE)) %>% 
  select(hour, low_soil_min, low_soil_max, high_soil_min, high_soil_max) %>% 
  distinct()

## ....B. Monthly ##########
monthly <- soil_raw %>% 
  filter(julian > 0 & julian < 31)

## ....C. Annual #########
annual <- soil_raw %>% 
  filter(year == YEAR)

## ....D. Decadal #########

decadal <- soil_raw %>% 
  filter(year > 1997 & year < 2004) %>% 
  # Scale julian from 0 to 1
  mutate(year_decimal = rescale(julian, to = c(0, 1))) %>% 
  mutate(year = year + year_decimal)


## 3. Janzify #########

## ....A. Daily overlap, increasing amount of time (# observations) #########
source("scripts/data_processing/janzify.R")
monthly_janzified <- janzify(monthly) %>% 
  mutate(time = "month")
annual_janzified <- janzify(annual) %>% 
  mutate(time = "year")
decadal_janzified <- janzify(decadal) %>% 
  mutate(time = "decade")

janzified <- monthly_janzified %>% 
  bind_rows(annual_janzified) %>% 
  bind_rows(decadal_janzified)

## ....B. Calc overlap for increasing temporal intervals #########

three_monthly <- soil_raw %>%
  # Approximating the date from the year and julian
  mutate(date = as_date((year - 1970) * 365 + julian) - 6) %>% 
  mutate(month = month(date)) %>% 
  mutate(interval = cut(month,
                        breaks=c(0, 3, 6, 9, 12), 
                        labels=c("1","2","3","4")))

six_monthly <- soil_raw %>%
  # Approximating the date from the year and julian
  mutate(date = as_date((year - 1970) * 365 + julian) - 6) %>% 
  mutate(month = month(date)) %>% 
  mutate(interval = cut(month,
                        breaks=c(0, 6, 12), 
                        labels=c("1","2")))

two_year <- soil_raw %>%
  mutate(interval = cut(year,
                        breaks=c(1998, 2000, 2002, 2004, 2006, 2007, 2010, 2012,
                                 2014, 2016), 
                        labels=c("1","2","3","4","5","6","7","8","9")))


daily_janzen <- janzify(soil_raw) %>% 
  mutate(time = "day")
monthly_janzen <- janzify_per_month(soil_raw) %>% 
  mutate(time = "month")
seasonal_janzen <- janzify_per_season(soil_raw) %>% 
  mutate(time = "season")
# threeMonth_janzen <- janzify_per_customWithinYear(three_monthly) %>% 
#   mutate(time = "three_month")
sixMonth_janzen <- janzify_per_customWithinYear(six_monthly) %>% 
  mutate(time = "six_month")
annual_janzen <- janzify_per_year(soil_raw) %>% 
  mutate(time = "year")
twoyear_janzen <- janzify_per_customBetweenYears(two_year) %>% 
  mutate(time = "two_year")

janzified <- daily_janzen %>% 
  bind_rows(monthly_janzen) %>% 
  bind_rows(seasonal_janzen) %>% 
  bind_rows(sixMonth_janzen) %>% 
  bind_rows(annual_janzen) %>% 
  bind_rows(twoyear_janzen)

# Re-order factors
time_factors <- c("day", "month", "season", "six_month", "year", "two_year")
janzified <- janzified %>% 
  mutate(time = factor(time, levels = time_factors))

## ....C. Randomly select years to add #########

all_janzified <- janzify(soil_raw)

years <- sample(c(1998:2015), size = 10, replace = FALSE)

# Filter to just sampled years
all_janzified <- all_janzified %>% 
  filter(year %in% years)

# Place flags for each iteration of adding a year
for (i in 1:10) {
  year_i <- all_janzified %>% 
    filter(year %in% years[1:i]) %>% 
    mutate(iteration = i)
  
  if (i == 1) {
    all_janz_plotReady <- year_i
  } else {
    all_janz_plotReady <- bind_rows(all_janz_plotReady, year_i)
  }
}



## 4. Plot data ##############

## ....A. Boxplot of Janzen overlap calc at higher timesteps ###########

ggplot(data = janzified, 
       aes(x = time, y = overlap)) +
  geom_boxplot(alpha = 0.5, lwd = .5) +
  labs(title = PLOT_TITLE, subtitle = "Soil temp") +
  theme_bw()


## ....B. Boxplots for random sample #########

ggplot(data = all_janz_plotReady, 
       aes(x = as.factor(iteration), y = overlap)) +
  geom_boxplot(alpha = 0.3, lwd = .5) +
  geom_violin(alpha = 0.5, lwd = .5) +
  labs(title = PLOT_TITLE, subtitle = "Soil temp") +
  xlab("Number of randomly drawn years") +
  theme_bw()

## Surface curation and plots ##########
surface_raw <- surface_raw %>% 
  select(-contains("soil"))
## 2. Generate daily, monthly, annual, ten-year timestep datasets #######

## ....A. Daily #########
daily <- fine_res %>% 
  filter(year == YEAR & julian == JULIAN) %>% 
  group_by(hour) %>% 
  mutate(low_surface_min = min(low_surface, na.rm = TRUE)) %>% 
  mutate(low_surface_max = max(low_surface, na.rm = TRUE)) %>% 
  mutate(high_surface_min = min(high_surface, na.rm = TRUE)) %>% 
  mutate(high_surface_max = max(high_surface, na.rm = TRUE)) %>% 
  select(hour, low_surface_min, low_surface_max, high_surface_min, high_surface_max) %>% 
  distinct()

## ....B. Monthly ##########
monthly <- surface_raw %>% 
  filter(julian > 150 & julian < 180)

## ....C. Annual #########
annual <- surface_raw %>% 
  filter(year == YEAR)

## ....D. Decadal #########

decadal <- surface_raw %>% 
  filter(year > 1997 & year < 2004) %>% 
  # Scale julian from 0 to 1
  mutate(year_decimal = rescale(julian, to = c(0, 1))) %>% 
  mutate(year = year + year_decimal)


## 3. Janzify #########

## ....A. Daily overlap, increasing amount of time (# observations) #########
source("scripts/data_processing/janzify.R")
monthly_janzified <- janzify(monthly) %>% 
  mutate(time = "month")
annual_janzified <- janzify(annual) %>% 
  mutate(time = "year")
decadal_janzified <- janzify(decadal) %>% 
  mutate(time = "decade")

janzified <- monthly_janzified %>% 
  bind_rows(annual_janzified) %>% 
  bind_rows(decadal_janzified)

## ....B. Calc overlap for increasing temporal intervals #########
three_monthly <- soil_raw %>%
  # Approximating the date from the year and julian
  mutate(date = as_date((year - 1970) * 365 + julian) - 6) %>% 
  mutate(month = month(date)) %>% 
  mutate(interval = cut(month,
                        breaks=c(0, 3, 6, 9, 12), 
                        labels=c("1","2","3","4")))

six_monthly <- soil_raw %>%
  # Approximating the date from the year and julian
  mutate(date = as_date((year - 1970) * 365 + julian) - 6) %>% 
  mutate(month = month(date)) %>% 
  mutate(interval = cut(month,
                        breaks=c(0, 6, 12), 
                        labels=c("1","2")))

two_year <- surface_raw %>%
  mutate(interval = cut(year,
                        breaks=c(1998, 2000, 2002, 2004, 2006, 2007, 2010, 2012,
                                 2014, 2016), 
                        labels=c("1","2","3","4","5","6","7","8","9")))


daily_janzen <- janzify(surface_raw) %>% 
  mutate(time = "day")
monthly_janzen <- janzify_per_month(surface_raw) %>% 
  mutate(time = "month")
seasonal_janzen <- janzify_per_season(surface_raw) %>% 
  mutate(time = "season")
# threeMonth_janzen <- janzify_per_customWithinYear(three_monthly) %>% 
#   mutate(time = "three_month")
sixMonth_janzen <- janzify_per_customWithinYear(six_monthly) %>% 
  mutate(time = "six_month")
annual_janzen <- janzify_per_year(surface_raw) %>% 
  mutate(time = "year")
twoyear_janzen <- janzify_per_customBetweenYears(two_year) %>% 
  mutate(time = "two_year")

janzified <- daily_janzen %>% 
  bind_rows(monthly_janzen) %>% 
  bind_rows(seasonal_janzen) %>% 
  bind_rows(sixMonth_janzen) %>% 
  bind_rows(annual_janzen) %>% 
  bind_rows(twoyear_janzen)

# Re-order factors
time_factors <- c("day", "month", "season", "six_month", "year", "two_year")
janzified <- janzified %>% 
  mutate(time = factor(time, levels = time_factors))
## ....C. Randomly select years to add #########

all_janzified <- janzify(surface_raw)

years <- sample(c(1998:2015), size = 10, replace = FALSE)

# Filter to just sampled years
all_janzified <- all_janzified %>% 
  filter(year %in% years)

# Place flags for each iteration of adding a year
for (i in 1:10) {
  year_i <- all_janzified %>% 
    filter(year %in% years[1:i]) %>% 
    mutate(iteration = i)
  
  if (i == 1) {
    all_janz_plotReady <- year_i
  } else {
    all_janz_plotReady <- bind_rows(all_janz_plotReady, year_i)
  }
}



## 4. Plot data ##############

## Boxplots, 3x panels separated by micro ###########

ggplot(data = janzified, 
       aes(x = time, y = overlap)) +
  geom_boxplot(alpha = 0.5, lwd = .5) +
  labs(title = PLOT_TITLE, subtitle = "Surface temp") +
  theme_bw()

## ....B. Boxplots for random sample #########

ggplot(data = all_janz_plotReady, 
       aes(x = as.factor(iteration), y = overlap)) +
  geom_boxplot(alpha = 0.3, lwd = .5) +
  geom_violin(alpha = 0.5, lwd = .5) +
  labs(title = PLOT_TITLE, subtitle = "Surface temp") +
  xlab("Number of randomly drawn years") +
  theme_bw()

