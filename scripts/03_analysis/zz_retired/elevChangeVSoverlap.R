# David Klinges
# File creation date: 2019-07-28
# This script determines the best fit model of elevation change on overlap
# RETIRED this script on 2019-08-07 because redundant with correct_elevChange.R
# I think I was only using this script to test out code to be implemented in
# correct_elevChange.R


## 1. Workspace prep ############

library(tidyverse)

mountains <- read_csv("data/03_compiled/mountains_janz_month_avg.csv")
mountains <- read_csv("data/03_compiled/temporal_rez/mountains_rez_allyears.csv")

## 2. Data prep ##########
# Average to the site and micro level to reduce noise and auto-correlation
# for model fitting

mountains <- mountains %>% 
  # Remove NA rows
  filter(complete.cases(janzenDscore))

mountains_avg <- mountains %>% 
  group_by(site, micro, elevation_change, foliage) %>% 
  summarize_at(vars(TAD, janzenDscore), mean)

mountains_avg <- mountains %>% 
  filter(temporal_rez == 30)

## 3. Regression analyses ##############

## ....A. D-scores ##########

# Visualized data to inspect
ggplot(mountains_avg, aes(elevation_change, TAD)) + 
  geom_point()

# linear fit
linear_dscore <- lm(TAD ~ elevation_change, mountains_avg)

cor(mountains_avg$janzenDscore, predict(linear_dscore))

plot(mountains_avg$elevation_change, mountains_avg$janzenDscore)
lines(mountains_avg$elevation_change, predict(linear_dscore),lty=2,col="red",lwd=3)

plot(mountains$elevation_change, mountains$TAD)
lines(mountains$elevation_change, predict(quadratic_fit),lty=2,col="red",lwd=3)
cor(mountains_avg$TAD, predict(quadratic_fit))

# quadratic fit
quadratic_dscore <- nls(janzenDscore ~ a + (b * elevation_change) + (c * elevation_change^2), 
                 start = list(a = 0.54, b = .00048, c = .0000005),
                 data = mountains_avg)
quadratic_dscore
cor(mountains_avg$janzenDscore, predict(quadratic_dscore))

plot(mountains_avg$elevation_change, mountains_avg$janzenDscore)
lines(mountains_avg$elevation_change, predict(quadratic_dscore),lty=2,col="red",lwd=3)

## ....B. TAD ########

# Visualized data to inspect
ggplot(mountains_avg, aes(elevation_change, TAD)) + 
  geom_point()

# linear fit
linear_TAD <- lm(TAD ~ elevation_change, mountains_avg)
linear_TAD
cor(mountains_avg$TAD, predict(linear_TAD))

plot(mountains_avg$elevation_change, mountains_avg$TAD)
lines(mountains_avg$elevation_change, predict(linear_TAD),lty=2,col="red",lwd=3)

# quadratic fit
quadratic_TAD <- nls(TAD ~ a + (b * elevation_change) + (c * elevation_change^2), 
                 start = list(a = 0.54, b = .00048, c = .0000005),
                 data = mountains_avg)
quadratic_TAD
cor(mountains_avg$TAD, predict(quadratic_TAD))

plot(mountains_avg$elevation_change, mountains_avg$TAD)
lines(mountains_avg$elevation_change, predict(quadratic_TAD),lty=2,col="red",lwd=3)
## 4. Save residuals ##########

# Note: we fit models using average values for each site-micro, but we don't want
# to use these averages in subsequent analyses. So we'll need to save the residuals
# from models of disaggregated data

# quadratic fit
quadratic_dscore <- nls(janzenDscore ~ a + (b * elevation_change) + (c * elevation_change^2), 
                        start = list(a = 0.54, b = .00048, c = .0000005),
                        data = mountains)

quadratic_TAD <- nls(TAD ~ a + (b * elevation_change) + (c * elevation_change^2), 
                        start = list(a = 0.54, b = .00048, c = .0000005),
                        data = mountains)


mountains <- mountains %>% 
  filter(complete.cases(janzenDscore)) %>% 
  mutate(janzenDscore_elevCont = quadratic_dscore$m$resid()) %>% 
  mutate(TAD_elevCont = quadratic_TAD$m$resid())

## 5. Write out data #########

write_csv(mountains, "data/03_compiled/elevation_controlled/mountains_temporalRez_elevCorrected.csv")
