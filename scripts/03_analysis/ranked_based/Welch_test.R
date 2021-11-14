# David Klinges
# File creation date: 2019-07-11
# This script performs Welch's test on overlap ~ macro and micro data


## 1. Workspace prep ##########
library(tidyverse)
library(onewaytests)

mountains <- read_csv("data/03_compiled/mountains_janz_month_avg.csv")

mountains <- mountains %>% 
  filter(site != "CO")

## NOT going to bother with testing for normality....#######

## Test for heteroscedascity #########

mountains$site <- as.factor(mountains$site)
mountains$micro <- as.factor(mountains$micro)
# Brown-Forsythe Test (https://en.wikipedia.org/wiki/Brown%E2%80%93Forsythe_test)
# does not assume normality
bf.test(TAD ~ site, mountains)
bf.test(TAD ~ micro, mountains)
# Results for both are significant....I'm a little skeptical but I might as well
# not assume homoscedascity


## Welch's: Test for differences in means between groups ########

## ....A. Compare macros ######### 

welch.test(TAD ~ macro, data = mountains, alpha = 0.05, rate = 0.1)

## Compare particular macros

welch.test(TAD ~ macro, data = 
             filter(mountains, macro == "coniferous" | macro == "non-forest"), 
           alpha = 0.05, rate = 0.1)

## ....B. Compare micros ######### 

welch.test(TAD ~ micro, data = mountains, alpha = 0.05, rate = 0.1)

## ....C. Compare foliage ######### 

# Recode foliage 
mountains_foliage <- mountains %>% 
  mutate(foliage = recode(foliage, "leaf-off" = "no foliage"))

welch.test(TAD ~ foliage, data = mountains, alpha = 0.05, rate = 0.1)

## Compare particular macros

welch.test(TAD ~ macro, data = 
             filter(mountains, foliage == "no foliage" | foliage == "leaf-off"), 
           alpha = 0.05, rate = 0.1)
