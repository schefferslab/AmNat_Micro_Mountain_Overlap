# David Klinges
# Script init: 2019-08-06
# This script regresses overlap ~ latitude
# Pulling workspace prep and curation from latitudeVSoverlap.R

## 1. Workspace prep ###########

library(tidyverse)

KDE_overlap <- read_csv("data/04_analysis/compare_density_distribs/KDE_overlap_monthly.csv")
asymptotes <- read_csv("data/04_analysis/temporal_rez/asymptote_values/microMacroFoliage_asympotote_values.csv")
TAD_monthly <- read_csv("data/03_compiled/temporal_rez/temporal_rez_avgyears.csv")
mountains <- read_csv("data/03_compiled/mountains.csv")


## 2. Data curation ##########

KDE_overlap <- KDE_overlap %>% 
  rename(KDE_overlap = overlap)

TAD_monthly <- TAD_monthly %>% 
  filter(temporal_rez == 30) %>% 
  group_by(micro, macro, foliage) %>% 
  summarize(TAD_at_month_timestep = mean(TAD)) %>% 
  ungroup()

asymptotes <- asymptotes %>% 
  mutate(parameter = gsub(".lin.", "", parameter)) %>% 
  filter(parameter != "c0" & parameter != "lrc") %>% 
  separate(parameter, into = c("micro", "macro", "foliage"), sep = "_") %>% 
  rename(asymptote = values)

mountains <- mountains %>% 
  dplyr::select(site, latitude) %>% 
  mutate(latitude = abs(latitude))

overlaps <- KDE_overlap %>% 
  left_join(mountains) %>% 
  left_join(TAD_monthly) %>% 
  left_join(asymptotes) %>% 
  distinct()

## 3. Regression analysis ###########

## ....A. Build models ##########
linear_TADmonthly <- lm(TAD_at_month_timestep ~ latitude, overlaps)
linear_asymptote <- lm(asymptote ~ latitude, overlaps)
linear_KDE <- lm(KDE_overlap ~ latitude, overlaps)


summary_TADmonthly <- summary(linear_TADmonthly)
summary_asymptote <- summary(linear_asymptote)
summary_KDE <- summary(linear_KDE)

## ....B. Save outputs ###########

lm_outputs <- function(summary) {
  # Some extracting for overall model p value
  f <- summary$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  
  results <- tibble(
    rSquared = summary$adj.r.squared,
    coefficient = rownames(summary$coefficients),
    coefficient_estimate = summary$coefficients[,1],
    coefficient_SE = summary$coefficients[,2],
    coefficient_t_value = summary$coefficients[,3],
    coefficient_P_value = summary$coefficients[,4],
    f_statistic = summary$fstatistic[1],
    overall_P_value = p
  )
return(results)
}

results_TADmonthly <- lm_outputs(summary_TADmonthly) %>% 
  mutate(independent = "latitude", dependent = "TADmonthly")

results_asymptote <- lm_outputs(summary_asymptote) %>% 
  mutate(independent = "latitude", dependent = "asymptote")

results_KDE <- lm_outputs(summary_KDE) %>% 
  mutate(independent = "latitude", dependent = "KDE_overlap")

results <- results_TADmonthly %>% 
  bind_rows(results_asymptote) %>% 
  bind_rows(results_KDE) %>% 
  dplyr::select(independent, dependent, everything())


## 4. Write out results #########

write_csv(results, "data/04_analysis/latitude/latitudeVSoverlap.csv")
