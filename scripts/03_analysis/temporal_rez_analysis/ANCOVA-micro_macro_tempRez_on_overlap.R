# David Klinges
# this script compares overlap for each microhabitat across the timestep
# regression


## Workspace prep ###############
library(tidyverse)
library(vegan)
library(car)
library(onewaytests)
library(nlme)
library(ppcor)

mountains <- read_csv("data/03_compiled/temporal_rez/mountains_rez_allyears.csv")

## Data cleaning ########

## Remove outliers
mountains <- mountains %>%
  group_by(micro) %>%
  mutate(mean = mean(TAD)) %>% 
  mutate(SD = sd(TAD)) %>% 
  mutate(TAD = ifelse(TAD > (mean + (3 * SD)) | 
                               TAD < (mean - (3 * SD)) , NA, TAD)) %>% 
  mutate(mean = mean(TAD_elevCorr)) %>% 
  mutate(SD = sd(TAD_elevCorr)) %>% 
  mutate(TAD_elevCorr = ifelse(TAD_elevCorr > (mean + (3 * SD)) | 
                        TAD_elevCorr < (mean - (3 * SD)) , NA, TAD_elevCorr)) %>% 
  ungroup()

## Remove NAs
mountains <- mountains %>% 
  filter(complete.cases(TAD))

## After checking nrow of new df vs old df, there are no statistical outliers

## Test assumptions for analysis of variance ##########
## ....A. Normality ##############

ggplot(mountains, aes(TAD)) + 
  geom_histogram(bins = 50) +
  facet_wrap( ~ micro, ncol = 3)
# distributions look fairly normal

# Could simulate some data and test GOF from a normal dist
# for each micro...will do soon

## ....B. Homoscedasticity #######

fligner.test(TAD ~ micro, mountains)

# Returns that variances not equal between groups.

# How much do variances differ?
var(filter(mountains, micro == "soil")$TAD, na.rm = TRUE) / 
  var(filter(mountains, micro == "canopy")$TAD, na.rm = TRUE)

# Not that much....surface variance is 1.4x higher than soil and 1.5x higher than canopy...
# Soil variance is 1.1x higher than canopy

# I tried multiple transformations including turning to ranks, and can't get 
# the variances to homogenized. So because my data is heteroscedastic, we're
# going to use Welch's ANOVA



## Determine value of asymptote for each micro #########

# Source code: https://stackoverflow.com/questions/41290857/evaluating-asymptote-in-r-nls-for-multiple-factors

# Get starting values for asymptotic model
# SSasympOff() is an offset asymptotic model, which I believe means it allows multiple
# curves
fo <- TAD ~ SSasympOff(temporal_rez, A, lrc, c0)
fm3 <- nls(fo, mountains)
# Extract starting values
starts <- coef(fm3)[c("lrc", "c0")]

## ....A. Convert micro, macro, and foliage flags to matrices ###########

# Convert micro flag into matrix of three cols (soil, surface, and canopy) 
# and values of 0s and 1s
micro_matrix <- mountains %>%
  spread(key = "micro", value = "micro") %>% 
  dplyr::select(soil, surface, canopy) %>% 
  # Recode
  mutate_all(function(x) ifelse(is.na(x), 0, 1))
# Convert to matrix, which nls prefers for some reason
micro_matrix <- as.matrix(micro_matrix)

macro_list <- unique(mountains$macro)
# Convert macro flag into matrix with values of 0s and 1s
macro_matrix <- mountains %>%
  spread(key = "macro", value = "macro") %>% 
  dplyr::select(one_of(macro_list)) %>% 
  # Recode
  mutate_all(function(x) ifelse(is.na(x), 0, 1))
# Convert to matrix, which nls prefers for some reason
macro_matrix <- as.matrix(macro_matrix)

foliage_matrix <- mountains %>%
  spread(key = "foliage", value = "foliage") %>% 
  dplyr::select(`leaf-off`, `leaf-on`) %>% 
  # Recode
  mutate_all(function(x) ifelse(is.na(x), 0, 1))
# Convert to matrix, which nls prefers for some reason
foliage_matrix <- as.matrix(foliage_matrix)

microMacro_matrix <- mountains %>%
  mutate(microMacro = paste(micro, macro, foliage, sep = "_")) %>% 
  spread(key = "microMacro", value = "microMacro") %>% 
  dplyr::select(contains("soil"), contains("surface"), contains("canopy")) %>% 
  # Recode
  mutate_all(function(x) ifelse(is.na(x), 0, 1))
# Convert to matrix, which nls prefers for some reason
microMacro_matrix <- as.matrix(microMacro_matrix)

## ....B. Get model fits #########
# Micro
formula_micro <- TAD ~ micro_matrix * SSasympOff(temporal_rez,1,lrc,c0)
# Macro
formula_macro <- TAD ~ macro_matrix * SSasympOff(temporal_rez,1,lrc,c0)
# Foliage
formula_foliage <- TAD ~ foliage_matrix * SSasympOff(temporal_rez,1,lrc,c0)
# Micro, macro, and foliage
formula_microMacro <- TAD ~  microMacro_matrix * SSasympOff(temporal_rez,1,lrc,c0)


fit_micro <- nls(formula_micro, data = mountains, start = starts, 
           algorithm = "plinear")

fit_macro <- nls(formula_macro, data = mountains, start = starts, 
                 algorithm = "plinear")

fit_foliage <- nls(formula_foliage, data = mountains, start = starts, 
                 algorithm = "plinear")

fit_microMacro <- nls(formula_microMacro, data = mountains, start = starts, 
                 algorithm = "plinear")

## ....C. Fix up model asymptote outputs and save ###########
values <- fit_microMacro$m$getAllPars()

microMacro_params <- tibble(
  parameter = rownames(as.data.frame(values)),
  values = values
) %>% 
  arrange(values)

summary(fm5)

cor(mountains$TAD, predict(fm5))

write_csv(microMacro_params, "data/04_analysis/temporal_rez/asymptote_values/microMacroFoliage_asympotote_values.csv")

## ....B. ANCOVA #############

fo <- TAD ~ SSasympOff(temporal_rez, 1, lrc, c0)
fm3 <- nls(fo, data = mountains)

# Extract starting values
starts <- coef(fm3)[c("xmid", "scal")]

fm1 <- gnls(TAD ~ SSlogis(temporal_rez, 1, xmid, scal), 
            data = group_by(temporal_rez, micro),
            weights = varPower())


Soybeantest <- Soybean %>% 
  mutate(Variety = as.character(Variety)) %>% 
  mutate(temporal_rez = runif(412)) %>% 
  mutate(TAD = slice(mountains, 1:412)$TAD)

# Subset data to play around (takes a long time to converge with all sites)
mountains_filtered <- mountains %>% 
  filter(site == "NC" | site == "ID" | site == "CO") %>% 
  mutate(TAD_trans = (TAD)^4)

fm1 <- gnls(TAD ~ SSasympOff(temporal_rez, Asym, lrc, c0), mountains,
            weights = varPower())
summary(fm1)

# variance increases with a power of the absolute fitted values
fm1 <- gnls(TAD ~ SSlogis(temporal_rez, 20, xmid, scal), Soybeantest,
            weights = varPower())
summary(fm1)

fm5 <- glm(TAD ~ temporal_rez * micro * macro * latitude, data = mountains_binary)

ancova_micro <- Anova(fit_microMacro)
ancova_micro <- welch.test(fm5)

## ....B. TAD ~ temporal_rez * macro #############

transformed_model <- lm(TAD_rank ~ temporal_rez * macro, data = mountains)
correlation_macro <- cor(mountains$TAD_rank, predict(transformed_model))

ancova_macro <- welch.test(transformed_model, type="II")

## ....C. TAD ~ temporal_rez * macro * micro #############

nonlinear_model <- nls(TAD ~ SSasympOff(temporal_rez,1,lrc,c0) * macro * micro * latitude, 
                       data = mountains_binary,
                       start = starts, 
                       trace = "TRUE")

transformed_model <- lm(TAD_rank ~ temporal_rez * macro * micro, 
                        data = mountains)
correlation_microMacro <- cor(mountains$TAD_rank, predict(transformed_model))

# You get the warning "Note: model has aliased coefficients sums of squares 
# computed by model comparison"...my guess is that there multicollinearity
# between micros and macros, b/c all canopy micro data is only from certain
# macros (those that are forest of course)
ancova_microMacro <- welch.test(transformed_model, type="II")

## ....D. Filter to certain macros to see which are sig. diff from each other ###########

mountains_filtered <- mountains %>% 
  filter(macro == "deciduous" | macro == "non-forest")

transformed_model <- lm(TAD_rank ~ temporal_rez * macro * micro * site, 
                        data = mountains_filtered)

correlation_microMacro <- cor(mountains_filtered$TAD_rank, predict(transformed_model))
ancova_microMacro <- welch.test(transformed_model, type="II")



## Write out results #############

ancovas <- list(ancova_micro, ancova_macro, ancova_microMacro)
correlations <- c(correlation_micro, correlation_macro, correlation_microMacro)

for (i in 1:length(ancovas)) {
  
  # using as_tibble removed the interaction terms, which are saved as the
  # rownames in the ancova outputs
  ancova_iter <- as.data.frame(ancovas[[i]]) %>% 
    rownames_to_column(var = "interaction_term") %>% 
    mutate(model = paste0("model_", i))

  if (i == 1) {
    ancova_results <- ancova_iter
    
  } else {
    ancova_results <- bind_rows(ancova_results, ancova_iter)
  }
}

ancova_results <- ancova_results %>% 
  mutate(model = ifelse(model == "model_1", "temporal_rez * micro",
                    ifelse(model == "model_2", "temporal_rez * macro",
                       ifelse(model == "model_3", "temporal_rez * micro * macro",
                              NA)))) %>% 
  select(model, interaction_term, everything())


write_csv(ancova_results, "data/04_analysis/temporal_rez/ANCOVA_results.csv")

## Recycling Bin #########
## Transformations ##########

## ....A. Perform transformations ############
temporal_rez <- temporal_rez %>% 
  # Log transform
  mutate(log_overlap = log(TAD)) %>%   
  # Remove inf
  mutate(log_overlap = ifelse(is.infinite(log_overlap), NA, log_overlap)) %>% 
  # Transform overlap to rank order
  mutate(TAD_rank = rank(TAD)) %>% 
  # Rank order, then log transform
  mutate(overlap_rankThenLog = log(TAD_rank)) %>% 
  # Log transform, then rank order
  mutate(overlap_logThenRank = rank(log_overlap))

## ....B. Plot to investigate transformed distribution/fit ###########

ggplot(temporal_rez, aes(TAD_rank)) + 
  geom_histogram(bins = 50) +
  facet_wrap( ~ micro, ncol = 3)
# No longer normal, but uniform

fligner.test(overlap_logThenRank ~ micro, temporal_rez)

plot <- ggplot(temporal_rez, aes(temporal_rez, TAD_rank)) +
  geom_point(aes(color = macro)) +
  geom_smooth(method = "gam", formula = y ~ log(x), linetype = "dashed",
              se = TRUE, show.legend = TRUE)+
  facet_wrap(~micro)

ggsave("transformed3_temporalrez.png")
# Still seems fairly asymptotic

