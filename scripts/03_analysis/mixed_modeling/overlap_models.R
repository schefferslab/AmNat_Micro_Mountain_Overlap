
# David Klinges
# 2019-08-28
# This script constructs and compares linear mixed models of overlap

## 1. Workspace Prep ###############

# Data curation
library(tidyverse)
library(lubridate)
# Visualization
library(ggplot2)
# Distribution fitting
library(fitdistrplus)
## Analysis
# building linear models
library(lme4)
library(gamlss)
library(mgcv)
library(glmmTMB)
# model selection
library(AICcmodavg)
library(memisc)
# multi-model inference
library(MuMIn)


mountains_monthly <- read_csv("data/03_compiled/mountains_overlap_monthly_avg.csv",
                              col_types = cols(snow_source_flag = col_character(),
                                               snowdepth = col_double()))


test <- mountains_janz_avg_monthly %>%
  dplyr::select(site, micro, month, TAD, TAD_elevCorr, kde, kde_elevCorr, janzenDscore, janzenDscore_elevCorr) %>%
  dplyr::rename(TAD_new = TAD, TAD_elevCorr_new = TAD_elevCorr, kde_new = kde,
            kde_elevCorr_new = kde_elevCorr, janzenDscore_new = janzenDscore,
            janzenDscore_elevCorr_new = janzenDscore_elevCorr) %>%
  full_join(dplyr::select(mountains_monthly, site, micro, month, TAD, TAD_elevCorr, kde,
                          kde_elevCorr, janzenDscore, janzenDscore_elevCorr))

ggplot(test, aes(TAD_new, TAD)) +
  geom_point(aes(color = site)) +
  geom_abline(intercept = 0, slope = 1) +
  lims(x = c(0, 40),
       y = c(0, 40))

mountains_monthly_all <- read_csv("data/03_compiled/mountains_overlap_monthly.csv",
                              col_types = cols(snow_source_flag = col_character(),
                                               snowdepth = col_double()))

mountains_daily <- read_csv("data/03_compiled/mountains_overlap_daily_avg.csv",
                            col_types = cols(snow_source_flag = col_character(),
                                             snowdepth = col_double()))


mountains_minmax <- read_csv("data/03_compiled/mountains.csv",
                             col_types = cols(
                               month = col_double()
                             ))

## 2. Data curation ###############

## ....A. Does timeseries length/representation bias results? ###########

# Count number of unique days per site
unique_days <- mountains_minmax %>% 
  dplyr::select(site, year, julian, micro) %>% 
  distinct() %>% 
  group_by(site, year, micro) %>% 
  count() %>% 
  dplyr::rename(n_days = n) %>% 
  group_by(site, micro) %>% 
  # Average number days across timeseries
  summarize(n_days = mean(n_days)) %>% 
  ungroup()

unique_month <- mountains_monthly_all %>% 
  dplyr::select(site, year, month, micro) %>% 
  distinct() %>% 
  group_by(site, year, micro) %>% 
  count() %>% 
  dplyr::rename(n_months = n) %>% 
  group_by(site, micro) %>% 
  # Average number months across timeseries
  summarize(n_months = mean(n_months)) %>% 
  ungroup()

unique_season <- mountains_monthly_all %>% 
  mutate(season = ifelse(month %in% c(12, 1, 2), "winter",
                  ifelse(month %in% c(3, 4, 5), "spring",
                   ifelse(month %in% c(6, 7, 8), "summer",
                   ifelse(month %in% c(9, 10, 11), "fall", NA))))) %>% 
  dplyr::select(site, year, season, micro) %>% 
  distinct() %>% 
  group_by(site, year, micro) %>% 
  count() %>% 
  dplyr::rename(n_seasons = n) %>% 
  group_by(site, micro) %>% 
  # Average number days across timeseries
  summarize(n_seasons = mean(n_seasons)) %>% 
  ungroup()

unique_year <- mountains_minmax %>% 
  dplyr::select(site, year, micro) %>% 
  distinct() %>% 
  group_by(site, micro) %>% 
  count() %>% 
  dplyr::rename(n_years = n)

mountains_monthly <- mountains_monthly %>% 
  left_join(unique_days) %>% 
  left_join(unique_month) %>% 
  left_join(unique_season) %>% 
  left_join(unique_year)

## ....B. Center and scale ##############

mountain_curation <- function(mountains) {
  
  mountains <- mountains %>% 
    # Take absolute value of latitude
    mutate(latitude = abs(latitude)) %>% 
    # Coerce all character attributes into factors
    mutate_if(is.character, as.factor) %>% 
    # Multiply foliage and veg for a new index....but if foliage cover is zero,
    # i.e. for non-forest, just use the veg_structure value
    mutate(veg_foliage = ifelse(foliage_cover > 0, foliage_cover * veg_structure,
                                veg_structure)) %>% 

    # Re-scale all continuous parameters
    mutate_at(vars(month, elevation_change, latitude, veg_structure, foliage_cover, 
                   veg_foliage, snowdepth, height, n_days, n_months, n_seasons, n_years), 
              ~ scales::rescale(., to = c(0, 1))) %>% 
    
    filter(complete.cases(TAD)) %>% 
    filter(complete.cases(janzenDscore)) %>% 

    # Exclude statistical outliers, if you wish 
    # mutate(TAD = ifelse(TAD > (mean(TAD) + (3 * sd(TAD))) | 
    #                       TAD < (mean(TAD) - (3 * sd(TAD))), NA, TAD)) %>% 
    # mutate(janzenDscore = ifelse(
    #   janzenDscore > (mean(janzenDscore) + (3 * sd(janzenDscore))) | 
    #  janzenDscore < (mean(janzenDscore) - (3 * sd(janzenDscore))), NA, janzenDscore)) %>% 
    # mutate(TAD_elevCorr = ifelse(TAD_elevCorr > (mean(TAD_elevCorr) + (3 * sd(TAD_elevCorr))) | 
    #                       TAD_elevCorr < (mean(TAD_elevCorr) - (3 * sd(TAD_elevCorr))), NA, TAD_elevCorr)) %>% 
    # mutate(janzenDscore_elevCorr = ifelse(
    #   janzenDscore_elevCorr > (mean(janzenDscore_elevCorr) + (3 * sd(janzenDscore_elevCorr))) | 
    #     janzenDscore_elevCorr < (mean(janzenDscore_elevCorr) - (3 * sd(janzenDscore_elevCorr))), NA, janzenDscore_elevCorr)) %>% 
    
    
    # Transform TAD to play with distributions
    filter(complete.cases(TAD)) %>% 
    mutate(TAD_cuberoot = TAD ^ 0.33) %>% 
    mutate(TAD_elevCorr_cuberoot = TAD_elevCorr ^ 0.33) %>% 
    mutate(TAD_positive = TAD - min(TAD, na.rm = TRUE) + 0.01) %>% 
    mutate(TAD_elevCorr_positive = TAD_elevCorr - min(TAD_elevCorr, na.rm = TRUE) + 0.01) %>% 
    
    # Transform Janzen D scores
    filter(complete.cases(janzenDscore)) %>% 
    mutate(janzenDscore_cuberoot = janzenDscore ^ 0.33) %>% 
    mutate(janzenDscore_elevCorr_cuberoot = janzenDscore_elevCorr ^ 0.33) %>% 
    mutate(janzenDscore_positive = janzenDscore - min(janzenDscore, na.rm = TRUE) + 0.01) %>% 
    mutate(janzenDscore_elevCorr_positive = janzenDscore_elevCorr - min(janzenDscore_elevCorr, na.rm = TRUE) + 0.01)
  
  
  mountains
}

# mountains_daily <- mountain_curation(mountains_daily)
mountains_monthly <- mountain_curation(mountains_monthly)

# Calculate minmax variance
minmax_curation <- function(minmax) {
  minmax <- minmax %>% 
    # filter out Inf's
    filter(is.finite(min) & is.finite(max)) %>% 
    mutate(minmax = max - min) %>% 
    mutate(veg_foliage = ifelse(foliage_cover > 0, foliage_cover * veg_structure,
                                veg_structure)) %>% 
    
    # Re-scale all continuous parameters
    mutate_at(vars(latitude, veg_structure, foliage_cover, 
                   veg_foliage, snowdepth, height), 
              ~ scales::rescale(., to = c(-1, 1)))
}

mountains_minmax <- minmax_curation(mountains_minmax)

## 3. Test assumptions ##############

## ....A. Determine distribution #############

## Overlap TAD monthly
ggplot(mountains_monthly, aes(TAD)) +
  geom_histogram()

ggplot(mountains_monthly, aes(TAD_elevCorr)) +
  geom_histogram()

descdist(filter(mountains_monthly, complete.cases(TAD))$TAD, boot=1000)
# descdist((mountains_monthly$TAD_elevCorr), boot=1000)
# descdist((mountains_monthly$TAD_elevCorr_positive), boot=1000)

## Overlap D-score monthly
# ggplot(mountains_monthly, aes(janzenDscore)) +
#   geom_histogram()
# 
# descdist((mountains_monthly$janzenDscore), boot=1000)

ggplot(mountains_monthly, aes(kde)) +
  geom_histogram()

ggplot(mountains_monthly, aes(kde_elevCorr)) +
  geom_histogram()

descdist((filter(mountains_monthly, complete.cases(kde))$kde), boot=1000)

## ....B. Compare goodness-of-fit of distributions #########

tad_fit <- mountains_monthly %>% 
  filter(complete.cases(TAD))
fit_norm <- fitdist(tad_fit$TAD, "norm")
fit_uniform <- fitdist(tad_fit$TAD, "unif")

out <- gofstat(list(fit_norm, fit_uniform), fitnames = c( "normal","uniform"))

janzenDscore_fit <- mountains_monthly %>% 
  filter(complete.cases(janzenDscore))
fit_norm <- fitdist(janzenDscore_fit$janzenDscore, "norm")
fit_uniform <- fitdist(janzenDscore_fit$janzenDscore, "unif")

out <- gofstat(list(fit_norm, fit_uniform), fitnames = c( "normal","uniform"))

kde_fit <- mountains_monthly %>% 
  filter(complete.cases(kde)) %>% 
  # source: https://stats.stackexchange.com/questions/31300/dealing-with-0-1-values-in-a-beta-regression
  # original paper: https://psycnet.apa.org/record/2006-03820-004?casa_token=Cm0U6hUUVN8AAAAA:Z4LgVnNgI2cRK_p1PbjU0rbZRUaTQraB7Z_MKvzNq5msfXbz3_iHrLM6lhDNwvKkbC5kKiE2lEoAeoefV4vwu94
  mutate(kde_trans = (kde * (length(kde)- 1) + 0.5)/length(kde))

fit_norm <- fitdist(kde_fit$kde_trans, "norm")
fit_lognorm <- fitdist(kde_fit$kde_trans, "lnorm")
fit_uniform <- fitdist(kde_fit$kde_trans, "unif")
fit_beta <- fitdist(kde_fit$kde_trans, "beta")

out <- gofstat(list(fit_norm, fit_lognorm, fit_uniform, fit_beta), 
               fitnames = c( "normal", "lognormal", "uniform", "beta"))

## LMM overlap wrapper #################

lmm_overlap <- function(
  mountains, # The input dataset
  CHOSEN_OVERLAP_ATTRIBUTE, # What measurement of overlap are you using
  RESOLUTION, # What temporal resultion is overlap calculated across?
  PARAMS_LIST, # A vector of the params you want to include in the model
  GLOBAL_FORMULA,
  MODEL_GROUP_NAME, # Describe anything particular about the model: corresponds
  SUMMARY_FILE_NAME, # Name of the model summary file you wish to write
  CONFIDENCE_INT # Do you want to calculate confidence intervals of model 
  # parameters? If interval estimates fail, will bootstrap for estimates
  # to the sub-directory that the outputs will reside in
) {
  ## 1. Necessary Curation ####
  ## ....A. Select down to just attributes that are model parameters ########
  
  mountains <- mountains %>% 
    dplyr::select(PARAMS_LIST)
  
  ## ....B. Filter out NAs in data ##############
  
  # Because we want to compare models that include snow, we need to remove
  # observations that don't have any snow
  mountains <- mountains %>%
    filter_all(complete.cases)
  
  ## ....C. Re-code characters as factors ##########
  
  mountains <- mountains %>% 
    mutate_if(is.character, as.factor)
  
  ## Model for overlap: all data ###########
  ## 2. Build your candidate models #############
  
  # Global is all of your parameters
  # Set global model contingent on what params were handed off to wrapper
  # Separating models with elevation change because uncorrected TAD scores
  # are gaussian, while elevation-corrected scores are log-normal
    
  print(unique(mountains$site))
  
  # If not checking for study duration bias....
  if (!grepl("n_", MODEL_GROUP_NAME)) {
    if ("elevation_change" %in% colnames(mountains)) {
      
      print("Fitting Macrogeography model 1: overlap ~ elevation_change + (1|site)...")
      linear_macro1 <- glmer(overlap ~ elevation_change +
                               (1|site),
                             na.action = na.fail,
                             data = mountains,
                             family = gaussian(link='identity'))
      
      print("Fitting Macrogeography model 2: overlap ~ elevation_change + latitude + (1|site)...")
      linear_macro2 <- glmer(overlap ~ elevation_change +
                               latitude +
                               (1|site),
                             na.action = na.fail,
                             data = mountains,
                             family = gaussian(link='identity'))
      
      print("Fitting Macrogeography model 3: overlap ~ latitude + (1|site)...")
      linear_macro3 <- glmer(overlap ~ latitude + (1|site),
                             na.action = na.fail,
                             data = mountains,
                             family = gaussian(link='identity'))
      
      print("Fitting Mesogeography model: overlap ~ elevation_change + veg_foliage + snowdepth (1|site)...")
      linear_meso <- glmer(overlap ~ elevation_change +
                             veg_foliage + snowdepth + (1|site),
                           na.action = na.fail,
                           data = mountains,
                           family = gaussian(link='identity'))
      
      linear_micro <- glmer(overlap ~ height +
                              (1|site),
                            na.action = na.fail,
                            data = mountains,
                            family = gaussian(link='identity'))
      
      print("Fitting global model...")
      linear_global <- glmer(GLOBAL_FORMULA,
                             na.action = na.fail,
                             data = mountains,
                             family = gaussian(link='identity'))
    }
    
    if (!"elevation_change" %in% colnames(mountains)) {
      
      print("Fitting global model...")
      linear_global <- glmer(GLOBAL_FORMULA,
                             na.action = na.fail,
                             data = mountains,
                             family = Gamma(link='identity'))
    }
  }

  # If checking for study duration bias....
  if (grepl("n_", MODEL_GROUP_NAME)) {
      if ("n_days" %in% PARAMS_LIST) {
        print("Fitting Macrogeography model 1: overlap ~ elevation_change + n_days + (1|site)...")
        linear_macro1 <- glmer(overlap ~ elevation_change +
                                 n_days + (1|site),
                               na.action = na.fail,
                               data = mountains,
                               family = gaussian(link='identity'))
        
        print("Fitting Macrogeography model 2: overlap ~ elevation_change + latitude + n_days + (1|site)...")
        linear_macro2 <- glmer(overlap ~ elevation_change +
                                 latitude +
                                 n_days + (1|site),
                               na.action = na.fail,
                               data = mountains,
                               family = gaussian(link='identity'))
        
        print("Fitting Macrogeography model 3: overlap ~ latitude + n_days + (1|site)...")
        linear_macro3 <- glmer(overlap ~ latitude + n_days + (1|site),
                               na.action = na.fail,
                               data = mountains,
                               family = gaussian(link='identity'))
        
        print("Fitting Mesogeography model: overlap ~ elevation_change + veg_foliage + snowdepth n_days + (1|site)...")
        linear_meso <- glmer(overlap ~ elevation_change +
                               veg_foliage + snowdepth + n_days + (1|site),
                             na.action = na.fail,
                             data = mountains,
                             family = gaussian(link='identity'))
        
        linear_micro <- glmer(overlap ~ height +
                                n_days + (1|site),
                              na.action = na.fail,
                              data = mountains,
                              family = gaussian(link='identity'))
        
        print("Fitting global model...")
        linear_global <- glmer(GLOBAL_FORMULA,
                               na.action = na.fail,
                               data = mountains,
                               family = gaussian(link='identity'))
      }
      if ("n_months" %in% PARAMS_LIST) {
        print("Fitting Macrogeography model 1: overlap ~ elevation_change + n_months + (1|site)...")
        linear_macro1 <- glmer(overlap ~ elevation_change +
                                 n_months + (1|site),
                               na.action = na.fail,
                               data = mountains,
                               family = gaussian(link='identity'))
        
        print("Fitting Macrogeography model 2: overlap ~ elevation_change + latitude + n_months + (1|site)...")
        linear_macro2 <- glmer(overlap ~ elevation_change +
                                 latitude +
                                 n_months + (1|site),
                               na.action = na.fail,
                               data = mountains,
                               family = gaussian(link='identity'))
        
        print("Fitting Macrogeography model 3: overlap ~ latitude + n_months + (1|site)...")
        linear_macro3 <- glmer(overlap ~ latitude + n_months + (1|site),
                               na.action = na.fail,
                               data = mountains,
                               family = gaussian(link='identity'))
        
        print("Fitting Mesogeography model: overlap ~ elevation_change + veg_foliage + snowdepth n_months + (1|site)...")
        linear_meso <- glmer(overlap ~ elevation_change +
                               veg_foliage + snowdepth + n_months + (1|site),
                             na.action = na.fail,
                             data = mountains,
                             family = gaussian(link='identity'))
        
        linear_micro <- glmer(overlap ~ height +
                                n_months + (1|site),
                              na.action = na.fail,
                              data = mountains,
                              family = gaussian(link='identity'))
        
        print("Fitting global model...")
        linear_global <- glmer(GLOBAL_FORMULA,
                               na.action = na.fail,
                               data = mountains,
                               family = gaussian(link='identity'))
      }
      if ("n_seasons" %in% PARAMS_LIST) {
        print("Fitting Macrogeography model 1: overlap ~ elevation_change + n_seasons + (1|site)...")
        linear_macro1 <- glmer(overlap ~ elevation_change +
                                 n_seasons + (1|site),
                               na.action = na.fail,
                               data = mountains,
                               family = gaussian(link='identity'))
        
        print("Fitting Macrogeography model 2: overlap ~ elevation_change + latitude + n_seasons + (1|site)...")
        linear_macro2 <- glmer(overlap ~ elevation_change +
                                 latitude +
                                 n_seasons + (1|site),
                               na.action = na.fail,
                               data = mountains,
                               family = gaussian(link='identity'))
        
        print("Fitting Macrogeography model 3: overlap ~ latitude + n_seasons + (1|site)...")
        linear_macro3 <- glmer(overlap ~ latitude + n_seasons + (1|site),
                               na.action = na.fail,
                               data = mountains,
                               family = gaussian(link='identity'))
        
        print("Fitting Mesogeography model: overlap ~ elevation_change + veg_foliage + snowdepth n_seasons + (1|site)...")
        linear_meso <- glmer(overlap ~ elevation_change +
                               veg_foliage + snowdepth + n_seasons + (1|site),
                             na.action = na.fail,
                             data = mountains,
                             family = gaussian(link='identity'))
        
        linear_micro <- glmer(overlap ~ height +
                                n_seasons + (1|site),
                              na.action = na.fail,
                              data = mountains,
                              family = gaussian(link='identity'))
        
        print("Fitting global model...")
        linear_global <- glmer(GLOBAL_FORMULA,
                               na.action = na.fail,
                               data = mountains,
                               family = gaussian(link='identity'))
      }
      if ("n_years" %in% PARAMS_LIST) {
        print("Fitting Macrogeography model 1: overlap ~ elevation_change + n_years + (1|site)...")
        linear_macro1 <- glmer(overlap ~ elevation_change +
                                 n_years + (1|site),
                               na.action = na.fail,
                               data = mountains,
                               family = gaussian(link='identity'))
        
        print("Fitting Macrogeography model 2: overlap ~ elevation_change + latitude + n_years + (1|site)...")
        linear_macro2 <- glmer(overlap ~ elevation_change +
                                 latitude +
                                 n_years + (1|site),
                               na.action = na.fail,
                               data = mountains,
                               family = gaussian(link='identity'))
        
        print("Fitting Macrogeography model 3: overlap ~ latitude + n_years + (1|site)...")
        linear_macro3 <- glmer(overlap ~ latitude + n_years + (1|site),
                               na.action = na.fail,
                               data = mountains,
                               family = gaussian(link='identity'))
        
        print("Fitting Mesogeography model: overlap ~ elevation_change + veg_foliage + snowdepth n_years + (1|site)...")
        linear_meso <- glmer(overlap ~ elevation_change +
                               veg_foliage + snowdepth + n_years + (1|site),
                             na.action = na.fail,
                             data = mountains,
                             family = gaussian(link='identity'))
        
        linear_micro <- glmer(overlap ~ height +
                                n_years + (1|site),
                              na.action = na.fail,
                              data = mountains,
                              family = gaussian(link='identity'))
        
        print("Fitting global model...")
        linear_global <- glmer(GLOBAL_FORMULA,
                               na.action = na.fail,
                               data = mountains,
                               family = gaussian(link='identity'))
      }
    }

  # In case you want to model beta distribution for kde:
  # linear_macro1 <- glmmTMB(overlap ~ elevation_change +
  #                          n_days + (1|site),
  #                        na.action = na.fail,
  #                        data = mountains,
  #                        family = list(family="beta",link="logit"))
  
  # Conduct model dredging
  c(na.action = "na.fail")
  # MuMIn
  print("Conducting model dredging on global model...")
  candidate_list <- dredge(global.model = linear_global)

  # If you want to interpret the candidate list itself, look at the models 
  # with lowest AIC (which will be at the top of the DF) and see what isn't 
  # NA in that model. Important parameters will be in all of the top-performing
  # models (i.e. will not be NA in the first few rows)
  
  ## 3. Model averaging ###################

  # Models with delta AIC less than 4
  # MuMIn
  print("Conducting model averaging...")
  
  # If only one model performs well, inform the user
  good_models <- candidate_list %>% 
    filter(delta < 4)
  
  if (nrow(good_models) == 1) {
    print("NOTE: only one model performs well, therefore only carrying forward this best model")
    model_avg_delta4 <- linear_global
  } 
  
  if (nrow(good_models) > 1) {
    model_avg_delta4 <- model.avg(candidate_list, subset = delta < 4)
  }

  # ## 4. Model predictions ###########
  # 
  # model_avg_predictions <- predict(model_avg_delta4)
  # 
  # averaged.subset <- predict(avgm, newdata, full = TRUE)
  # averaged.subset <- predict(avgm, newdata, full = FALSE)
  # 
  # 
  # model_avg_delta4_preds<-predict(model_avg_delta4, type="response")
  # linear_noVegStructure_preds
  # coef(linear_noVegStructure)
  # 
  # # Now plot this stuff
  # 
  ## 5. Write out models and comparison ##########

  print("Saving outputs from model dreding/averaging...")
  
  saveRDS(model_avg_delta4, file = paste0("data/04_analysis/model_outputs/",
                                   CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, "/",
                                   MODEL_GROUP_NAME, "/global_avg_model.rds"))

  saveRDS(mountains, file = paste0("data/04_analysis/model_outputs/",
                                          CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, "/",
                                          MODEL_GROUP_NAME, "/mountains.rds"))

    
  # Write out dredge candidate list
  write_csv(candidate_list, paste0("data/04_analysis/model_outputs/",
                                   CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, "/",
                                   MODEL_GROUP_NAME, "/dredged_candidate_list.csv"))

  ## ....A. Print out averaged model #####################
  # Save various components of the averaged model
    summary <- summary(model_avg_delta4)
  
  if (CONFIDENCE_INT) {
    print("...calculting confidence intervals of averaged model...")
    confidence <- try(confint(model_avg_delta4))

    if (class(confidence) == "try-error") {
      confidence <- try(confint(model_avg_delta4, method = "boot"))
    }

  }

  sink(paste0("data/04_analysis/model_outputs/",
              CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, "/",
              MODEL_GROUP_NAME, "/", SUMMARY_FILE_NAME))

  print(summary)
  
  # Save coefficients to model output table
  if(class(summary$coefmat.full) == "matrix") {
    coefficients <- as.data.frame(summary$coefmat.full) %>% 
      dplyr::rename(estimate = Estimate, SE = `Std. Error`) %>%
      rownames_to_column()
  } else {
    coefficients <- as.data.frame(summary$coefficients) %>% 
      dplyr::rename(estimate = Estimate, SE = `Std. Error`) %>%
      rownames_to_column()
  }

  
  conf_df <- as.data.frame(confidence) %>% 
    rownames_to_column()
  
  out_results_global <- full_join(coefficients, conf_df) %>% 
    mutate(model = "global")
  
  write_csv(out_results_global, paste0("data/04_analysis/model_outputs/",
                                      CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, "/",
                                      MODEL_GROUP_NAME, "/", "model_results_global.csv"))

  if(CONFIDENCE_INT) {
    print("Parameter Confidence Intervals")
    print(confidence)
  }
  sink()
  # Ending the sink does not seem to release the file from R, so closing all
  # connections just in case. Probably has something to do with wrapping this all
  # in a function?
  closeAllConnections()

  ## ....B. Print out macro model 1 ##################

  if(exists("linear_macro1")) {
    print("Saving outputs from macrogeography model 1...")
    
    # Save various components of the averaged model
    summary <- summary(linear_macro1)
    print("...calculting confidence intervals of macrogeography model 1...")
    confidence <- try(confint(linear_macro1))
    
    if (class(confidence) == "try-error") {
      confidence <- try(confint(linear_macro1, method = "boot"))
    }
    
    # Now calculate confidence intervals manually
    # Extract coefficient estimates and SE's
    coeff_table <- as.data.frame(summary(linear_macro1)[10])
    coeffs <- coeff_table[,1]
    st_error <- coeff_table[,2]
    # Calc CI
    confidence_manual <- tibble(
      param = rownames(coeff_table),
      coeff = coeffs,
      st_error = st_error,
      CI_95_low = coeffs - (st_error * 1.96),
      CI_95_high = coeffs + (st_error * 1.96)
    )
    
    sink(paste0("data/04_analysis/model_outputs/",
                CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, "/",
                MODEL_GROUP_NAME, "/", "macro_model_1.txt"))

    print(summary)
    
    print("Parameter Confidence Intervals using confint()")
    print(confidence)
    
    print("Parameter Confidence Intervals calculated by hand")
    print(confidence_manual)
    
    sink()
    # Ending the sink does not seem to release the file from R, so closing all
    # connections just in case. Probably has something to do with wrapping this all
    # in a function?
    closeAllConnections()

    # Save coefficients to model output table
    coefficients <- as.data.frame(summary$coefficients) %>% 
      dplyr::rename(estimate = Estimate, SE = `Std. Error`) %>%
      rownames_to_column()
    
    conf_df <- as.data.frame(confidence) %>% 
      rownames_to_column()
    
    out_results_macro1 <- full_join(coefficients, conf_df) %>% 
      mutate(model = "macro1")
    
    # write_csv(out_results_macro1, paste0("data/04_analysis/model_outputs/",
    #                                      CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, "/",
    #                                      MODEL_GROUP_NAME, "/", "model_results_macro1.csv"))

  }

  ## ....C. Print out macro model 2 ##################

  if(exists("linear_macro2")) {
    print("Saving outputs of macrogeography model 2...")
    # Save various components of the averaged model
    summary <- summary(linear_macro2)
    print("...calculting confidence intervals of macrogeography model 2...")
    confidence <- try(confint(linear_macro2))
    
    if (class(confidence) == "try-error") {
      
      confidence <- try(confint(linear_macro2, method = "boot"))
    }
    
    # Now calculate confidence intervals manually
    # Extract coefficient estimates and SE's
    coeff_table <- as.data.frame(summary(linear_macro2)[10])
    coeffs <- coeff_table[,1]
    st_error <- coeff_table[,2]
    # Calc CI
    confidence_manual <- tibble(
      param = rownames(coeff_table),
      coeff = coeffs,
      st_error = st_error,
      CI_95_low = coeffs - (st_error * 1.96),
      CI_95_high = coeffs + (st_error * 1.96)
    )
    
    sink(paste0("data/04_analysis/model_outputs/",
                CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, "/",
                MODEL_GROUP_NAME, "/", "macro_model_2.txt"))

    print(summary)
    
    print("Parameter Confidence Intervals using confint()")
    print(confidence)
    
    print("Parameter Confidence Intervals calculated by hand")
    print(confidence_manual)
    
    sink()
    # Ending the sink does not seem to release the file from R, so closing all
    # connections just in case. Probably has something to do with wrapping this all
    # in a function?
    closeAllConnections()

    # Save coefficients to model output table
    coefficients <- as.data.frame(summary$coefficients) %>% 
      dplyr::rename(estimate = Estimate, SE = `Std. Error`) %>%
      rownames_to_column()
    
    conf_df <- as.data.frame(confidence) %>% 
      rownames_to_column()
    
    out_results_macro2 <- full_join(coefficients, conf_df) %>% 
      mutate(model = "macro2")
    
    # write_csv(out_results_macro2, paste0("data/04_analysis/model_outputs/",
    #                                      CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, "/",
    #                                      MODEL_GROUP_NAME, "/", "model_results_macro2.csv"))
    

  }

  ## ....D. Print out macro model 3 ##################
  
  if(exists("linear_macro3")) {
    print("Saving outputs of macrogeography model 3...")
    # Save various components of the averaged model
    summary <- summary(linear_macro3)
    print("...calculting confidence intervals of macrogeography model 3...")
    confidence <- try(confint(linear_macro3))
    
    if (class(confidence) == "try-error") {
      
      confidence <- try(confint(linear_macro3, method = "boot"))
    }
    
    # Now calculate confidence intervals manually
    # Extract coefficient estimates and SE's
    coeff_table <- as.data.frame(summary(linear_macro3)[10])
    coeffs <- coeff_table[,1]
    st_error <- coeff_table[,2]
    # Calc CI
    confidence_manual <- tibble(
      param = rownames(coeff_table),
      coeff = coeffs,
      st_error = st_error,
      CI_95_low = coeffs - (st_error * 1.96),
      CI_95_high = coeffs + (st_error * 1.96)
    )
    
    sink(paste0("data/04_analysis/model_outputs/",
                CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, "/",
                MODEL_GROUP_NAME, "/", "macro_model_3.txt"))
    
    print(summary)
    
    print("Parameter Confidence Intervals using confint()")
    print(confidence)
    
    print("Parameter Confidence Intervals calculated by hand")
    print(confidence_manual)
    
    sink()
    # Ending the sink does not seem to release the file from R, so closing all
    # connections just in case. Probably has something to do with wrapping this all
    # in a function?
    closeAllConnections()
    
    # Save coefficients to model output table
    coefficients <- as.data.frame(summary$coefficients) %>% 
      dplyr::rename(estimate = Estimate, SE = `Std. Error`) %>%
      rownames_to_column()
    
    conf_df <- as.data.frame(confidence) %>% 
      rownames_to_column()
    
    out_results_macro3 <- full_join(coefficients, conf_df) %>% 
      mutate(model = "macro")
    
    write_csv(out_results_macro3, paste0("data/04_analysis/model_outputs/",
                                         CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, "/",
                                         MODEL_GROUP_NAME, "/", "model_results_macro.csv"))
  }
  
  ## ....E. Print out meso model 3 ##################
  
  if(exists("linear_meso")) {
    print("Saving outputs of macrogeography model 3...")
    # Save various components of the averaged model
    summary <- summary(linear_meso)
    print("...calculting confidence intervals of macrogeography model 3...")
    confidence <- try(confint(linear_meso))
    
    if (class(confidence) == "try-error") {
      
      confidence <- try(confint(linear_meso, method = "boot"))
    }
    
    # Now calculate confidence intervals manually
    # Extract coefficient estimates and SE's
    coeff_table <- as.data.frame(summary(linear_meso)[10])
    coeffs <- coeff_table[,1]
    st_error <- coeff_table[,2]
    # Calc CI
    confidence_manual <- tibble(
      param = rownames(coeff_table),
      coeff = coeffs,
      st_error = st_error,
      CI_95_low = coeffs - (st_error * 1.96),
      CI_95_high = coeffs + (st_error * 1.96)
    )
    
    sink(paste0("data/04_analysis/model_outputs/",
                CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, "/",
                MODEL_GROUP_NAME, "/", "meso_model.txt"))
    
    print(summary)
    
    print("Parameter Confidence Intervals using confint()")
    print(confidence)
    
    print("Parameter Confidence Intervals calculated by hand")
    print(confidence_manual)
    
    sink()
    # Ending the sink does not seem to release the file from R, so closing all
    # connections just in case. Probably has something to do with wrapping this all
    # in a function?
    closeAllConnections()
    
    # Save coefficients to model output table
    coefficients <- as.data.frame(summary$coefficients) %>% 
      dplyr::rename(estimate = Estimate, SE = `Std. Error`) %>%
      rownames_to_column()
    
    conf_df <- as.data.frame(confidence) %>% 
      rownames_to_column()
    
    out_results_meso <- full_join(coefficients, conf_df) %>% 
      mutate(model = "meso")
    
    write_csv(out_results_meso, paste0("data/04_analysis/model_outputs/",
                                         CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, "/",
                                         MODEL_GROUP_NAME, "/", "model_results_meso.csv"))
    
  }
  
  
  ## ....F. Print out micro model ##################

  if(exists("linear_micro")) {
    print("...Saving outputs of microgeography model...")
    # Save various components of the averaged model
    summary <- summary(linear_micro)
    print("...calculting confidence intervals of microgeography model...")
    confidence <- try(confint(linear_micro))
    
    if (class(confidence) == "try-error") {
      confidence <- try(confint(linear_micro, method = "boot"))
    }
    
    # Now calculate confidence intervals manually
    # Extract coefficient estimates and SE's
    coeff_table <- as.data.frame(summary(linear_micro)[10])
    coeffs <- coeff_table[,1]
    st_error <- coeff_table[,2]
    # Calc CI
    confidence_manual <- tibble(
      param = rownames(coeff_table),
      coeff = coeffs,
      st_error = st_error,
      CI_95_low = coeffs - (st_error * 1.96),
      CI_95_high = coeffs + (st_error * 1.96)
    )
    
    sink(paste0("data/04_analysis/model_outputs/",
                CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, "/",
                MODEL_GROUP_NAME, "/", "linear_micro.txt"))

    print(summary)
    
    print("Parameter Confidence Intervals using confint()")
    print(confidence)
    
    print("Parameter Confidence Intervals calculated by hand")
    print(confidence_manual)

    sink()
    # Ending the sink does not seem to release the file from R, so closing all
    # connections just in case. Probably has something to do with wrapping this all
    # in a function?
    closeAllConnections()

    # Save coefficients to model output table
    coefficients <- as.data.frame(summary$coefficients) %>% 
      dplyr::rename(estimate = Estimate, SE = `Std. Error`) %>%
      rownames_to_column()
    
    conf_df <- as.data.frame(confidence) %>% 
      rownames_to_column()
    
    out_results_micro <- full_join(coefficients, conf_df) %>% 
      mutate(model = "micro")
    write_csv(out_results_micro, paste0("data/04_analysis/model_outputs/",
                                  CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, "/",
                                  MODEL_GROUP_NAME, "/", "model_results_micro.csv"))
  }


  # ## ....G. Print out temporal model ##################
  # 
  # if(exists("linear_temporal")) {
  #   print("...Saving outputs of temporal model...")
  #   # Save various components of the averaged model
  #   summary <- summary(linear_temporal)
  #   print("...calculting confidence intervals of temporal...")
  #   confidence <- try(confint(linear_temporal))
  #   
  #   if (class(confidence) == "try-error") {
  #     confidence <- try(confint(linear_temporal, method = "boot"))
  #   }
  #   
  #   # Extract coefficient estimates and SE's
  #   coeff_table <- as.data.frame(summary(linear_temporal)[10])
  #   coeffs <- coeff_table[,1]
  #   st_error <- coeff_table[,2]
  #   # Calc CI
  #   confidence_manual <- tibble(
  #     param = rownames(coeff_table),
  #     coeff = coeffs,
  #     st_error = st_error,
  #     CI_95_low = coeffs - (st_error * 1.96),
  #     CI_95_high = coeffs + (st_error * 1.96)
  #   )
  #   
  #   sink(paste0("data/04_analysis/model_outputs/",
  #               CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, "/",
  #               MODEL_GROUP_NAME, "/", "linear_temporal.txt"))
  # 
  #   print(summary)
  #   
  #   print("Parameter Confidence Intervals using confint()")
  #   print(confidence)
  #   
  #   print("Parameter Confidence Intervals calculated by hand")
  #   print(confidence_manual)
  #   
  #   sink()
  #   # Ending the sink does not seem to release the file from R, so closing all
  #   # connections just in case. Probably has something to do with wrapping this all
  #   # in a function?
  #   closeAllConnections()
  # 
  # # Save coefficients to model output table
  # coefficients <- as.data.frame(summary$coefmat.full) %>% 
  #   dplyr::rename(estimate = Estimate, SE = `Std. Error`) %>% 
  #   rownames_to_column()
  # 
  # conf_df <- as.data.frame(confidence) %>% 
  #   rownames_to_column()
  # 
  # out_results <- full_join(coefficients, conf_df) %>% 
  #   mutate(model = MODEL_GROUP_NAME)
  # 
  # write_csv(out_results, paste0("data/04_analysis/model_outputs/",
  #                               CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, "/",
  #                               MODEL_GROUP_NAME, "/", "model_results.csv"))
  # }
}


## 4. Execute modeling ################
## TAD monthly, checking for n_day, n_season, n_year bias ###########
## .... TAD, monthly, all micros, n_days ##########

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = TAD)

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "TAD",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "elevation_change", "latitude", "height",
                            "macro", "veg_foliage", "snowdepth", "n_days", "site"),
            GLOBAL_FORMULA = "overlap ~ elevation_change + latitude + height +
                            veg_foliage + snowdepth + n_days +(1|site)",
            MODEL_GROUP_NAME = "all_micros_bias_check/n_days",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
)

## .... TAD, monthly, all micros, n_months ##########

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = TAD)

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "TAD",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "elevation_change", "latitude", "height",
                            "macro", "veg_foliage", "snowdepth", "n_months", "site"),
            GLOBAL_FORMULA = "overlap ~ elevation_change + latitude + height +
                            veg_foliage + snowdepth + n_months +(1|site)",
            MODEL_GROUP_NAME = "all_micros_bias_check/n_months",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
            
)

## .... TAD, monthly, all micros, n_seasons ##########

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = TAD)

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "TAD",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "elevation_change", "latitude", "height",
                            "macro", "veg_foliage", "snowdepth", "n_seasons", "site"),
            GLOBAL_FORMULA = "overlap ~ elevation_change + latitude + height +
                            veg_foliage + snowdepth + n_seasons + (1|site)",
            MODEL_GROUP_NAME = "all_micros_bias_check/n_seasons",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
            
)

## .... TAD, monthly, all micros, n_years ##########

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = TAD)

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "TAD",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "elevation_change", "latitude", "height",
                            "macro", "veg_foliage", "snowdepth", "n_years", "site"),
            GLOBAL_FORMULA = "overlap ~ elevation_change + latitude + height +
                            veg_foliage + snowdepth + n_years + (1|site)",
            MODEL_GROUP_NAME = "all_micros_bias_check/n_years",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
            
)

## TAD monthly ###################

## ....1. TAD, monthly, all micros ##########

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = TAD)

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "TAD",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "elevation_change", "latitude", "height",
                            "macro", "veg_foliage",
                            "snowdepth", "site"),
            GLOBAL_FORMULA = "overlap ~ elevation_change + latitude + height +
                            veg_foliage + snowdepth + (1|site)",
            MODEL_GROUP_NAME = "all_micros",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
              
)

## ....2. TAD_elevCorr, monthly, all micros ##########

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = TAD_elevCorr_positive)

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "TAD_elevCorr_positive",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "latitude", "height",
                            "macro", "veg_foliage",
                            "snowdepth", "site"),
            GLOBAL_FORMULA = "overlap ~ latitude + height +
                            veg_foliage + snowdepth + (1|site)",
            MODEL_GROUP_NAME = "all_micros",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
)


## ....3. TAD, monthly, soil only #################

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = TAD) %>% 
  # Filter down to just soil data
  filter(micro == "soil")

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "TAD",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "elevation_change", "latitude",
                            "macro", "height", "veg_foliage",
                            "snowdepth", "site"),
            GLOBAL_FORMULA = "overlap ~ elevation_change + latitude + veg_foliage + snowdepth + (1|site)",
            MODEL_GROUP_NAME = "soil_only",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
)

## ....4. TAD, monthly, surface only #################

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = TAD) %>% 
  # Filter down to just soil data
  filter(micro == "surface")

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "TAD",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "elevation_change", "latitude",
                            "macro", "height", "veg_foliage",
                            "snowdepth", "site"),
            GLOBAL_FORMULA = "overlap ~ elevation_change + latitude + veg_foliage + snowdepth + (1|site)",
            MODEL_GROUP_NAME = "surface_only",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
)

## ....5. TAD, monthly, canopy only #################

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = TAD) %>% 
  # Filter down to just soil data
  filter(micro == "canopy")

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "TAD",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "elevation_change", "latitude",
                            "macro", "height", "veg_foliage",
                            "snowdepth", "site"),
            GLOBAL_FORMULA = "overlap ~ elevation_change + latitude + veg_foliage + snowdepth + (1|site)",
            MODEL_GROUP_NAME = "canopy_only",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
)

## ....6. TAD_elevCorr, monthly, surface only, nonforest ########

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = TAD_elevCorr_positive) %>% 
  # Filter down to just soil data
  filter(micro == "surface") %>% 
  # Filter to non-forest
  filter(macro %in% c("alpine meadow", "hot desert", "scrub shrub", "developed",
                      "meadow near forest"))

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "TAD_elevCorr_positive",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "latitude", "veg_foliage",
                            "snowdepth", "site"),
            GLOBAL_FORMULA = "overlap ~ latitude + veg_foliage + snowdepth + (1|site)",
            MODEL_GROUP_NAME = "surface_only_nonforest",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
)

## ....6. TAD_elevCorr, monthly, surface only, forest ########

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = TAD_elevCorr_positive) %>% 
  # Filter down to just soil data
  filter(micro == "surface") %>% 
  # Filter to non-forest
  filter(macro %in% c("tropical broadleaf", "Ponderosa pine", "deciduous", 
                    "Dense coniferous", "degraded tropical broadleaf"))

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "TAD_elevCorr_positive",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "latitude", "veg_foliage",
                            "snowdepth", "site"),
            GLOBAL_FORMULA = "overlap ~ latitude + veg_foliage + snowdepth + (1|site)",
            MODEL_GROUP_NAME = "surface_only_forest",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
)


## Janzen D-score monthly #################

## ....2. janzenDscore, monthly, all micros ##########

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = janzenDscore_positive)

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "janzenDscore_positive",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "elevation_change", "latitude", "height",
                            "macro", "veg_foliage",
                            "snowdepth", "site"),
            GLOBAL_FORMULA = "overlap ~ latitude + elevation_change + height +
                            veg_foliage + snowdepth + (1|site)",
            MODEL_GROUP_NAME = "all_micros",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
            
)

## ....3. janzenDscore, monthly, soil only #################

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = janzenDscore_positive) %>% 
  # Filter down to just soil data
  filter(micro == "soil")

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "janzenDscore_positive",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "latitude", "height", "elevation_change", "macro", "veg_foliage",
                            "snowdepth", "site"),
            GLOBAL_FORMULA = "overlap ~ latitude + elevation_change + veg_foliage + snowdepth + (1|site)",
            MODEL_GROUP_NAME = "soil_only",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
            
)

## ....4. janzenDscore, monthly, surface only #################

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = janzenDscore_positive) %>% 
  # Filter down to just soil data
  filter(micro == "surface")

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "janzenDscore_positive",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "latitude", "height","elevation_change", "macro", "veg_foliage",
                            "snowdepth", "site"),
            GLOBAL_FORMULA = "overlap ~ latitude + elevation_change + veg_foliage + snowdepth + (1|site)",
            MODEL_GROUP_NAME = "surface_only",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
            
)

## ....5. janzenDscore, monthly, canopy only #################

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = janzenDscore_positive) %>% 
  # Filter down to just soil data
  filter(micro == "canopy")

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "janzenDscore_positive",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "latitude", "height", "elevation_change", "macro", "veg_foliage",
                            "snowdepth", "site"),
            GLOBAL_FORMULA = "overlap ~ latitude + elevation_change + veg_foliage + snowdepth + (1|site)",
            MODEL_GROUP_NAME = "canopy_only",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
            
)





## KDE monthly, checking for n_day, n_season, n_year bias ###########
## .... kde, monthly, all micros, n_days ##########

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = kde)

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "kde",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "elevation_change", "latitude", "height",
                            "macro", "veg_foliage", "snowdepth", "n_days", "site"),
            GLOBAL_FORMULA = "overlap ~ elevation_change + latitude + height +
                            veg_foliage + snowdepth + n_days +(1|site)",
            MODEL_GROUP_NAME = "all_micros_bias_check/n_days",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
            
)

## .... kde, monthly, all micros, n_months ##########

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = kde)

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "kde",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "elevation_change", "latitude", "height",
                            "macro", "veg_foliage", "snowdepth", "n_months", "site"),
            GLOBAL_FORMULA = "overlap ~ elevation_change + latitude + height +
                            veg_foliage + snowdepth + n_months +(1|site)",
            MODEL_GROUP_NAME = "all_micros_bias_check/n_months",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
            
)

## .... kde, monthly, all micros, n_seasons ##########

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = kde)

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "kde",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "elevation_change", "latitude", "height",
                            "macro", "veg_foliage", "snowdepth", "n_seasons", "site"),
            GLOBAL_FORMULA = "overlap ~ elevation_change + latitude + height +
                            veg_foliage + snowdepth + n_seasons + (1|site)",
            MODEL_GROUP_NAME = "all_micros_bias_check/n_seasons",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
            
)

## .... kde, monthly, all micros, n_years ##########

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = kde)

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "kde",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "elevation_change", "latitude", "height",
                            "macro", "veg_foliage", "snowdepth", "n_years", "site"),
            GLOBAL_FORMULA = "overlap ~ elevation_change + latitude + height +
                            veg_foliage + snowdepth + n_years + (1|site)",
            MODEL_GROUP_NAME = "all_micros_bias_check/n_years",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
            
)

## KDE monthly ################
## ....1. KDE, monthly, all micros ##########

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = kde)

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "kde",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "elevation_change", "latitude", "height",
                            "macro", "veg_foliage",
                            "snowdepth", "site"),
            GLOBAL_FORMULA = "overlap ~ elevation_change + latitude + height +
                            veg_foliage + snowdepth + (1|site)",
            MODEL_GROUP_NAME = "all_micros",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
            
)

## ....3. KDE, monthly, soil only #################

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = kde) %>% 
  # Filter down to just soil data
  filter(micro == "soil")

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "kde",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "elevation_change", "latitude",
                            "macro", "height", "veg_foliage",
                            "snowdepth", "site"),
            GLOBAL_FORMULA = "overlap ~ elevation_change + latitude + veg_foliage + snowdepth + (1|site)",
            MODEL_GROUP_NAME = "soil_only",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
)

## ....4. KDE, monthly, surface only #################

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = kde) %>% 
  # Filter down to just soil data
  filter(micro == "surface")

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "kde",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "elevation_change", "latitude",
                            "macro", "height", "veg_foliage",
                            "snowdepth", "site"),
            GLOBAL_FORMULA = "overlap ~ elevation_change + latitude + veg_foliage + snowdepth + (1|site)",
            MODEL_GROUP_NAME = "surface_only",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
)

## ....5. KDE, monthly, canopy only #################

# Rename overlap attribute
mountains <- mountains_monthly %>% 
  dplyr::rename(overlap = kde) %>% 
  # Filter down to just soil data
  filter(micro == "canopy")

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "kde",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "elevation_change", "latitude",
                            "macro", "height", "veg_foliage",
                            "snowdepth", "site"),
            GLOBAL_FORMULA = "overlap ~ elevation_change + latitude + veg_foliage + snowdepth + (1|site)",
            MODEL_GROUP_NAME = "canopy_only",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
)

## Thermal variance #################
## ....0. Thermal variance, monthly, all micros ########

# Rename overlap attribute
mountains <- mountains_minmax_monthly %>% 
  dplyr::rename(overlap = minmax) %>% 
  # Loophole my own coding so that I can model with gaussian distribution
  mutate(elevation_change = 1)

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "minmax",
            RESOLUTION = "monthly",
            PARAMS_LIST = c("overlap", "latitude", "height",
                            "veg_foliage",
                            "snowdepth", "site"),
            GLOBAL_FORMULA = "overlap ~ latitude + height +
                            veg_foliage + snowdepth + (1|site)",
            MODEL_GROUP_NAME = "all_micros",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
            
)



## ....0. Thermal variance, daily, all micros ########

# Rename overlap attribute
mountains <- mountains_minmax %>% 
  dplyr::rename(overlap = minmax) %>% 
  # Loophole my own coding so that I can model with gaussian distribution
  mutate(elevation_change = 1) %>% 
  # Because Gamma distribution doesn't allow non-positive, and technically there
  # always must be some thermal variance....adding to 0 values
  mutate(overlap = ifelse(overlap == 0, overlap + 0.0001, overlap))

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "minmax",
            RESOLUTION = "daily",
            PARAMS_LIST = c("overlap", "latitude", "height",
                            "veg_foliage",
                            "snowdepth", "site"),
            GLOBAL_FORMULA = "overlap ~ latitude + height +
                            veg_foliage + snowdepth + (1|site)",
            MODEL_GROUP_NAME = "all_micros",
            SUMMARY_FILE_NAME = "model_avg_summary.txt",
            CONFIDENCE_INT = TRUE
            
)




## ..D. TAD daily #################
## ....1. TAD, daily, all micros ##########

# Rename overlap attribute
mountains <- mountains_daily %>% 
  dplyr::rename(overlap = TAD_positive)

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "TAD_positive",
            RESOLUTION = "daily",
            PARAMS_LIST = c("overlap", "elevation_change", "latitude", "height",
                            "macro", "foliage_cover", "veg_structure", "snowdepth", 
                            "site"),
            MODEL_GROUP_NAME = "all_micros"
)

## ....2. TAD_elevCorr, daily, all micros ##########

# Rename overlap attribute
mountains <- mountains_daily %>% 
  dplyr::rename(overlap = TAD_elevCorr_positive)

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "TAD_elevCorr_positive",
            RESOLUTION = "daily",
            PARAMS_LIST = c("overlap", "latitude", "height",
                            "macro", "foliage_cover", "veg_structure", "snowdepth", 
                            "site"),
            MODEL_GROUP_NAME = "all_micros"
)

## ....3. TAD, daily, soil only #################

# Rename overlap attribute
mountains <- mountains_daily %>% 
  dplyr::rename(overlap = TAD_positive) %>% 
  # Filter down to just soil data
  filter(micro == "soil")

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "TAD_positive",
            RESOLUTION = "daily",
            PARAMS_LIST = c("overlap", "elevation_change", "height", "latitude",
                            "macro", "foliage_cover", "veg_structure", "snowdepth", 
                            "site"),
            MODEL_GROUP_NAME = "soil_only"
)

## ....4. TAD, daily, surface only #################

# Rename overlap attribute
mountains <- mountains_daily %>% 
  dplyr::rename(overlap = TAD_positive) %>% 
  # Filter down to just soil data
  filter(micro == "surface")

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "TAD_positive",
            RESOLUTION = "daily",
            PARAMS_LIST = c("overlap", "elevation_change", "height", "latitude",
                            "macro", "foliage_cover", "veg_structure", "snowdepth", 
                            "site"),
            MODEL_GROUP_NAME = "surface_only"
)

## ....5. TAD, daily, canopy only #################

# Rename overlap attribute
mountains <- mountains_daily %>% 
  dplyr::rename(overlap = TAD_positive) %>% 
  # Filter down to just soil data
  filter(micro == "canopy")

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "TAD_positive",
            RESOLUTION = "daily",
            PARAMS_LIST = c("overlap", "elevation_change", "height", "latitude",
                            "macro", "foliage_cover", "veg_structure", "snowdepth", 
                            "site"),
            MODEL_GROUP_NAME = "canopy_only"
)




## ....6. TAD_elevCorr, daily, soil only #################

# Rename overlap attribute
mountains <- mountains_daily %>% 
  dplyr::rename(overlap = TAD_elevCorr_positive) %>% 
  # Filter down to just soil data
  filter(micro == "soil")

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "TAD_elevCorr_positive",
            RESOLUTION = "daily",
            PARAMS_LIST = c("overlap", "height", "latitude",
                            "macro", "foliage_cover", "veg_structure", "snowdepth", 
                            "site"),
            MODEL_GROUP_NAME = "soil_only"
)

## ....7. TAD_elevCorr, daily, surface only #################

# Rename overlap attribute
mountains <- mountains_daily %>% 
  dplyr::rename(overlap = TAD_elevCorr_positive) %>% 
  # Filter down to just soil data
  filter(micro == "surface")

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "TAD_elevCorr_positive",
            RESOLUTION = "daily",
            PARAMS_LIST = c("overlap", "height", "latitude",
                            "macro", "foliage_cover", "veg_structure", "snowdepth", 
                            "site"),
            MODEL_GROUP_NAME = "surface_only"
)

## ....8. TAD_elevCorr, daily, canopy only #################

# Rename overlap attribute
mountains <- mountains_daily %>% 
  dplyr::rename(overlap = TAD_elevCorr_positive) %>% 
  # Filter down to just soil data
  filter(micro == "canopy")

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "TAD_elevCorr_positive",
            RESOLUTION = "daily",
            PARAMS_LIST = c("overlap", "height", "latitude",
                            "macro", "foliage_cover", "veg_structure", "snowdepth", 
                            "site"),
            MODEL_GROUP_NAME = "canopy_only"
)








## ..E. JanzenDscore, daily ##################
## ....1 janzenDscore, daily, all micros ##########

# Rename overlap attribute
mountains <- mountains_daily %>% 
  dplyr::rename(overlap = janzenDscore_positive)

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "janzenDscore_positive",
            RESOLUTION = "daily",
            PARAMS_LIST = c("overlap", "elevation_change", "latitude", "height",
                            "macro", "foliage_cover", "veg_structure", "snowdepth", 
                            "site"),
            MODEL_GROUP_NAME = "all_micros"
)

## ....2. janzenDscore_elevCorr, daily, all micros ##########

# Rename overlap attribute
mountains <- mountains_daily %>% 
  dplyr::rename(overlap = janzenDscore_elevCorr_positive)

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "janzenDscore_elevCorr_positive",
            RESOLUTION = "daily",
            PARAMS_LIST = c("overlap", "latitude", "height",
                            "macro", "foliage_cover", "veg_structure", "snowdepth", 
                            "site"),
            MODEL_GROUP_NAME = "all_micros"
)

## ....3. janzenDscore, daily, soil only #################

# Rename overlap attribute
mountains <- mountains_daily %>% 
  dplyr::rename(overlap = janzenDscore_positive) %>% 
  # Filter down to just soil data
  filter(micro == "soil")

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "janzenDscore_positive",
            RESOLUTION = "daily",
            PARAMS_LIST = c("overlap", "elevation_change", "height", "latitude",
                            "macro", "foliage_cover", "veg_structure", "snowdepth", 
                            "site"),
            MODEL_GROUP_NAME = "soil_only"
)

## ....4. janzenDscore, daily, surface only #################

# Rename overlap attribute
mountains <- mountains_daily %>% 
  dplyr::rename(overlap = janzenDscore_positive) %>% 
  # Filter down to just soil data
  filter(micro == "surface")

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "janzenDscore_positive",
            RESOLUTION = "daily",
            PARAMS_LIST = c("overlap", "elevation_change", "height", "latitude",
                            "macro", "foliage_cover", "veg_structure", "snowdepth", 
                            "site"),
            MODEL_GROUP_NAME = "surface_only"
)

## ....5. janzenDscore, daily, canopy only #################

# Rename overlap attribute
mountains <- mountains_daily %>% 
  dplyr::rename(overlap = janzenDscore_positive) %>% 
  # Filter down to just soil data
  filter(micro == "canopy")

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "janzenDscore_positive",
            RESOLUTION = "daily",
            PARAMS_LIST = c("overlap", "elevation_change", "height", "latitude",
                            "macro", "foliage_cover", "veg_structure", "snowdepth", 
                            "site"),
            MODEL_GROUP_NAME = "canopy_only"
)




## ....6. janzenDscore_elevCorr, daily, soil only #################

# Rename overlap attribute
mountains <- mountains_daily %>% 
  dplyr::rename(overlap = janzenDscore_elevCorr_positive) %>% 
  # Filter down to just soil data
  filter(micro == "soil")

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "janzenDscore_elevCorr_positive",
            RESOLUTION = "daily",
            PARAMS_LIST = c("overlap", "height", "latitude",
                            "macro", "foliage_cover", "veg_structure", "snowdepth", 
                            "site"),
            MODEL_GROUP_NAME = "soil_only"
)

## ....7. janzenDscore_elevCorr, daily, surface only #################

# Rename overlap attribute
mountains <- mountains_daily %>% 
  dplyr::rename(overlap = janzenDscore_elevCorr_positive) %>% 
  # Filter down to just soil data
  filter(micro == "surface")

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "janzenDscore_elevCorr_positive",
            RESOLUTION = "daily",
            PARAMS_LIST = c("overlap", "height", "latitude",
                            "macro", "foliage_cover", "veg_structure", "snowdepth", 
                            "site"),
            MODEL_GROUP_NAME = "surface_only"
)

## ....8. janzenDscore_elevCorr, daily, canopy only #################

# Rename overlap attribute
mountains <- mountains_daily %>% 
  dplyr::rename(overlap = janzenDscore_elevCorr_positive) %>% 
  # Filter down to just soil data
  filter(micro == "canopy")

lmm_overlap(mountains = mountains,
            CHOSEN_OVERLAP_ATTRIBUTE = "janzenDscore_elevCorr_positive",
            RESOLUTION = "daily",
            PARAMS_LIST = c("overlap", "height", "latitude",
                            "macro", "foliage_cover", "veg_structure", "snowdepth", 
                            "site"),
            MODEL_GROUP_NAME = "canopy_only"
)




## Model for overlap: all data ###########
## ....1. Build your candidate models #############

# Null is the intercept: no fixed effect or continouos covariates
linear_null <- lmer(overlap ~ 1 + (1|site), data = mountains,
                    REML = FALSE, na.action = na.fail)

# Global is all of your parameters
linear_global <- lmer(overlap ~ elevation_change + latitude +
                        micro + macro + foliage + veg_structure + snowdepth +
                        (1|site), data = mountains, 
                      REML = FALSE, na.action = na.fail)

linear_elevChange <- lmer(overlap ~ elevation_change + (1|site), data = mountains, 
                          REML = FALSE, na.action = na.fail)
linear_latitude <- lmer(overlap ~ latitude + (1|site), data = mountains, 
                        REML = FALSE, na.action = na.fail)
linear_micro <- lmer(overlap ~ micro + (1|site), data = mountains, 
                     REML = FALSE, na.action = na.fail)
linear_macro <- lmer(overlap ~ macro + (1|site), data = mountains, 
                     REML = FALSE, na.action = na.fail)
linear_foliage <- lmer(overlap ~ foliage + (1|site), data = mountains, 
                       REML = FALSE, na.action = na.fail)
linear_vegStructure <- lmer(overlap ~ veg_structure + (1|site), data = mountains, 
                            REML = FALSE, na.action = na.fail)
linear_snowdepth <- lmer(overlap ~ snowdepth + (1|site), data = mountains, 
                         REML = FALSE, na.action = na.fail)

# OR alternatively, do model dredging
c(na.action = "na.fail")
candidate_list <- dredge(linear_global)
# Interpreting this output is to look at the models with lowest AIC, 
# and see what isn't NA in that model

## ....2. Compare candidate models #############

candidatelist <- list(
  linear_null, linear_elevChange, linear_latitude, 
  linear_micro, linear_macro, 
  linear_foliage, linear_vegStructure, linear_snowdepth)

modelnames <- c("linear_null", "linear_elevChange", "linear_latitude", 
              "linear_micro", "linear_macro", 
              "linear_foliage", "linear_vegStructure", "linear_snowdepth")

aic_results <- aictab(cand.set = candidatelist, modnames = modelnames)

## ....3. Model averaging ###################

# Models with delta AIC less than 10
model_avg_delta4 <- model.avg(candidate_list, subset = delta < 4)

## ....4. Model predictions ###########

model_avg_predictions <- predict(model_avg_delta4)

averaged.subset <- predict(avgm, newdata, full = TRUE)
averaged.subset <- predict(avgm, newdata, full = FALSE)


# model_avg_delta4_preds<-predict(model_avg_delta4, type="response")
# linear_noVegStructure_preds
# coef(linear_noVegStructure)

# Now plot this stuff

## ....5. Evaluate the best candidate model ################

# This is when you look at the significance of each of the parameters....not 
# exactly sure how but can discuss with Baecher later


## ....6. Write out models and comparison NEEDS WORK ##########

write_csv(candidate_list, paste0("data/04_analysis/model_outputs/", 
          CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, "/dredged_candidate_list.csv"))

write_csv(aic_results, paste0("data/04_analysis/model_outputs/", 
          CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, "/aic_results.csv"))

write_csv(aic_results, paste0("data/04_analysis/model_outputs/", 
                              CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, "/aic_results.csv"))

linear_model_avg_delta4 <- as.data.frame(model_avg_delta4$coefficients) %>% 
  rownames_to_column(var = "parameter") %>% 
  mutate(model = "linear_model_avg_delta4")

linear_null_df <- as.data.frame(getSummary(linear_null)$coef) %>% 
  rownames_to_column(var = "parameter") %>% 
  mutate(model = "linear_everything") %>% 
  dplyr::rename(overlap_estimate = est.overlap, overlap_SE = se.overlap, P_value = p.overlap) %>% 
  dplyr::select(model, everything())
linear_elevChange_df <- as.data.frame(getSummary(linear_elevChange)$coef) %>% 
  rownames_to_column(var = "parameter") %>% 
  mutate(model = "linear_noLatitude") %>% 
  dplyr::rename(overlap_estimate = est.overlap, overlap_SE = se.overlap, P_value = p.overlap) %>% 
  dplyr::select(model, everything())
linear_latitude_df <- as.data.frame(getSummary(linear_latitude)$coef) %>% 
  rownames_to_column(var = "parameter") %>% 
  mutate(model = "linear_noMicro") %>% 
  dplyr::rename(overlap_estimate = est.overlap, overlap_SE = se.overlap, P_value = p.overlap) %>% 
  dplyr::select(model, everything())
linear_micro_df <- as.data.frame(getSummary(linear_micro)$coef) %>% 
  rownames_to_column(var = "parameter") %>% 
  mutate(model = "linear_noMacro") %>%
  dplyr::rename(overlap_estimate = est.overlap, overlap_SE = se.overlap, P_value = p.overlap) %>% 
  dplyr::select(model, everything())
linear_macro_df <- as.data.frame(getSummary(linear_macro)$coef) %>% 
  rownames_to_column(var = "parameter") %>% 
  mutate(model = "linear_noFoliage") %>% 
  dplyr::rename(overlap_estimate = est.overlap, overlap_SE = se.overlap, P_value = p.overlap) %>% 
  dplyr::select(model, everything())
linear_foliage_df <- as.data.frame(getSummary(linear_foliage)$coef) %>% 
  rownames_to_column(var = "parameter") %>% 
  mutate(model = "linear_noFoliage") %>% 
  dplyr::rename(overlap_estimate = est.overlap, overlap_SE = se.overlap, P_value = p.overlap) %>% 
  dplyr::select(model, everything())
linear_snowdepth_df <- as.data.frame(getSummary(linear_snowdepth)$coef) %>% 
  rownames_to_column(var = "parameter") %>% 
  mutate(model = "linear_noSnow") %>% 
  dplyr::rename(overlap_estimate = est.overlap, overlap_SE = se.overlap, P_value = p.overlap) %>% 
  dplyr::select(model, everything())

## Write out results
write_csv(linear_model_avg_delta4, paste0("data/04_analysis/model_outputs/", 
                                 CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, 
                                 "/linear_model_avg_delta4.csv"))

write_csv(linear_null_df, paste0("data/04_analysis/model_outputs/", 
                                 CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, 
                                 "/linear_null.csv"))

write_csv(linear_elevChange_df, paste0("data/04_analysis/model_outputs/", 
                                       CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, 
                                       "/linear_elevChange.csv"))

write_csv(linear_latitude_df, paste0("data/04_analysis/model_outputs/", 
                                       CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, 
                                       "/linear_latitude.csv"))

write_csv(linear_micro_df, paste0("data/04_analysis/model_outputs/", 
                                     CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, 
                                     "/linear_micro.csv"))

write_csv(linear_macro_df, paste0("data/04_analysis/model_outputs/", 
                                     CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, 
                                     "/linear_macro.csv"))

write_csv(linear_foliage_df, paste0("data/04_analysis/model_outputs/", 
                                  CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, 
                                  "/linear_foliage.csv"))

write_csv(linear_snowdepth_df, paste0("data/04_analysis/model_outputs/", 
                                  CHOSEN_OVERLAP_ATTRIBUTE, "/", RESOLUTION, 
                                  "/linear_snowdepth.csv"))









## Model for overlap: per micro #################


