# David Klinges
# This script generates plot of the magnitude of change in overlap d-score 
#   values for each site

## 1. Workspace prep ##############

library(tidyverse)
library(strucchange)
library(mgcv)
library(R.utils)

temporal_rez_data <- read_csv("data/03_compiled/temporal_rez/mountains_rez_allyears.csv")
mountains <- read_csv("data/03_compiled/elevation_controlled/mountains_janz_month_avg.csv")

## 2. Data cleaning ##############

temporal_rez_data <- temporal_rez_data %>% 
  filter(site %in% list("AU", "AZ", "CO", "CR"))

## 3. Loop through data and pull out breakpoints for each site, micro, season #########

sites <- unique(temporal_rez_data$site)

system.time({
  
  # For each site...
  for (i in 1:length(sites)) {
    
    temporal_rez_site <- temporal_rez_data %>% 
      filter(site == sites[i])
    
    # ...and for each micro in that site...
    for (j in 1:length(unique(temporal_rez_site$micro))) {
      
      mic <- unique(temporal_rez_site$micro)[j]
      
      temporal_rez_micro <- temporal_rez_site %>% 
        filter(micro == mic)
      
      # ...and for each season represented....
      for (k in 1:length(unique(temporal_rez_micro$foliage))) {
        
        foil <- unique(temporal_rez_micro$foliage)[k]
        temporal_rez_foliage <- temporal_rez_micro %>% 
          filter(foliage == foil) %>% 
          select(site, micro, foliage, temporal_rez, TAD)
        
        # Plot the current iterator data set and the GAM best fit
        # plot <- qplot(temporal_rez,TAD,data=temporal_rez_foliage) +
        #   stat_smooth(aes(outfit=fit<<-..y..), method = "gam", formula = y ~ log(x))
        
        # Fit a log model to the data
        model_log <- 
          try(
            withTimeout(
              gam(TAD ~ log(temporal_rez), data = temporal_rez_foliage), 
              timeout = 500, onTimeout = "warning"
            ), 
            silent = FALSE)
        
        # Fit an asymptotic model to the data
        model_asym <- try(nls(TAD ~ SSasymp(temporal_rez, Asym, r0, lrc), temporal_rez_foliage), 
                          silent = FALSE)
        
        # If either of the models were successful...
        if (class(model_log)[1] == "gam" | class(model_asym) == "nls") {
          
          print(paste("at least one successful model for", sites[i], 
                      mic, foil,
                      sep = " "))
          
          # If log model successful....
          if (class(model_log)[1] == "gam") {
            
            print("saving log model...")
            
            # Save correlation between predicted values and observed values
            correlation_logModel <- cor(temporal_rez_foliage$TAD, predict(model_log))
            
            y <- model_log$fitted.values
            x <- temporal_rez_foliage$temporal_rez
            # plot(y ~ x)
            
            intercept_logModel <- model_log$coefficients[1]
            B1_logModel <- model_log$coefficients[2]
            
            # Create df from x and y to access fitted values in a second
            fit_values <- tibble(
              x = x,
              y = y
            ) %>% 
              distinct()
            
            print("analyzing breakpoints...")
            
            bp_logModel <- try(
              withTimeout(
                breakpoints(formula = y ~ x, h = 10, breaks = 2),
                timeout = 500, onTimeout = "warning"
              )
            )
            
            # If breakpoint analysis successful...
            if(class(bp_logModel)[1] == "breakpointsfull") {
              
              print("saving breakpoints...")
              
              # Save timesteps at breakpoints
              timestep_breaks_logModel <- bp_logModel$breakpoints
              
              # Save real data overlap values of iterator dataset at breakpoints
              true_overlap_breaks_logModel <- temporal_rez_foliage %>% 
                filter(temporal_rez == timestep_breaks_logModel[1] | 
                         temporal_rez == timestep_breaks_logModel[2]) %>% 
                group_by(temporal_rez) %>% 
                summarize(TAD = mean(TAD))
              true_overlap_breaks_logModel <- true_overlap_breaks_logModel$TAD
              
              # Save fitted overlap values at breakpoints (hence why we saved fit_values before)
              fitted_overlap_breaks_logModel <- fit_values %>% 
                filter(x == timestep_breaks_logModel[1] | x == timestep_breaks_logModel[2])
              fitted_overlap_breaks_logModel <- fitted_overlap_breaks_logModel$y
              
              # If there's only one value that was saved from the real dataset..
              if (length(true_overlap_breaks_logModel) < 2) {
                # It means that the second breakpoint, and perhaps the first, were outside the 
                # timerange of the data, e.g. a larger temporal rez than the length of the 
                # timeseries. So just store NA there
                number_NAs <- 2 - length(true_overlap_breaks_logModel)
                for (n in 1:number_NAs) {
                  true_overlap_breaks_logModel[n] = NA
                }

              }
              
              # Do the same for the fitted values
              if (length(fitted_overlap_breaks_logModel) < 2) {
                number_NAs <- 2 - length(fitted_overlap_breaks_logModel)
                for (n in 1:number_NAs) {
                  fitted_overlap_breaks_logModel[n] = NA
                }

              }
              
              }
              
            else {
              
              print(paste("Failed to calculate log model breakpoints for", sites[i], 
                          mic, foil,
                          sep = " "))
              
              timestep_breaks_logModel <- c(NA, NA)
              true_overlap_breaks_logModel <- c(NA, NA)
              fitted_overlap_breaks_logModel <- c(NA, NA)
            }
          } else {
            
            print(paste("Failed to build log model for", sites[i], 
                        mic, foil,
                        sep = " "))
            
            intercept_logModel<- NA
            B1_logModel <- NA
            correlation_logModel <- NA
            timestep_breaks_logModel <- c(NA, NA)
            true_overlap_breaks_logModel <- c(NA, NA)
            fitted_overlap_breaks_logModel <- c(NA, NA)
          }
          
          # If asymptotic model successful...
          if (class(model_asym) == "nls") {
            
            print("saving asymptotic model...")
            
            # Save correlation between predicted values and observed values
            correlation_asymModel <- cor(temporal_rez_foliage$TAD, predict(model_asym))
            
            y <- model_asym$m$fitted()
            x <- temporal_rez_foliage$temporal_rez
            # plot(y ~ x)
            
            
            # Saved model parameters
            asymModel_asym <- coef(model_asym)[1]
            asymModel_r0 <- coef(model_asym)[2]
            asymModel_lrc <- coef(model_asym)[3]
            
            # Create df from x and y to access fitted values in a second
            fit_values <- tibble(
              x = x,
              y = y
            ) %>% 
              distinct()
            
            print("analyzing asymptotic breakpoints...")
            
            bp_asymModel <- try(
              withTimeout(
                breakpoints(formula = y ~ x, h = 10, breaks = 2),
                timeout = 500, onTimeout = "warning"
              )
            )
            
            if(class(bp_asymModel)[1] == "breakpointsfull") {
              
              print("saving asymptotic breakpoints...")
              
              # Save timesteps at breakpoints
              timestep_breaks_asymModel <- bp_asymModel$breakpoints
              # Save real data overlap values of iterator dataset at breakpoints
              true_overlap_breaks_asymModel <- temporal_rez_foliage %>% 
                filter(temporal_rez == timestep_breaks_asymModel[1] | 
                         temporal_rez == timestep_breaks_asymModel[2]) %>% 
                group_by(temporal_rez) %>% 
                summarize(TAD = mean(TAD))
              true_overlap_breaks_asymModel <- true_overlap_breaks_asymModel$TAD
              
              # Save fitted overlap values at breakpoints (hence why we saved fit_values before)
              fitted_overlap_breaks_asymModel <- fit_values %>% 
                filter(x == timestep_breaks_asymModel[1] | x == timestep_breaks_asymModel[2])
              fitted_overlap_breaks_asymModel <- fitted_overlap_breaks_asymModel$y
              
              # If there's only one value that was saved from the real dataset..
              if (length(true_overlap_breaks_asymModel) < 2) {
                # It means that the second breakpoint, and perhaps the first, were outside the 
                # timerange of the data, e.g. a larger temporal rez than the length of the 
                # timeseries. So just store NA there
                number_NAs <- 2 - length(true_overlap_breaks_asymModel)
                for (n in 1:number_NAs) {
                  true_overlap_breaks_asymModel[n] = NA
                }
                
              }
              
              if (length(fitted_overlap_breaks_asymModel) < 2) {
                # It means that the second breakpoint, and perhaps the first, were outside the 
                # timerange of the data, e.g. a larger temporal rez than the length of the 
                # timeseries. So just store NA there
                number_NAs <- 2 - length(fitted_overlap_breaks_asymModel)
                for (n in 1:number_NAs) {
                  fitted_overlap_breaks_asymModel[n] = NA
                }

              }
              
            }
            
          else {
            
            print(paste("Failed to calculate asym model breakpoints for", sites[i], 
                        mic, foil,
                        sep = " "))
          
            timestep_breaks_asymModel <- c(NA, NA)
            true_overlap_breaks_asymModel <- c(NA, NA)
            fitted_overlap_breaks_asymModel <- c(NA, NA)
          }
          } else {
            
            print(paste("Failed to build asymptotic model for ", sites[i], 
                        mic, foil,
                        sep = " "))
            
            asymModel_asym <- NA
            asymModel_r0 <- NA
            asymModel_lrc <- NA
            correlation_asymModel <- NA
            timestep_breaks_asymModel <- c(NA, NA)
            true_overlap_breaks_asymModel <- c(NA, NA)
            fitted_overlap_breaks_asymModel <- c(NA, NA)
          }
          
          print("saving results from this iteration.")
          
          # If first iteration...
          if (i == 1 & j == 1 & k == 1) {
            
            # Create the output dataframe
            breakpoints_bySite_df <- tibble(
              site = sites[i],
              micro = mic,
              foliage = foil,
              intercept_logModel = intercept_logModel,
              B1_logModel = B1_logModel,
              correlation_logModel = correlation_logModel, 
              timestep_break_1_logModel = timestep_breaks_logModel[1],
              timestep_break_2_logModel = timestep_breaks_logModel[2],
              trueOverlap_break_1_logModel = true_overlap_breaks_logModel[1],
              trueOverlap_break_2_logModel = true_overlap_breaks_logModel[2],
              fittedOverlap_break_1_logModel = fitted_overlap_breaks_logModel[1],
              fittedOverlap_break_2_logModel = fitted_overlap_breaks_logModel[2],
              asymModel_asym = asymModel_asym, 
              asymModel_r0 = asymModel_r0,
              asymModel_lrc = asymModel_lrc,
              correlation_asymModel = correlation_asymModel, 
              timestep_break_1_asymModel = timestep_breaks_asymModel[1],
              timestep_break_2_asymModel = timestep_breaks_asymModel[2],
              trueOverlap_break_1_asymModel = true_overlap_breaks_asymModel[1],
              trueOverlap_break_2_asymModel = true_overlap_breaks_asymModel[2],
              fittedOverlap_break_1_asymModel = fitted_overlap_breaks_asymModel[1],
              fittedOverlap_break_2_asymModel = fitted_overlap_breaks_asymModel[2]
            )
            
          } else { # Other bind to the existing output dataframe
            breakpoints_bySite_df <- 
              bind_rows(breakpoints_bySite_df, 
                        tibble(
                          site = sites[i],
                          micro = mic,
                          foliage = foil,
                          intercept_logModel = intercept_logModel,
                          B1_logModel = B1_logModel,
                          correlation_logModel = correlation_logModel, 
                          timestep_break_1_logModel = timestep_breaks_logModel[1],
                          timestep_break_2_logModel = timestep_breaks_logModel[2],
                          trueOverlap_break_1_logModel = true_overlap_breaks_logModel[1],
                          trueOverlap_break_2_logModel = true_overlap_breaks_logModel[2],
                          fittedOverlap_break_1_logModel = fitted_overlap_breaks_logModel[1],
                          fittedOverlap_break_2_logModel = fitted_overlap_breaks_logModel[2],
                          asymModel_asym = asymModel_asym, 
                          asymModel_r0 = asymModel_r0,
                          asymModel_lrc = asymModel_lrc,
                          correlation_asymModel = correlation_asymModel, 
                          timestep_break_1_asymModel = timestep_breaks_asymModel[1],
                          timestep_break_2_asymModel = timestep_breaks_asymModel[2],
                          trueOverlap_break_1_asymModel = true_overlap_breaks_asymModel[1],
                          trueOverlap_break_2_asymModel = true_overlap_breaks_asymModel[2],
                          fittedOverlap_break_1_asymModel = fitted_overlap_breaks_asymModel[1],
                          fittedOverlap_break_2_asymModel = fitted_overlap_breaks_asymModel[2]
                        ))
          }
          
            
          }
        
        # If NEITHER model was successful...
        else {
          print(paste("Failed to build both log AND asym models for", sites[i], 
                      mic, foil,
                      sep = " "))
        }
      }
    }
  }
  
})

## 4. Loop through data and pull out breakpoints for each macro, micro, season #########

macros <- unique(temporal_rez_data$macro)

system.time({
  
  # For each macrosystem....
  for (i in 1:length(macros)) {
    
    temporal_rez_macro <- temporal_rez_data %>% 
      filter(macro == macros[i])
    
    # ...and for each micro in that macro...
    for (j in 1:length(unique(temporal_rez_macro$micro))) {
      
      mic <- unique(temporal_rez_macro$micro)[j]
      
      temporal_rez_micro <- temporal_rez_macro %>% 
        filter(micro == mic)
      
      # ...and for each season represented....
      for (k in 1:length(unique(temporal_rez_micro$foliage))) {
        
        foil <- unique(temporal_rez_micro$foliage)[k]
        temporal_rez_foliage <- temporal_rez_micro %>% 
          filter(foliage == foil) %>% 
          dplyr::select(macro, micro, foliage, temporal_rez, TAD)
        
        
        print(paste("Model fitting for ", macros[i], 
                    mic, foil, ":",
                    sep = " "))
        
        # Plot the current iterator data set and the GAM best fit
        # plot <- qplot(temporal_rez,TAD,data=temporal_rez_foliage) +
        #   stat_smooth(aes(outfit=fit<<-..y..), method = "gam", formula = y ~ log(x))
        
        # Fit a log model to the data
        model_log <- 
          try(
            withTimeout(
              gam(TAD ~ log(temporal_rez), data = temporal_rez_foliage), 
              timeout = 120, onTimeout = "warning"
            ), 
            silent = FALSE)
        
        # Fit an asymptotic model to the data
        model_asym <- try(nls(TAD ~ SSasymp(temporal_rez, Asym, r0, lrc), temporal_rez_foliage), 
                          silent = FALSE)
        
        # If either of the models were successful...
        if (class(model_log)[1] == "gam" | class(model_asym) == "nls") {
          
          print(paste("....at least one successful model."))
          
          # If log model successful....
          if (class(model_log)[1] == "gam") {
            
            print("....saving log model...")
            
            # Save correlation between predicted values and observed values
            correlation_logModel <- cor(temporal_rez_foliage$TAD, predict(model_log))
            
            y <- model_log$fitted.values
            x <- temporal_rez_foliage$temporal_rez
            # plot(y ~ x)
            
            intercept_logModel <- model_log$coefficients[1]
            B1_logModel <- model_log$coefficients[2]
            
            # Create df from x and y to access fitted values in a second
            fit_values <- tibble(
              x = x,
              y = y
            ) %>% 
              distinct()
            
            print("....analyzing breakpoints of log model...")
            
            bp_logModel <- try(
              withTimeout(
                breakpoints(formula = y ~ x, h = 10, breaks = 2),
                timeout = 120, onTimeout = "warning"
              )
            )
            
            # If breakpoint analysis successful...
            if(class(bp_logModel)[1] == "breakpointsfull") {
              
              print("....Breakpoint analysis success. Saving breakpoints...")
              
              # Save timesteps at breakpoints
              timestep_breaks_logModel <- bp_logModel$breakpoints
              
              # Save real data overlap values of iterator dataset at breakpoints
              true_overlap_breaks_logModel <- temporal_rez_foliage %>% 
                filter(temporal_rez == timestep_breaks_logModel[1] | 
                         temporal_rez == timestep_breaks_logModel[2]) %>% 
                group_by(temporal_rez) %>% 
                summarize(TAD = mean(TAD))
              true_overlap_breaks_logModel <- true_overlap_breaks_logModel$TAD
              
              # Save fitted overlap values at breakpoints (hence why we saved fit_values before)
              fitted_overlap_breaks_logModel <- fit_values %>% 
                filter(x == timestep_breaks_logModel[1] | x == timestep_breaks_logModel[2])
              fitted_overlap_breaks_logModel <- fitted_overlap_breaks_logModel$y
              
              # If there's only one value that was saved from the real dataset..
              if (length(true_overlap_breaks_logModel) < 2) {
                # It means that the second breakpoint, and perhaps the first, were outside the 
                # timerange of the data, e.g. a larger temporal rez than the length of the 
                # timeseries. So just store NA there
                number_NAs <- 2 - length(true_overlap_breaks_logModel)
                for (n in 1:number_NAs) {
                  true_overlap_breaks_logModel[n] = NA
                }
                
              }
              
              # Do the same for the fitted values
              if (length(fitted_overlap_breaks_logModel) < 2) {
                number_NAs <- 2 - length(fitted_overlap_breaks_logModel)
                for (n in 1:number_NAs) {
                  fitted_overlap_breaks_logModel[n] = NA
                }
                
              }
              
            }
            
            else {
              
              print(paste("....Failed to calculate log model breakpoints."))
              
              timestep_breaks_logModel <- c(NA, NA)
              true_overlap_breaks_logModel <- c(NA, NA)
              fitted_overlap_breaks_logModel <- c(NA, NA)
            }
          } else {
            
            print(paste("....Failed to build log model."))
            
            intercept_logModel<- NA
            B1_logModel <- NA
            correlation_logModel <- NA
            timestep_breaks_logModel <- c(NA, NA)
            true_overlap_breaks_logModel <- c(NA, NA)
            fitted_overlap_breaks_logModel <- c(NA, NA)
          }
          
          # If asymptotic model successful...
          if (class(model_asym) == "nls") {
            
            print("....Saving asymptotic model...")
            
            # Save correlation between predicted values and observed values
            correlation_asymModel <- cor(temporal_rez_foliage$TAD, predict(model_asym))
            
            y <- model_asym$m$fitted()
            x <- temporal_rez_foliage$temporal_rez
            # plot(y ~ x)
            
            
            # Saved model parameters
            asymModel_asym <- coef(model_asym)[1]
            asymModel_r0 <- coef(model_asym)[2]
            asymModel_lrc <- coef(model_asym)[3]
            
            # Create df from x and y to access fitted values in a second
            fit_values <- tibble(
              x = x,
              y = y
            ) %>% 
              distinct()
            
            print("....analyzing asymptotic breakpoints...")
            
            bp_asymModel <- try(
              withTimeout(
                breakpoints(formula = y ~ x, h = 10, breaks = 2),
                timeout = 120, onTimeout = "warning"
              )
            )
            
            if(class(bp_asymModel)[1] == "breakpointsfull") {
              
              print("....saving asymptotic breakpoints...")
              
              # Save timesteps at breakpoints
              timestep_breaks_asymModel <- bp_asymModel$breakpoints
              # Save real data overlap values of iterator dataset at breakpoints
              true_overlap_breaks_asymModel <- temporal_rez_foliage %>% 
                filter(temporal_rez == timestep_breaks_asymModel[1] | 
                         temporal_rez == timestep_breaks_asymModel[2]) %>% 
                group_by(temporal_rez) %>% 
                summarize(TAD = mean(TAD))
              true_overlap_breaks_asymModel <- true_overlap_breaks_asymModel$TAD
              
              # Save fitted overlap values at breakpoints (hence why we saved fit_values before)
              fitted_overlap_breaks_asymModel <- fit_values %>% 
                filter(x == timestep_breaks_asymModel[1] | x == timestep_breaks_asymModel[2])
              fitted_overlap_breaks_asymModel <- fitted_overlap_breaks_asymModel$y
              
              # If there's only one value that was saved from the real dataset..
              if (length(true_overlap_breaks_asymModel) < 2) {
                # It means that the second breakpoint, and perhaps the first, were outside the 
                # timerange of the data, e.g. a larger temporal rez than the length of the 
                # timeseries. So just store NA there
                number_NAs <- 2 - length(true_overlap_breaks_asymModel)
                for (n in 1:number_NAs) {
                  true_overlap_breaks_asymModel[n] = NA
                }
                
              }
              
              if (length(fitted_overlap_breaks_asymModel) < 2) {
                # It means that the second breakpoint, and perhaps the first, were outside the 
                # timerange of the data, e.g. a larger temporal rez than the length of the 
                # timeseries. So just store NA there
                number_NAs <- 2 - length(fitted_overlap_breaks_asymModel)
                for (n in 1:number_NAs) {
                  fitted_overlap_breaks_asymModel[n] = NA
                }
                
              }
              
            }
            
            else {
              
              print(paste("....Failed to calculate asym model breakpoints."))
              
              timestep_breaks_asymModel <- c(NA, NA)
              true_overlap_breaks_asymModel <- c(NA, NA)
              fitted_overlap_breaks_asymModel <- c(NA, NA)
            }
          } else {
            
            print(paste("....Failed to build asymptotic model."))
            
            asymModel_asym <- NA
            asymModel_r0 <- NA
            asymModel_lrc <- NA
            correlation_asymModel <- NA
            timestep_breaks_asymModel <- c(NA, NA)
            true_overlap_breaks_asymModel <- c(NA, NA)
            fitted_overlap_breaks_asymModel <- c(NA, NA)
          }
          
          print("....Saving results from this iteration.")
          
          # If first iteration...
          if (i == 1 & j == 1 & k == 1) {
            
            # Create the output dataframe
            breakpoints_byMacro_df <- tibble(
              macro = macros[i],
              micro = mic,
              foliage = foil,
              intercept_logModel = intercept_logModel,
              B1_logModel = B1_logModel,
              correlation_logModel = correlation_logModel, 
              timestep_break_1_logModel = timestep_breaks_logModel[1],
              timestep_break_2_logModel = timestep_breaks_logModel[2],
              trueOverlap_break_1_logModel = true_overlap_breaks_logModel[1],
              trueOverlap_break_2_logModel = true_overlap_breaks_logModel[2],
              fittedOverlap_break_1_logModel = fitted_overlap_breaks_logModel[1],
              fittedOverlap_break_2_logModel = fitted_overlap_breaks_logModel[2],
              asymModel_asym = asymModel_asym, 
              asymModel_r0 = asymModel_r0,
              asymModel_lrc = asymModel_lrc,
              correlation_asymModel = correlation_asymModel, 
              timestep_break_1_asymModel = timestep_breaks_asymModel[1],
              timestep_break_2_asymModel = timestep_breaks_asymModel[2],
              trueOverlap_break_1_asymModel = true_overlap_breaks_asymModel[1],
              trueOverlap_break_2_asymModel = true_overlap_breaks_asymModel[2],
              fittedOverlap_break_1_asymModel = fitted_overlap_breaks_asymModel[1],
              fittedOverlap_break_2_asymModel = fitted_overlap_breaks_asymModel[2]
            )
            
          } else { # Other bind to the existing output dataframe
            breakpoints_byMacro_df <- 
              bind_rows(breakpoints_byMacro_df, 
                        tibble(
                          macro = macros[i],
                          micro = mic,
                          foliage = foil,
                          intercept_logModel = intercept_logModel,
                          B1_logModel = B1_logModel,
                          correlation_logModel = correlation_logModel, 
                          timestep_break_1_logModel = timestep_breaks_logModel[1],
                          timestep_break_2_logModel = timestep_breaks_logModel[2],
                          trueOverlap_break_1_logModel = true_overlap_breaks_logModel[1],
                          trueOverlap_break_2_logModel = true_overlap_breaks_logModel[2],
                          fittedOverlap_break_1_logModel = fitted_overlap_breaks_logModel[1],
                          fittedOverlap_break_2_logModel = fitted_overlap_breaks_logModel[2],
                          asymModel_asym = asymModel_asym, 
                          asymModel_r0 = asymModel_r0,
                          asymModel_lrc = asymModel_lrc,
                          correlation_asymModel = correlation_asymModel, 
                          timestep_break_1_asymModel = timestep_breaks_asymModel[1],
                          timestep_break_2_asymModel = timestep_breaks_asymModel[2],
                          trueOverlap_break_1_asymModel = true_overlap_breaks_asymModel[1],
                          trueOverlap_break_2_asymModel = true_overlap_breaks_asymModel[2],
                          fittedOverlap_break_1_asymModel = fitted_overlap_breaks_asymModel[1],
                          fittedOverlap_break_2_asymModel = fitted_overlap_breaks_asymModel[2]
                        ))
          }
          
          
        }
        
        # If NEITHER model was successful...
        else {
          print(paste("....FAILED TO BUILD ANY MODEL."))
        }
      }
    }
  }
  
})

## 5. Write out results #########
write_csv(breakpoints_byMacro_df, "data/04_analysis/temporal_rez/breakpoints_byMacro_logAndAsym.csv")


write_csv(breakpoints_bySite_df, "data/04_analysis/temporal_rez/breakpoints_bySite_logAndAsym.csv")

mountains <- mountains %>% 
  select(site, micro, macro, latitude, elevation_change, height) %>% 
  distinct()

breaks <- breakpoints_bySite_df %>% 
  left_join(mountains)

ggplot(breaks, aes(abs(latitude), timestep_breaks)) +
  geom_jitter(aes(color = micro), size = 3)

## 3. Explore breakpoint df ###############



lines(fitted(bp, breaks = 1) ~ x, col = 4, lwd = 1.5)
lines(fitted(bp, breaks = 2) ~ x, col = 4, lwd = 1.5)
lines(fitted(bp, breaks = 3) ~ x, col = 4, lwd = 1.5)
lines(fitted(bp, breaks = 4) ~ x, col = 4, lwd = 1.5)
lines(fitted(bp, breaks = 5) ~ x, col = 4, lwd = 1.5)
lines(fitted(break_data, breaks = 2) ~ x, col = 2, lwd = 1.5)





log_model <- geom_smooth(data = temporal_rez_data, 
            method = "gam", formula = y ~ log(x))

## Example ############

data <- tibble(
  x = c(0:126),
  y = c(seq(from = 0, to = 10, by = .1), c(seq(from = 10, to = 15, by = .2)))
)
break_data <- breakpoints(y ~ x, h = 30)

plot(y ~ x, pch = 19)
lines(fitted(break_data, breaks = 1) ~ x, col = 4, lwd = 1.5)
lines(fitted(break_data, breaks = 2) ~ x, col = 2, lwd = 1.5)

