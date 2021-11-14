# David Klinges
# File creation date: 2019-07-11
# This script plots temporal rez data


## 1. Workspace prep ############

library(tidyverse)

temporal_rez <- read_csv("data/03_compiled/temporal_rez/temporal_rez_data_elevControlled.csv")


## 2. Data cleaning and filtering #########

# temporal_rez <- temporal_rez %>% 
#   mutate(log_overlap = log(overlap_v2)) %>% 
#   # We're going to filter to just those rows that don't throw a NaN or inf when
#   # you take the log.
#   # NOTE: we're still going to use overlap_v2 and the log() function in the formula]
#   # in nls() below, which will help with testing correlations and plotting later. 
#   # But doing this filtering ensures we remove all the "bad" observations, meaning
#   # the negative 
#   filter(complete.cases(log_overlap) & is.finite(log_overlap))

temporal_rez <- temporal_rez %>% 
  mutate(overlap_v2 = overlap_v2 + 50)

macros <- unique(temporal_rez$macro)

## 3. Non-linear regression #########

for (macro in 1:length(unique(macros))) {
  
  # Filter to that macro
  chosen_macro <- macros[macro]
  macro_data <- temporal_rez %>% 
    filter(macro == chosen_macro)

  for (micro in 1:length(unique(macro_data$micro))) {
    
    micros <- unique(macro_data$micro)
    chosen_micro <- micros[micro]
    # And filter to the given micro. These are separate for convenience sake
    # b/c not all macros have all 3 micros (most don't)
    macroMicro_data <- macro_data %>% 
      filter(micro == chosen_micro)
   
    ## ....A. Test model fit ##########
    
    # TRY to acquire a model fit with a self-starting asymptotic model...
    # try statement ensures that if the data inputs fail, the loop won't end
    fit <- try(nls(overlap_v2 ~ SSasymp(temporal_rez, Asym, r0, lrc), macroMicro_data), 
                    silent = FALSE)

    print(fit)
    
    # If the data was valid and the non-linear model converged....
    if (class(fit) == "nls") {
     
      # ...Find and save the correlation between the data and model predictions
      correlation <- cor(macroMicro_data$overlap_v2, predict(fit))
     
      ## .....B. Save output ##########
      # Save the model and fit
      output_iter <- tibble(
         macro = macros[macro],
         micro = micros[micro],
         fit = fit[2],
         correlation = correlation
      )
     
     ## ....C. Plot data and predictions ###########
     
     # Generate a df for the predictions
     predictions <- tibble(
       temporal_rez = macroMicro_data$temporal_rez,
       predicted_overlap = predict(fit)
     )
     
     # Plot
     # ggplot(macroMicro_data = temporal_rez, aes(temporal_rez, overlap_v2)) +
     #   geom_point() +
     #   geom_line(macroMicro_data = predictions, aes(temporal_rez, predicted_overlap))

     plot(macroMicro_data$temporal_rez, macroMicro_data$overlap_v2, 
          main = paste(unique(macroMicro_data$macro), unique(macroMicro_data$micro),
                       sep = " ")) +
     lines(macroMicro_data$temporal_rez, predict(fit),lty=2,col="red",lwd=3)
     
     # ggsave(, paste0())
     
     ## ....D. Bind to final output #########
     if (macro == 1 & micro == 1) {
       output_final <- output_iter
     } else {
       output_final <- bind_rows(output_final, output_iter)
     }
    }
  }
}

subset <- temporal_rez %>% 
  filter(macro == "deciduous", micro == "surface")

ggplot(subset, aes(temporal_rez, overlap_v2)) +
  geom_point(aes(color = site))

# ## SCRAPPED #############
# ## ....A. Scrapped SSasympOrig model ########
# 
# # Set the asymptote at the maximum overlap, for now
# 
# local({   Asym <- max(north_carolina_soil$overlap_v2, na.rm = TRUE); 
# lrc <- -10
# SSasympOrig(input = north_carolina_soil$temporal_rez, 
#             Asym,
#             lrc)
# })
# 
# start <- getInitial(overlap_v2 ~ SSasympOrig(input = north_carolina_soil$temporal_rez, 
#                                              Asym = Asym,
#                                              lrc = lrc),
#                     data = north_carolina_soil
# )
# 
# asym_regression <- nls(overlap_v2 ~ SSasympOrig(input = temporal_rez, 
#                                                 Asym,
#                                                 lrc),
#                        start = start,
#                        data = north_carolina_soil
# )
# 
# summary(asym_regression)
# 
# 
# cor(north_carolina_soil$overlap_v2, predict(asym_regression))
# 
# 
# plot(north_carolina_soil$temporal_rez, north_carolina_soil$overlap_v2)
# lines(north_carolina_soil$temporal_rez, predict(asym_regression),lty=2,col="red",lwd=3)
# 
# ## ....B. Scrapped custom SS log model ###########
# 
# log_model <- function(x, rate, asym) {
#   rate * log(x) + asym
# }
# 
# init_log_model <-
#   function(mCall, data, LHS) {
#     y <- eval(LHS,data)
#     rate <- 1.3
#     asym <- max(y)
#   }
# 
# SS_logModel <- selfStart(log_model, init_log_model, c("rate", "asym"))
# fitmodel <- nls(overlap_v2 ~ SS_logModel(x = temporal_rez, rate, asym), 
#                 data = north_carolina_soil)
# 
# ## ....D. Test plotting #########
# 
# ggplot(data = filter(north_carolina_soil, micro == "soil"), aes(temporal_rez, overlap_v2)) + 
#   geom_point()
# 
# ggplot(data = filter(mitchell_rez, micro == "soil", season == "winter"), aes(temporal_rez, overlap_v2)) + 
#   geom_point() +
#   ylim(c(-6, 10))
# 
#   geom_line(data = tibble(north_carolina_soil$temporal_rez, predict(nl_regression)), 
#             aes(temporal_rez, predict(nl_regression)))
# 
# ggplot(data = north_carolina_soil, aes(temporal_rez, overlap_v2)) + 
#   geom_point()
# ## 3. Simulated data ######
# 
# x<-seq(0,50,1)
# y<-((runif(1,10,20)*x)/(runif(1,0,10)+x))+rnorm(51,0,1)
# 
# m<-nls(y~a*x/(b+x), start = c(a = 1, b = 1))
# 
# cor(y,predict(m))
# 
# plot(x,y)
# lines(x,predict(m),lty=2,col="red",lwd=3)
