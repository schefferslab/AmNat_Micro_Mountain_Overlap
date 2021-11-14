# David Klinges
# File creation date: 2019-07-29
# This script applies an model-fitted elevation change correction to overlap values

correct_elevChange <- function(data, # Dataframe of interest
                               column, # Column name not as string
                               fit, # Desired fit: at the moment only quadratic supported
                               beta0,
                               beta1,
                               beta2
                               ) {
  
  if (fit == "quadratic") {

      column_enquo <- enquo(column)
      quad_formula <- glue::glue("{rlang::quo_text(column_enquo)} ~ a + (b * elevation_change) + (c * elevation_change^2)")
      
      # quadratic fit
      quadratic_fit <- nls(quad_formula, 
                           start = list(a = beta0, # a is intercept, which will change considerably
                                        # contingent on the temporal resolution
                                        b = beta1, 
                                        c = beta2),
                           data = data)
      
      data <- data %>%
        ungroup() %>% 
        filter(is.finite(column)) %>%
        filter(complete.cases(column)) %>%
        filter(is.finite(elevation_change)) %>%
        filter(complete.cases(elevation_change)) %>%
        mutate(column_elevCorr = quadratic_fit$m$resid())
  }
  
  if (fit == "linear") {
    
    column_enquo <- enquo(column)
    linear_formula <- glue::glue("{rlang::quo_text(column_enquo)} ~ elevation_change")
    
    # linear fit
    linear_fit <- lm(linear_formula, data = data)
    
    data <- data %>%
      ungroup() %>% 
      filter(is.finite(column)) %>%
      filter(complete.cases(column)) %>%
      filter(is.finite(elevation_change)) %>%
      filter(complete.cases(elevation_change)) %>%
      mutate(column_elevCorr = linear_fit$residuals)
  }

  return(data$column_elevCorr)
}
