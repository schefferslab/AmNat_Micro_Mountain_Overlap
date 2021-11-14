# David Klinges
# klingesd@si.edu
# Interval set
# Subset a dataset to a given interval (provided in # of rows)
# Used to group daily observations to weekly or monthly


#Create a function that summarizes a data frame to a specific interval of observations
interval_set <- function(df, int, new) {
  out<- data.frame()
  temp <- df
  
  #for 1 to the # of times the interval fits into the length of df, rounded up
  for(w in 1:ceiling(nrow(temp)/int)) { 
    loop <- temp %>%
      slice(1:int) %>% #slice to just the number of rows corresponding to the interval
      mutate(new = w)
    out <- rbind(out, loop)
    temp <- slice(temp, (int+1):nrow(temp))
  }
  out <- out %>%
    group_by(new) %>%
    summarize_all(funs(mean))
  return(out)
}

######## Summarize overlap to weekly values ##########
soil_overlap_weekly <- interval_set(soil_overlap, 7, "week")
ground_overlap_weekly <- interval_set(ground_overlap, 7, "week")
canopy_overlap_weekly <- interval_set(canopy_overlap, 7, "week")
######## Summarize overlap to monthly values ##########
soil_overlap_monthly <- interval_set(soil_overlap, 30, "month")
ground_overlap_monthly <- interval_set(ground_overlap, 30, "month")
canopy_overlap_monthly <- interval_set(canopy_overlap, 30, "month")
