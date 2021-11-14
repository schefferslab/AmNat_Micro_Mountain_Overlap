# David Klinges
# This script conducts Kruskal-Wallis tests of d-scores from leaf-on vs leaf-off
#   d-scores compare thermal distribution of microhabitats at high elevation vs 
#   low elevation, only conducted for North Carolina data
# Dependant on objects created in "North Carolina Overlap Script_DHK edits"

setwd("/Volumes/PHOTOS/Career Stuff/Mountain Passes/Scripts")
getwd()

require(tidyverse)

#input temp only
nc_data<-read.csv("compiled.dataset.csv")

head(nc_data)

######## Create overlap data frames ##########
soil_overlap <- play_500_soil[, -c(2:5, 7)]
soil_overlap <- mutate(soil_overlap, foliage = case_when((julian < 150 | julian  > 280) ~ 0, (julian > 150 & julian < 281) ~ 1))

ground_overlap <- play_500_grnd[, -c(2:5, 7)]
ground_overlap <- mutate(ground_overlap, foliage = case_when((julian < 150 | julian  > 280) ~ 0, (julian > 150 & julian < 281) ~ 1))

canopy_overlap <- play_500_can[, -c(2:5, 7)]
canopy_overlap <- mutate(canopy_overlap, foliage = case_when((julian < 150 | julian  > 280) ~ 0, (julian > 150 & julian < 281) ~ 1))


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

############KRUSKAL-WALLIS OF OVERLAP VALUES, LEAF-OFF VS LEAF-ON#############
  ########### Compare daily averages ###########
#Compare leaf-off and -on, soil. Overlap method 1
kruskal.test(overlap ~ foliage, data = soil_overlap)
#Compare leaf-off and -on, soil. Overlap method 2
kruskal.test(overlap_v2 ~ foliage, data = soil_overlap)

#Compare leaf-off and -on, ground. Overlap method 1
kruskal.test(overlap ~ foliage, data = ground_overlap)
#Compare leaf-off and -on, ground. Overlap method 2
kruskal.test(overlap_v2 ~ foliage, data = ground_overlap)

#Compare leaf-off and -on, canopy. Overlap method 1
kruskal.test(overlap ~ foliage, data = canopy_overlap)
#Compare leaf-off and -on, canopy. Overlap method 2
kruskal.test(overlap_v2 ~ foliage, data = canopy_overlap)

  ########### Compare weekly averages ###########
#Compare leaf-off and -on, soil. Overlap method 1
kruskal.test(overlap ~ foliage, data = soil_overlap_weekly)
#Compare leaf-off and -on, soil. Overlap method 2
kruskal.test(overlap_v2 ~ foliage, data = soil_overlap_weekly)

#Compare leaf-off and -on, ground. Overlap method 1
kruskal.test(overlap ~ foliage, data = ground_overlap_weekly)
#Compare leaf-off and -on, ground. Overlap method 2
kruskal.test(overlap_v2 ~ foliage, data = ground_overlap_weekly)

#Compare leaf-off and -on, canopy. Overlap method 1
kruskal.test(overlap ~ foliage, data = canopy_overlap_weekly)
#Compare leaf-off and -on, canopy. Overlap method 2
kruskal.test(overlap_v2 ~ foliage, data = canopy_overlap_weekly)

  ########### Compare monthly averages ##########


soil_overlap_off <- filter(soil_overlap, foliage == 0)
soil_overlap_on <- filter(soil_overlap, foliage == 1)
ground_overlap_off <- filter(ground_overlap, foliage == 0)
ground_overlap_on <- filter(ground_overlap, foliage == 1)
canopy_overlap_off <- filter(canopy_overlap, foliage == 0)
canopy_overlap_on <- filter(canopy_overlap, foliage == 1)


t.test(canopy_overlap_off$overlap, canopy_overlap_on$overlap)
