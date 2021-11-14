#David Klinges
#this script conducts Kruskal-Wallis tests of temperature between elevations
#only for temperate zone
#Step 1: organize data
# - uses combined min and max data
# - several levels of data size: full data, daily min and max, weekly AVERAGE of min and max (which is different from weekly min and max)
#Step 2: conduct KW tests, for each microbiome, SEPERATELY for leaf-on and leaf-off
#Step 3: compare p-values of leaf-on and leaf-off
#dependant on objects created in "North Carolina Overlap Script_DHK edits"

require(tidyverse)


############ Prep data for comparisons ###########
  ####### Seperate leaf-on and leaf-off ###########
leafon_elev_1700_can <- filter(elev_1700_can, julian >150 & julian<281)
leafon_elev_1700_grnd <- filter(elev_1700_grnd, julian >150 & julian<281)
leafon_elev_1700_soil <- filter(elev_1700_soil, julian >150 & julian<281)
leafon_elev_500_can <- filter(elev_500_can, julian >150 & julian<281)
leafon_elev_500_grnd <- filter(elev_500_grnd, julian >150 & julian<281)
leafon_elev_500_soil <- filter(elev_500_soil, julian >150 & julian<281)
leafon <- rbind(leafon_elev_1700_can, leafon_elev_1700_grnd, leafon_elev_1700_soil,
                leafon_elev_500_can, leafon_elev_500_grnd, leafon_elev_500_soil)

leafon_elev_1700_can <- filter(elev_1700_can, julian <150 | julian>280)
leafoff_elev_1700_grnd <- filter(elev_1700_grnd, julian <150 | julian>280)
leafoff_elev_1700_soil <- filter(elev_1700_soil, julian <150 | julian>280)
leafoff_elev_500_can <- filter(elev_500_can, julian <150 | julian>280)
leafoff_elev_500_grnd <- filter(elev_500_grnd, julian <150 | julian>280)
leafoff_elev_500_soil <- filter(elev_500_soil, julian <150 | julian>280)
leafoff <- rbind(leafon_elev_1700_can, leafon_elev_1700_grnd, leafon_elev_1700_soil,
                 leafon_elev_500_can, leafon_elev_500_grnd, leafon_elev_500_soil)

#create list of leafoff dfs
off_list <- as.list(leafoff_elev_500_soil, leafoff_elev_500_grnd, leafoff_elev_500_can,
                    leafoff_elev_1700_soil, leafoff_elev_1700_grnd,leafoff_elev_1700_can)

#create list of leafon dfs
on_list <- as.list(leafon_elev_500_soil, leafon_elev_500_grnd, leafon_elev_500_can,
                    leafon_elev_1700_soil, leafon_elev_1700_grnd,leafon_elev_1700_can)

  ####### Calc daily min and max #########
#create function that creates daily min and daily max dfs, and then rbinds them
dailyminmax <- function(df) {
  dailymin <- df %>%
    group_by(julian) %>%
    summarize(Temp = min(Temp)) %>%
    mutate(minmax = "min")
  dailymax <- df %>%
    group_by(julian) %>%
    summarize(Temp = max(Temp)) %>%
  mutate(minmax = "max")
  daily <- rbind(dailymin, dailymax)
  return(daily)
}

#leaf off
leafoff_elev_500_soil_daily <- dailyminmax(leafoff_elev_500_soil)
leafoff_elev_500_grnd_daily <- dailyminmax(leafoff_elev_500_grnd)
leafoff_elev_500_can_daily <- dailyminmax(leafoff_elev_500_can)
leafoff_elev_1700_soil_daily <- dailyminmax(leafoff_elev_1700_soil)
leafoff_elev_1700_grnd_daily <- dailyminmax(leafoff_elev_1700_grnd)
leafoff_elev_1700_can_daily <- dailyminmax(leafoff_elev_1700_can)

dailyoff_list <- as.list(leafoff_elev_500_soil_daily, leafoff_elev_500_grnd_daily, leafoff_elev_500_can_daily,
                    leafoff_elev_1700_soil_daily, leafoff_elev_1700_grnd_daily,leafoff_elev_1700_can_daily)
#leaf on
leafon_elev_500_soil_daily <- dailyminmax(leafon_elev_500_soil)
leafon_elev_500_grnd_daily <- dailyminmax(leafon_elev_500_grnd)
leafon_elev_500_can_daily <- dailyminmax(leafon_elev_500_can)
leafon_elev_1700_soil_daily <- dailyminmax(leafon_elev_1700_soil)
leafon_elev_1700_grnd_daily <- dailyminmax(leafon_elev_1700_grnd)
leafon_elev_1700_can_daily <- dailyminmax(leafon_elev_1700_can)

dailyon_list <- as.list(leafon_elev_500_soil_daily, leafon_elev_500_grnd_daily, leafon_elev_500_can_daily,
                    leafon_elev_1700_soil_daily, leafon_elev_1700_grnd_daily,leafon_elev_1700_can_daily)

  ###### add foliage column ########
#create new column that corresponds to foliage state
foliage_off <- function(df) {
out <- df %>%
    mutate(foliage = "leafoff")
}

foliage_on <- function(df) {
out <- df %>%
    mutate(foliage = "leafon")
}

#leaf off
leafoff_elev_500_soil_daily <- foliage_off(leafoff_elev_500_soil_daily)
leafoff_elev_500_grnd_daily <- foliage_off(leafoff_elev_500_grnd_daily)
leafoff_elev_500_can_daily <- foliage_off(leafoff_elev_500_can_daily)
leafoff_elev_1700_soil_daily <- foliage_off(leafoff_elev_1700_soil_daily)
leafoff_elev_1700_grnd_daily <- foliage_off(leafoff_elev_1700_grnd_daily)
leafoff_elev_1700_can_daily <- foliage_off(leafoff_elev_1700_can_daily)
#leaf on
leafon_elev_500_soil_daily <- foliage_on(leafon_elev_500_soil_daily)
leafon_elev_500_grnd_daily <- foliage_on(leafon_elev_500_grnd_daily)
leafon_elev_500_can_daily <- foliage_on(leafon_elev_500_can_daily)
leafon_elev_1700_soil_daily <- foliage_on(leafon_elev_1700_soil_daily)
leafon_elev_1700_grnd_daily <- foliage_on(leafon_elev_1700_grnd_daily)
leafon_elev_1700_can_daily <- foliage_on(leafon_elev_1700_can_daily)

  ###### add elevation column ##########
elev_500 <- function(df) {
  out <- df %>%
    mutate(elev = 500)
}

elev_1700 <- function(df) {
  out <- df %>%
    mutate(elev = 1700)
}

#add elev 500
leafoff_elev_500_soil_daily <- elev_500(leafoff_elev_500_soil_daily)
leafoff_elev_500_grnd_daily <- elev_500(leafoff_elev_500_grnd_daily)
leafoff_elev_500_can_daily <- elev_500(leafoff_elev_500_can_daily)
leafon_elev_500_soil_daily <- elev_500(leafon_elev_500_soil_daily)
leafon_elev_500_grnd_daily <- elev_500(leafon_elev_500_grnd_daily)
leafon_elev_500_can_daily <- elev_500(leafon_elev_500_can_daily)
#add elev 1700
leafoff_elev_1700_soil_daily <- elev_1700(leafoff_elev_1700_soil_daily)
leafoff_elev_1700_grnd_daily <- elev_1700(leafoff_elev_1700_grnd_daily)
leafoff_elev_1700_can_daily <- elev_1700(leafoff_elev_1700_can_daily)
leafon_elev_1700_soil_daily <- elev_1700(leafon_elev_1700_soil_daily)
leafon_elev_1700_grnd_daily <- elev_1700(leafon_elev_1700_grnd_daily)
leafon_elev_1700_can_daily <- elev_1700(leafon_elev_1700_can_daily)


##Combine 500 and 1700
leafoff_soil_daily <- rbind(leafoff_elev_500_soil_daily, leafoff_elev_1700_soil_daily)
leafoff_grnd_daily <- rbind(leafoff_elev_500_grnd_daily, leafoff_elev_1700_grnd_daily)
leafoff_can_daily <- rbind(leafoff_elev_500_can_daily, leafoff_elev_1700_can_daily)

leafon_soil_daily <- rbind(leafon_elev_500_soil_daily, leafon_elev_1700_soil_daily)
leafon_grnd_daily <- rbind(leafon_elev_500_grnd_daily, leafon_elev_1700_grnd_daily)
leafon_can_daily <- rbind(leafon_elev_500_can_daily, leafon_elev_1700_can_daily)


############ Kruskal-Wallis, 20-min interval temp (combined min and max)#############
#uses raw data

soil_leafoff <- filter(elev_soil, julian < 150 | julian  > 280)
soil_leafon <- filter(elev_soil, julian > 150 & julian < 280)
grnd_leafoff <- filter(elev_grnd, julian < 150 | julian  > 280)
grnd_leafon <- filter(elev_grnd, julian > 150 & julian < 280)
can_leafoff <- filter(elev_can, julian < 150 | julian  > 280)
can_leafon <- filter(elev_can, julian > 150 & julian < 280)

#Compare 500 and 1700 for soil, leaf off
kruskal.test(Temp ~ elev, data = soil_leafoff)

#Compare 500 and 1700 for soil, leaf on
kruskal.test(Temp ~ elev, data = soil_leafon)

#Compare 500 and 1700 for ground, leaf off
kruskal.test(Temp ~ elev, data = grnd_leafoff)

#Compare 500 and 1700 for ground, leaf on
kruskal.test(Temp ~ elev, data = grnd_leafon)

#Compare 500 and 1700 for canopy, leaf off
kruskal.test(Temp ~ elev, data = can_leafoff)

#Compare 500 and 1700 for canopy, leaf on
kruskal.test(Temp ~ elev, data = can_leafon)


############ Kruskal-Wallis, daily temp (combined min and max) ########

kruskal.test(Temp ~ elev, data = leafoff_soil_daily)
kruskal.test(Temp ~ elev, data = leafoff_grnd_daily)
kruskal.test(Temp ~ elev, data = leafoff_can_daily)

?kruskal.test(Temp ~ elev, data = leafon_soil_daily)
kruskal.test(Temp ~ elev, data = leafon_grnd_daily)
kruskal.test(Temp ~ elev, data = leafon_can_daily)



############ Kruskal-Wallis, weekly temp (combined min and max) #########

#Seperate min and max data, so we can average each seperately
sep_min <- function(df) {
  out <- df %>%
    filter(minmax == "min")
}

sep_max <- function(df) {
  out <- df %>%
    filter(minmax == "max")
}

#min
leafoff_soil_daily_min <- sep_min(leafoff_soil_daily)
leafoff_grnd_daily_min <- sep_min(leafoff_grnd_daily)
leafoff_can_daily_min <- sep_min(leafoff_can_daily)
leafon_soil_daily_min <- sep_min(leafon_soil_daily)
leafon_grnd_daily_min <- sep_min(leafon_grnd_daily)
leafon_can_daily_min <- sep_min(leafon_can_daily)
#max
leafoff_soil_daily_max <- sep_max(leafoff_soil_daily)
leafoff_grnd_daily_max <- sep_max(leafoff_grnd_daily)
leafoff_can_daily_max <- sep_max(leafoff_can_daily)
leafon_soil_daily_max <- sep_max(leafon_soil_daily)
leafon_grnd_daily_max <- sep_max(leafon_grnd_daily)
leafon_can_daily_max <- sep_max(leafon_can_daily)


#function to subset to interval
#there will be a warning if there is any columns with strings that are summarized
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

#for min data
leafoff_soil_weekly_min <- interval_set(leafoff_soil_daily_min, 7, "week")
leafoff_grnd_weekly_min <- interval_set(leafoff_grnd_daily_min, 7, "week")
leafoff_can_weekly_min <- interval_set(leafoff_can_daily_min, 7, "week")
leafon_soil_weekly_min <- interval_set(leafon_soil_daily_min, 7, "week")
leafon_grnd_weekly_min <- interval_set(leafon_grnd_daily_min, 7, "week")
leafon_can_weekly_min <- interval_set(leafon_can_daily_min, 7, "week")
#for max data
leafoff_soil_weekly_max <- interval_set(leafoff_soil_daily_max, 7, "week")
leafoff_grnd_weekly_max <- interval_set(leafoff_grnd_daily_max, 7, "week")
leafoff_can_weekly_max <- interval_set(leafoff_can_daily_max, 7, "week")
leafon_soil_weekly_max <- interval_set(leafon_soil_daily_max, 7, "week")
leafon_grnd_weekly_max <- interval_set(leafon_grnd_daily_max, 7, "week")
leafon_can_weekly_max <- interval_set(leafon_can_daily_max, 7, "week")

#combine min and max again
leafoff_soil_weekly <- rbind(leafoff_soil_weekly_min, leafoff_soil_weekly_max)
leafoff_grnd_weekly <- rbind(leafoff_grnd_weekly_min, leafoff_grnd_weekly_max)
leafoff_can_weekly <- rbind(leafoff_can_weekly_min, leafoff_can_weekly_max)
leafon_soil_weekly <- rbind(leafon_soil_weekly_min, leafon_soil_weekly_max)
leafon_grnd_weekly <- rbind(leafon_grnd_weekly_min, leafon_grnd_weekly_max)
leafon_can_weekly <- rbind(leafon_can_weekly_min, leafon_can_weekly_max)

kruskal.test(Temp ~ elev, data = leafoff_soil_weekly)
kruskal.test(Temp ~ elev, data = leafoff_grnd_weekly)
kruskal.test(Temp ~ elev, data = leafoff_can_weekly)

kruskal.test(Temp ~ elev, data = leafon_soil_weekly)
kruskal.test(Temp ~ elev, data = leafon_grnd_weekly)
kruskal.test(Temp ~ elev, data = leafon_can_weekly)






############ Kruskal-Wallis, monthly temp (combined min and max) ########

#for min data
leafoff_soil_monthly_min <- interval_set(leafoff_soil_daily_min, 30, "month")
leafoff_grnd_monthly_min <- interval_set(leafoff_grnd_daily_min, 30, "month")
leafoff_can_monthly_min <- interval_set(leafoff_can_daily_min, 30, "month")
leafon_soil_monthly_min <- interval_set(leafon_soil_daily_min, 30, "month")
leafon_grnd_monthly_min <- interval_set(leafon_grnd_daily_min, 30, "month")
leafon_can_monthly_min <- interval_set(leafon_can_daily_min, 30, "month")
#for max data
leafoff_soil_monthly_max <- interval_set(leafoff_soil_daily_max, 30, "month")
leafoff_grnd_monthly_max <- interval_set(leafoff_grnd_daily_max, 30, "month")
leafoff_can_monthly_max <- interval_set(leafoff_can_daily_max, 30, "month")
leafon_soil_monthly_max <- interval_set(leafon_soil_daily_max, 30, "month")
leafon_grnd_monthly_max <- interval_set(leafon_grnd_daily_max, 30, "month")
leafon_can_monthly_max <- interval_set(leafon_can_daily_max, 30, "month")

#combine min and max again
leafoff_soil_monthly <- rbind(leafoff_soil_monthly_min, leafoff_soil_monthly_max)
leafoff_grnd_monthly <- rbind(leafoff_grnd_monthly_min, leafoff_grnd_monthly_max)
leafoff_can_monthly <- rbind(leafoff_can_monthly_min, leafoff_can_monthly_max)
leafon_soil_monthly <- rbind(leafon_soil_monthly_min, leafon_soil_monthly_max)
leafon_grnd_monthly <- rbind(leafon_grnd_monthly_min, leafon_grnd_monthly_max)
leafon_can_monthly <- rbind(leafon_can_monthly_min, leafon_can_monthly_max)

kruskal.test(Temp ~ elev, data = leafoff_soil_monthly)
kruskal.test(Temp ~ elev, data = leafoff_grnd_monthly)
kruskal.test(Temp ~ elev, data = leafoff_can_monthly)

kruskal.test(Temp ~ elev, data = leafon_soil_monthly)
kruskal.test(Temp ~ elev, data = leafon_grnd_monthly)
kruskal.test(Temp ~ elev, data = leafon_can_monthly)

