# David Klinges
# File creation date: 2019.09.16
# This script curates Alaska Ameriflux sites to 
#   prep for analysis and generating figures

## 1. Workspace Prep #############

library(tidyverse)
library(lubridate)
library(readxl)

## Import data 

## ....A. Import low: Happy Valley ##################

# Note: as of 2019-09-19 only year 1996 is used, which is imported outside the 
# below loop

# low_HVa_raw <- read_csv("data/01_primary/arctic/AK_ameriflux/low/AMF_US-HVa_BASE_HH_2-1.csv",
#                         skip = 2)
# 
# low_HV_list <- c(list.files(path = "data/01_primary/arctic/AK_ameriflux/low/happyValley_soil",
#                             # All of the data files are in their own subdirectories
#                             #   so need to be recursive
#                             pattern = ".dat", recursive = FALSE))
# 
# not_happy <- which(sapply(low_HV_list, function(x)
#   !grepl("hap", x)))
# 
# low_HV_list <- low_HV_list[-not_happy]

# Import all the data and bind together
# for (i in 1:length(low_HV_list)) {
#   
#   # Read in data
#   low_HV_soil <- read_delim(paste0(getwd(), "/data/01_primary/arctic/AK_ameriflux/low/happyValley_soil/", 
#                                        low_HV_list[[i]]),
#                                 delim = ",")
#   
#   # If without column headers, don't import now- we'll do so separately
#   if(!"Date" %in% colnames(low_HV_soil)) {
#     year <- paste0("19", substr(low_HV_list[[i]], start = 4, stop = 5))
#     
#     colnames(low_HV_soil) <- c("DayIndex","1cm(C)","8cm(C)","15cm(C)",
#                                "22cm(C)","29cm(C)","50cm(C)","75cm(C)","100cm(C)")
#     # low_HV_soil <- NA
#   }
# 
#   if (i == 1) {
#     low_HV_soil_out <- low_HV_soil
#   } else {
#     low_HV_soil_out <- bind_rows(low_HV_soil_out, low_HV_soil)
#   }
# }

# Now import files without col headers
# low_HV_1993 <- read_delim("data/01_primary/arctic/AK_ameriflux/low/happyValley_soil/hap93-94.dat",
#                           delim = " ", col_names = c("HourIndex","1cm","8cm",
#                                                      "15cm","22cm", "29cm","50cm","75cm","100cm"))
# 
# 
# 
# low_HV_1995 <-  read_delim("data/01_primary/arctic/AK_ameriflux/low/happyValley_soil/hap95.dat",
#                            delim = " ", col_names = c("HourIndex","1cm","8cm",
#                                                       "15cm","22cm", "29cm","50cm","75cm","100cm"))
# 
low_HV_1996 <-  read_delim("data/01_primary/arctic/AK_ameriflux/low/happyValley_soil/hap96.dat",
                           delim = " ", col_names = c("HourIndex","1cm","8cm",
                                    "15cm","22cm", "29cm","50cm","75cm","100cm"))


# low_HV_2000 <-  read_delim("data/01_primary/arctic/AK_ameriflux/low/happyValley_soil/hap00t.dat",
#                            delim = " ", col_names = c("HourIndex","1cm","8cm",
#                                                       "15cm","22cm", "29cm","50cm","75cm","100cm"))
# 
# low_HV_2001 <-  read_delim("data/01_primary/arctic/AK_ameriflux/low/happyValley_soil/hap01t.dat",
#                            delim = " ", col_names = c("HourIndex","1cm","8cm",
#                                                       "15cm","22cm", "29cm","50cm","75cm","100cm"))
# 

## ....B. Import high: Toolik Lake ###############


toolik_high_soil_raw <- read_csv("data/01_primary/arctic/AK_ameriflux/toolik_high/disaggregated_soil/96dltlo.csv")

## Discarded code: can't use air temps, and soil temps are aggregated
# toolik_list_air <- c(list.files(path = "data/01_primary/arctic/AK_ameriflux/toolik_high/ggd906_main",
#                            # All of the data files are in their own subdirectories
#                            #   so need to be recursive
#                            pattern = ".txt", recursive = TRUE))
# 
# # Import all the data and bind together
# for (i in 1:length(toolik_list_air)) {
# 
#   # Read in data
#   high_toolik_air <- read_delim(paste0(getwd(), "/data/01_primary/arctic/AK_ameriflux/toolik_high/ggd906_main/", 
#                                   toolik_list_air[[i]]),
#                            delim = ",")
#   
#   # To avoid binding numeric/character errors, select down cols and coerce to double
#   # Soil temps are daily averages so not including
#   high_toolik_air <- high_toolik_air %>% 
#     dplyr::select(DATE, JULIAN, `AT 1M`, `ATMAX 1M`, `ATMIN 1M`) %>% 
#     mutate_at(vars(JULIAN, `ATMAX 1M`, `ATMIN 1M`), as.double)
# 
#   if (i == 1) {
#     high_toolik_air_out <- high_toolik_air
#   } else {
#     high_toolik_air_out <- bind_rows(high_toolik_air_out, high_toolik_air)
#   }
# }
# 
# toolik_list_soil <- c(list.files(path = "data/01_primary/arctic/AK_ameriflux/toolik_high/ggd906_tussock",
#                                 # All of the data files are in their own subdirectories
#                                 #   so need to be recursive
#                                 pattern = ".txt", recursive = TRUE))
# 
# # Import all the data and bind together
# for (i in 1:length(toolik_list_soil)) {
#   
#   # Read in data
#   high_toolik_soil <- read_delim(paste0(getwd(), "/data/01_primary/arctic/AK_ameriflux/toolik_high/ggd906_tussock/", 
#                                        toolik_list_soil[[i]]),
#                                 delim = ",")
#   
#   if("DATE" %in% colnames(high_toolik_soil)) {
#     
#     high_toolik_soil <- high_toolik_soil %>% 
#       dplyr::rename(Date = DATE, Julian = JULIAN)
#   }
#   # To avoid binding numeric/character errors, select down cols and coerce to double
#   # Soil temps are daily averages so not including
#   high_toolik_soil <- high_toolik_soil %>% 
#     dplyr::select(Date, Julian, `CT TEMP 3M`,  `CT TEMP MAX 3M`, `CT TEMP MIN 3M`, 
#                   `CT1 10CM`) %>% 
#     mutate_at(vars(Julian, `CT TEMP 3M`,  `CT TEMP MAX 3M`, `CT TEMP MIN 3M`, 
#                    `CT1 10CM`), as.double)
#   
#   if (i == 1) {
#     high_toolik_soil_out <- high_toolik_soil
#   } else {
#     high_toolik_soil_out <- bind_rows(high_toolik_soil_out, high_toolik_soil)
#   }
# }
# 
# # USE ONLY CONTROL TEMPS HERE
# high_toolik_air <- read_csv("data/01_primary/arctic/AK_ameriflux/toolik_high/2000-present_Wet_Sedge_Houlry Weather.csv")

## ....C. Import snow depth #######

# snowdepth <- read_tsv("data/01_primary/arctic/AK_ameriflux/snowdepth/chandalar_shelf_dot.txt")
snowdepth <- read_excel("data/01_primary/arctic/AK_ameriflux/snowdepth/IC_1991 2007_2018_3hour_SnowD.xlsx")

## 2. Curate data #########

## ....A. Low: Happy Valley ##############

low_HV_soil <- low_HV_1996 %>% 
  mutate(datetime = as_date(seq.POSIXt(from = ISOdate(1995, 08, 19, 0, 0, 0), 
                               to = ISOdate(1996, 08, 05, 0, 0, 0),
                               by = "hour"))) %>% 
  dplyr::rename(soil = `8cm`) %>% 
  mutate(year = year(datetime)) %>% 
  mutate(julian = yday(datetime)) %>% 
  dplyr::select(year, julian, soil)


# low_HV_soil <- low_HV_soil_out %>% 
#   dplyr::select(Date, Time, `8cm(C)`) %>% 
#   # only include rows that have a Time, or else you're just getting daily avgs
#   filter(complete.cases(Time)) %>% 
#   separate(Date, into = c("day", "month", "year"), sep = "-") %>% 
#   mutate(year = as.double(year)) %>% 
#   mutate(year = ifelse(year > 20, year + 1900, year + 2000)) %>% 
#   mutate(date = ymd(paste(year, month, day, sep = "-"))) %>% 
#   dplyr::rename(soil = `8cm(C)`) %>% 
#   mutate(julian = yday(date)) %>% 
#   dplyr::select(year, julian, soil)
# 
# low_HVa <- low_HVa_raw %>% 
#   dplyr::select(TIMESTAMP_START, TA, TS_2) %>%
#   dplyr::rename(time = TIMESTAMP_START, surface = TA, soil = TS_2) %>%
#   mutate(surface = replace(surface, surface == -9999, NA)) %>%
#   mutate(soil = replace(soil, soil == -9999, NA)) %>%
#   separate(time, into = c("year", "month"), sep = 4) %>%
#   separate(month, into = c("month", "day"), sep = 2) %>%
#   separate(day, into = c("day", "hour"), sep = 2) %>%
#   separate(hour, into = c("hour", "minute"), sep = 2) %>%
#   mutate(date = as_date(paste(year, month, day, sep = "-"))) %>% 
#   mutate(julian = yday(date)) %>% 
#   mutate(year = as.double(year)) %>% 
#   dplyr::select(year, julian, surface, soil)
# 
# low_happyValley <- low_HV_soil %>% 
#   bind_rows(low_HVa)

low_happyValley <- low_HV_soil

## ....B. High: Toolik Lake ############

toolik_high_soil <- toolik_high_soil_raw %>% 
  dplyr::rename(soil = `SOIL1 20CM`, year = YEAR, julian = JULIAN) %>%
  dplyr::select(year, julian, soil)

## 3. Wrangle snow data ###########

# Coords are 68.607554,-149.300246


snowdepth <- snowdepth %>% 
  mutate(year = year(Date)) %>% 
  mutate(julian = yday(Date)) %>% 
  rename(snowdepth = SnowDepth) %>% 
  mutate(snowdepth = as.double(snowdepth)) %>% 
  filter(complete.cases(snowdepth)) %>% 
  dplyr::select(year, julian, snowdepth)

snowdepth_avg <- snowdepth %>% 
  group_by(julian) %>% 
  summarize(snowdepth = mean(snowdepth, na.rm = TRUE))

## 3. Conduct overlap data curation #########

low_happyValley <- low_happyValley %>% 
  mutate(surface = NA)

toolik_high_soil <- toolik_high_soil %>% 
  mutate(surface = NA)

source("scripts/00_source_code/data_curation_program.R")
northAK_list <- prep_flux_data(low_dataset = low_happyValley, 
                            high_dataset = toolik_high_soil)


# Now remove surface cols
for (i in 1:length(northAK_list)) {
  
  northAK_list[[i]] <- northAK_list[[i]] %>% 
    dplyr::select(-contains("surface"))
}

# Now remove surface variable
northAK_list[[3]] <- northAK_list[[3]] %>% 
  filter(micro == "soil")

northAK_list[[5]] <- northAK_list[[5]] %>% 
  filter(micro == "soil")

for (i in 1:length(northAK_list)) {
  
  northAK_list[[i]] <- northAK_list[[i]] %>% 
    mutate(site = "AK_ameriflux") %>% 
    mutate(macro = "alpine meadow") %>% 
    mutate(foliage = "leaf-off") %>% 
    mutate(foliage_cover = 0) %>% 
    mutate(flora = "unknown but likely irrelevant") %>% 
    mutate(latitude = 68.9)
  
  if (i == 3 | i == 5) {
    
    northAK_list[[i]] <- northAK_list[[i]] %>% 
      mutate(height = -0.09) %>% 
      mutate(height_notes = "from metadata") %>% 
      mutate(altitude = dplyr::recode(elevation, "low" = 298, "high" = 719 ))
  }
  
  # Add snow data
  if (i == 3) {
    # For all years
    northAK_list[[i]] <- northAK_list[[i]] %>% 
      left_join(snowdepth) %>% 
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "single_avg", 
                                       "measured_daily")) %>% 
      # All snowdepth data is recorded at the weather station
      mutate(snowdepth = ifelse(is.na(snowdepth), 0, 
                                snowdepth))
  }
  if (i == 5) {
    # For avg years
    northAK_list[[i]] <- northAK_list[[i]] %>% 
      left_join(snowdepth_avg) %>% 
      mutate(snow_source_flag = ifelse(is.na(snowdepth), "single_avg", 
                                       "measured_daily")) %>% 
      mutate(snowdepth = ifelse(is.na(snowdepth), 0, 
                                snowdepth))
    
  }
}

## 4. Write out data ##############

write_csv(northAK_list[[1]], "data/01_primary/arctic/AK_ameriflux/derivative/AK_ameriflux_high_all_years.csv")
write_csv(northAK_list[[2]],  "data/01_primary/arctic/AK_ameriflux/derivative/AK_ameriflux_low_all_years.csv")
write_csv(northAK_list[[3]], "data/01_primary/arctic/AK_ameriflux/derivative/AK_ameriflux_tall.csv")
write_csv(northAK_list[[4]],  "data/01_primary/arctic/AK_ameriflux/derivative/AK_ameriflux_wide.csv")
write_csv(northAK_list[[5]], "data/01_primary/arctic/AK_ameriflux/derivative/AK_ameriflux_avgyears_tall.csv")
write_csv(northAK_list[[6]], "data/01_primary/arctic/AK_ameriflux/derivative/AK_ameriflux_avgyears_wide.csv")


