

## 1. Workspace prep ###########

library(tidyverse)
library(lubridate)
library(fs)

## ....A. Load Temperature datasets ##############

# Identify paths for all data files
temp_list <- c(list.files(path = "data/01_primary/tropical/Madagascar/indata/", 
                           # Most of the data files are in their own subdirectories
                           #   so need to be recursive
                           pattern = "Temp", recursive = FALSE))

# Import all csv files
temp_list <- lapply(temp_list, FUN = function(x) {
  filename <- print(path_file(x))
  data <- read_csv(paste0(getwd(), "/data/01_primary/tropical/Madagascar/indata/", x))
  data <- data %>% 
    mutate(filename = filename)
  })


temp_data_raw <- bind_rows(do.call(bind_rows, temp_list))


## ....B. Load Humidity datasets ##############

# Identify paths for all data files
humid_list <- c(list.files(path = "data/01_primary/tropical/Madagascar/indata/", 
                          # Most of the data files are in their own subdirectories
                          #   so need to be recursive
                          pattern = "Moist", recursive = FALSE))

# Import all csv files
humid_list <- lapply(humid_list, FUN = function(x) {
  filename <- print(path_file(x))
  data <- read_csv(paste0(getwd(), "/data/01_primary/tropical/Madagascar/indata/", x))
  data <- data %>% 
    mutate(filename = filename)
})


humid_data_raw <- bind_rows(do.call(bind_rows, humid_list))


## 2. Data curation ############

temp_data <- temp_data_raw %>% 
  separate(`Date/Time`, into = c("date", "time"), sep = " ") %>% 
  mutate(date = mdy(date)) %>% 
  dplyr::rename(temp = Value) %>% 
  dplyr::select(-Unit) %>% 
  # Remove "Temp" from filename
  mutate(filename = gsub("Temp_", "", filename))

humid_data <- humid_data_raw %>% 
  separate(`Date/Time`, into = c("date", "time"), sep = " ") %>% 
  mutate(date = mdy(date)) %>% 
  dplyr::rename(rh = Value) %>% 
  dplyr::select(-Unit) %>% 
  # Remove "Moist" from filename
  mutate(filename = gsub("Moist_", "", filename))

mada_data <- full_join(temp_data, humid_data) %>% 
  dplyr::select(filename, date, time, temp, rh)

## 3. Write out data ########

write_csv(mada_data, "data/01_primary/tropical/Madagascar/Mada_temp_and_rh.csv")
  
## 4. What dataset is most complete? ##########

# This is just temperature
mada_og <- read_csv("data/01_primary/tropical/Madagascar/Madagascar.csv")

# This is temp and RH
mada_compiled <- read_csv("data/01_primary/tropical/Madagascar/compiled.dataset.csv")

mada_compiled_rh <- mada_compiled %>% 
  filter(Unit == "%RH") %>% 
  dplyr::rename(rh = Value) %>% 
  dplyr::select(-file.name, -para, -Unit)

mada_compiled_temp <- mada_compiled %>% 
  filter(Unit == "C") %>% 
  dplyr::rename(temp = Value) %>% 
  dplyr::select(-file.name, -para, -Unit)

mada_compiled_2 <- full_join(mada_compiled_rh, mada_compiled_temp)
  
nrow(mada_compiled_2)
nrow(mada_data)
# The new file I just compiled appears a little more comprehensive
  
