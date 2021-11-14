# David Klinges
# File creation date: 2019.08.22
# This script downloads NOAA database queries (purpose here is to extract 
# snowdepth) 


# Data type descriptions: https://cran.r-project.org/web/packages/rnoaa/vignettes/ncdc_attributes.html

## 1. Workspace prep #########

library(tidyverse)
library(rnoaa)
library(lubridate)

token <- "KriorsxnRpjvjiKBKPCykqwqccRELoTP"

## 2. Designate data query ##########

noaa_query <- function(
  stationid, # The GHCND station ID
  locationid = NULL, # For inputting zip codes if you don't know station ID
  startdate, 
  enddate,
  datatypeid # typically SNWD, which is snow depth 
) {
  
  # The NOAA database query can only handle small temporal ranges...for the current
  # data just a month at a time. So we'll need to do some looping
  
  # Coerce to date object
  start_date <- as_date(startdate)
  end_date <- as_date(enddate)
  
  for (year in year(start_date):year(end_date)) {
    
    for (month in month(start_date):month(end_date)) {
      
      # Need to pad month iterator with a "0" to make it a month
      if (month < 10) {
        month <- paste0("0", month)
      }
      
      # Designate the start and end dates for the query, which need to be in the
      # same month of the same year

      start_date_iter <- as_date(paste(year, month, "01", sep = "-"))
      end_date_iter <- as_date(paste(year, month, 
                                     days_in_month(start_date_iter), # last day of month
                                     sep = "-"))
      
      # Inform user of iteration
      print(paste("Querying for", year, month, sep = " "))
      
      # Now query database
      query <- ncdc(datasetid = 'GHCND', datatypeid = datatypeid, 
                    stationid = stationid,
                    locationid = locationid,
                    # When we input these into the NOAA query, they need to be character strings
                    # and NOT date objects...for some reason date objects screws with the query
                    # (e.g. thinks a month is more than a year)
                    startdate = as.character(start_date_iter), 
                    enddate = as.character(end_date_iter),
                    token = token)
      
      # Save query from this iteration
      if (year == year(start_date) & month == "01") {
        data_out <- query$data
      } else {
        data_out <- bind_rows(data_out, query$data)
      }
      
    }
    
  }
  return(data_out)
}
  
