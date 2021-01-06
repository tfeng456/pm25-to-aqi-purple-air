library(dplyr)
library(lubridate)
library(stringr)
library(con2aqi)


pm_to_aqi_filename <- function(filename) {
  
  read_csv_filename <- function(filename){
    ret <- read.csv(filename, header = TRUE)
    ret$Source <- filename
    ret
  }
  
  # sensor_data_raw must be read using read_csv_filename
  sensor_data_raw <- read_csv_filename(filename)
  
  # save sensor lat/long
  sensor_data_raw$sensor_coords <- sensor_data_raw$Source %>% 
    str_extract("\\((-?\\d+(\\.\\d+)?)\\s*(-?\\d+(\\.\\d+)?)\\)")
  
  # save sensor name
  sensor_data_raw$sensor_name <- sensor_data_raw$Source %>% 
    str_extract("(?:(?!\\s\\((-?\\d+(\\.\\d+)?)\\s*(-?\\d+(\\.\\d+)?)\\)).)*")
  
  # subset data so that only relevant columns remain
  sensor_data_subset <- sensor_data_raw %>% 
    select(created_at, PM2.5_ATM_ug.m3, sensor_name, sensor_coords)
  
  # convert date info into appropriate format
  sensor_data_subset$created_at <- ymd_hms(sensor_data_subset$created_at)
  

  sensor_data_subset <- sensor_data_subset %>%
    mutate(date = as.Date(created_at, format = "%y/%m/%d")) %>%   # create column of just date without the time - "date"
    group_by(date, sensor_name, sensor_coords) %>% # group by day while retaining sensor info
    summarize(mean_pm25 = mean(PM2.5_ATM_ug.m3)) %>% # take daily mean
    mutate(aqi_num = con2aqi(pollutant ="pm25", con = mean_pm25)) # convert mean pm25 into aqi
  
  # write.csv(sensor_data_subset, "data_output.csv")
  sensor_data_subset
  
}