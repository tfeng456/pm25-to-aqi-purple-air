library(dplyr)
library(lubridate)
library(stringr)
library(con2aqi)

pm_to_aqi_basic <- function(sensor_data_raw) {
  
  sensor_data_subset <- sensor_data_subset %>%
    mutate(date = as.Date(created_at, format = "%y/%m/%d")) %>%   # create column of just date without the time - "date"
    group_by(date) %>% # group by day while retaining sensor info
    summarize(mean_pm25 = mean(PM2.5_ATM_ug.m3)) %>% # take daily mean
    mutate(aqi_num = con2aqi(pollutant ="pm25", con = mean_pm25)) # convert mean pm25 into aqi
  
  return(sensor_data_subset)
}