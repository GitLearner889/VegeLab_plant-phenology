require(here)
require(readxl)
require(dplyr)
require(photobiology)
require(data.table)
require(sf)
require(DescTools)
require(lubridate)
require(ggplot2)
require(tidyr)

#source(here::here("programs","libraries.R"))
source(here("functions/funct_spatial-iterpolation.R"))

#### 0. Data importation ####

# iButton data retrival 
df_temperature_all_raw = read.table(here("data/data_environment/ibutton_all.txt"), header = T, sep = "\t")
# site location data retrival
df_sites_info = read_excel(here("data/site.xlsx"))

#### 1. Data cleaning ####

##### 1.1 Location data #####
# keep only location of iButton (inside trapnest)
df_sites_positions = df_sites_info |> 
  dplyr::select(site_id,site_lon_trapnest,site_lat_trapnest) |> 
  mutate(site_id = as.numeric(site_id),
         site_lon_trapnest = as.numeric(site_lon_trapnest),
         site_lat_trapnest = as.numeric(site_lat_trapnest))

##### 1.2 iButton data #####
# format date and time of temperature data
df_temp = df_temperature_all_raw %>% 
  dplyr::select(date,time,site,temp,hum,dewpoint,day_night) |> # keep only intresting variables
  rename(site_id = site) |> 
  mutate(datetime = as.POSIXct( # create a global variable of date and time, of date class
    paste(as.Date(date,format = "%Y-%m-%d"), time), 
    format = "%Y-%m-%d %H:%M:%S"),
    tz = "Europe/Paris") 
# add day or night information 
df_temp = df_temp %>% 
  mutate(day_time = is_daytime(date = datetime,
                               tz = "Europe/Paris", 
                               geocode = tibble(lon = 7.76811, lat = 48.58325))) %>% # choose the location of botanical garden of Strasbourg (to heavy to implement it for each specific location, and the results is almost the same)
  mutate(day_time = ifelse(day_time, "Day", "Night"))
# add location of sites
df_temp = df_temp |> 
  left_join(df_sites_positions, by = "site_id")
# remove an outlier data 
df_temp = df_temp |> 
  filter(temp < 60)

# verification of day time/night time compatibility
if(F){
  df = df_temp |> 
    mutate(same_period = as.factor(paste(day_night,day_time,sep="-")))
  
  df_diff_daytime = df |> 
    filter(same_period == "day-Night")
  
  print(paste("There is ",nrow(df_diff_daytime), "time points of night that should be day according to raw data calculation which represent", round(nrow(df_diff_daytime)/nrow(df),3)*100, "% of all data")) # the number is negligeable, each time it concerns only the transition of night to day or between day to night
  summary(as.factor(df$same_period))
  print(paste((df_diff_daytime |> distinct(site_id) |> nrow() / df |> distinct(site_id) |> nrow())*100, "% of the sites are concerned"))
}


#### 2. Temperature averaging  ####

##### 2.1 Daily mean ####
# While calculating the mean temperature of day and night number of measure that should be made in a day 
# Data-frame off all session (measured made during the same day and at the same lux period)
df_temp_session <- df_temp %>%
  arrange(site_id, datetime) %>%
  group_by(site_id) %>%
  mutate(
    time_diff = difftime(datetime, lag(datetime), units = "hours"),
    new_session = if_else(# change the session if:
      is.na(day_time != lag(day_time)) | # first session (initialization) 
        day_time != lag(day_time) | # switch from day time to night time or vice versa 
        time_diff > 12 , #  the time between the the two recordings is greater than 12 h 
      1, 0), 
    session_id = cumsum(new_session), # all measures made during the same session will have the same session_id, because add 0, only when session switch add 1
    session_start = first(datetime), # retrieve start and ending time of the session
    session_end = last(datetime)
  ) # so a night session start at sunset of day D and stops at sunrise of day D+1



##### 2.2 Mean temperature interpolation #####

for(date in df_temp_mean$date){
  empty_dates = 
}
  

