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
###### 2.1.1 Computing ####
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

# Now can calculate for each session mean temperature and quality indices
df_session_summary <- df_temp_session %>%
  group_by(site_id, day_time, session_id) %>%
  summarise(
    nb_records = n(),
    mean_temp = round(mean(temp), 2),
    max_temp = round(max(temp), 2),
    min_temp = round(min(temp), 2),
    session_start = min(datetime),
    session_end = max(datetime),
    duration_h = round(difftime(max(datetime), min(datetime), units = "hours"), 1),
    .groups = "drop"
  )

# Clearing of the incomplete data (not enough records and/or during a too short period of time)
df_session_summary_cleared = df_session_summary |> 
  filter(as.numeric(duration_h) >= 7 & as.numeric(duration_h) <=16 & nb_records > 6) 

# Applying the cretirions to have a relevant and clean daily mean temperature dataset
df_temp_daily_mean_cl = df_session_summary_cleared |> 
  mutate(date = as.Date(session_start)) |> 
  select(site_id, date, mean_temp,day_time) |> 
  left_join(df_sites_positions, by = "site_id")

###### 2.1.2 Table exploration ########
# Exploration of the problems of the session data
if(F){
  # Distribution of number of records per session
  df_session_summary |> 
    group_by(nb_records) |> 
    count() |> 
    View()
  # Focus on the session with more than 50 records
  df_session_summary |> 
    filter(nb_records > 50) |> 
    View()
  # Distribution of the duration of session
  df_session_summary |> 
    group_by(duration_h) |> 
    count() |> 
    View()
  # Check if there is days where there is more than 2 sessions 
  df_session_summary |> 
    mutate(date = lubridate::as_date(session_start)) |> 
    group_by(date,site_id) |> 
    mutate(n_session_date = n()) |> 
    ungroup() |> 
    filter(n_session_date == 3) |> 
    View()
  # Check that with the good filters, if there is still days with 3 sessions
  df_session_summary |> 
    filter(as.numeric(duration_h) >= 7 & as.numeric(duration_h) <=16 & nb_records >= 6) |> 
    mutate(date = lubridate::as_date(session_start)) |> 
    group_by(date,site_id) |> 
    mutate(n_session_date = n()) |> 
    ungroup() |> 
    filter(n_session_date == 3) |> 
    View()
}

###### 2.1.3 Graphical exploration ###### 

# Graphs of temperature evolution for each site
if(F){
  # Temperature evolution over time for the 16 first sites
  df_temp_mean |> 
    filter(site_id <= 27) |> 
    ggplot(aes(x = as.Date(as.character(date)), y = mean_temp, color = day_time)) +
    geom_line() +
    scale_color_manual(values = c("steelblue","gold")) +
    theme_bw() +
    facet_wrap(~ site_id)
  # Temperature evolution over time for the 16 following sites
  df_temp_mean |> 
    filter(site_id > 27 & site_id <=46) |> 
    ggplot(aes(x = as.Date(as.character(date)), y = mean_temp, color = day_time)) +
    geom_line() +
    scale_color_manual(values = c("steelblue","gold")) +
    theme_bw() +
    facet_wrap(~ site_id)
  # Temperature evolution over time for the 16 following sites
  df_temp_mean |> 
    filter(site_id > 46 & site_id <=68) |> 
    ggplot(aes(x = as.Date(as.character(date)), y = mean_temp, color = day_time)) +
    geom_line() +
    scale_color_manual(values = c("steelblue","gold")) +
    theme_bw() +
    facet_wrap(~ site_id)
  # Temperature evolution over time for the 16 following sites
  df_temp_mean |> 
    filter(site_id > 68 & site_id <=92) |> 
    ggplot(aes(x = as.Date(as.character(date)), y = mean_temp, color = day_time)) +
    geom_line() +
    scale_color_manual(values = c("steelblue","gold")) +
    theme_bw() +
    facet_wrap(~ site_id)
  # Temperature evolution over time for the last 12 sites
  df_temp_mean |> 
    filter(site_id > 92 & site_id <=110) |> 
    ggplot(aes(x = as.Date(as.character(date)), y = mean_temp, color = day_time)) +
    geom_line() +
    scale_color_manual(values = c("steelblue","gold")) +
    theme_bw() +
    facet_wrap(~ site_id)
}



##### 2.2 Mean temperature interpolation #####

for(date in df_temp_mean$date){
  empty_dates = 
}
  

