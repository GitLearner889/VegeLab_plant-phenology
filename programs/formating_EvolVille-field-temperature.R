#source(here::here("programs","libraries.R"))

require(here)
require(readxl)
require(dplyr)
require(photobiology)

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

