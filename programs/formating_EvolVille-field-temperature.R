#source(here::here("programs","libraries.R"))

requires(dplyr)
requires(photobiology)

#### 0. Data importation ####

# iButton data retrival 
df_temperature_all_raw = read.table(here("data/data_environnement/ibutton_all.txt"), header = T, sep = "\t")
# site location data retrival
df_sites_info = read_excel(here("data/site.xlsx"))

#### 1. Data cleaning ####

##### 1.1 Location data #####
# keep only location of iButton (inside trapnest)
df_sites_positions = df_sites_info |> 
  select(site_id,site_lon_trapnest,site_lat_trapnest) |> 
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


