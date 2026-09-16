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



