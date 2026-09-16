source(here::here("programs","libraries.R"))

requires(dplyr)
requires(photobiology)

#### 0. Data importation ####

# iButton data retrival 
df_temperature_all_raw = read.table(here("data/data_environnement/ibutton_all.txt"), header = T, sep = "\t")
# site location data retrival
df_sites_info = read_excel(here("data/site.xlsx"))

