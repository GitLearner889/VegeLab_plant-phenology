 source(here::here("programs/libraries.R"))
 
 #### Initialization ####
 ##### 1.1 Data importation ####
 
 df_temp_all_cor = read.csv2(here("data/data_environment/ibutton_daily_temperature_corr_and_interpolated.csv")) |> 
   dplyr::select(-X)
 df_temp_all_obs = df_temp_all_cor |> 
   filter(data_type == "observed")
 
 df_sites_positions = df_temp_all_cor |> 
   distinct(site_id, site_lon,site_lat)
 
 
 