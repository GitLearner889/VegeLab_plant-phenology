 source(here::here("programs/libraries.R"))
 
 #### 1. Initialization ####
 ##### 1.1 Data importation ####
 df_temp_all_cor = read.csv2(here("data/data_environment/ibutton_daily_temperature_corr_and_interpolated.csv")) |> 
   dplyr::select(-X)
 df_temp_all_obs = df_temp_all_cor |> 
   filter(data_type == "observed")
 
 df_sites_positions = df_temp_all_cor |> 
   distinct(site_id, site_lon,site_lat)
 
 # Matrice of distance computing 
 sites_sf <- df_sites_positions %>% 
   st_as_sf(coords = c("site_lon", "site_lat"), crs = 4326) %>% 
   st_transform(2154) # adapted to calculate distance on small scales
 
 dist_sites <- as.matrix(st_distance(sites_sf)) # compute distance and make it a matrix object
 rownames(dist_sites) <- df_sites_positions_corr$site_id
 colnames(dist_sites) <- df_sites_positions_corr$site_id
 
 ##### 1.2 Functions creation ####
 
 # Inverse Distance Weighting function
 idw_point <- function(distances, data_values, beta = -2){
   weights <- distances^beta
   as.vector((weights %*% data_values) / rowSums(weights))
 }
 
 # Temperature interpolation of missing site function
 interpolate_missing_sites = function(data, sites_infos, dist_matrix,  min_sites = 30) {
   df_temp_interp <- data %>% 
     group_by(date, day_time) %>% # for each day and time period
     group_modify(~ {
       subdata <- .x # give a name to the susampled dataframe
       
       missing <- sites_infos %>%  # recover the id and positing of the sites without temp data (i.e. not present in the dataframe)
         filter(!site_id %in% subdata$site_id)
       
       if(nrow(missing) == 0 || nrow(subdata) < min_sites) return(tibble()) # check if there are missing sites or if there are enough observed sites
       
       distances <- dist_sites[ # subsample the matrices of distance to have only the distance between missing sites and non missing sites
         as.character(missing$site_id),
         as.character(subdata$site_id),
         drop = FALSE
       ]
       
       missing %>% # interpolate thanks to ibw function the temperature of the missing sites
         mutate(
           daily_temp = idw_point(distances = distances, data_values = subdata$daily_temp),
           normalised_rank = NA_real_,
           nb_sites_evaluated = nrow(subdata)
         )
       
     }) %>% 
     ungroup()
   return(df_temp_interp)
 }
 
 #### 2. Exploration of efficiency ####
 
 ##### 2.1 Cross test ####
 
 df_temp_all_obs |> 
   group_by(nb_sites_observed) |>
   count() |> 
   arrange(desc(nb_sites_observed)) |> 
   print(n = 20)
 
 df_temp_all_obs