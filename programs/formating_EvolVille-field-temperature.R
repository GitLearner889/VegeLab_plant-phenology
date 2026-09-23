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

df_sites_positions_corr = df_sites_info |> 
  mutate(site_lon = case_when(
    !is.na(site_lon_trapnest) ~ site_lon_trapnest,
    T ~ site_lon_flora),
    site_lat = case_when(
      !is.na(site_lat_trapnest) ~ site_lat_trapnest,
      T ~ site_lat_flora)) |> 
  dplyr::select(site_id,site_lon,site_lat) |> 
  mutate(site_id = as.numeric(site_id),
         site_lon = as.numeric(site_lon),
         site_lat = as.numeric(site_lat)) |> 
  filter(site_id %in% df_temperature_all_raw$site)
  

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
  left_join(df_sites_positions_corr, by = "site_id")
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
  left_join(df_sites_positions_corr, by = "site_id") |> 
  rename(daily_temp = "mean_temp")

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
  df_temp_daily_mean_cl |> 
    filter(site_id <= 27) |> 
    ggplot(aes(x = as.Date(as.character(date)), y = mean_temp, color = day_time)) +
    geom_line() +
    scale_color_manual(values = c("steelblue","gold")) +
    theme_bw() +
    facet_wrap(~ site_id)
  # Temperature evolution over time for the 16 following sites
  df_temp_daily_mean_cl |> 
    filter(site_id > 27 & site_id <=46) |> 
    ggplot(aes(x = as.Date(as.character(date)), y = mean_temp, color = day_time)) +
    geom_line() +
    scale_color_manual(values = c("steelblue","gold")) +
    theme_bw() +
    facet_wrap(~ site_id)
  # Temperature evolution over time for the 16 following sites
  df_temp_daily_mean_cl |> 
    filter(site_id > 46 & site_id <=68) |> 
    ggplot(aes(x = as.Date(as.character(date)), y = mean_temp, color = day_time)) +
    geom_line() +
    scale_color_manual(values = c("steelblue","gold")) +
    theme_bw() +
    facet_wrap(~ site_id)
  # Temperature evolution over time for the 16 following sites
  df_temp_daily_mean_cl |> 
    filter(site_id > 68 & site_id <=92) |> 
    ggplot(aes(x = as.Date(as.character(date)), y = mean_temp, color = day_time)) +
    geom_line() +
    scale_color_manual(values = c("steelblue","gold")) +
    theme_bw() +
    facet_wrap(~ site_id)
  # Temperature evolution over time for the last 12 sites
  df_temp_daily_mean_cl |> 
    filter(site_id > 92 & site_id <=110) |> 
    ggplot(aes(x = as.Date(as.character(date)), y = mean_temp, color = day_time)) +
    geom_line() +
    scale_color_manual(values = c("steelblue","gold")) +
    theme_bw() +
    facet_wrap(~ site_id)
}

# Graphs of missing mean temperature data at the global scale
if(F){
  # Number of sites recorded across time
  df_temp_daily_mean_cl |> 
    group_by(date) |> 
    summarise(nb_sites_recording = n()/2) |> 
    ggplot(aes(x = date, y = nb_sites_recording)) +
    geom_col()
}

##### 2.2 Monthly mean #####

df_temp_monthly_data = df_temp_daily_mean_cl |> 
  mutate(year = year(date),
         month = month(date),
         days_per_month = days_in_month(date)) |> 
  group_by(year,month, site_id,day_time) |> 
  mutate(nb_days = n()) |> 
  ungroup() |> 
  mutate(prop_days = nb_days/days_per_month) 

df_temp_monthly_mean_cl = df_temp_monthly_data |> 
  filter(prop_days == 1) |> 
  group_by(site_id,year,month,day_time) |> 
  summarise(monthly_temp = round(mean(daily_temp),2)) |> 
  filter(year >2021 & year < 2026)


# Graphs of proportion of days followed each month since the beginning for each site
if(F){
  # With a clored gradient
  df_temp_monthly_data |> 
    filter(year > 2020 & year <2026) |> 
    mutate(date = as.Date(paste(year,month,"01", sep="-"))) |> 
    ggplot(aes(x = date, y = as.factor(site_id), fill = prop_days)) +
    geom_tile() +
    scale_fill_gradientn(
      colors = c("#FFFFFF", "#00FF00", "#0000FF", "#FF0000", "#000000"),
      values = scales::rescale(c(0, 0.25, 0.5, 0.75, 1)),
      name = "Prop. jours\navec données",
      limits = c(0, 1)) + 
    theme_bw()
  # In black and white 
  df_temp_monthly_data |> 
    filter(year > 2020 & year <2026) |> 
    mutate(date = as.Date(paste(year,month,"01", sep="-"))) |> 
    ggplot(aes(x = date, y = as.factor(site_id), fill = prop_days)) +
    geom_tile() +
    scale_fill_gradientn(
      colors = c("#FFFFFF","#000000"),
      values = scales::rescale(c(0,1)),
      name = "Prop. jours\navec données",
      limits = c(0, 1)) + 
    theme_bw()
  # Show only months that are followed everyday 
  df_temp_monthly_data |> 
    filter(year > 2020 & year <2026 & prop_days == 1) |> 
    mutate(date = as.Date(paste(year,month,"01", sep="-"))) |> 
    ggplot(aes(x = date, y = as.factor(site_id), fill = prop_days)) +
    geom_tile() +
    scale_fill_gradientn(
      colors = c("#FFFFFF","#000000"),
      values = scales::rescale(c(0,1)),
      name = "Prop. jours\navec données",
      limits = c(0, 1)) + 
    theme_bw()
}

#### 3. Ranking as a proxy ####

func_rank_temporal_and_group <- function(df, 
                                         var_use_to_rank = "monthly_temp", 
                                         group_col = "day_time", 
                                         temporal_id = "date", 
                                         element_id = "site_id") {
  df_rank = df %>%
    # compute the rank for each group, for each temporal_id
    group_by(!!sym(group_col), !!sym(temporal_id)) |> 
    mutate(raw_rank = rank(!!sym(var_use_to_rank), ties.method = "average"),
           nb_sites_evaluated = n(),
           normalised_rank = (raw_rank - 1) / (nb_sites_evaluated - 1)) |> 
    ungroup() |> 
    dplyr::select(all_of(c(temporal_id, group_col,element_id,"normalised_rank", "nb_sites_evaluated")))
  
  new_df = df |> 
    left_join(df_rank, by = c(group_col, temporal_id, element_id)) |> 
    ungroup()
  
  return(new_df)
}

##### 3.1 Daily basis ####

df_temp_daily_mean_cl_rk = func_rank_temporal_and_group(df_temp_daily_mean_cl, 
                                                          temporal_id = "date", 
                                                          group_col = "day_time",
                                                          var_use_to_rank = "daily_temp",
                                                          element_id = "site_id")


##### 3.2 Monthly basis ####

df_temp_monthly_mean_cl_rk = func_rank_temporal_and_group(df_temp_monthly_mean_cl |> 
                               mutate(date = paste(year,month, sep = "-")), 
                             temporal_id = "date", 
                             group_col = "day_time",
                             var_use_to_rank = "monthly_temp",
                             element_id = "site_id")


# Graphs number of sites records over time 
if(F){
  # Daily baisis
  df_temp_daily_mean_cl_rk |> 
    filter(date > "2021-12-12" & day_time == "Day") |> 
    distinct(date,nb_sites_evaluated) |>
    ggplot(aes(x = date, y = nb_sites_evaluated)) +
    geom_col() +
    theme_minimal()
  # Daily basis
  df_temp_monthly_mean_cl_rk |> 
    filter(day_time == "Day") |> 
    mutate(date = as.Date(paste0(date,"-01"))) |> 
    distinct(date,nb_sites_evaluated) |>
    ggplot(aes(x = date, y = nb_sites_evaluated)) +
    geom_col() +
    theme_minimal()
}

# Graph of rank distribution for each site
if(F){
  df_temp_daily_mean_cl_rk |> 
    filter(nb_sites_evaluated > 30) |> 
    ggplot(aes(x = normalised_rank)) +
    geom_histogram() +
    facet_wrap(~ site_id, scales = "free_y")
}

# Graph of mean rank site distribution
if(F){
  # Distribution from the cooler site to the warmer at night
  df_temp_daily_mean_cl_rk |> 
    filter(nb_sites_evaluated > 30) |>
    group_by(site_id,day_time) |> 
    summarise(mean_rank = mean(normalised_rank), .groups = "drop") |>
    group_by(site_id) |> 
    mutate(site_night_order = mean_rank[day_time == "Night"]) |> 
    ungroup() |> 
    arrange(site_night_order) |> 
    mutate(site_id = factor(site_id, levels = unique(site_id))) |> 
    ggplot(aes(x = site_id, y = mean_rank, fill = day_time)) +
    geom_col(position = position_dodge(width = 0.8)) +
    scale_fill_manual(values = c("Day" = "gold", "Night" = "steelblue")) +
    labs(x = "Site", y = "Mean temperature rank", 
         title = "Mean temperature rank per site and day time") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank())
  
  # Distribution from the cooler site to the warmer at day
  df_temp_daily_mean_cl_rk |> 
    filter(nb_sites_evaluated > 30) |>
    group_by(site_id,day_time) |> 
    summarise(mean_rank = mean(normalised_rank), .groups = "drop") |>
    group_by(site_id) |> 
    mutate(site_day_order = mean_rank[day_time == "Day"]) |> 
    ungroup() |> 
    arrange(site_day_order) |> 
    mutate(site_id = factor(site_id, levels = unique(site_id))) |> 
    ggplot(aes(x = site_id, y = mean_rank, fill = day_time)) +
    geom_col(position = position_dodge(width = 0.8)) +
    scale_fill_manual(values = c("Day" = "gold", "Night" = "steelblue")) +
    labs(x = "Site", y = "Mean temperature rank", 
         title = "Mean temperature rank per site and day time") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank())
  
}

# Graph of relationship between average temperature and average rank
if(F){
  df_temp_daily_mean_cl_rk |> 
    filter(nb_sites_evaluated > 30) |>
    group_by(site_id,day_time) |> 
    summarise(mean_rank = mean(normalised_rank), 
              mean_temp = mean(daily_temp),
              .groups = "drop") |>
    ggplot(aes(x = mean_rank, y = mean_temp, color = as.factor(site_id))) +
    geom_point() +
    facet_wrap(~ day_time, scales = "free_y") +
    theme_bw() +
    labs(x = "Mean temperature rank", y = "Mean temperature", color = "Site")
}

#### 4. Spatial interpolation #### 

##### 4.1 Matrice of distance computing #####
sites_sf <- df_sites_positions_corr %>% 
  st_as_sf(coords = c("site_lon", "site_lat"), crs = 4326) %>% 
  st_transform(2154) # adapted to calculate distance on small scales

dist_sites <- as.matrix(st_distance(sites_sf)) # compute distance and make it a matrix object
rownames(dist_sites) <- df_sites_positions_corr$site_id
colnames(dist_sites) <- df_sites_positions_corr$site_id

##### 4.2 Interpolation ####

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

df_interp_30 <- interpolate_missing_sites(data = df_temp_daily_mean_cl_rk, 
                                          sites_infos = df_sites_positions_corr, 
                                          dist_matrix = dist_sites,
                                          min_sites = 30) |> 
  mutate(data_type = "interpolated")

##### 4.3 Data merging  #####

data_temp_daily_complete_min30 =  bind_rows(
  df_temp_daily_mean_cl_rk |> mutate(data_type = "observed"),
  df_interp_30
) |> 
  mutate(data_type = as.factor(data_type)) |> 
  rename(normalised_rank_old = normalised_rank,
         nb_sites_observed = nb_sites_evaluated)

# Add new ranking
data_temp_daily_complete_min30_rk = func_rank_temporal_and_group(df = data_temp_daily_complete_min30,
                             var_use_to_rank = "daily_temp",
                             group_col = "day_time",
                             temporal_id = "date",
                             element_id = "site_id")
  


# Graphs number of sites followed over time 
if(F){
  data_temp_daily_complete_min30 |> 
    rename(nb_sites_observed = nb_sites_evaluated) |> 
    group_by(date, day_time) |> 
    mutate(nb_all_sites = n()) |> 
    ungroup() |> 
    filter(date > "2021-12-12" & day_time == "Day") |> 
    distinct(date,nb_all_sites) |>
    ggplot(aes(x = date, y = nb_all_sites)) +
    geom_col() +
    theme_minimal()
  }

if(F){
  # correlation between old and new ranking
  data_temp_daily_complete_min30_rk |> 
    ggplot(aes(x = normalised_rank_old, y = normalised_rank )) +
    geom_point()
  # Distribution of diffrence between old and new ranking
  data_temp_daily_complete_min30_rk |> 
    ggplot(aes(x = normalised_rank_old - normalised_rank )) +
    geom_histogram()
} 

#### 5. Out put ####

data_daily_temp = df_temp_daily_mean_cl_rk
data_monthly_temp = df_temp_monthly_mean_cl_rk
data_daily_temp_interpolated = data_temp_daily_complete_min30_rk

rm(df_temp, df_temp_session, df_sites_info, df_sites_positions,df_sites_positions_corr, df_temperature_all_raw, df_temp_daily_mean_cl_rk, df_temp_daily_mean_cl,df_temp_monthly_mean_cl, df_temp_monthly_mean_cl_rk)
