source(here::here("programs/libraries.R"))

#### 0. Data importation ####
# Import plant data
data_sqr = read_excel(here("data/data_plant/Square2020_2025.xlsx")) # quadrat distribution
data_traits = read_excel(here("data/data_plant/TraitsBDD2020_2025.xlsx")) # trait for each species
data_cwm = read_excel(here("data/data_plant/CWM2020_2025.xlsx")) # community weighted mean per session
data_quanti = read_excel(here("data/data_plant/Quanti2020_2025.xlsx")) # quantitative indices per session
# Rename dataframes variables
data_sqr = data_sqr |> 
  rename(site_id = SITE,
         month = MOIS,
         day = JOUR,
         year = ANNEE,
         spe_name = LB_NOM,
         spe_id = CD_REF_18,
         rank = RANG,
         phenology = Phenology,
         square = Square) |> 
  mutate(session_id = paste(site_id,year, sep = "_")) |> 
  relocate(session_id)

data_traits = data_traits |> 
  rename(spe_name = Species,
         urbanity_class = Urbanity_class,
         reward = Reward) |> 
  mutate(flw_early = ifelse(is.na(flw_early), NA,as.numeric(flw_early)),
         flw_late = ifelse(is.na(flw_late), NA,as.numeric(flw_late)),
         flw_long = ifelse(is.na(flw_long), NA,as.numeric(flw_long)))

data_quanti = data_quanti |> 
  rename(site_id = Site,
         year = Year) |> 
  mutate(session_id = paste(site_id,year, sep = "_")) |> 
  relocate(session_id)

data_cwm = data_cwm |> 
  rename(year = site_year) |> 
  mutate(session_id = paste(site_id,year, sep = "_")) |> 
  relocate(session_id)

#### 1. Plant data manipulation ####

# List of target species
list_target_species = tibble(spe_id = c(79908,127454,127439,94207,113893,106653), 
                             spe_name = c("Achillea millefolium","Trifolium repens","Trifolium pratense","Dactylis glomerata","Plantago lanceolata","Lotus corniculatus"),
                             family = c("Asteraceae", "Fabaceae","Fabaceae", "Poaceae", "Plantaginaceae", "Fabaceae")) |> 
  arrange(spe_name)

# Dataframe of abundance for each species in each session
data_spe_session = data_sqr |> 
  group_by(session_id,site_id,year,spe_id,spe_name) |> 
  summarise(AB = n()) |> 
  ungroup()

data_trgtspe_session = data_spe_session |> 
  filter(spe_id %in% list_target_species$spe_id) 

# Dataframe of abundance of species across all sites for each year
data_trgtspe_distribution = data_trgtspe_session |> 
  group_by(spe_id,spe_name,year) |> 
  summarise(freq_site = n(),
            AB_Tot = sum(AB)) |> 
  ungroup()


#### 2. Site data manipulation  ####

##### 2.1 Pre-selected sites ####
# Sites that should be explored in late September
potential_sites_of_sampling = data.frame(site_id = c(74, 27, 75, 23, 92, 58, 96, 55, 45, 72)) |> 
  arrange(site_id)

# Count for each year the number of time the species is present in one of the 10 potential sites
data_trgtspe_session_potential_sites = data_trgtspe_session |> 
  mutate(potential_site = case_when(
    site_id %in% potential_sites_of_sampling$site_id ~ 1,
    T ~ 0
  )) |> 
  group_by(spe_id,year) |> 
  mutate(nb_present_potentiel_site = sum(potential_site)) |> 
  ungroup()

# Dataframe of abundance of species across all sites for each year
data_trgtspe_distribution_2025 = data_trgtspe_session_potential_sites |> 
  group_by(spe_id,spe_name,year,nb_present_potentiel_site) |> 
  summarise(freq_site = n(),
            AB_Tot = sum(AB)) |> 
  ungroup() |> 
  filter(year == 2025) |>
  left_join(data_traits |> # Add information on end of flowering periode
              dplyr::select(spe_name,flw_late), by = "spe_name") |> 
  mutate(fruiting = ifelse(flw_late >=8 & flw_late < 10, T, F)) 


#### 3. Management data ####
# Retrieve management data 
source(here("programs/formatage-gestion.R"))

# Reduce dataframe to needed values
data_mngt_reduced =  data_management |> 
  filter(as.numeric(year) <= 2025) |> # to be coherent with plant data
  mutate(gestion_classe_fct = as.factor(gestion_classe)) |> 
  mutate(gestion_classe_num = as.numeric(gestion_classe_fct)) |> 
  dplyr::select(session_id,site_id, year, gestion, gestion_classe_num)

##### 3.1 Indices for each- sites ##### 
# Quantitative indices about management
data_mngt_quanti_per_site = data_mngt_reduced |> 
  group_by(site_id) |> 
  summarise(mean_mngt_class = round(mean(gestion_classe_num, na.rm = T),2),
            sd_mngt_class = round(sd(gestion_classe_num, na.rm = T),3),
            min_mngt_class = min(gestion_classe_num, na.rm = T),
            max_mngt_class = max(gestion_classe_num, na.rm = T),
            nb_session = n())

# Add managemeny infos of potential sites
potential_sites_of_sampling = potential_sites_of_sampling |> 
  left_join(data_mngt_quanti_per_site, by = "site_id", )

##### 3.2 Indices for each species #####

sd_weighted <- function(x, w, na.rm = TRUE) {
  if (na.rm) {
    valide <- !(is.na(x) | is.na(w))
    if (sum(valide) < 2) return(NA_real_)
    x <- x[valide]
    w <- w[valide]
  }
  
  mu <- sum(w * x) / sum(w) # weighted mean
  sqrt(sum(w * (x - mu)^2) / (sum(w) - 1))
}

# Compute management indice of each species weighted by its abundance
data_trgtspe_session_mngt = data_trgtspe_session |> 
  left_join(data_mngt_reduced |>  # Add gestion intensity for each session
              dplyr::select(-site_id,-year), by = "session_id")

trgtspe_management_indices =  data_trgtspe_session_mngt|> 
  group_by(spe_id,spe_name) |> 
  summarise(
    weighted_mean_mngt_intensity = round(sum(gestion_classe_num * AB, na.rm = T) / sum(AB),2),
    weighted_sd_mngt_intensity = round(sd_weighted(x = gestion_classe_num, w = AB, na.rm = T),3),
    mean_mngt_intensity = round(mean(gestion_classe_num, na.rm = T),2),
    sd_mngt_intensity = round(sd(gestion_classe_num, na.rm = T),3),
    min_mngt_intensity = min(gestion_classe_num, na.rm = T),
    max_mngt_intensity = max(gestion_classe_num, na.rm = T),
    tot_session = n_distinct(session_id),
    tot_site = n_distinct(site_id),
    tot_ab = sum(AB),
    tot_mngt_class = n_distinct(gestion_classe_num)) |> 
  ungroup()


#### 4. Temperature data ####

data_temp = read.csv2(file = here("data/data_environment/ibutton_daily_temperature_corr_and_interpolated.csv"))

data_temp_60sites = data_temp |> 
  filter(nb_sites_evaluated == 60 )

##### 4.1 Temperature for each site ####
data_temp_60sites_mean = data_temp_60sites |> 
  group_by(site_id,day_time) |> 
  summarise(mean_temperature = mean(daily_temp),
            mean_rank = mean(normalised_rank))

if(F){
  # Plot mean rank against mean temperature
  data_temp_60sites_mean |> 
    ggplot(aes(x = mean_rank, y = mean_temperature, color = as.factor(site_id))) +
    geom_point() +
    facet_wrap(~ day_time, scales = "free_y") +
    theme_bw()
  
  # Plot mean night temperature against mean day one
  data_temp_60sites_mean |> 
    pivot_wider(id_cols = "site_id", 
                values_from = c("mean_temperature","mean_rank"), 
                names_from = "day_time") |> 
    ggplot(aes(x = mean_temperature_Night, y = mean_temperature_Day, color = as.factor(site_id))) +
    geom_point() +
    theme_bw()
  
  # Plot mean night rank against mean day one
  data_temp_60sites_mean |> 
    pivot_wider(id_cols = "site_id", 
                values_from = c("mean_temperature","mean_rank"), 
                names_from = "day_time") |> 
    ggplot(aes(x = mean_rank_Night, y = mean_rank_Day, color = as.factor(site_id))) +
    geom_point() +
    theme_bw()
}

##### 4.2 Temperature for each species #####

# Fancy but overkill function to compute a double weighted mean calculus 
func_double_weight_mean <- function(data, cols_target = c(""), first_weight_str = "AB", second_weight_str = "nb_months") {
  first_weight <- rlang::sym(first_weight_str)
  second_weight <- rlang::sym(second_weight_str)
  
  data %>%
    mutate(total_weight = !!first_weight * !!second_weight) %>% # compute total weight
    pivot_longer(cols = all_of(cols_target),
                 names_to = "variable",
                 values_to = "valeur") %>%
    filter(!is.na(valeur)) %>% # remove from calculation values that don't have data
    group_by(spe_id, spe_name, variable) %>% # compute for each species and each site the weighted mean of each variable
    summarise(
      weighted_mean = round(weighted.mean(valeur, total_weight, na.rm = TRUE),2),
      nb_sessions       = n(),
      nb_mois_median   = median(!!second_weight),
      nb_eff            = sum(total_weight)^2 / sum(total_weight^2),
      sum_total_weight      = sum(total_weight),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = variable, 
                values_from = c(weighted_mean, nb_sessions, nb_eff))
}

# Compute mean annual temperature per site 
data_temp_60sites_yearly_avrg =  data_temp_60sites |>
  mutate(year = year(date),
         month = month(date)) |> 
  group_by(site_id,year,day_time) |> 
  summarise(yearly_mean_temp = mean(daily_temp, na.rm = T),
            yearly_mean_rank = mean(normalised_rank, na.rm = T),
            nb_months = n_distinct(month)) |> 
  pivot_wider(id_cols = c("site_id","year", "nb_months"),
              names_from = "day_time",
              values_from = c("yearly_mean_temp", "yearly_mean_rank")) |> 
  mutate(session_id = paste(site_id,year, sep = "_")) |> 
  ungroup()

###### 4.2.1 Targets species ####

# Compute temperature indice of each species weighted by its abundance
data_trgtspe_session_temp = data_trgtspe_session |> 
  left_join(data_temp_60sites_yearly_avrg |>  # Add gestion intensity for each session
              dplyr::select(-site_id,-year) |> 
              filter(session_id %in% data_trgtspe_session$session_id), by = "session_id") |> 
  mutate(nb_months = ifelse(is.na(nb_months), 0, nb_months))



# Result of fancy function 
if(F){
  func_double_weight_mean(data = data_trgtspe_session_temp, 
                          cols_target =  c("yearly_mean_temp_Day", "yearly_mean_temp_Night"),
                          first_weight_str = "AB",
                          second_weight_str = "nb_months") |> 
    View()
}


trgtspe_temp_indices =  data_trgtspe_session_temp|> 
  group_by(spe_id,spe_name) |> 
  summarise(
    weighted_mean_night_temp = round(sum(yearly_mean_temp_Night * AB*nb_months, na.rm = T) / sum(AB*nb_months),2),
    weighted_mean_day_temp = round(sum(yearly_mean_temp_Day * AB*nb_months, na.rm = T) / sum(AB*nb_months),2),
    weighted_mean_night_rank = round(sum(yearly_mean_rank_Night * AB*nb_months, na.rm = T) / sum(AB*nb_months),2),
    weighted_mean_day_rank = round(sum(yearly_mean_rank_Day * AB*nb_months, na.rm = T) / sum(AB*nb_months),2),
    nb_session = n()
    ) |> 
  ungroup()

###### 4.2.2 All species ######
if(F){
  data_spe_session_temp = data_spe_session |> 
    left_join(data_temp_60sites_yearly_avrg |>  # Add gestion intensity for each session
                dplyr::select(-site_id,-year) |> 
                filter(session_id %in% data_trgtspe_session$session_id), by = "session_id") |> 
    mutate(nb_months = ifelse(is.na(nb_months), 0, nb_months)) |> 
    mutate(target_spe = ifelse(spe_id %in% list_target_species$spe_id, T, F))
  
  spe_temp_indices =  data_spe_session_temp |> 
    group_by(spe_id,spe_name, target_spe) |> 
    summarise(
      weighted_mean_night_temp = round(sum(yearly_mean_temp_Night * AB*nb_months, na.rm = T) / sum(AB*nb_months),2),
      weighted_mean_day_temp = round(sum(yearly_mean_temp_Day * AB*nb_months, na.rm = T) / sum(AB*nb_months),2),
      weighted_mean_night_rank = round(sum(yearly_mean_rank_Night * AB*nb_months, na.rm = T) / sum(AB*nb_months),2),
      weighted_mean_day_rank = round(sum(yearly_mean_rank_Day * AB*nb_months, na.rm = T) / sum(AB*nb_months),2),
      nb_session = n()
    ) |> 
    ungroup()
  
  spe_temp_indices |> 
    filter(nb_session > 30) |> 
    ggplot(aes(x = nb_session, y = weighted_mean_night_temp, label = spe_name, color = target_spe)) +
    geom_text_repel() +
    geom_point() +
    theme_bw() +
    theme(legend.position = "none") +
    labs(x = "Number of sessions where the species is found" , y = "Mean night temperature of the species" )
}

#### 5. Recap of all infos ####

##### 5.1 Target species recap #####
trgtspe_complete_infos = trgtspe_temp_indices |> 
  dplyr::select(spe_id,weighted_mean_night_temp,weighted_mean_day_temp) |> 
  left_join(trgtspe_management_indices |> 
              dplyr::select(spe_id, spe_name, weighted_mean_mngt_intensity, tot_session, tot_site,tot_ab),
            by = "spe_id") |> 
  rename(management_intensity = weighted_mean_mngt_intensity,
         night_temp = weighted_mean_night_temp,
         day_temp = weighted_mean_day_temp) |> 
  relocate(spe_id, spe_name, management_intensity, night_temp, day_temp)


#
if(F){
  trgtspe_complete_infos |> View()
  
  trgtspe_complete_infos |> 
    ggplot(aes(x = management_intensity, y = night_temp, label = spe_name)) +
    geom_point() +
    geom_text_repel() +
    theme_bw()
}

##### 5.2 Site recap #####

back_up_sites = c(61,81,46,88,89,44,24,49,105,11,10,95,70,103,30)

data_sites_infos = data_temp_60sites |> 
  distinct(site_id,site_lon,site_lat) |> 
  left_join(data_mngt_quanti_per_site |> select(site_id, mean_mngt_class, sd_mngt_class), 
            by = "site_id") |> 
  left_join(data_temp_60sites_mean |> 
              pivot_wider(id_cols = "site_id",
                          values_from = c("mean_temperature", "mean_rank"),
                          names_from = "day_time"),
            by = "site_id") |> 
  mutate(type_site = case_when(
    site_id %in% potential_sites_of_sampling$site_id ~  "prospected",
    site_id %in% back_up_sites ~  "back_up",
    T ~ "none"
  ))
  

# Relation between site temperature at night and management intensity
data_sites_infos |> 
  ggplot(aes(x = mean_mngt_class, y = mean_temperature_Night)) +
  geom_point()
# Spatial distribution of sites according to management intensity 
data_sites_infos |> 
  ggplot(aes(x = site_lon, y = site_lat, color = mean_mngt_class, shape = type_site, label = site_id )) +
  geom_point(size = 3 ) +
  geom_text_repel(aes(hjust = 0.5 , vjust = -0.8), colour = "black", size = 4) +
  theme_bw()
# Spatial distribution of sites according to management intensity and temperature
data_sites_infos |>
  ggplot(aes(x = site_lon, y = site_lat)) +
  geom_point(aes(color = mean_temperature_Night,
                 size = mean_mngt_class),
             alpha = 0.9, stroke = 1.2) +
  geom_text_repel( data = . %>% filter(type_site == "prospected"),
    aes(label = paste(site_id)),
    color = "red", size = 3.5, fontface = "bold") +
  geom_text_repel(data = . %>% filter(type_site != "prospected"),
    aes(label = site_id),
    color = "grey40", size = 3) +
  scale_color_viridis_c(option = "plasma") +
  scale_size(range = c(1, 5)) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom") +
  coord_sf()

###### 5.2.1 Site ranking  ####
sites_raking = data_sites_infos |> 
  mutate(rank_temperature_Night = rank(mean_temperature_Night),
         rank_temperature_Day = rank(mean_temperature_Day),
         rank_mngt_class = rank(mean_mngt_class)) |> 
  select(site_id, starts_with("rank_"), type_site)


#### 5.2.2 Add flora information ####

data_trgtspe_ab_2025 = data_trgtspe_session |> 
  filter(year == 2025) |> 
  select(-session_id, -year) |> 
  arrange(spe_name) |> 
  mutate(spe_name = case_when(
    spe_name == "Plantago lanceolata" ~ "Pl",
    spe_name == "Trifolium repens" ~ "Tr",
    spe_name == "Trifolium pratense" ~ "Tp",
    spe_name == "Dactylis glomerata" ~ "Dg",
    spe_name == "Lotus corniculatus" ~ "Lc",
    spe_name == "Achillea millefolium" ~ "Ac",
    T ~ NA
  )) |> 
  group_by(site_id) |> 
  mutate(nb_trgtspe = n())  |> 
  ungroup() |> 
  pivot_wider(id_cols = c("site_id", "nb_trgtspe"), 
              values_from = AB, 
              names_from = spe_name, 
              names_prefix = "ab_", 
              values_fill = 0)

data_sites_infos_final = data_sites_infos |>
  dplyr::select(-site_lon, -site_lat,-sd_mngt_class, -mean_rank_Day, -mean_rank_Night) |> 
  mutate(mean_temperature_Day = round(mean_temperature_Day,2),
         mean_temperature_Night = round(mean_temperature_Night,2),
         mean_mngt_class = round(mean_mngt_class,1)) |> 
  left_join(data_trgtspe_ab_2025, by = "site_id") |> 
  relocate(site_id, type_site)

# Correlation between management and night temperature for prospected and back up sites
data_sites_infos_final |> 
  filter(type_site != "none") |> 
  ggplot(aes(x = mean_mngt_class, y = mean_temperature_Night, label = site_id, colour = type_site, size = nb_trgtspe)) +
  geom_point() +
  geom_text_repel(fontface = "bold", size = 5) +
  theme_minimal() +
  labs(x = "Intensité de gestion", y = "Température nocturne moyenne", colour = "Classe", size = "Escpèces\ncibles")
# Correlation between management and night temperature for all sites
data_sites_infos_final |> 
  ggplot(aes(x = mean_mngt_class, y = mean_temperature_Night, label = site_id, colour = type_site, size = nb_trgtspe)) +
  geom_point() +
  geom_text_repel(fontface = "bold",size = 5) +
  theme_minimal() +
  labs(x = "Intensité de gestion", y = "Température nocturne moyenne", colour = "Classe",size = "Escpèces\ncibles")

data_sites_infos_final = data_sites_infos_final |> 
  mutate(temp_night_class = ifelse(mean_temperature_Night < 12, "Fr", "Ch"),
         mngt_night_class = ifelse(mean_mngt_class < 4, "NF", "F"),
         class = paste(temp_night_class,mngt_night_class,sep = "/")) 

###### 5.2.3 Contengency table of site propeerties and species presence #######



library(leaflet)

data_sites_infos_final |> 
  left_join(data_sites_infos |>  dplyr::select(site_id, site_lon, site_lat)) |> 
  ggplot(aes(x = site_lon, y = site_lat, label = site_id, color = type_site)) +
  geom_point() +
  geom_text_repel() +
  theme_bw() +
  theme(legend.position = "none")
