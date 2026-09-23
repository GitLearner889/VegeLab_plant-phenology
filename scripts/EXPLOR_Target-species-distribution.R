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
data_trgtspe_session = data_sqr |> 
  filter(spe_id %in% list_target_species$spe_id) |> 
  group_by(session_id,site_id,year,spe_id,spe_name) |> 
  summarise(AB = n()) |> 
  ungroup()

# Dataframe of abundance of species across all sites for each year
data_trgtspe_distribution = data_trgtspe_session |> 
  group_by(spe_id,spe_name,year) |> 
  summarise(freq_site = n(),
            AB_Tot = sum(AB)) |> 
  ungroup()

#### 2. Site data manipulation  ####

##### 2.1 Pre-selected sites ####
# Sites that should be explored in late september
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

data_temp_60sites_mean = data_temp_60sites |> 
  group_by(site_id,day_time) |> 
  summarise(mean_temperature = mean(daily_temp),
            mean_rank = mean(normalised_rank))

data_temp_60sites_mean |> 
  ggplot(aes(x = mean_rank, y = mean_temperature, color = as.factor(site_id))) +
  geom_point() +
  facet_wrap(~ day_time, scales = "free_y") +
  theme_bw()

data_temp_60sites_mean |> 
  pivot_wider(id_cols = "site_id", 
              values_from = c("mean_temperature","mean_rank"), 
              names_from = "day_time") |> 
  ggplot(aes(x = mean_temperature_Night, y = mean_temperature_Day, color = as.factor(site_id))) +
  geom_point() +
  theme_bw()

data_temp_60sites_mean |> 
  pivot_wider(id_cols = "site_id", 
              values_from = c("mean_temperature","mean_rank"), 
              names_from = "day_time") |> 
  ggplot(aes(x = mean_rank_Night, y = mean_rank_Day, color = as.factor(site_id))) +
  geom_point() +
  theme_bw()

# Create a dataframe with important variable to choose species

data_selection = data_spe_distribution_2025 |>
  left_join(spe_management_indices |> select(spe_name, weighted_mean_mngt_intensity, weighted_sd_mngt_intensity), by = "spe_name") 



