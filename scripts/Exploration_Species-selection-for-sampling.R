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

#### 1. Data species #### 
data_spe_session = data_sqr |> # Dataframe of abundance for each species in each session
  group_by(session_id,site_id,year,spe_id,spe_name) |> 
  summarise(AB = n()) |> 
  ungroup()

data_spe_distribution = data_spe_session |> # Dataframe of abundance of species across all sites for each year
  group_by(spe_id,spe_name,year) |> 
  summarise(freq_site = n(),
            AB_Tot = sum(AB)) |> 
  ungroup()

most_frequent_species = data_spe_distribution |> # Keep only most frequent species across sites
  filter(freq_site >= 10)

september_fruiting_spe = data_traits |>  # Defines species that should be fruiting in september
  filter(flw_late>=8 & flw_late != "NA")

list_selected_species = most_frequent_species |> 
  # filter(spe_name %in% september_fruiting_spe$spe_name) |> 
  left_join(data_traits |> 
              dplyr::select(spe_name,flw_late), by = "spe_name" )|> 
  mutate(fruiting = ifelse(flw_late >=8,T,F)) |> 
  group_by(spe_id,spe_name, fruiting, flw_late) |> 
  summarise(mean_freq = round(mean(freq_site)),
            max_freq = max(freq_site),
            mean_ab = round(mean(AB_Tot)),
            max_ab = max(AB_Tot)) |> 
  ungroup()


list_selected_species_2025 = list_selected_species |> 
  filter(year == 2025) 

#### 2. Species and site selection  ####
# Sites that should be explored in late september
potential_sites_of_sampling = data.frame(site_id = c(74, 27, 75, 23, 92, 58, 96, 55, 45, 72)) |> 
  arrange(site_id)

# Count for each year the number of time the species is present in one of the 10 potential sites
temp_data = data_spe_session |> 
  mutate(potential_site = case_when(
    site_id %in% potential_sites_of_sampling$site_id ~ 1,
    T ~ 0
  )) |> 
  group_by(spe_id,year) |> 
  mutate(nb_present_potentiel_site = sum(potential_site)) |> 
  ungroup()

# Verification
if(F){
  temp_data |> distinct(year, spe_name, nb_present_potentiel_site) |> nrow() ==  temp_data |> distinct(year, spe_name) |> nrow()}


data_spe_distribution_2025 = temp_data |> # Dataframe of abundance of species across all sites for each year
  group_by(spe_id,spe_name,year,nb_present_potentiel_site) |> 
  summarise(freq_site = n(),
            AB_Tot = sum(AB)) |> 
  ungroup() |> 
  filter(year == 2025) |> # Keep only most frequent species across sites
  filter(freq_site >= 10) |> # Keep only most frequent species across sites
  mutate(fruiting = case_when(
    spe_name %in% september_fruiting_spe$spe_name ~ T,
    T ~ F
  )) 

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
  left_join(data_mngt_quanti_per_site, by = "session_id", )

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
data_spe_session_mngt = data_spe_session |> 
  left_join(data_mngt_reduced |>  # Add gestion intensity for each session
              dplyr::select(-site_id,-year), by = "session_id")

spe_management_indices =  data_spe_session_mngt|> 
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

## Graphs 

hist(spe_management_indices$mean_mngt_intensity)
hist(spe_management_indices$weighted_mean_mngt_intensity)


# Sélectionner quelques espèces d'intérêt (ex : les plus abondantes)
species_of_intrest <- spe_management_indices %>%
  slice_max(tot_ab, n = 12) %>%
  pull(spe_name)

species_of_intrest <- list_selected_species %>%
 filter(fruiting) %>%
  pull(spe_name) 

# Distribution of intensity of gestion per species
ggplot(data = filter(data_spe_session_mngt, spe_name %in% species_of_intrest),
       aes(x = gestion_classe_num)) +
  geom_histogram(binwidth = 1, fill = "#6d4aff", color = "white") +
  facet_wrap(~ spe_name, scales = "free_y") +
  labs(x = "Intensité de gestion (classe)",y = "Nombre d'observations", title = "Distribution des intensités de gestion par espèce") +
  theme_minimal()
# Distribution of intensity of gestion per species weighted by their abundance
ggplot(data = filter(data_spe_session_mngt, spe_name %in% species_of_intrest),
       aes(x = gestion_classe_num, weight = AB)) +
  geom_histogram(binwidth = 1, fill = "#118911", color = "white") +
  facet_wrap(~ spe_name, scales = "free_y") +
  labs(x = "Intensité de gestion (classe)", y = "Abondance cumulée", title = "Distribution pondérée des intensités de gestion par espèce") +
  theme_minimal()

# 
ggplot(spe_management_indices, aes(x = weighted_mean_mngt_intensity, y = weighted_sd_mngt_intensity)) +
    geom_point(aes(size = tot_ab, color = tot_mngt_class),
             alpha = 0.6) +
  scale_size_continuous(range = c(1, 8), name = "Abondance totale") +
  scale_color_viridis_c(name = "Nb classes de gestion") +
  labs(
    x = "Intensité de gestion moyenne pondérée",
    y = "Écart-type pondéré",
    title = "Positionnement des espèces le long du gradient de gestion",
    subtitle = "Espèces en haut : fréquentent des gestions variées"
  ) +
  theme_minimal()


list_selected_species |> 
  left_join(spe_management_indices |> select(-spe_name), by = "spe_id") |> View()

