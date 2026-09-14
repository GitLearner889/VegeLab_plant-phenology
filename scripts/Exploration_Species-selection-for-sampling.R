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
         square = Square)

data_traits = data_traits |> 
  rename(spe_name = Species,
         urbanity_class = Urbanity_class,
         reward = Reward)

data_quanti = data_quanti |> 
  rename(site_id = Site,
         year = Year)

data_cwm = data_cwm |> 
  rename(year = site_year)

#### 1. Data management #### 
data_spe_session = data_sqr |> # Dataframe of abundance for each species in each session
  group_by(site_id,year,spe_id,spe_name) |> 
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
  mutate(fruiting = case_when(
    spe_name %in% september_fruiting_spe$spe_name ~ T,
    T ~ F
  )) |> 
  group_by(spe_id,spe_name, fruiting) |> 
  summarise(mean_freq = round(mean(freq_site)),
            max_freq = max(freq_site),
            mean_ab = round(mean(AB_Tot)),
            max_ab = max(AB_Tot)) |> 
  ungroup()


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


data_spe_distribution_2025 = data_spe_distribution |> 
  filter(year == 2025) |> # Keep only most frequent species across sites
  filter(freq_site >= 10) |> # Keep only most frequent species across sites
  mutate(fruiting = case_when(
    spe_name %in% september_fruiting_spe$spe_name ~ T,
    T ~ F
  )) |> 
  mutate()


#### Management data ####
# Retrieve management data 
source(here("programs/formatage-gestion.R"))

# Quantitative indices about management
data_mngt_quanti_per_site = data_management |> 
  filter(as.numeric(year) <= 2025) |> # to be coherent with plant data
  mutate(gestion_classe_fct = as.factor(gestion_classe)) |> 
  mutate(gestion_classe_num = as.numeric(gestion_classe_fct)) |> 
  group_by(site_id) |> 
  summarise(mean_mngt_class = round(mean(gestion_classe_num, na.rm = T),2),
            sd_mngt_class = round(sd(gestion_classe_num, na.rm = T),3),
            min_mngt_class = min(gestion_classe_num, na.rm = T),
            max_mngt_class = max(gestion_classe_num, na.rm = T),
            nb_session = n())

# Add managemeny infos of potential sites
potential_sites_of_sampling = potential_sites_of_sampling |> 
  left_join(data_mngt_quanti_per_site, by = "site_id")



