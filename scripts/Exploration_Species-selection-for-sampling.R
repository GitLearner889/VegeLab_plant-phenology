source(here::here("programs/libraries.R"))

#### 0. Data importation ####
# Import plant data
data_sqr = read_excel(here("data/Square2020_2025.xlsx")) # quadrat distribution
data_traits = read_excel(here("data/TraitsBDD2020_2025.xlsx")) # trait for each species
data_cwm = read_excel(here("data/CWM2020_2025.xlsx")) # community weighted mean per session
data_quanti = read_excel(here("data/Quanti2020_2025.xlsx")) # quantitative indices per session
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

most_frequent_species = data_spe_distribution |> # Keep only more frequent species across sites
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


