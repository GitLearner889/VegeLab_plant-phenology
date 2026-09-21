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


