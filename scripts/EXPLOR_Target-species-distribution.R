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
