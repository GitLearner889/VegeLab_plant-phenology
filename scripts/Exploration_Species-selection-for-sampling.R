source(here::here("programs/libraries.R"))

#### 0. Data importation ####
# Import plant data
data_sqr = read_excel(here("data/Square2020_2025.xlsx")) # quadrat distribution
data_traits = read_excel(here("data/TraitsBDD2020_2025.xlsx")) # trait for each species
data_cwm = read_excel(here("data/CWM2020_2025.xlsx")) # community weighted mean per session
data_quanti = read_excel(here("data/Quanti2020_2025.xlsx")) # quantitative indices per session


#### 1. Data management #### 
data_spe_session = data_sqr |> # Dataframe of abundance for each species in each session
  group_by(SITE,ANNEE,CD_REF_18,LB_NOM) |> 
  summarise(AB = n()) |> 
  ungroup()

data_spe_distribution = data_spe_session |> # Dataframe of abundance of species across all sites for each year
  group_by(CD_REF_18,LB_NOM,ANNEE) |> 
  summarise(Freq_site = n(),
            AB_Tot = sum(AB)) |> 
  ungroup()

most_frequent_species = data_spe_distribution |> # Keep only more frequent species across sites
  filter(Freq_site >= 10)

september_fruiting_spe = data_traits |>  # Defines species that should be fruiting in september
  filter(flw_late>=8)
