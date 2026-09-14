source(here::here("programs/libraries.R"))

# Import plant data
df_sqr = read_excel(here("data/Square2020_2025.xlsx")) # quadrat distribution
df_traits = read_excel(here("data/TraitsBDD2020_2025.xlsx")) # trait for each species
df_cwm = read_excel(here("data/CWM2020_2025.xlsx")) # community weighted mean per session
df_quanti = read_excel(here("data/Quanti2020_2025.xlsx")) # quantitative indices per session



df_spe = df_sqr |> 
  group_by(SITE,ANNEE,CD_REF_18,LB_NOM) |> 
  summarise(AB = n()) |> 
  ungroup()

df_spe_distribution = df_spe |> 
  group_by(CD_REF_18,LB_NOM,ANNEE) |> 
  summarise(Freq_site = n(),
            AB_Tot = sum(AB)) |> 
  ungroup()

df_spe_distribution |> 
  filter(Freq_site >= 10) |> 
  View()

df_traits |> 
  filter(flw_late>=8) |> 
  View()
