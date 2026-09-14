source(here::here("programs","libraries.R"))

#### Data retrieval ####
sheets = excel_sheets(here("data/data_management/gestion_2026-09-14.xlsx"))
df_management_raw = read_excel(here("data/data_management/gestion_2026-09-14.xlsx"), sheets[1])

##### 2.2 Variables de gestion #####

df_management = df_management_raw %>% 
  rename(fauche_type = `Type de fauche majoritaire`,
         fauche_periode = `période de fauche`,
         fauche_nombre = `Nombre max de fauches par an`,
         fauche_residus = `Export des résidus de fauche`,
         site_id = site,
         year = `Année de gestion`,
         perturbations = `Perturbations du site`,
         outils = Outils) %>% 
  dplyr::select(-`Type de fauche / Pâturage détaillé`,-`Nombre de fauches par an`,-`Dates de fauche détaillées ou Pression de pâturage détaillé`,-`pression de gestion non contrôlée`,-comment) %>% 
  filter(year <=2025) %>% 
  mutate(fauche_type = case_when(
    fauche_type == "Tondu" ~ "T",
    fauche_type == "Fauché" ~ "F",
    fauche_type == "Broyé" ~ "B",
    fauche_type == "Non" ~ "PF",
    fauche_type == "Paturé" ~ "PAT",
    T ~ NA),
    
    fauche_periode = case_when(
      fauche_periode == "estivale" ~ "E",
      fauche_periode == "tardive" ~  "T",
      fauche_periode == "toute l'année" ~ "PET",
      fauche_periode == "estivale et tardive" ~ "ET",
      fauche_periode == "precoce" ~ "P",
      fauche_periode == "precoce et tardive" ~ "PT",
      fauche_periode == "Non" ~ "PF",
      T ~ NA),
    fauche_residus = case_when(
      fauche_type == "PF" ~ "PF",
      fauche_residus == "Pat" ~ "PAT",
      fauche_residus == "Non" ~ "NEx",
      fauche_residus == "Oui" ~ "Ex",
      T ~ NA
    ))  %>% 
  mutate(gestion = paste(fauche_type, fauche_nombre, fauche_periode, fauche_residus , sep = "_")) %>% 
  mutate(gestion = case_when(
    gestion == "NA_NA_NA_NA" ~ NA,
    str_detect(gestion,"PAT") ~ "PAT",
    str_detect(gestion,"PF") ~ "PF",
    T ~ gestion,
  )) %>% 
    relocate(site_id,year,fauche_type,fauche_nombre,fauche_periode,fauche_residus,outils,perturbations)

# Calcul des intensites de gestion
df_management = df_management %>% 
  mutate(session_id = paste(site_id,year, sep = "_"),
         fauche_residus = as.factor(fauche_residus),
         fauche_periode = as.factor(fauche_periode),
         fauche_type = as.factor(fauche_type),
         year = as.factor(year),
         gestion_classe = case_when(
           fauche_type == "PF" ~ "0 - Aucun passage",
           fauche_type == "PAT" | (fauche_nombre == 1 & fauche_residus == "NEx") ~ "1 - 1 passage sans export",
           fauche_nombre == 1 ~ "2 - 1 passage avec export",
           fauche_nombre == 2 ~ "3 - 2 passages",
           fauche_nombre < 5~ "4 - 3-4 passages",
           fauche_nombre <= 7 ~ "5 - 5-7 passages", 
           fauche_nombre > 7 ~ "6 - Plus de 7 passages",
           is.na(fauche_nombre) ~ NA), 
         habitat_type = case_when(
           fauche_nombre == 0 | (fauche_nombre == 1 & fauche_residus == "NEx") ~ "friche",
           fauche_nombre <= 2 ~ "prairie",
           fauche_nombre > 2 ~ "gazon"
         ),
         habitat_type = factor(habitat_type, levels = c("friche","prairie","gazon"))
  ) %>% 
  relocate(session_id) 

# New name
data_management = df_management
rm(df_management_raw, df_management)