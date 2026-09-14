source(here::here("programs","libraries.R"))
test = F

df_temperature_all_raw = read.table(here("data/data_environnement/ibutton_all.txt"), header = T, sep = "\t")

df_temperature_all = df_temperature_all_raw %>% 
  mutate(datetime_cet = ymd_hms(datetime_cet),
         datetime_utc = ymd_hms(datetime_utc),
         date = ymd(date),
         sunset = ymd_hms(sunset),
         sunrise = ymd_hms(sunrise)) %>% # time = lubridate::hms(time)
  mutate(year = year(date),
         month = month(date),
         week = week(date),
         day = day(date))
  
if(test){ 
   # Regarde quelles sont les dates ou aucune infos de periode (nuit/jour) n'est presente
    df_na = df_temperature_all %>% 
      filter(is.na(day_night))
    df_na %>%  # Uniquement les derniers dimanches de mars (ie jour ou on change d'heure) 
      distinct(date) %>%  
      print()
    print("        ")
    df_na %>%  # Que de 2h a 3h du matin, l'heure qui saute 
      mutate(time_hours = hour(hms(time))) %>% 
      distinct(time_hours) %>% 
      print()
    rm(df_na)
  
  # Calcul du jour et de la nuit
  df_day_night = df_temperature_all %>% 
    mutate(day_night_cet = case_when(
      datetime_cet <= sunset & datetime_cet >= sunrise ~ "day",
      datetime_cet > sunset | datetime_cet < sunrise ~ "night",
      T ~ NA
    )) %>% 
    mutate(day_night_utc = case_when(
      datetime_utc <= sunset & datetime_utc >= sunrise ~ "day",
      datetime_utc > sunset | datetime_utc < sunrise ~ "night",
      T ~ NA
    ))
  
  df_day_night %>% 
    mutate(dn_utc = as.factor(paste(day_night,day_night_utc, sep = "_")),
           dn_cet = as.factor(paste(day_night,day_night_cet, sep = "_"))) %>% 
    group_by(dn_cet) %>% 
    count() %>% 
    print()
  
  df_day_night %>% 
    mutate(dn_utc = as.factor(paste(day_night,day_night_utc, sep = "_")),
           dn_cet = as.factor(paste(day_night,day_night_cet, sep = "_"))) %>% 
    group_by(dn_utc) %>% 
    count() %>% 
    print()
  
  rm(df_day_night)
}

df_temp_triees = df_temperature_all %>%
  select(-file,-datetime_chr,-datetime_cet,-sunrise,-sunset,-datetime_utc)  %>% 
  rename(site_id = "site") %>% 
  mutate(day_night = ifelse(is.na(day_night),"night",day_night),
         datetime = ymd_hms(paste(date, time))) %>% 
  filter(temp <= 60 & !is.na(temp)) # Erreur, 1 donnees avec une temperature a 89 °C

df_temp_stat = df_temp_triees %>% 
  group_by(site_id,year,month,week,day_night) %>% 
  summarise(max_temp = max(temp),
            mean_temp = mean(temp),
            min_temp = min(temp),
            nb_obs = n())

df_temp_stat = df_temp_triees %>% 
  group_by(site_id,year,month,week) %>% 
  summarise(max_temp = max(temp),
            mean_temp = mean(temp),
            min_temp = min(temp),
            nb_obs = n())

df_temp_stat %>% 
  filter(nb_obs < 250) %>% 
  ggplot(aes(x = nb_obs)) +
  geom_histogram()


ggplot(df_temperature_all,aes(x = date, y = temp)) +
  geom_line() +
  facet_wrap(~ site)

df_temperature_all %>% 
  filter(site == 1) %>% 
  ggplot(aes(x = date, y = temp)) +
  geom_line() +
  facet_wrap(~ site)



df_temp_triees %>% 
  group_by(site_id) %>% 
  summarise(mean_temp = mean(temp)) %>% 
  view()
#### Date ####

df_date = df_temp_triees %>% 
  group_by(site_id,date) %>% 
  summarise(nb_obs_per_day = n())

df_date %>% 
  ggplot(aes(x= nb_obs_per_day)) +
  geom_histogram()
df_date %>% 
  ggplot(aes(x= as.factor(nb_obs_per_day))) +
  geom_bar()

df_date %>% 
  group_by(nb_obs_per_day) %>% 
  summarise(n = n()) %>% View()

df = df_temp_triees %>% 
  left_join(df_date, by = c("date","site_id"))

df %>% filter(nb_obs_per_day == 24) %>% 
  distinct(date) %>% 
  View()

df %>% filter(nb_obs_per_day == 48) %>% 
  distinct(date) %>% 
  View()

df %>% filter(nb_obs_per_day<18 & nb_obs_per_day > 7) %>% 
  distinct(date) %>% 
  View()

df %>% filter(nb_obs_per_day<18 & nb_obs_per_day > 7) %>% 
  distinct(site_id) %>% 
  View()

df %>% filter(nb_obs_per_day >48) %>% 
  distinct(date) %>% 
  View()

df %>% filter(nb_obs_per_day >48) %>% 
  distinct(site_id) %>% 
  View()

## 
df %>% filter(nb_obs_per_day >48) %>%  
  ggplot(aes(x = hms(time))) +
  geom_histogram() +
  facet_wrap(vars(site_id, date))

df %>% filter(nb_obs_per_day >48) %>%  
  ggplot(aes(x = hms(time),y = temp)) +
  geom_point() +
  facet_wrap(vars(site_id, date))

##

df %>% filter(nb_obs_per_day == 7) %>% 
  ggplot(aes(x = hms(time))) +
  geom_histogram() +
  facet_wrap(vars(site_id, date))

df %>% filter(nb_obs_per_day == 6) %>% 
  ggplot(aes(x = hms(time))) +
  geom_histogram() +
  facet_wrap(vars(site_id, date))

df %>% filter(nb_obs_per_day == 5) %>% 
  ggplot(aes(x = hms(time))) +
  geom_histogram() +
  facet_wrap(vars(site_id, date))


#### 1. Code temperature journaliere ####


df_hourly <- df_temp_triees %>%
  mutate(hour = hour(datetime)) %>% 
  group_by(site_id, date, hour) %>%
  summarise(temp = mean(temp, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(month = month(date),
         year = year(date))

df_cycle_month <- df_hourly %>%
  group_by(site_id, month, hour) %>%
  summarise(temp_mean = mean(temp, na.rm = TRUE),
            temp_sd = sd(temp, na.rm = TRUE),
            n = n(),
            .groups = "drop")

##### 1.1 Profil journalier par site pour chaque moi####

ggplot(df_cycle_month, aes(x = hour, y = temp_mean, color = factor(month), group = month)) +
  geom_line(size = 1) +
  facet_wrap(~site_id) +
  scale_x_continuous(breaks = seq(0, 23, by = 3)) +
  labs(x = "Heure", y = "Température moyenne (°C)", color = "Mois") +
  theme_bw()

##### 1.2 Profil des sites les plus chauds et les plus froids #####
# Defini les sites les plus chauds et les plus froids (moyenne ensemble des releves)
site_mean_temp <- df_hourly %>%
  group_by(site_id) %>%
  summarise(
    mean_temp = mean(temp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_temp))

hot_sites <- site_mean_temp %>%
  slice_max(mean_temp, n = 10)

cold_sites <- site_mean_temp %>%
  slice_min(mean_temp, n = 10)

df_compare <- df_hourly %>%
  mutate(group = case_when(
    site_id %in% hot_sites$site_id ~ "Hot",
    site_id %in% cold_sites$site_id ~ "Cold",
    TRUE ~ NA_character_)) %>%
  filter(!is.na(group))

df_compare_cycle <- df_compare %>%
  group_by(group, month, hour) %>%
  summarise(temp_mean = mean(temp, na.rm = TRUE),
            temp_sd = sd(temp, na.rm = TRUE),
            .groups = "drop")

ggplot(df_compare_cycle, aes(x = hour, y = temp_mean, color = group, group = group)) +
  geom_line(size = 1.2) +
  facet_wrap(~month) +
  scale_x_continuous(breaks = seq(0, 23, by = 3)) +
  scale_color_discrete(type = c("Hot" = "red", "Cold" = "steelblue")) +
  labs(x = "Heure", y = "Température moyenne (°C)", color = "Groupe") +
  theme_bw()

##### 1.3 Amplitude thermique des sites les plus chaud et les plus froids#####
daily_amplitude <- df_hourly %>%
  group_by(site_id, date) %>%
  summarise(
    tmin = min(temp, na.rm = TRUE),
    tmax = max(temp, na.rm = TRUE),
    amplitude = tmax - tmin,
    .groups = "drop"
  )

daily_amplitude <- daily_amplitude %>%
  mutate(
    group = case_when(
      site_id %in% hot_sites$site_id ~ "Hot",
      site_id %in% cold_sites$site_id ~ "Cold",
      TRUE ~ NA_character_
    ),
    month = month(date)
  ) %>%
  filter(!is.na(group))

ggplot(daily_amplitude,
       aes(x = factor(month), y = amplitude, fill = group)) +
  geom_boxplot() +
  labs(x = "Mois", y = "Amplitude thermique journalière (°C)") +
  scale_fill_discrete(type = c("Hot" = "red", "Cold" = "steelblue")) +
  theme_bw()

##### 1.4 Difference journalieres entres les sites chauds et froids#####

###### 1.4.1 Diffrence sur toute la periode de suivi ####
df_diff <- df_compare %>%
  group_by(group, month, hour) %>%
  summarise(temp_mean = mean(temp, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = group, values_from = temp_mean) %>%
  mutate(diff_hot_cold = Hot - Cold)

ggplot(df_diff, aes(x = hour, y = diff_hot_cold)) +
  geom_line(size = 1.2) +
  facet_wrap(~month) +
  scale_x_continuous(breaks = seq(0, 23, by = 3)) +
  geom_hline(yintercept = 0,linetype = "dashed" ) +
  labs(x = "Heure", y = "Écart thermique Hot - Cold (°C)", title = "Différence thermique journalière entre sites chauds et froids") +
  theme_bw()

###### 1.4.2 Différences pour chaque annee ######
df_diff_year <- df_compare %>%
  group_by(year, month, hour, group) %>%
  summarise(temp_mean = mean(temp, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = group, values_from = temp_mean) %>%
  mutate(diff_hot_cold = Hot - Cold)

df_diff_year <- df_diff_year %>%
  mutate(month_lab = month(month,label = TRUE, abbr = FALSE))

ggplot(df_diff_year, aes(x = hour, y = diff_hot_cold, color = factor(year), group = year)) +
  geom_line(size = 1) +
  facet_wrap(~month_lab) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 23, by = 3)) +
  labs(x = "Heure", y = "Écart thermique Hot - Cold (°C)", color = "Année", title = "Variation journalière de l'écart thermique entre sites chauds et froids") +
  theme_bw()

# Courbes lissees
ggplot(df_diff_year, aes(x = hour, y = diff_hot_cold, color = factor(year))) +
  geom_smooth(se = FALSE, span = 0.4, linewidth = 1) +
  facet_wrap(~month_lab) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 23, by = 3)) +
  labs(x = "Heure", y = "Écart thermique Hot - Cold (°C)", color = "Année", title = "Variation journalière lissée de l'écart thermique entre sites chauds et froids") +
  theme_bw()

# Lissage par mesure de la moyenne sur 3h glissantes

library(slider)

df_diff_year <- df_diff_year %>%
  arrange(year, month, hour) %>%
  group_by(year, month) %>%
  mutate(diff_smooth = slide_dbl(diff_hot_cold, mean,
                                 .before = 1, .after = 1, .complete = FALSE,
                                 na.rm = TRUE))

ggplot(df_diff_year,aes(x = hour, y = diff_smooth, color = factor(year))) +
  geom_line(size = 1) +
  facet_wrap(~month_lab) +
  labs(x = "Heure", y = "Écart thermique Hot - Cold (°C)", color = "Année", title = "Variation journalière lissée de l'écart thermique entre sites chauds et froids") +
  theme_bw()


