library(tidyverse)
library(sf)
library(terra)
library(readxl)

saussan <- read_excel("/home/ptaconet/contributions_diverses_projets_mivegec/vectrap/entomo_data/eid_Data Saussan/Saussan 22 - data EID - BGS - 03_10_2025_Taconet.xlsx")
saussan$male = as.numeric(saussan$male)
saussan$femelle = as.numeric(saussan$femelle)


saussan2 = saussan %>%
  dplyr::filter(traitement=='C') %>%
  mutate(nb_jours = as.numeric(relevé - depart)) %>%
  mutate(nb_femelles_jours = femelle/nb_jours) %>%
  mutate(week = week(relevé)) %>%
  mutate(piege = as.factor(piege))

saussan2 %>%
  group_by(piege) %>%
  mutate(count=n()) %>%
  ggplot(aes(x = reorder(piege,nb_femelles_jours, FUN = median,na.rm = TRUE), y = nb_femelles_jours)) +
  geom_boxplot() +
  geom_label(aes(label= count , y = 30), size = 4, position = position_dodge(width = 0.75)) +
  ggtitle("abundance per trap") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


saussan2 %>%
  ggplot(aes(x = week, y = nb_femelles_jours)) +
  geom_line() +
  facet_wrap(.~piege)

library(MASS)
library(dplyr)
library(emmeans)
library(multcomp)

# Keep only rows with a numeric total
df2 <- saussan2 %>% filter(!is.na(nb_femelles_jours))

# Fit negative binomial model
model_nb <- glm.nb(nb_femelles_jours ~ piege, data = df2)

# Test if trap affects the mean count
anova(model_nb, test = "Chisq")

# Pairwise comparisons between traps
emmeans(model_nb, pairwise ~ piege)

letters_df <- emmeans(model_nb, ~ piege) |>
  cld(alpha = 0.05, Letters = letters) |>
  as.data.frame()


# Make a boxplot + add letters above each box
ggplot(saussan2, aes(x = reorder(piege,nb_femelles_jours, FUN = median,na.rm = TRUE), y = nb_femelles_jours)) +
  geom_boxplot() +
  geom_text(data = letters_df,
            aes(x = piege, y = emmean, label = .group),
            vjust = -0.5, color = "red", size = 5) +
  ggtitle("Abundance per trap (with CLD letters)") +
  theme(axis.text.x = element_text(angle = 90))


## Saussan fabregues pignan

saussan_fabregues_pignan <- read_excel("/home/ptaconet/contributions_diverses_projets_mivegec/vectrap/entomo_data/eid_pigan_fabregues_saussan/DATA EID IRD PAUL TACONNET/autodis21 data BGS pour analyse 211104.xlsx")
colnames(saussan_fabregues_pignan) <- c("week","nb_jours","site","trap_code","male","femelle","indetermine","total","nb_femelles_jours","nb_moustiques_jours","fourmis","remarques")
saussan_fabregues_pignan$nb_femelles_jours = as.numeric(saussan_fabregues_pignan$nb_femelles_jours)

saussan_fabregues_pignan %>%
  filter(!is.na(nb_femelles_jours)) %>%
  filter(trap_code != "BG PI 12") %>%
  group_by(week,site) %>%
  summarise(nb_femelles_jours=mean(nb_femelles_jours, na.rm = T)) %>%
  ggplot(aes(x=week, y = nb_femelles_jours, color = site, group = site)) + geom_line()


saussan_fabregues_pignan2 = saussan_fabregues_pignan %>%
  filter(!is.na(nb_femelles_jours)) %>%
  filter(trap_code != "BG PI 12")

saussan_fabregues_pignan2 %>%
  group_by(trap_code) %>%
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(trap_code,nb_femelles_jours, FUN = median,na.rm = TRUE), y = nb_femelles_jours)) +
  geom_boxplot() +
  geom_label(aes(label= count , y = 20), size = 3, position = position_dodge(width = 0.75)) +
  ggtitle("abundance per trap") +
  facet_wrap(.~site, scale = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylim(c(0,20))


## vectrap

df_vectrap <- read_xlsx("entomo_data/VECTRAP_2021-2023 monitoring results_pour paul.xlsx", sheet = "data 2021 to 2023 - Montpellier")

df_vectrap <- df_vectrap %>%
  filter(!is.na(date_turn_off)) %>%
  rename(date_instal = date_turn_on , daterec = date_turn_off, Site = commune) %>%
  mutate(week = week(daterec), Mois_numeric = month(daterec), Year = year(daterec)) %>%
  mutate(blocks=as.character(blocks)) %>%
  mutate(nb_femelles_jour = as.numeric(number_female)/as.numeric((daterec-date_instal)))%>%
  mutate(nb_males_jour = as.numeric(number_male)/as.numeric((daterec-date_instal))) %>%
  mutate(nb_moustiques_jour = as.numeric(number_total)/as.numeric((daterec-date_instal)))

df_vectrap <- df_vectrap %>%
  filter(modality=="C", species == "Aedes albopictus") %>%
  filter(blocks %in% c(1,4,6,7))

df_vectrap %>%
  group_by(trap_code) %>%
  mutate(count = n()) %>%
  ggplot(aes(x = reorder(trap_code,nb_femelles_jour, FUN = median,na.rm = TRUE), y = nb_femelles_jour)) +
  geom_boxplot() +
  geom_label(aes(label= count , y = 50), size = 3, position = position_dodge(width = 0.75)) +
  ggtitle("abundance per trap") +
  facet_wrap(Site~., scales= "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

df_vectrap %>%
  group_by(week,Site,Year) %>%
  summarise(nb_femelles_jours=mean(nb_femelles_jour, na.rm = T)) %>%
  ggplot(aes(x=week,y = nb_femelles_jours, color = Site, group = Site)) + geom_line() + facet_wrap(.~Year)

## these colombine

pieges_lieu <- st_read("/home/ptaconet/contributions_diverses_projets_mivegec/modeling_vector_mtp/localisation_piege_provisoire.gpkg")  %>%
  st_drop_geometry() %>%
  dplyr::select(ID_PIEGE,lieu)

pieges <- st_read("/home/ptaconet/contributions_diverses_projets_mivegec/modeling_vector_mtp/P02_TRAPS_LOCATION.gpkg") %>%
  filter(TYPE_PIEGE == 'bg-sentinel') %>%
  mutate(LATITUDE=as.numeric(LATITUDE), LONGITUDE = as.numeric(LONGITUDE)) %>%
  left_join(pieges_lieu) %>%
  mutate(lieu = ifelse(ID_PIEGE %in% c("BG_12","BG_13"),"Alco",lieu)) %>%
  mutate(lieu = ifelse(ID_PIEGE %in% c("BG_15","BG_16"),"Hotel de Ville",lieu)) %>%
  rename(site = lieu) %>%
  st_transform(2154)

df_montpellier <- read.csv("/home/ptaconet/contributions_diverses_projets_mivegec/modeling_vector_mtp/P02_BG-ADULTS_LABO_DATA.csv", stringsAsFactors = F, sep = ";") %>%
  mutate(HEURE_COLLECTE = ifelse(is.na(HEURE_COLLECTE),"10:00",HEURE_COLLECTE)) %>%
  rename(ID_COLLECTE_STRING = ID_COLLECTE) %>%
  mutate(DATE_POSE = parse_date(DATE_POSE,"%d/%m/%Y")) %>%
  mutate(DATE_COLLECTE = parse_date(DATE_COLLECTE,"%d/%m/%Y")) %>%
  mutate(across(everything(), ~ gsub("\\s*\\([^\\)]+\\)", "", .))) %>%
  left_join(pieges, by = "ID_PIEGE") %>%
  group_by(ID_PIEGE,DATE_POSE) %>%
  mutate(row_number = row_number()) %>%
  ungroup() %>%
  mutate(DATE_POSE = ifelse(row_number==2,as.character(as.Date(DATE_POSE)+1),as.character(as.Date(DATE_POSE)))) %>%
  rename(NB_ALBO_F_PP = NB_ALBO_PP, NB_ALBO_F_GRAVIDE = NB_ALBO_GRAVIDE, NB_ALBO_F_P_ND= NB_ALBO_P_ND, NB_ALBO_F_PT_PP=NB_ALBO_PT_PP ,NB_ALBO_F_PT_NP=NB_ALBO_PT_NP) %>%
  arrange(DATE_COLLECTE) %>%
  mutate(projet="these_colombine") %>%
  mutate(NB_ALBO_F=as.numeric(NB_ALBO_F), NB_ALBO_M=as.numeric(NB_ALBO_M), NB_ALBO_TOT=as.numeric(NB_ALBO_TOT)) %>%
  rename(trap_code = ID_PIEGE, date_installation = DATE_POSE, date_releve = DATE_COLLECTE, nb_femelles = NB_ALBO_F,nb_males =NB_ALBO_M , nb_tot = NB_ALBO_TOT) %>%
  mutate(date_releve=as.Date(date_releve), date_installation=as.Date(date_installation), week = week(date_releve)) %>%
  mutate(nb_jours = as.numeric(date_releve - date_installation)) %>%
  mutate(nb_moustiques_jour = nb_tot/nb_jours, nb_males_jour =nb_males/nb_jours, nb_femelles_jour = nb_femelles/nb_jours) %>%
  dplyr::select(trap_code, site,week,  date_installation, date_releve, nb_femelles, nb_males, nb_tot,nb_moustiques_jour, nb_males_jour, nb_femelles_jour, projet ) %>%
  mutate(blocks = NA)

ggplot(df_montpellier,aes(x = reorder(trap_code,nb_femelles_jour, FUN = median,na.rm = TRUE), y = nb_femelles_jour)) +
  geom_boxplot(outliers = FALSE) +
  ggtitle("abundance per trap") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


