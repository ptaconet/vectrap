library(tidyverse)
library(sf)
library(readxl)

#### Objectif : créer 2 jeux de données
# 1 dataset trap_coordinates (trap_code, project, date_debut, date_fin, longitude, latitude)
# 1 dataset data_collection (trap_code, project, date_instal, date_releve, n_jours, femelles_jour, males_jour, tot_jours)

########################################
## vectrap (EID)
########################################

## trapping method : BG SENTINEL, , baited with CO2 and BG-Lure or equivalent aromatic lures from the same manufacturer, placed in areas protected for wind, rainfall and direct sunlight and close to vegetation.

pieges_loc <-  read_xlsx("entomo_data/VECTRAP_2021-2023 monitoring results_pour paul.xlsx", sheet = "Notice_coord geo")
colnames(pieges_loc) <- c("trap_code","Longitude","Latitude")

pieges_loc_vectrap_sf <- pieges_loc %>%
  st_as_sf(.,coords = c("Longitude","Latitude"), crs = 2154)%>%
  mutate(projet = "vectrap")


df_vectrap <- read_xlsx("entomo_data/VECTRAP_2021-2023 monitoring results_pour paul.xlsx", sheet = "data 2021 to 2023 - Montpellier")

df_vectrap <- df_vectrap %>%
  filter(!is.na(date_turn_off)) %>%
  rename(date_instal = date_turn_on , daterec = date_turn_off, Site = commune) %>%
  mutate(week = week(daterec), Mois_numeric = month(daterec), Year = year(daterec)) %>%
  mutate(blocks=as.character(blocks)) %>%
  mutate(nb_femelles_jour = as.numeric(number_female)/as.numeric((daterec-date_instal)))%>%
  mutate(nb_males_jour = as.numeric(number_male)/as.numeric((daterec-date_instal))) %>%
  mutate(nb_moustiques_jour = as.numeric(number_total)/as.numeric((daterec-date_instal))) %>%
  mutate(exposure_days = as.numeric(daterec-date_instal)) %>%
  mutate(Site = tolower(Site)) %>%
  mutate(Site = ifelse(Site == "st clement",  "saint clement", Site))



df_vectrap <- df_vectrap %>%
  filter(modality=="C", species == "Aedes albopictus") %>%
  filter(blocks %in% c(1,4,6,7))


df_vectrap <- df_vectrap %>%
  dplyr::select(trap_code,week, Site, blocks, date_instal, daterec, exposure_days, number_female, number_male, number_total, nb_femelles_jour, nb_males_jour, nb_moustiques_jour) %>%
  rename(date_installation = date_instal, date_releve = daterec, nb_femelles = number_female, nb_males = number_male, nb_tot = number_total, site = Site ) %>%
  mutate(projet = "vectrap")

pieges_loc_vectrap_sf <- pieges_loc_vectrap_sf %>%
  filter(trap_code %in% unique(df_vectrap$trap_code)) %>%
  left_join(df_vectrap %>% dplyr::select(trap_code,site) %>% unique())


######################################
## saussan 2022 (EID)
######################################

## trapping method : BG-Sentinel Chaque BG-S a été installé sous la végétation, dans un microenvironnement considéré favorable à la présence d’Aedes albopictus

saussan <- read_excel("/home/ptaconet/contributions_diverses_projets_mivegec/vectrap/entomo_data/eid_Data Saussan/Saussan 22 - data EID - BGS - 03_10_2025_Taconet.xlsx")
saussan$male = as.numeric(saussan$male)
saussan$femelle = as.numeric(saussan$femelle)

saussan = saussan %>%
  dplyr::filter(traitement=='C') %>%
  mutate(total=as.numeric(total)) %>%
  mutate(nb_jours = as.numeric(relevé - depart)) %>%
  mutate(nb_femelles_jour = femelle/nb_jours, nb_males_jour = male/nb_jours, nb_moustiques_jour = total/nb_jours) %>%
  mutate(week = week(relevé)) %>%
  mutate(piege = as.factor(piege)) %>%
  filter(!is.na(total))

saussan <- saussan %>%
  dplyr::select(piege, depart, relevé, nb_jours, week, male, femelle, total, nb_femelles_jour, nb_males_jour, nb_moustiques_jour) %>%
  rename(trap_code = piege, date_installation = depart, date_releve = relevé, exposure_days = nb_jours, nb_femelles = femelle, nb_males = male, nb_tot = total) %>%
  mutate(projet = "saussan", site = "saussan", blocks = NA)

colnames(saussan)

pieges_loc_saussan_sf <- st_read("/home/ptaconet/contributions_diverses_projets_mivegec/vectrap/entomo_data/eid_Data Saussan/BGS 2022.gpkg") %>%
  st_transform(2154) %>%
  dplyr::select(Ident) %>%
  rename(trap_code = Ident, geometry = geom) %>%
  mutate(trap_code= gsub(" ","",trap_code)) %>%
  mutate(trap_code= gsub("BG","",trap_code))%>%
  mutate(projet = "saussan", site = "saussan") %>%
  dplyr::filter(trap_code %in% unique(saussan$trap_code))


######################################
## saussan fabregues pignan 2021 (EID)
######################################

## trapping method : BG (sentinel ou pro ? )

saussan_fabregues_pignan <- read_excel("/home/ptaconet/contributions_diverses_projets_mivegec/vectrap/entomo_data/eid_pigan_fabregues_saussan/DATA EID IRD PAUL TACONNET/autodis21 data BGS pour analyse 211104.xlsx")
colnames(saussan_fabregues_pignan) <- c("week","nb_jours","site","trap_code","male","femelle","indetermine","total","nb_femelles_jours","nb_moustiques_jours","fourmis","remarques")
saussan_fabregues_pignan$nb_femelles_jour = as.numeric(saussan_fabregues_pignan$nb_femelles_jours)

# saussan_fabregues_pignan %>%
#   filter(!is.na(nb_femelles_jours)) %>%
#   filter(trap_code != "BG PI 12") %>%
#   group_by(week,site) %>%
#   summarise(nb_femelles_jours=mean(nb_femelles_jours, na.rm = T)) %>%
#   ggplot(aes(x=week, y = nb_femelles_jours, color = site, group = site)) + geom_line()


saussan_fabregues_pignan = saussan_fabregues_pignan %>%
  filter(!is.na(nb_femelles_jours)) %>%
  filter(trap_code != "BG PI 12") %>%
  mutate(trap_code= gsub(" ","",trap_code)) %>%
  mutate(trap_code= gsub("BG","",trap_code)) %>%
  mutate(nb_jours=as.numeric(nb_jours)) %>%
  dplyr::mutate(week = gsub("S","",week), nb_femelles = as.numeric(femelle), nb_males = as.numeric(male), nb_tot = as.numeric(total), nb_moustiques_jour = as.numeric(nb_moustiques_jours), nb_males_jour= as.numeric(nb_males)/as.numeric(nb_jours)) %>%
  mutate(date_releve = as.Date(paste(2021, week, 1, sep="-"), "%Y-%U-%u"), date_installation = date_releve-as.numeric(nb_jours)) %>%
  dplyr::select(trap_code,site, week, date_installation, date_releve, nb_jours, nb_femelles,nb_males, nb_tot, nb_moustiques_jour, nb_males_jour , nb_femelles_jour) %>%
  rename(exposure_days = nb_jours) %>%
  mutate(projet = "saussan_fabregues_pignan", blocks = NA) %>%
  mutate(site = case_when(site == "FA" ~ "fabregues",
                          site == "PI" ~ "pignan",
                          site == "SA" ~ "saussan")) %>%
  dplyr::filter(!is.na(nb_femelles_jour))

colnames(saussan_fabregues_pignan)

pieges_loc_saussanFabPig_sf <- st_read("/home/ptaconet/contributions_diverses_projets_mivegec/vectrap/entomo_data/eid_pigan_fabregues_saussan/DATA EID IRD PAUL TACONNET/autodis 21 - data pour annelise dec 21/Autodis 2021 - shapefile - Tran - Copie/Pièges BGS.shp") %>%
  rename(trap_code = CODE) %>%
  dplyr::select(trap_code) %>%
  mutate(trap_code= gsub(" ","",trap_code)) %>%
  mutate(trap_code= gsub("BG","",trap_code)) %>%
  mutate(projet = "saussan_fabregues_pignan")%>%
  dplyr::filter(trap_code %in% unique(saussan_fabregues_pignan$trap_code)) %>%
  left_join(saussan_fabregues_pignan %>% dplyr::select(trap_code,site) %>% unique())


##########################################
### Montpellier 2023 (thèse colombine)
##########################################

## trapping method : BG Pro

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
  mutate(DATE_COLLECTE=as.Date(DATE_COLLECTE), DATE_POSE = as.Date(DATE_POSE)) %>%
  mutate(projet="these_colombine") %>%
  mutate(exposure_days = as.numeric(DATE_COLLECTE - DATE_POSE)) %>%
  mutate(NB_ALBO_F=as.numeric(NB_ALBO_F), NB_ALBO_M=as.numeric(NB_ALBO_M), NB_ALBO_TOT=as.numeric(NB_ALBO_TOT)) %>%
  rename(trap_code = ID_PIEGE, date_installation = DATE_POSE, date_releve = DATE_COLLECTE, nb_femelles = NB_ALBO_F,nb_males =NB_ALBO_M , nb_tot = NB_ALBO_TOT) %>%
  mutate(date_releve=as.Date(date_releve), date_installation=as.Date(date_installation), week = week(date_releve)) %>%
  mutate(nb_jours = as.numeric(date_releve - date_installation)) %>%
  mutate(nb_moustiques_jour = nb_tot/nb_jours, nb_males_jour =nb_males/nb_jours, nb_femelles_jour = nb_femelles/nb_jours) %>%
  dplyr::select(trap_code, site,week,  date_installation, date_releve, exposure_days, nb_femelles, nb_males, nb_tot,nb_moustiques_jour, nb_males_jour, nb_femelles_jour, projet ) %>%
  mutate(blocks = NA)


pieges_loc_mtp_sf <- pieges %>%
  rename(trap_code = ID_PIEGE, geometry = geom) %>%
  dplyr::select(trap_code) %>%
  mutate(projet="these_colombine", site = "montpellier")


##########################################
## restalboc 2023 (altopictus)
##########################################

df_restalboc_sf <- read.csv("entomo_data/altopictus_restalboc/datatot2.csv", sep = ";",dec = ",") %>%
  dplyr::filter(capture_type =="HLC") %>%
  dplyr::select(site_identity, X, Y) %>%
  unique() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4236) %>%
  st_transform(2154) %>%
  mutate(projet="murviel_2023", site = "murviel") %>%
  rename(trap_code = site_identity) %>%
  dplyr::select(trap_code, projet, geometry, site)


df_restalboc <- read.csv("entomo_data/altopictus_restalboc/datatot2.csv", sep = ";",dec = ",") %>%
  dplyr::filter(capture_type =="HLC") %>%
  dplyr::select(site_identity, date_releve,total_mosquito,number_female,number_male) %>%
  mutate(date_releve = parse_date(date_releve,"%d/%m/%Y")) %>%
  mutate(projet="murviel_2023") %>%
  rename(trap_code = site_identity, nb_tot = total_mosquito, nb_femelles = number_female, nb_males = number_male) %>%
  mutate(blocks = NA, site = "murviel", week = week(date_releve), date_installation = date_releve, exposure_days = 0.01, nb_moustiques_jour = nb_tot, nb_males_jour = nb_males, nb_femelles_jour = nb_femelles)



##########################################
## bind all
##########################################

data_mosquitoes <- rbind(df_vectrap, saussan, saussan_fabregues_pignan, df_montpellier, df_restalboc)
trap_coordinates <- rbind(pieges_loc_vectrap_sf, pieges_loc_saussan_sf, pieges_loc_saussanFabPig_sf, pieges_loc_mtp_sf, df_restalboc_sf)

write.csv(data_mosquitoes,"entomo_data/fusion_data/data_mosquitoes.csv")
st_write(trap_coordinates,"entomo_data/fusion_data/trap_coordinates.gpkg", overwrite = T,append=FALSE )
