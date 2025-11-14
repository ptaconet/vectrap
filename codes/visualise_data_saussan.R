library(tidyverse)
library(sf)
library(terra)
library(readxl)

saussan <- read_excel("/home/ptaconet/contributions_diverses_projets_mivegec/vectrap/entomo_data/eid_Data Saussan/Saussan 22 - data EID - BGS - 03_10_2025_Taconet.xlsx")
saussan$male = as.numeric(saussan$male)
saussan$femelle = as.numeric(saussan$femelle)


saussan2 = saussan %>%
  dplyr::filter(traitement=='C'|periode=="avant") %>%
  mutate(week = week(relevé)) %>%
  group_by(piege, week) %>%
  summarise(number_female=mean(femelle, na.rm = T))


ggplot(saussan2,aes(x = reorder(piege,number_female,na.rm = TRUE), y = number_female)) +
  geom_boxplot() +
  ggtitle("abundance per trap") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


