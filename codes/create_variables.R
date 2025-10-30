library(tidyverse)
library(sf)
library(terra)
library(readxl)


trap_coordinates <- read_xlsx("entomo_data/VECTRAP_2021-2023 monitoring results_pour paul.xlsx", sheet = "Notice_coord geo")
colnames(trap_coordinates) <- c("trap_code","Longitude","Latitude")
trap_coordinates <- trap_coordinates %>%
  st_as_sf(.,coords = c("Longitude","Latitude"), crs = 2154)



vegetation_basse <- st_read("data/NATURE-EN-VILLE_1-0__GPKG_WGS84_FRA_2021-01-01/034_Communes_COSIA_light.gpkg", layer = "vegetation_basse_permanente")
couvert_arbore <-  st_read("data/NATURE-EN-VILLE_1-0__GPKG_WGS84_FRA_2021-01-01/034_Communes_COSIA_light.gpkg", layer = "couvert_arbore")
filosofi_insee <-  st_read("data/Filosofi2019_carreaux_200m_gpkg/carreaux_200m_met.gpkg")
popfine <-  st_read("/home/ptaconet/contributions_diverses_projets_mivegec/modeling_vector_mtp/data/MMM_MMM_PopFine/MMM_MMM_PopFine.shp") ## Donnes de densite de population
mnh <- rast("data/lidar_hd/merged_lidar_mosaic.tif")
bdnb <- st_read("/home/ptaconet/contributions_diverses_projets_mivegec/vectrap/data/bdnb_open_data_millesime_2024-10-a_dep34_gpkg/bdnb_montpellier.gpkg")
urbanatlas <- st_read("/home/ptaconet/contributions_diverses_projets_mivegec/modeling_vector_mtp/data/FR010L2_MONTPELLIER_UA2018_v013/Data/FR010L2_MONTPELLIER_UA2018_v013.gpkg")  %>%
  filter(class_2018 %in% c("Other roads and associated land","Fast transit roads and associated land")) # for the roads
pcrs_plaques <- st_read("data/MMM_MMM_PCRS_SHP/merged_pcrs_plaques.gpkg")
pcrs_avaloirs <- st_read("data/MMM_MMM_PCRS_SHP/merged_pcrs_avaloirs.gpkg")




roi <- st_buffer(trap_coordinates[1,],200)
bbox_roi <- st_bbox(roi)
bbox_roi_4326 <- st_transform(bbox_roi,4326)
## données récupérées à la volee

# batiments (osm)


q <- opq(bbox = bbox_roi_4326)
bbox_matrix <- matrix(c(bb["xmin"], bb["xmax"], bb["ymin"], bb["ymax"]),
                      nrow = 2, byrow = FALSE,
                      dimnames = list(c("x", "y"), c("min", "max")))

q1 <- add_osm_feature(q,key = 'building')
batiments <- osmdata_sf(q1) %>% trim_osmdata(bbox_matrix)

batiments <- batiments$osm_polygons

# lidar



# PCRS Montpellier


## ## ##
## MNS from lidar
## ## ##

r <- rast("/home/ptaconet/contributions_diverses_projets_mivegec/vectrap/data/lidar hd/LHD_FXX_0771_6285_MNH_O_0M50_LAMB93_IGN69.tif")

crop(r,vegetation_basse)
