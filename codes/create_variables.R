library(tidyverse)
library(sf)
library(terra)
library(readxl)
library(raster)
library(fasterize)
library(landscapemetrics)
library(exactextractr)
library(GLCMTextures)

## read the trap data

# trap_coordinates <- read_xlsx("entomo_data/VECTRAP_2021-2023 monitoring results_pour paul.xlsx", sheet = "Notice_coord geo")
# colnames(trap_coordinates) <- c("trap_code","Longitude","Latitude")
# trap_coordinates <- trap_coordinates %>%
#   st_as_sf(.,coords = c("Longitude","Latitude"), crs = 2154)

trap_coordinates <- st_read("entomo_data/fusion_data/trap_coordinates.gpkg")
trap_coordinates <- trap_coordinates %>% filter(projet %in% c("vectrap","saussan","saussan_fabregues_pignan"))

limites_villages <- st_read("entomo_data/limites_villages.gpkg")

## read the environmental data

couvert_arbore <- st_read("data/NATURE-EN-VILLE_1-0__GPKG_WGS84_FRA_2021-01-01/mmm_natureenville_couvertarbore.gpkg")
vegetation_basse <-  st_read("data/NATURE-EN-VILLE_1-0__GPKG_WGS84_FRA_2021-01-01/mmm_natureenville_vegetationbasse.gpkg")
vegetation <- rbind(couvert_arbore,vegetation_basse)
bati <- st_read("data/BDPARCELLAIRE_1-2_VECTEUR_SHP_LAMB93_D034_2017-03-03/BDPARCELLAIRE/1_DONNEES_LIVRAISON_2017-06-00185/BDPV_1-2_SHP_LAMB93_D034/BATIMENT.SHP")
roads <- st_read("data/routes_osm.gpkg")
parcels <- st_read("data/BDPARCELLAIRE_1-2_VECTEUR_SHP_LAMB93_D034_2017-03-03/BDPARCELLAIRE/1_DONNEES_LIVRAISON_2017-06-00185/BDPV_1-2_SHP_LAMB93_D034/PARCELLE.SHP")

mnh_vegetation <- rast("data/lidar_hd/merged_lidar_mosaic_vegetation.tif")
mnh_bati <- rast("data/lidar_hd/merged_lidar_mosaic_bati.tif")

pcrs_plaques <- st_read("data/MMM_MMM_PCRS_SHP/merged_pcrs_plaques.gpkg") %>% dplyr::select(-reseau)
pcrs_avaloirs <- st_read("data/MMM_MMM_PCRS_SHP/merged_pcrs_avaloirs.gpkg")
pcrs_plaque_avaloir <- rbind(pcrs_plaques,pcrs_avaloirs) %>% st_transform(2154)
filosofi_insee <-  st_read("data/Filosofi2019_carreaux_200m_gpkg/carreaux_200m_met.gpkg")
popfine <-  st_read("/home/ptaconet/contributions_diverses_projets_mivegec/modeling_vector_mtp/data/MMM_MMM_PopFine/MMM_MMM_PopFine.shp") ## Donnes de densite de population

tif_files_ortho <- list.files("data/ortho_montpellier/RGB", pattern = "\\.tif$", full.names = TRUE)
rasters_ortho <- lapply(tif_files_ortho, rast)


# locate parcels where traps are located
parcels$n <- seq(1,nrow(parcels))
parcels_intersect_traps <- st_intersects(trap_coordinates,parcels,prepared = TRUE)
parcels_intersect_traps <- parcels[which(parcels$n %in% as.numeric(parcels_intersect_traps)),]
a <- st_intersection(parcels_intersect_traps,trap_coordinates) %>%
  dplyr::select(n,trap_code) %>%
  st_drop_geometry()
parcels_intersect_traps <- left_join(parcels_intersect_traps,a)

## define buffers
buffer_sizes <- c(5,10,20,50,100)
buffer_sizes_textures <- c(5,10,20,50,100)


## loop for land cover and textures

# df_lsm_landcover <- data.frame()
# df_texture <- data.frame()


###################################
##### A l'échelle de la parcelle
###################################

for(i in 1:nrow(parcels_intersect_traps)){

  cat("dealing with parcel n°",i,"over",nrow(parcels_intersect_traps),"\n")
  th_roi <- parcels_intersect_traps[i,] %>% st_buffer(1, endCapStyle = "SQUARE")

  #### detailed vegetation  ####
  # rasterize land cover layers
  th_vegetation<- st_intersection(vegetation,th_roi) %>% dplyr::select(numero)
  th_bati <- st_intersection(bati,th_roi) %>% mutate(numero = 1) %>% dplyr::select(numero) %>% rename(geom = geometry)
  th_roads <- st_intersection(roads,th_roi) %>% mutate(numero = 2) %>% dplyr::select(numero) %>% st_buffer(2.5)

  landcover <- rbind(th_vegetation,th_bati,th_roads)
  #landcover <- landcover %>% st_cast("MULTIPOLYGON")

  r <- raster(landcover, res = 0.2, crs = crs(landcover)) ## conversion to a raster
  landcover_rast <- fasterize(landcover, r, field="numero", fun = "max")  ## creates a raster with 0 for empty cells (background)

  # fill na values with 3 (other built-up elements)
  landcover_rast[is.na(landcover_rast)] <- 3
  landcover_rast <- mask(landcover_rast,th_roi)

  landcover_data_dic <- data.frame(class = c(1,2,3,8,9,10,11,12,17),
                                   class_label = c("Bati","Routes","Autres zones anthropisées","Conifère","Feuillu","Broussaille","Vigne","Pelouse","Prairie"))

  th_df_lsm_landcover <- landscapemetrics::calculate_lsm(landscape = landcover_rast,
                                                         what = c("lsm_c_ca","lsm_c_area_mn",  "lsm_c_ed", "lsm_c_te","lsm_c_frac_mn", "lsm_c_np","lsm_c_pland" , "lsm_l_shdi", "lsm_c_clumpy", "lsm_c_cohesion", "lsm_l_cohesion", "lsm_l_contag"))

  metrics_defs <- landscapemetrics::list_lsm() # list of landscape metrics

  th_df_lsm_landcover1 <- th_df_lsm_landcover %>%
    dplyr::mutate(value=ifelse(metric %in% c("area_mn","ca","ed","np","pland","te") & is.na(value), 0, value)) %>%
    mutate(value = ifelse(metric=='ca', value*10000,value)) %>% # conversion ha en m2
    dplyr::left_join(landcover_data_dic) %>%
    dplyr::select(level,class,class_label,metric,value) %>%
    rename(val=value,pixval=class) %>%
    mutate(trap_code=th_roi$trap_code) %>%
    mutate(layer_id = "LCPNG") %>%
    dplyr::select(trap_code,pixval,level,metric,val,layer_id) %>%
    left_join(metrics_defs) %>%
    dplyr::select(-c(level,metric,name,type)) %>%
    pivot_wider(names_from = c(function_name,layer_id,pixval), values_from = val, names_sep = "_") %>%
    mutate_at(vars(contains(c('lsm_c_area','lsm_c_pland','lsm_c_ca'))),funs(replace_na(.,0))) %>%
    mutate(trap_code = as.character(trap_code))


  #### grouped vegetation  ####

  landcover_rast <- classify(rast(landcover_rast), cbind(c(8,9,10,11,12,17), 4))

  landcover_data_dic <- data.frame(class = c(1,2,3,4),
                                   class_label = c("Bati","Routes","Autres zones anthropisée","Végétation"))


  th_df_lsm_landcover <- landscapemetrics::calculate_lsm(landscape = landcover_rast,
                                                         what = c("lsm_c_ca","lsm_c_area_mn",  "lsm_c_ed", "lsm_c_te","lsm_c_frac_mn", "lsm_c_np","lsm_c_pland" , "lsm_l_shdi", "lsm_c_clumpy", "lsm_c_cohesion", "lsm_l_cohesion", "lsm_l_contag"))

  metrics_defs <- landscapemetrics::list_lsm() # list of landscape metrics


  th_df_lsm_landcover2 <- th_df_lsm_landcover %>%
    filter(class == 4 | level == "landscape") %>%
    dplyr::mutate(value=ifelse(metric %in% c("area_mn","ca","ed","np","pland","te") & is.na(value), 0, value)) %>%
    mutate(value = ifelse(metric=='ca', value*10000,value)) %>%# conversion ha en m2
    dplyr::left_join(landcover_data_dic) %>%
    dplyr::select(level,class,class_label,metric,value) %>%
    rename(val=value,pixval=class) %>%
    mutate(trap_code=th_roi$trap_code) %>%
    mutate(layer_id = "LCPG") %>%
    dplyr::select(trap_code,pixval,level,metric,val,layer_id) %>%
    left_join(metrics_defs) %>%
    dplyr::select(-c(level,metric,name,type)) %>%
    pivot_wider(names_from = c(function_name,layer_id,pixval), values_from = val, names_sep = "_") %>%
    mutate_at(vars(contains(c('lsm_c_area','lsm_c_pland','lsm_c_ca'))),funs(replace_na(.,0))) %>%
    mutate(trap_code = as.character(trap_code))


  th_df_lsm_landcover <- left_join(th_df_lsm_landcover1,th_df_lsm_landcover2)

  write.csv(th_df_lsm_landcover,paste0("data/lsm_results/parcel_",th_roi$trap_code,".csv"), row.names = F)

}

df_lsm_landcover_parcel <- list.files(file.path("data","lsm_results"), full.names = T, pattern = "parcel") %>%
  purrr::map_dfr(.,~read.csv(.))


###################################
##### A l'échelle du village
###################################

for(i in 1:nrow(limites_villages)){

  cat("dealing with village n°",i,"over",nrow(limites_villages),"\n")
  th_roi <- limites_villages[i,]

  #### detailed vegetation  ####
  # rasterize land cover layers
  th_vegetation<- st_intersection(vegetation,th_roi) %>% dplyr::select(numero)
  th_bati <- st_intersection(bati,th_roi) %>% mutate(numero = 1) %>% dplyr::select(numero) %>% rename(geom = geometry)
  th_roads <- st_intersection(roads,th_roi) %>% mutate(numero = 2) %>% dplyr::select(numero) %>% st_buffer(2.5)

  landcover <- rbind(th_vegetation,th_bati,th_roads)

  r <- raster(landcover, res = 0.5, crs = crs(landcover)) ## conversion to a raster
  landcover_rast <- fasterize(landcover, r, field="numero", fun = "max")  ## creates a raster with 0 for empty cells (background)

  # fill na values with 3 (other built-up elements)
  landcover_rast[is.na(landcover_rast)] <- 3
  landcover_rast <- mask(landcover_rast,th_roi)

  landcover_data_dic <- data.frame(class = c(1,2,3,8,9,10,11,12,17),
                                   class_label = c("Bati","Routes","Autres zones anthropisées","Conifère","Feuillu","Broussaille","Vigne","Pelouse","Prairie"))

  th_df_lsm_landcover <- landscapemetrics::calculate_lsm(landscape = landcover_rast,
                                                         what = c("lsm_c_ca","lsm_c_area_mn",  "lsm_c_ed", "lsm_c_te","lsm_c_frac_mn", "lsm_c_np","lsm_c_pland" , "lsm_l_shdi", "lsm_c_clumpy", "lsm_c_cohesion", "lsm_l_cohesion", "lsm_l_contag"))

  metrics_defs <- landscapemetrics::list_lsm() # list of landscape metrics

  th_df_lsm_landcover1 <- th_df_lsm_landcover %>%
    dplyr::mutate(value=ifelse(metric %in% c("area_mn","ca","ed","np","pland","te") & is.na(value), 0, value)) %>%
    dplyr::left_join(landcover_data_dic) %>%
    dplyr::select(level,class,class_label,metric,value) %>%
    rename(val=value,pixval=class) %>%
    mutate(site=th_roi$site) %>%
    mutate(layer_id = "LCVNG") %>%
    dplyr::select(site,pixval,level,metric,val,layer_id) %>%
    left_join(metrics_defs) %>%
    dplyr::select(-c(level,metric,name,type)) %>%
    pivot_wider(names_from = c(function_name,layer_id,pixval), values_from = val, names_sep = "_") %>%
    mutate_at(vars(contains(c('lsm_c_area','lsm_c_pland','lsm_c_ca'))),funs(replace_na(.,0))) %>%
    mutate(site = as.character(site))


  #### grouped vegetation  ####

  landcover_rast <- classify(rast(landcover_rast), cbind(c(8,9,10,11,12,17), 4))

  landcover_data_dic <- data.frame(class = c(1,2,3,4),
                                   class_label = c("Bati","Routes","Autres zones anthropisée","Végétation"))


  th_df_lsm_landcover <- landscapemetrics::calculate_lsm(landscape = landcover_rast,
                                          what = c("lsm_c_ca","lsm_c_area_mn",  "lsm_c_ed", "lsm_c_te","lsm_c_frac_mn", "lsm_c_np","lsm_c_pland" , "lsm_l_shdi", "lsm_c_clumpy", "lsm_c_cohesion", "lsm_l_cohesion", "lsm_l_contag"))

  metrics_defs <- landscapemetrics::list_lsm() # list of landscape metrics


  th_df_lsm_landcover2 <- th_df_lsm_landcover %>%
    filter(class == 4 | level == "landscape") %>%
    dplyr::mutate(value=ifelse(metric %in% c("area_mn","ca","ed","np","pland","te") & is.na(value), 0, value)) %>%
    dplyr::left_join(landcover_data_dic) %>%
    dplyr::select(level,class,class_label,metric,value) %>%
    rename(val=value,pixval=class) %>%
    mutate(site=th_roi$site) %>%
    mutate(layer_id = "LCVG") %>%
    dplyr::select(site,pixval,level,metric,val,layer_id) %>%
    left_join(metrics_defs) %>%
    dplyr::select(-c(level,metric,name,type)) %>%
    pivot_wider(names_from = c(function_name,layer_id,pixval), values_from = val, names_sep = "_") %>%
    mutate_at(vars(contains(c('lsm_c_area','lsm_c_pland','lsm_c_ca'))),funs(replace_na(.,0))) %>%
    mutate(site = as.character(site))


  th_df_lsm_landcover <- left_join(th_df_lsm_landcover1,th_df_lsm_landcover2)

  write.csv(th_df_lsm_landcover,paste0("data/lsm_results/site_",th_roi$site,".csv"), row.names = F)

}

df_lsm_landcover <- list.files(file.path("data","lsm_results"), full.names = T, pattern = "site") %>%
  purrr::map_dfr(.,~read.csv(.))


#### Socio economics data
df_filosofi <- sf::st_intersection(limites_villages,filosofi_insee) %>%
  st_drop_geometry() %>%
  group_by(site) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  dplyr::select(site, ind, men, men_pauv, ind_snv, log_av45, log_45_70, log_70_90, log_ap90, log_soc)

df_filosofi = df_filosofi %>%
  mutate(site = as.character(site))
colnames(df_filosofi) <- paste0('FIL_VILLAGE_',colnames(df_filosofi))
colnames(df_filosofi)[1] <- 'site'


#### Plaques et avaloirs
plaques_avaloirs <- sf::st_intersection(limites_villages,pcrs_plaque_avaloir) %>%
  st_drop_geometry() %>%
  group_by(site,type_objet) %>%
  summarise(n_plaques = n(), .groups = "drop") %>%
  complete(
    site = as.character(site),
    type_objet = unique(pcrs_plaque_avaloir$type_objet),
    fill = list(n_plaques = 0)
  )

plaques_avaloirs <- plaques_avaloirs %>%
  pivot_wider(names_from = c(type_objet), values_from = n_plaques, names_glue = "PLAQUES_{type_objet}") %>%
  mutate(site = as.character(site))




### bind all results

df_model_village <- df_lsm_landcover %>%
  left_join(df_filosofi) %>%
  left_join(plaques_avaloirs)

write.csv(df_model,"data/df_model_village.csv")





###################################
##### A l'échelle du piège
###################################

for(i in 1:nrow(trap_coordinates)){

  cat("dealing with trap n°",i,"over",nrow(trap_coordinates),"\n")
  th_roi <- st_buffer(trap_coordinates[i,],200, endCapStyle = "SQUARE")


  #### detailed vegetation  ####
  # rasterize land cover layers
  th_vegetation<- st_intersection(vegetation,th_roi) %>% dplyr::select(numero)
  th_bati <- st_intersection(bati,th_roi) %>% mutate(numero = 1) %>% dplyr::select(numero) %>% rename(geom = geometry)
  th_roads <- st_intersection(roads,th_roi) %>% mutate(numero = 2) %>% dplyr::select(numero) %>% st_buffer(2.5)

  landcover <- rbind(th_vegetation,th_bati,th_roads)

  r <- raster(landcover, res = 0.5, crs = crs(landcover)) ## conversion to a raster
  landcover_rast <- fasterize(landcover, r, field="numero", fun = "max")  ## creates a raster with 0 for empty cells (background)

  # fill na values with 3 (other built-up elements)
  landcover_rast[is.na(landcover_rast)] <- 3

  landcover_data_dic <- data.frame(class = c(1,2,3,8,9,10,11,12,17),
                                   class_label = c("Bati","Routes","Autres zones anthropisées","Conifère","Feuillu","Broussaille","Vigne","Pelouse","Prairie"))


  th_df_lsm_landcover <- buffer_sizes %>%
    set_names(buffer_sizes) %>%
    map_dfr(~landscapemetrics::sample_lsm(landscape = landcover_rast,
                                          y =  trap_coordinates[i,],
                                          plot_id = trap_coordinates[i,]$trap_code,
                                          what = c("lsm_c_ca","lsm_c_area_mn",  "lsm_c_ed", "lsm_c_te","lsm_c_frac_mn", "lsm_c_np","lsm_c_pland" , "lsm_l_shdi", "lsm_c_clumpy", "lsm_c_cohesion", "lsm_l_cohesion", "lsm_l_contag"),
                                          shape = "circle",
                                          size = .,
                                          all_classes = T),
            .id = "buffer")

  metrics_defs <- landscapemetrics::list_lsm() # list of landscape metrics

  th_df_lsm_landcover1 <- th_df_lsm_landcover %>%
    dplyr::mutate(value=ifelse(metric %in% c("area_mn","ca","ed","np","pland","te") & is.na(value), 0, value)) %>%
    dplyr::left_join(landcover_data_dic) %>%
    dplyr::select(buffer,level,class,class_label,metric,value,plot_id) %>%
    rename(val=value,pixval=class) %>%
    mutate(trap_code=plot_id) %>%
    mutate(buffer=as.numeric(buffer)) %>%
    mutate(layer_id = "LCTNG") %>%
    dplyr::select(trap_code,buffer,pixval,level,metric,val,layer_id) %>%
    left_join(metrics_defs) %>%
    dplyr::select(-c(level,metric,name,type)) %>%
    pivot_wider(names_from = c(function_name,layer_id,buffer,pixval), values_from = val, names_sep = "_") %>%
    mutate_at(vars(contains(c('lsm_c_area','lsm_c_pland','lsm_c_ca'))),funs(replace_na(.,0))) %>%
    mutate(trap_code = as.character(trap_code))


  #### grouped vegetation  ####

  landcover_rast <- classify(rast(landcover_rast), cbind(c(8,9,10,11,12,17), 4))

  landcover_data_dic <- data.frame(class = c(1,2,3,4),
                                   class_label = c("Bati","Routes","Autres zones anthropisée","Végétation"))


  th_df_lsm_landcover <- buffer_sizes %>%
    set_names(buffer_sizes) %>%
    map_dfr(~landscapemetrics::sample_lsm(landscape = landcover_rast,
                                          y =  trap_coordinates[i,],
                                          plot_id = trap_coordinates[i,]$trap_code,
                                          what = c("lsm_c_ca","lsm_c_area_mn",  "lsm_c_ed", "lsm_c_te","lsm_c_frac_mn", "lsm_c_np","lsm_c_pland" , "lsm_l_shdi", "lsm_c_clumpy", "lsm_c_cohesion", "lsm_l_cohesion", "lsm_l_contag"),
                                          shape = "circle",
                                          size = .,
                                          all_classes = T),
            .id = "buffer")

  metrics_defs <- landscapemetrics::list_lsm() # list of landscape metrics

  th_df_lsm_landcover2 <- th_df_lsm_landcover %>%
    filter(class == 4 | level == "landscape") %>%
    dplyr::mutate(value=ifelse(metric %in% c("area_mn","ca","ed","np","pland","te") & is.na(value), 0, value)) %>%
    dplyr::left_join(landcover_data_dic) %>%
    dplyr::select(buffer,level,class,class_label,metric,value,plot_id) %>%
    rename(val=value,pixval=class) %>%
    mutate(trap_code=plot_id) %>%
    mutate(buffer=as.numeric(buffer)) %>%
    mutate(layer_id = "LCTG") %>%
    dplyr::select(trap_code,buffer,pixval,level,metric,val,layer_id) %>%
    left_join(metrics_defs) %>%
    dplyr::select(-c(level,metric,name,type)) %>%
    pivot_wider(names_from = c(function_name,layer_id,buffer,pixval), values_from = val, names_sep = "_") %>%
    mutate_at(vars(contains(c('lsm_c_area','lsm_c_pland','lsm_c_ca'))),funs(replace_na(.,0))) %>%
    mutate(trap_code = as.character(trap_code))

  th_df_lsm_landcover <- left_join(th_df_lsm_landcover1,th_df_lsm_landcover2)

  write.csv(th_df_lsm_landcover,paste0("data/lsm_results/trap_",trap_coordinates[i,]$trap_code,".csv"), row.names = F)




  ##### textures ########

  # open good piece of orthophoto
  x = as.numeric(st_coordinates(trap_coordinates[i,])[,1])
  y = as.numeric(st_coordinates(trap_coordinates[i,])[,2])

  inside_any <- sapply(rasters_ortho, function(r)
    x >= xmin(r) & x <= xmax(r) &
      y >= ymin(r) & y <= ymax(r)
  )

  if(any(inside_any)){
  th_rast_ortho <- rasters_ortho[inside_any][[1]]

  th_roi_small <- st_buffer(trap_coordinates[i,],50, endCapStyle = "SQUARE")
  th_rast_ortho <- crop(th_rast_ortho,th_roi_small)

  lum <- 0.299 * th_rast_ortho[[1]] + 0.587 * th_rast_ortho[[2]] + 0.114 * th_rast_ortho[[3]]

  # Define moving window: 11x11 pixels (≈0.55 m at 5 cm GSD)

  # Compute GLCM features
  textures <- glcm_textures(lum, w = c(11,11), n_levels = 8, quant_method = "prob")

  th_df_texture_buffer <- buffer_sizes_textures %>%
    set_names(buffer_sizes_textures) %>%
    map_dfr(~exact_extract(textures,
                           st_buffer(trap_coordinates[i,],.),
                           fun=c("mean"),
                           append_cols = "trap_code"),
            .id = "buffer")

  th_df_texture_buffer <- th_df_texture_buffer %>%
    mutate(buffer = as.numeric(buffer)) %>%
    pivot_wider(names_from = buffer, values_from = -c(buffer,trap_code)) %>%
    mutate(trap_code = as.character(trap_code))

  th_df_texture_parcel <- exact_extract(textures, parcels_intersect_traps[which(parcels_intersect_traps$trap_code==trap_coordinates$trap_code[i]),], fun=c("mean"), append_cols = "trap_code")

  colnames(th_df_texture_parcel)[2:ncol(th_df_texture_parcel)] <- paste0("textureparcel_",colnames(th_df_texture_parcel)[2:ncol(th_df_texture_parcel)])

  write.csv(th_df_texture_buffer,paste0("data/textures_results/",trap_coordinates[i,]$trap_code,"_buffer.csv"), row.names = F)
  write.csv(th_df_texture_parcel,paste0("data/textures_results/",trap_coordinates[i,]$trap_code,"_parcel.csv"), row.names = F)

  }
  #df_texture <- rbind(df_texture,th_df_texture)

}


df_lsm_landcover <- list.files(file.path("data","lsm_results"), full.names = T, pattern = "trap") %>%
  purrr::map_dfr(.,~read.csv(.))

df_texture_parcel <- list.files(file.path("data","textures_results"), full.names = T, pattern = "parcel") %>%
  purrr::map_dfr(.,~read.csv(.))

df_texture_buffer <- list.files(file.path("data","textures_results"), full.names = T, pattern = "buffer") %>%
  purrr::map_dfr(.,~read.csv(.))


## hauteur du bati et vegetation dans la parcelle
veg_hauteur_parcels <- exact_extract(mnh_vegetation,parcels_intersect_traps,fun=c("median","mean","stdev","coefficient_of_variation"),append_cols = "trap_code")
bati_hauteur_parcels <- exact_extract(mnh_bati,parcels_intersect_traps,fun=c("median","mean","stdev","coefficient_of_variation"),append_cols = "trap_code")

colnames(veg_hauteur_parcels)[2:ncol(veg_hauteur_parcels)] <- paste0("vegetationheightparcel_",colnames(veg_hauteur_parcels)[2:ncol(veg_hauteur_parcels)])
colnames(bati_hauteur_parcels)[2:ncol(bati_hauteur_parcels)] <- paste0("batiheightparcel_",colnames(bati_hauteur_parcels)[2:ncol(bati_hauteur_parcels)])


veg_hauteur_buffer <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  map_dfr(~exact_extract(mnh_vegetation,
                         st_buffer(trap_coordinates,.),
                         fun=c("median","mean","stdev","coefficient_of_variation"),
                         append_cols = "trap_code"),
          .id = "buffer")
colnames(veg_hauteur_buffer)[3:ncol(veg_hauteur_buffer)] <- paste0("vegetationheightbuffer_",colnames(veg_hauteur_buffer)[3:ncol(veg_hauteur_buffer)])

veg_hauteur_buffer <- veg_hauteur_buffer %>%
  pivot_wider(names_from = buffer, values_from = -c(buffer,trap_code)) %>%
  mutate(trap_code = as.character(trap_code))


bati_hauteur_buffer <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  map_dfr(~exact_extract(mnh_bati,
                         st_buffer(trap_coordinates,.),
                         fun=c("median","mean","stdev","coefficient_of_variation"),
                         append_cols = "trap_code"),
          .id = "buffer")
colnames(bati_hauteur_buffer)[3:ncol(bati_hauteur_buffer)] <- paste0("batiheightbuffer_",colnames(bati_hauteur_buffer)[3:ncol(bati_hauteur_buffer)])

bati_hauteur_buffer <- bati_hauteur_buffer %>%
  pivot_wider(names_from = buffer, values_from = -c(buffer,trap_code)) %>%
  mutate(trap_code = as.character(trap_code))



#### Socio economics data
df_filosofi <- sf::st_intersection(trap_coordinates,filosofi_insee) %>%
  st_drop_geometry() %>%
  dplyr::select(trap_code, men, men_pauv, ind_snv, log_av45, log_45_70, log_70_90, log_ap90, log_soc)

df_filosofi = df_filosofi %>%
  mutate(trap_code = as.character(trap_code))
colnames(df_filosofi) <- paste0('FIL_',colnames(df_filosofi))
colnames(df_filosofi)[1] <- 'trap_code'


#### Population density data
POP <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  purrr::map_dfr(~sf::st_intersection(st_buffer(trap_coordinates,.), popfine),
                 .id = "buffer") %>%
  st_drop_geometry() %>%
  group_by(buffer,trap_code) %>%
  summarise(pop = sum(pop_2016), .groups = "drop") %>%
  complete(
    buffer = as.character(buffer_sizes),
    trap_code = unique(trap_coordinates$trap_code),
    fill = list(pop = 0)
  ) %>%
  as_tibble()

POP <- POP %>%
  mutate(buffer = as.numeric(buffer)) %>%
  pivot_wider(names_from = buffer, values_from = -c(buffer,trap_code), names_glue = "POP_{buffer}_{.value}") %>%#, values_fill = list(val = 0))
  mutate(trap_code = as.character(trap_code))


#### Plaques et avaloirs
plaques_avaloirs <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  purrr::map_dfr(~sf::st_intersection(st_buffer(trap_coordinates,.), pcrs_plaque_avaloir),
                 .id = "buffer") %>%
  st_drop_geometry() %>%
  group_by(buffer,trap_code,type_objet) %>%
  summarise(n_plaques = n(), .groups = "drop") %>%
  complete(
    buffer = as.character(buffer_sizes),
    trap_code = unique(trap_coordinates$trap_code),
    type_objet = unique(pcrs_plaque_avaloir$type_objet),
    fill = list(n_plaques = 0)
  )

plaques_avaloirs <- plaques_avaloirs %>%
  mutate(buffer = as.numeric(buffer)) %>%
  pivot_wider(names_from = c(buffer,type_objet), values_from = n_plaques, names_glue = "PLAQUES_{buffer}_{type_objet}") %>%
  mutate(trap_code = as.character(trap_code))




df_lsm_landcover_parcel <- list.files(file.path("data","lsm_results"), full.names = T, pattern = "parcel") %>%
  purrr::map_dfr(.,~read.csv(.))

df_model_village <- read.csv("data/df_model_village.csv")
df_model_village <- trap_coordinates %>%
  dplyr::select(trap_code, site) %>%
  st_drop_geometry() %>%
  left_join(df_model_village)

### bind all results

df_model <- df_lsm_landcover %>%
  left_join(df_texture_parcel) %>%
  left_join(df_texture_buffer) %>%
  left_join(veg_hauteur_parcels) %>%
  left_join(bati_hauteur_parcels) %>%
  left_join(df_lsm_landcover_parcel) %>%
  left_join(veg_hauteur_buffer) %>%
  left_join(bati_hauteur_buffer) %>%
  left_join(df_filosofi) %>%
  left_join(POP) %>%
  left_join(plaques_avaloirs) %>%
  left_join(df_model_village)

write.csv(df_model,"data/df_model.csv")
