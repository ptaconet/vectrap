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

trap_coordinates <- read_xlsx("entomo_data/VECTRAP_2021-2023 monitoring results_pour paul.xlsx", sheet = "Notice_coord geo")
colnames(trap_coordinates) <- c("trap_code","Longitude","Latitude")
trap_coordinates <- trap_coordinates %>%
  st_as_sf(.,coords = c("Longitude","Latitude"), crs = 2154)


## read the environmental data

couvert_arbore <- st_read("data/NATURE-EN-VILLE_1-0__GPKG_WGS84_FRA_2021-01-01/mmm_natureenville_couvertarbore.gpkg")
vegetation_basse <-  st_read("data/NATURE-EN-VILLE_1-0__GPKG_WGS84_FRA_2021-01-01/mmm_natureenville_vegetationbasse.gpkg")
vegetation <- rbind(couvert_arbore,vegetation_basse)
bati <- st_read("/home/ptaconet/contributions_diverses_projets_mivegec/vectrap/data/bdnb_open_data_millesime_2024-10-a_dep34_gpkg/bdnb_montpellier.gpkg")
roads <- st_read("data/routes_osm.gpkg")

mnh_vegetation <- rast("data/lidar_hd/merged_lidar_mosaic_vegetation.tif")
mnh_bati <- rast("data/lidar_hd/merged_lidar_mosaic_bati.tif")

pcrs_plaques <- st_read("data/MMM_MMM_PCRS_SHP/merged_pcrs_plaques.gpkg") %>% dplyr::select(-reseau)
pcrs_avaloirs <- st_read("data/MMM_MMM_PCRS_SHP/merged_pcrs_avaloirs.gpkg")
pcrs_plaque_avaloir <- rbind(pcrs_plaques,pcrs_avaloirs) %>% st_transform(2154)
filosofi_insee <-  st_read("data/Filosofi2019_carreaux_200m_gpkg/carreaux_200m_met.gpkg")
popfine <-  st_read("/home/ptaconet/contributions_diverses_projets_mivegec/modeling_vector_mtp/data/MMM_MMM_PopFine/MMM_MMM_PopFine.shp") ## Donnes de densite de population

tif_files_ortho <- list.files("data/ortho_montpellier/RGB", pattern = "\\.tif$", full.names = TRUE)
rasters_ortho <- lapply(tif_files_ortho, rast)


## define buffers
buffer_sizes <- c(5,10,20,50,100,250)
buffer_sizes_textures <- c(5,10,20,50)



veg_hauteur <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  map_dfr(~exact_extract(mnh_vegetation,
                         st_buffer(trap_coordinates,.),
                         fun=c("max","mean","stdev","coefficient_of_variation"),
                         append_cols = "trap_code"),
          .id = "buffer")


bati_hauteur <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  map_dfr(~exact_extract(mnh_bati,
                         st_buffer(trap_coordinates,.),
                         fun=c("max","mean","stdev","coefficient_of_variation"),
                         append_cols = "trap_code"),
          .id = "buffer")




#### Socio economics data
df_filosofi <- sf::st_intersection(trap_coordinates,filosofi_insee) %>%
  st_drop_geometry() %>%
  dplyr::select(trap_code, men, men_pauv, ind_snv, log_av45, log_45_70, log_70_90, log_ap90, log_soc)


#### Population density data
POP <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  purrr::map_dfr(~sf::st_intersection(st_buffer(trap_coordinates,.), popfine),
                 .id = "buffer") %>%
  st_drop_geometry() %>%
  group_by(buffer,trap_code) %>%
  summarise(sum = sum(pop_2016), .groups = "drop") %>%
  complete(
    buffer = as.character(buffer_sizes),
    trap_code = unique(trap_coordinates$trap_code),
    fill = list(sum = 0)
  ) %>%
  as_tibble()


#### Plaques et avaloirs
plaques_avaloirs <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  purrr::map_dfr(~sf::st_intersection(st_buffer(trap_coordinates,.), pcrs_plaque_avaloir),
                 .id = "buffer") %>%
  st_drop_geometry() %>%
  group_by(buffer,trap_code,nature_m3m) %>%
  summarise(n_elements = n(), .groups = "drop") %>%
  complete(
    buffer = as.character(buffer_sizes),
    trap_code = unique(trap_coordinates$trap_code),
    nature_m3m = "Plaque du réseau télécom carrée ou rectangulaire",
    fill = list(n_elements = 0)
  ) %>%
  filter(nature_m3m == "Plaque du réseau télécom carrée ou rectangulaire")



## loop for land cover and textures

# df_lsm_landcover <- data.frame()
# df_texture <- data.frame()


for(i in 1:nrow(trap_coordinates)){

  cat("dealing with trap n°",i,"over",nrow(trap_coordinates),"\n")
  th_roi <- st_buffer(trap_coordinates[i,],300, endCapStyle = "SQUARE")

  # rasterize land cover layers
  th_vegetation<- st_intersection(vegetation,th_roi) %>% dplyr::select(numero)
  th_bati <- st_intersection(bati,th_roi) %>% mutate(numero = 1) %>% dplyr::select(numero)
  th_roads <- st_intersection(roads,th_roi) %>% mutate(numero = 2) %>% dplyr::select(numero) %>% st_buffer(2.5)

  landcover <- rbind(th_vegetation,th_bati,th_roads)

  r <- raster(landcover, res = 0.5, crs = crs(landcover)) ## conversion to a raster
  landcover_rast <- fasterize(landcover, r, field="numero", fun = "max")  ## creates a raster with 0 for empty cells (background)

  # fill na values with 3 (other built-up elements)
  landcover_rast[is.na(landcover_rast)] <- 3

  landcover_data_dic <- data.frame(class = c(1,2,3,8,9,10,11,12,17),
                                   class_label = c("Bati","Routes","Other built-up","Conifère","Feuillu","Broussaille","Vigne","Pelouse","Prairie"))


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

  th_df_lsm_landcover <- th_df_lsm_landcover %>%
    dplyr::mutate(value=ifelse(metric %in% c("area_mn","ca","ed","np","pland","te") & is.na(value), 0, value)) %>%
    dplyr::left_join(landcover_data_dic) %>%
    dplyr::select(buffer,level,class,class_label,metric,value,plot_id)

  write.csv(th_df_lsm_landcover,paste0("data/lsm_results/",trap_coordinates[i,]$trap_code,".csv"), row.names = F)
  #df_lsm_landcover <- rbind(df_lsm_landcover,th_df_lsm_landcover)


  ##### textures ########

  # open good piece of orthophoto
  x = as.numeric(st_coordinates(trap_coordinates[i,])[,1])
  y = as.numeric(st_coordinates(trap_coordinates[i,])[,2])

  inside_any <- sapply(rasters_ortho, function(r)
    x >= xmin(r) & x <= xmax(r) &
      y >= ymin(r) & y <= ymax(r)
  )

  th_rast_ortho <- rasters_ortho[inside_any][[1]]

  th_roi_small <- st_buffer(trap_coordinates[i,],50, endCapStyle = "SQUARE")
  th_rast_ortho <- crop(th_rast_ortho,th_roi_small)

  lum <- 0.299 * th_rast_ortho[[1]] + 0.587 * th_rast_ortho[[2]] + 0.114 * th_rast_ortho[[3]]

  # Define moving window: 11x11 pixels (≈0.55 m at 5 cm GSD)

  # Compute GLCM features
  textures <- glcm_textures(lum, w = c(11,11), n_levels = 8, quant_method = "prob")

  th_df_texture <- buffer_sizes_textures %>%
    set_names(buffer_sizes_textures) %>%
    map_dfr(~exact_extract(textures,
                           st_buffer(trap_coordinates[i,],.),
                           fun=c("mean"),
                           append_cols = "trap_code"),
            .id = "buffer")

  th_df_texture <- th_df_texture %>%
    pivot_longer(-c("buffer","trap_code"))

  write.csv(th_df_texture,paste0("data/textures_results/",trap_coordinates[i,]$trap_code,".csv"), row.names = F)
  #df_texture <- rbind(df_texture,th_df_texture)

}
