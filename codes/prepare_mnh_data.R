library(sf)
library(terra)

couvert_arbore <- st_read("data/NATURE-EN-VILLE_1-0__GPKG_WGS84_FRA_2021-01-01/mmm_natureenville_couvertarbore.gpkg")
vegetation_basse <-  st_read("data/NATURE-EN-VILLE_1-0__GPKG_WGS84_FRA_2021-01-01/mmm_natureenville_vegetationbasse.gpkg")
bati <- st_read("/home/ptaconet/contributions_diverses_projets_mivegec/vectrap/data/bdnb_open_data_millesime_2024-10-a_dep34_gpkg/bdnb_montpellier.gpkg")
mnh <- rast("data/lidar_hd/merged_lidar_mosaic.tif")

vegetation <- rbind(couvert_arbore,vegetation_basse)

mnh_vegetation <- mask(x = mnh, mask = vegetation)
writeRaster(mnh_vegetation,"data/lidar_hd/merged_lidar_mosaic_vegetation.tif")
mnh_bati <- mask(x = mnh, mask = bati)
writeRaster(mnh_bati,"data/lidar_hd/merged_lidar_mosaic_bati.tif")
