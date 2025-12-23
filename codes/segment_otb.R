library(terra)
library(sf)
library(dplyr)

files <- list.files("/home/ptaconet/contributions_diverses_projets_mivegec/vectrap/data/ortho_montpellier/IRC_secteur_zoo", pattern = "tif", full.names = TRUE)
files <- files[-which(grepl("aux.xml", files))]

# get extents of the rasters
extents <- lapply(files, function(f) {
  ext(rast(f))
})

# Convert extents to SpatVector polygons
ext_vect <- vect(lapply(extents, as.polygons))

# Convert to sf
ext_sf <- st_as_sf(ext_vect)

# Add an ID for tracking
ext_sf$extent_id <- seq_len(nrow(ext_sf))
ext_sf$file <- files

st_crs(ext_sf) = 2154


# get extents of the points
trap_coordinates <- st_read("entomo_data/fusion_data/trap_coordinates.gpkg")
trap_coordinates <- trap_coordinates %>% filter(projet %in% c("vectrap","saussan","saussan_fabregues_pignan"))

bbox_by_group <- trap_coordinates %>%
  st_buffer(500) %>%
  group_by(site) %>%
  summarise(
    geometry = st_as_sfc(st_bbox(st_union(geom))),
    .groups = "drop"
  )

## intersect with site bounding box

intersections <- st_join(
  bbox_by_group,
  ext_sf,
  join = st_intersects
)

## loop for each site

th_site="clapiers"

th_site_files <- intersections %>%
  filter(site == th_site)

th_site_files <- th_site_files$file

for(i in 1:length(th_site_files))

# rasters <- sprc(th_site_files)
# r <- merge(rasters)
#

#Source the OTB environment (very important) to be done directly in the shell
system("source /home/ptaconet/Téléchargements/OTB-9.1.0-Linux/otbenv.profile")

path_to_ndvi <- "/home/ptaconet/ndvi.tif"
otb_appli<-paste0("otbcli_RadiometricIndices -in ", th_site_files," -channels.red 2 -channels.nir 1 -list Vegetation:NDVI -out ",path_to_ndvi)
system(otb_appli)

path_to_segmented <- "/home/ptaconet/segmented.tif"
otb_appli<-paste0("otbcli_GenericRegionMerging -in ", path_to_ndvi," -criterion bs -threshold 23 -speed 1 -cw 0.96 -sw 0.04 -out ",path_to_segmented)
system(otb_appli)

gdal_appli<-paste0("gdal_polygonize.py ", path_to_segmented," ",gsub(".tif", ".gpkg", path_to_segmented)," -b 1")
system(gdal_appli)

# ==== 3. Compute NDVI ====
red <- r[[2]]
nir <- r[[1]]

ndvi <- (nir - red) / (nir + red)

writeRaster(ndvi,"/home/ptaconet/ndvi2.tif", overwrite = T)

#####" parametres OTB qui fonctionnent bien pour la segmentation

system(otb_appli)

# Algo GenericRegionMerging
# sur image NDVI
# threshold = 23
# spectral = 0.96
# spatial = 0.04
