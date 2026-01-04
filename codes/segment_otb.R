library(terra)
library(sf)
library(dplyr)

path_to_processing_folder <- "/home/ptaconet/contributions_diverses_projets_mivegec/vectrap/data/ortho_montpellier/traitements"
path_to_output_folder <- "/home/ptaconet/contributions_diverses_projets_mivegec/vectrap/data/ortho_montpellier/output_classif"
source_otb_call <- "source /home/ptaconet/Téléchargements/OTB-9.1.0-Linux/otbenv.profile && "  #Source the OTB environment (very important) to be done directly in the shell


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




for(i in 1:length(th_site_files)){

th_site_file <- th_site_files[i]

tile_name = gsub(".tif","",basename(th_site_file))

cat("processing tile", tile_name, "..." )

# mosaiquer
# cmd<-paste0(source_otb_call,"otbcli_Mosaic -il ",paste(th_site_files, collapse = " ")," -comp.feather none -harmo.method none -out /home/ptaconet/mosaic.tif")
# system(paste("bash -c", shQuote(cmd)))

tictoc::tic()

#calculer ndvi
cmd <- paste0(source_otb_call,"otbcli_RadiometricIndices -in ", th_site_file," -channels.nir 1 -channels.red 2 -channels.green 3 -list Vegetation:NDVI -out ",path_to_processing_folder,"/ndvi.tif")
#cmd <- paste0(source_otb_call,"otbcli_RadiometricIndices -in /home/ptaconet/mosaic.tif -channels.nir 1 -channels.red 2 -channels.green 3 -list Vegetation:NDVI -out /home/ptaconet/ndvi.tif")
system(paste("bash -c", shQuote(cmd)))

# rescaler ndvi entre 0 et 255
cmd <- paste0(source_otb_call,'otbcli_BandMathX -il ',path_to_processing_folder,'/ndvi.tif -out ',path_to_processing_folder,'/ndvi_255.tif -exp "((im1b1 + 1) / 2) * 255"')
system(paste("bash -c", shQuote(cmd)))

# recuperer vert et nir de IRC
cmd <- paste0(source_otb_call,'otbcli_BandMathX -il ', th_site_file,' -out ',path_to_processing_folder,'/green_nir.tif -exp "im1b1; im1b3"')
#cmd <- paste0(source_otb_call,'otbcli_BandMathX -il /home/ptaconet/mosaic.tif -out /home/ptaconet/green_nir.tif -exp "im1b1; im1b3"')
system(paste("bash -c", shQuote(cmd)))

# merger ndvi, nir, green (dabord splitter IRC puis concatener)
cmd <- paste0(source_otb_call,'otbcli_ConcatenateImages -il ',path_to_processing_folder,'/green_nir.tif ',path_to_processing_folder,'/ndvi_255.tif -out ',path_to_processing_folder,'/green_nir_ndvi.tif')
system(paste("bash -c", shQuote(cmd)))

# mean shift segmentation
cmd <- paste0(source_otb_call,"otbcli_LargeScaleMeanShift -in ",path_to_processing_folder,"/green_nir_ndvi.tif -spatialr 10 -ranger 20 -minsize 800 -ram 8000 -mode vector -mode.vector.out ",path_to_processing_folder,"/segment.gpkg")
system(paste("bash -c", shQuote(cmd)))

#  reprojeter en EPSG:2154
gdal_appli<-paste0("ogr2ogr -a_srs EPSG:2154 ",path_to_processing_folder,"/segment_project.gpkg ",path_to_processing_folder,"/segment.gpkg")
system(gdal_appli)

# ajouter la hauteur dans les covariables
cmd <- paste0(source_otb_call,"otbcli_ZonalStatistics -in /home/ptaconet/contributions_diverses_projets_mivegec/vectrap/data/lidar_hd/mnh_filled.tif -inzone.vector.in ",path_to_processing_folder,"/segment_project.gpkg -out.vector.filename ",path_to_output_folder,"/",tile_name,".gpkg")
system(paste("bash -c", shQuote(cmd)))

file.remove(list.files(path_to_processing_folder, recursive = TRUE, full.names = T))

tictoc::toc()

}



# ouvrir le fichier vecteur
segment <- st_read(paste0(path_to_processing_folder,"/segment_final.gpkg"))















# path_to_mosaic <- "/home/ptaconet/mosaic.tif"
# otb_appli<-paste0("otbcli_Mosaic -il ",paste(th_site_files, collapse = " ")," -comp.feather none -harmo.method none -out ",path_to_mosaic," uint8")
# system(otb_appli)

cmd <- paste0(source_otb_call,"otbcli_RadiometricIndices -in ", th_site_files," -channels.nir 1 -channels.red 2 -channels.green 3 -list Vegetation:NDVI -out /home/ptaconet/ndvi.tif")
system(paste("bash -c", shQuote(cmd)))

# then Apply spatial smoothing to NDVI before segmentation
cmd <- paste0(source_otb_call,"otbcli_MeanShiftSmoothing -in /home/ptaconet/ndvi.tif -spatialr 20 -ranger 0.15 -ram 8000 -fout /home/ptaconet/ndvi_smooth.tif")
system(paste("bash -c", shQuote(cmd)))

# segment
cmd <- paste0(source_otb_call,"otbcli_GenericRegionMerging -in /home/ptaconet/ndvi_smooth.tif -criterion bs -threshold 20 -speed 1 -cw 0.97 -sw 0.03 -out /home/ptaconet/segmented.tif")
system(paste("bash -c", shQuote(cmd)))

# set src
gdal_appli<-"gdal_edit.py /home/ptaconet/segmented.tif -a_srs EPSG:2154"
system(gdal_appli)

# polygonize
gdal_appli<-"gdal_polygonize.py /home/ptaconet/segmented.tif /home/ptaconet/segmented.gpkg -b 1"
system(gdal_appli)


#####" parametres OTB qui fonctionnent bien pour la segmentation

# sur image NDVI
# threshold = 23
# spectral = 0.96
# spatial = 0.04
