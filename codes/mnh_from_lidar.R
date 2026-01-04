library(lidR)
library(sf)

dalles_lidar_hd_mtp <- st_read("data/lidar_hd/dalles_lidar_hd_mtp.gpkg")

urls <- dalles_lidar_hd_mtp$url

for (url in urls) {
  # Extract the filename from the URL
  filename <- basename(url)
  destfile <- paste0("/home/ptaconet/contributions_diverses_projets_mivegec/vectrap/data/lidar_hd/raw_lidar_files/",filename)

  # Download the file
  download.file(url, destfile = destfile, mode = "wb")

  las <- readLAS(destfile)

  mnt = grid_terrain(las, 0.25, knnidw())
  mns = grid_canopy(las, 0.25, p2r())
  mnh <- mns - mnt
  mnh <- terra::rast(mnh)

  mnh2 <- terra::focal(mnh, w=5, fun=mean, na.policy="only")

  terra::writeRaster(mnh,paste0("/home/ptaconet/contributions_diverses_projets_mivegec/vectrap/data/lidar_hd/mnh_25cm/",gsub(".copc.laz",".tif",filename)),overwrite=TRUE)

  rm(destfile)
}



# --- Step 4: Load all GeoTIFFs ---
tif_files <- list.files("/home/ptaconet/contributions_diverses_projets_mivegec/vectrap/data/lidar_hd/mnh_25cm", pattern = "\\.tif$", full.names = TRUE)
rasters <- lapply(tif_files, rast)

# --- Step 5: Merge (mosaic) all tiles ---
mosaic_raster <- do.call(mosaic, rasters)

# --- Step 6: Save the merged raster ---
writeRaster(mosaic_raster, "/home/ptaconet/contributions_diverses_projets_mivegec/vectrap/data/lidar_hd/mnh_25cm/merged_lidar_mosaic_25cm.tif", overwrite = TRUE)

cat("✅ Mosaic saved as merged_lidar_mosaic.tif\n")




