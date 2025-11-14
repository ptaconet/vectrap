# --- Step 4: Load all GeoTIFFs ---
 tif_files <- list.files("data/ortho_montpellier/RGB", pattern = "\\.tif$", full.names = TRUE)
# [1] "data/ortho_montpellier/RGB/zone1.tif" "data/ortho_montpellier/RGB/zone2.tif" "data/ortho_montpellier/RGB/zone3.tif"
# [4] "data/ortho_montpellier/RGB/zone4.tif"
rasters <- lapply(tif_files, rast)

# --- Step 5: Merge (mosaic) all tiles ---
mosaic_raster <- do.call(mosaic, rasters)

writeRaster(mosaic_raster, "data/ortho_montpellier/RGB/merged_RGB_ortho.tif", overwrite = TRUE)
