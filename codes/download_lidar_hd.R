urls <- readLines("/home/ptaconet/Téléchargements/dalles.txt")

for (url in urls) {
  # Extract the filename from the URL
  filename <- sub(".*FILENAME=", "", url)
  destfile <- file.path("data","lidar_hd", filename)

  # Download the file
  download.file(url, destfile = destfile, mode = "wb")

  cat("Downloaded:", filename, "\n")
}

# --- Step 4: Load all GeoTIFFs ---
tif_files <- list.files("data/lidar_hd", pattern = "\\.tif$", full.names = TRUE)
rasters <- lapply(tif_files, rast)

# --- Step 5: Merge (mosaic) all tiles ---
mosaic_raster <- do.call(mosaic, rasters)

# --- Step 6: Save the merged raster ---
writeRaster(mosaic_raster, "data/lidar_hd/merged_lidar_mosaic.tif", overwrite = TRUE)

cat("✅ Mosaic saved as merged_lidar_mosaic.tif\n")
