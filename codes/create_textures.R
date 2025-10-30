# -----------------------------------------------------------
# R script: compute NDVI + GLCM contrast & entropy from 4-band drone mosaic
# Bands assumed order: Blue, Green, Red, NIR
# -----------------------------------------------------------

# Load required libraries
library(terra)
library(GLCMTextures)

# ==== 1. Input & output paths ====
infile  <- "data/ortho_montpellier/IRC secteur zoo/7714-62848.tif"     # <-- replace with your 4-band GeoTIFF
out_ndvi <- "ndvi.tif"
out_contrast <- "ndvi_glcm_contrast.tif"
out_entropy  <- "ndvi_glcm_entropy.tif"

# ==== 2. Read the 4-band GeoTIFF ====
r <- rast(infile)

# Check band order (should be: NIR=1, Red=2, Green=3)
print(r)

# ==== 3. Compute NDVI ====
red <- r[[2]]
nir <- r[[1]]

ndvi <- (nir - red) / (nir + red)
names(ndvi) <- "NDVI"

# Write NDVI raster
writeRaster(ndvi, out_ndvi, overwrite=TRUE)
cat("NDVI written to", out_ndvi, "\n")

# Define moving window: 31x31 pixels (≈1.55 m at 5 cm GSD)
w <- c(31, 31)

# Compute GLCM features
# 'glcm' returns several features; we extract contrast and entropy
textures <- glcm_textures(ndvi, w = w, n_levels = 16, quant_method = "prob", shift = c(1,0))


# Extract layers
contrast <- textures$glcm_contrast
entropy  <- textures$glcm_entropy
names(contrast) <- "GLCM_Contrast"
names(entropy)  <- "GLCM_Entropy"

# ==== 5. Write rasters ====
writeRaster(contrast, out_contrast, overwrite=TRUE)
writeRaster(entropy,  out_entropy,  overwrite=TRUE)

cat("GLCM Contrast written to", out_contrast, "\n")
cat("GLCM Entropy written to", out_entropy, "\n")

# ==== 6. Optional: quick map view ====
# plot(ndvi, main="NDVI")
# plot(contrast, main="GLCM Contrast")
# plot(entropy, main="GLCM Entropy")
