# -----------------------------------------------------------
# R script: compute NDVI + GLCM contrast & entropy from 4-band drone mosaic
# Bands assumed order: Blue, Green, Red, NIR
# -----------------------------------------------------------

# Load required libraries
library(terra)
library(GLCMTextures)

# ==== 1. Input & output paths ====
infile  <- "data/ortho_montpellier/RGB/zone3.tif"     # <-- replace with your 4-band GeoTIFF

# ==== 2. Read the 4-band GeoTIFF ====
r <- rast(infile)

# Check band order (should be: NIR=1, Red=2, Green=3)
print(r)

# ==== 3. Compute NDVI ====
# assume band order is R=1, G=2, B=3
R <- r[[1]]
G <- r[[2]]
B <- r[[3]]

lum <- 0.299 * R + 0.587 * G + 0.114 * B
names(lum) <- "luminance"

# Define moving window: 31x31 pixels (≈1.55 m at 5 cm GSD)
window_sizes <- c(11, 31, 101)       # small, medium, large (odd numbers)


# Compute GLCM features
# 'glcm' returns several features; we extract contrast and entropy
textures <- glcm_textures(lum, w = c(window_sizes[2],window_sizes[2]), n_levels = 8, quant_method = "prob")

# ==== 5. Write rasters ====
writeRaster(textures, "textures_11.tif", overwrite=TRUE)


