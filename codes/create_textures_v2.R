# ------------------------------------------------
# R script: Multi-scale texture ("noisiness") from RGB drone mosaic
# Input: 3-band GeoTIFF (R, G, B) at 5 cm GSD
# Output: GeoTIFFs for each texture + stacked multiband file
# ------------------------------------------------

# Required packages:
# install.packages(c("terra","glcm","OpenImageR"))
library(terra)
library(glcm)
library(OpenImageR)

# ---- Paths (edit) ----
infile <- "rgb_mosaic.tif"          # 3-band RGB: band order must be R, G, B
out_prefix <- "textures_"            # will create textures_*.tif
out_stack  <- "textures_stack.tif"

# ---- Read input ----
r <- rast("data/ortho_montpellier/RGB/zone3.tif")
if (nlyr(r) < 3) stop("Input must be 3-band RGB.")
# assume band order is R=1, G=2, B=3
R <- r[[1]]
G <- r[[2]]
B <- r[[3]]

# ---- Compute luminance (grayscale) ----
lum <- 0.299 * R + 0.587 * G + 0.114 * B
names(lum) <- "luminance"

# ---- Parameters ----
glcm_levels <- 8                     # quantize to 8 gray levels
window_sizes <- c(11, 31, 101)       # small, medium, large (odd numbers)
# LBP parameters
lbp_P <- 8
lbp_R <- 1

# Utility: function to scale raster to 0..(levels-1) integer
scale_to_levels <- function(x, levels=8) {
  # x: SpatRaster single layer; scale based on min/max; return integer raster
  xr <- x
  vmin <- global(xr, min, na.rm=TRUE)[[1]]
  vmax <- global(xr, max, na.rm=TRUE)[[1]]
  # avoid zero division
  if (is.na(vmin) || is.na(vmax) || vmax == vmin) {
    stop("Bad raster min/max for scaling.")
  }
  scaled <- ((xr - vmin) / (vmax - vmin)) * (levels - 1)
  scaled <- clamp(scaled, lower=0, upper=(levels-1))
  scaled_int <- as.int(round(scaled))
  return(scaled_int)
}

# Pre-scale luminance for GLCM
lum_q <- scale_to_levels(lum, levels = glcm_levels)

lum_q <- raster(lum_q)
# Prepare stack to collect outputs
out_layers <- list()

# ---- Loop over window sizes and compute features ----
for (w in window_sizes) {
  cat("Processing window", w, "x", w, "...\n")
  # GLCM: contrast and entropy on quantized luminance
  glcm_res <- glcm(lum_q, window = c(w, w),
                   statistics = c("contrast", "entropy", "homogeneity", "dissimilarity"),
                   shift = matrix(c(1,0, 0,1, 1,1, 1,-1), ncol=2, byrow=TRUE),
                   na_opt = "any", na_val = NA)
  # glcm returns list with named rasters
  # rename outputs with scale suffix
  contrast <- glcm_res$contrast
  names(contrast) <- paste0("glcm_contrast_w", w)
  entropy  <- glcm_res$entropy
  names(entropy) <- paste0("glcm_entropy_w", w)
  hom <- glcm_res$homogeneity
  names(hom) <- paste0("glcm_homogeneity_w", w)
  diss <- glcm_res$dissimilarity
  names(diss) <- paste0("glcm_dissimilarity_w", w)
  asm <- glcm_res$ASM
  names(asm) <- paste0("glcm_ASM_w", w)

  # local standard deviation on raw luminance
  local_sd <- focal(lum, w = matrix(1, nrow=w, ncol=w), fun = sd, na.policy="omit", filename="")
  names(local_sd) <- paste0("local_sd_w", w)

  # Edge density: Sobel approximation using focal kernels
  kx <- matrix(c(-1,0,1,-2,0,2,-1,0,1), nrow=3, byrow=TRUE)
  ky <- matrix(c(-1,-2,-1,0,0,0,1,2,1), nrow=3, byrow=TRUE)
  gx <- focal(lum, w = kx, fun = sum, na.policy="omit")
  gy <- focal(lum, w = ky, fun = sum, na.policy="omit")
  grad <- sqrt(gx^2 + gy^2)
  # threshold edges using high percentile (adaptive)
  thr <- global(grad, fun=function(x,...) quantile(x, probs=0.85, na.rm=TRUE))[[1]]
  edges_bin <- grad > thr
  # local fraction of edge pixels in w x w window
  edge_density <- focal(edges_bin, w = matrix(1, nrow=w, ncol=w), fun = mean, na.policy="omit")
  names(edge_density) <- paste0("edge_density_w", w)

  # LBP: compute LBP image (OpenImageR expects matrix). We'll compute on scaled 0..255 grayscale.
  lum_255 <- round(((lum - global(lum, min, na.rm=TRUE)[[1]]) /
                      (global(lum, max, na.rm=TRUE)[[1]] - global(lum, min, na.rm=TRUE)[[1]] + 1e-12)) * 255)
  # convert to matrix
  lum_mat <- as.matrix(lum_255)
  lbp_img <- LBP(lum_mat, P = lbp_P, R = lbp_R, method = "riu2") # rotation-invariant uniform LBP
  # convert back to rast
  lbp_r <- rast(lbp_img)
  ext(lbp_r) <- ext(lum)
  crs(lbp_r) <- crs(lum)
  names(lbp_r) <- "lbp"
  # local variance of LBP within window (as a measure of micro-noise)
  lbp_var <- focal(lbp_r, w = matrix(1, nrow=w, ncol=w), fun = var, na.policy="omit")
  names(lbp_var) <- paste0("lbp_var_w", w)

  # collect layers
  out_layers[[paste0("contrast_w", w)]] <- contrast
  out_layers[[paste0("entropy_w", w)]]  <- entropy
  out_layers[[paste0("hom_w", w)]]      <- hom
  out_layers[[paste0("diss_w", w)]]     <- diss
  out_layers[[paste0("asm_w", w)]]      <- asm
  out_layers[[paste0("sd_w", w)]]       <- local_sd
  out_layers[[paste0("edge_w", w)]]     <- edge_density
  out_layers[[paste0("lbpvar_w", w)]]   <- lbp_var
}

# ---- Write outputs ----
# stack all layers into a single SpatRaster
stack_list <- rast(out_layers)  # terra will align extents/crs
# write each layer separately and also stacked file
for (i in 1:nlyr(stack_list)) {
  nm <- names(stack_list[[i]])
  outfile <- paste0(out_prefix, nm, ".tif")
  writeRaster(stack_list[[i]], outfile, overwrite = TRUE)
  cat("Wrote", outfile, "\n")
}
# stacked multi-band
writeRaster(stack_list, out_stack, overwrite = TRUE)
cat("Wrote stacked textures to", out_stack, "\n")

# ---- Optional: compute aggregated stats per-household if you have polygons ----
# households <- vect("household_polygons.geojson")  # polygon shapefile
# stats <- extract(stack_list, households, fun = mean, na.rm=TRUE)
# write.csv(stats, "household_texture_stats.csv", row.names=FALSE)

cat("Done.\n")
