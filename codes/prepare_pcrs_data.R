pcrs_plaque <- list.files("data/MMM_MMM_PCRS_SHP", pattern = "PLAQUE.shp", full.names = TRUE, recursive = TRUE)

# --- Step 1: Read and keep only the desired columns ---
shapes <- lapply(pcrs_plaque, function(path) {
  shp <- st_read(path, quiet = TRUE)

  # Keep only desired columns (if they exist)
  cols_to_keep <- intersect(names(shp), c("nature_m3m", "type_objet", "reseau"))
  shp <- shp[, c(cols_to_keep, "geometry")]

  return(shp)
})

merged_pcrs_plaques <- do.call(rbind, shapes)

st_write(merged_pcrs_plaques,"data/MMM_MMM_PCRS_SHP/merged_pcrs_plaques.gpkg")



pcrs_avaloirs <- list.files("data/MMM_MMM_PCRS_SHP", pattern = "AVALOIR.shp", full.names = TRUE, recursive = TRUE)

# --- Step 1: Read and keep only the desired columns ---
shapes <- lapply(pcrs_avaloirs, function(path) {
  shp <- st_read(path, quiet = TRUE)

  # Keep only desired columns (if they exist)
  cols_to_keep <- intersect(names(shp), c("nature_m3m", "type_objet"))
  shp <- shp[, c(cols_to_keep, "geometry")]

  return(shp)
})

merged_pcrs_avaloirs <- do.call(rbind, shapes)

st_write(merged_pcrs_avaloirs,"data/MMM_MMM_PCRS_SHP/merged_pcrs_avaloirs.gpkg")
