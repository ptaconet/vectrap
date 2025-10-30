library(readxl)
library(tidyverse)
library(ggmap)
library(sf)
library(patchwork)



pieges_loc <-  read_xlsx("entomo_data/VECTRAP_2021-2023 monitoring results_pour paul.xlsx", sheet = "Notice_coord geo")
colnames(pieges_loc) <- c("trap_code","Longitude","Latitude")

pieges_loc_sf <- pieges_loc %>%
  st_as_sf(.,coords = c("Longitude","Latitude"), crs = 2154) %>%
  st_transform(4326)

pieges_loc$Longitude_wgs84 <- st_coordinates(pieges_loc_sf)[,1]
pieges_loc$Latitude_wgs84 <- st_coordinates(pieges_loc_sf)[,2]

df <- read_xlsx("entomo_data/VECTRAP_2021-2023 monitoring results_pour paul.xlsx", sheet = "data 2021 to 2023 - Montpellier")

df <- df %>%
  filter(!is.na(date_turn_off)) %>%
  rename(date_instal = date_turn_on , daterec = date_turn_off, Site = commune) %>%
  mutate(week = week(daterec), Mois_numeric = month(daterec), Year = year(daterec)) %>%
  mutate(blocks=as.character(blocks)) %>%
  mutate(effectif_jour = as.numeric(number_female)/as.numeric((daterec-date_instal))) %>%
  left_join(pieges_loc)


df <- df %>%
  filter(modality=="C"|period=="Pré traitement", species == "Aedes albopictus")


ggplot(df %>% filter(blocks %in% c(1,4,6,7)), aes(x = reorder(trap_code,effectif_jour,na.rm = TRUE), y = effectif_jour)) +
  geom_boxplot() +
  facet_wrap(.~Year, scales = "free_x") +
  ggtitle("abundance per trap") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylim(c(0,75))


df %>%
  group_by(Site, week, blocks) %>%
  summarise(number_female=mean(number_female, na.rm = T)) %>%
  ggplot(aes(x = week, y = number_female)) +
  facet_wrap(.~blocks) +
  geom_line() +
  #geom_smooth(method = "gam", se = F) +
  theme_bw() +
  ggtitle("mean abundance per trap for each year and site")


### mapping

col_sites <- "blocks"
df$Site = df[[col_sites]]

ggmap::register_stadiamaps("46e02592-4347-4401-95bb-66873ec3a35b")


maps_list <- list()
sites <- unique(df[,col_sites])
sites <- sites[[col_sites]]
sites <- sites[!is.na(sites)]


for(i in 1:length(sites)){

  bbox <-  c(left = min(df$Longitude_wgs84[which(df[[col_sites]] == sites[i])])-0.001, bottom = min(df$Latitude_wgs84[which(df[[col_sites]] == sites[i])])-0.001, right = max(df$Longitude_wgs84[which(df[[col_sites]] == sites[i])])+0.001, top = max(df$Latitude_wgs84[which(df[[col_sites]] == sites[i])])+0.001)
  maps_list[[i]] <-  get_stadiamap(bbox, maptype = "stamen_terrain", zoom = 17)

}

names(maps_list) <- sites

# bbox_stclement <- c(left = min(df$Longitude_wgs84[which(df$Site == "ST CLEMENT")])-0.001, bottom = min(df$Latitude_wgs84[which(df$Site == "ST CLEMENT")])-0.001, right = max(df$Longitude_wgs84[which(df$Site == "ST CLEMENT")])+0.001, top = max(df$Latitude_wgs84[which(df$Site == "ST CLEMENT")])+0.001)
# bbox_castelnau <- c(left = min(df$Longitude_wgs84[which(df$Site == "CASTELNAU")])-0.001, bottom = min(df$Latitude_wgs84[which(df$Site == "CASTELNAU")])-0.001, right = max(df$Longitude_wgs84[which(df$Site == "CASTELNAU")])+0.001, top = max(df$Latitude_wgs84[which(df$Site == "CASTELNAU")])+0.001)
# bbox_clapiers <- c(left = min(df$Longitude_wgs84[which(df$Site == "CLAPIERS")])-0.001, bottom = min(df$Latitude_wgs84[which(df$Site == "CLAPIERS")])-0.001, right = max(df$Longitude_wgs84[which(df$Site == "CLAPIERS")])+0.001, top = max(df$Latitude_wgs84[which(df$Site == "CLAPIERS")])+0.001)
# bbox_allsites <- c(left = min(df$Longitude_wgs84)-0.01, bottom = min(df$Latitude_wgs84)-0.01, right = max(df$Longitude_wgs84)+0.01, top = max(df$Latitude_wgs84)+0.01)
#

# map_stclement <- get_stadiamap(bbox_stclement, maptype = "stamen_terrain", zoom = 16)
# map_castelnau <- get_stadiamap(bbox_castelnau, maptype = "stamen_terrain", zoom = 16)
# map_clapiers <- get_stadiamap(bbox_clapiers, maptype = "stamen_terrain", zoom = 16)
# map_allsites <- get_stadiamap(bbox_allsites, maptype = "stamen_terrain", zoom = 13)


## Plot the location of the traps

# df_sf <- df %>%
#   st_as_sf( crs = "OGC:CRS84", coords = c("Longitude_wgs84", "Latitude_wgs84"))
#
# calc_angle <- function(lon,lat) {
#   cent_lon <- mean(lon)
#   cent_lat <- mean(lat)
#   ang <- atan2(lat - cent_lat, lon - cent_lon)
#
#   return(ang)
# }
#
# bbox <-df %>%
#   group_by(Site) %>%
#   summarise(xmin = min(Longitude),ymin = min(Latitude), xmax=max(Longitude),  ymax = max(Latitude)) %>%
#   gather(x,Longitude,c('xmin','xmax')) %>%
#   gather(y,Latitude,c('ymin','ymax')) %>%
#   st_as_sf(coords=c('Longitude','Latitude'),crs=4326,remove=F) %>%
#   group_by(Site) %>%
#   rename(lon = Longitude, lat = Latitude) %>%
#   mutate(angle = calc_angle(lon,lat)) %>%
#   arrange(angle) %>%
#   summarise(do_union=FALSE) %>%
#   st_cast('POLYGON')
#
# bbox = bbox[dim(bbox)[1]:1,]
#
# cbp1 <- c("#999999", "#E69F00", "#56B4E9")
#
# traps_map <- ggmap(map_allsites) +
#   coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
#   geom_sf(data = bbox, aes(col=Site), inherit.aes = FALSE, alpha = 0.7) +
#   geom_sf(data = df_sf, aes(col=Site), size = 1, inherit.aes = FALSE)  +
#   theme_bw() +
#   scale_color_manual(values = cbp1) +
#   ggtitle("Locations of the areas and traps") +
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())




df$Latitude = df$Latitude_wgs84
df$Longitude = df$Longitude_wgs84


fun_get_map <- function(df, site, temporal_grouping_column, map_type, spat_res = 1/2220){

  # Define a function to fix the bbox to be in EPSG:3857
  ggmap_bbox <- function(map) {
    if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
    # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
    # and set the names to what sf::st_bbox expects:
    map_bbox <- setNames(unlist(attr(map, "bb")),
                         c("ymin", "xmin", "ymax", "xmax"))

    # Coonvert the bbox to an sf polygon, transform it to 3857,
    # and convert back to a bbox (convoluted, but it works)
    bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))

    # Overwrite the bbox of the ggmap object with the transformed coordinates
    attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
    attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
    attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
    attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
    map
  }

  # define theme
  my_theme <- function() {
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.background = element_rect(fill = "transparent"), # necessary to avoid drawing panel outline
      panel.border = element_rect(colour = "grey", fill=NA, linewidth=1),
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank()
    )
  }



  map = maps_list[[site]]
  df <- df %>% filter(Site == site)


  if(map_type=="heatmap"){

    # df2 <- tidyr::uncount(df, effectif_jour)

    df2 <- df %>%
      group_by(!!sym(temporal_grouping_column), trap_code) %>%
      summarise(nb_rec = n(), effectif_jour_mn = trunc(mean(effectif_jour, na.rm = T)), effectif_jour_sd = sd(effectif_jour, na.rm = T), Latitude = mean(Latitude), Longitude = mean(Longitude)) %>%
      tidyr::uncount(effectif_jour_mn)


    final_map <- ggmap(map) +
      geom_hdr(aes(Longitude, Latitude, fill = after_stat(probs)), data = df2, alpha = .5, method = "kde") +
      geom_hdr_lines(aes(Longitude, Latitude), data = df2, method = "kde", linewidth = 0.2, show.legend=F) +
      scale_fill_brewer(name="Egg density", palette = "YlOrRd", labels=c("Lowest","","","Highest")) +
      {if(temporal_grouping_column=="Year")geom_point(data = df2,aes(x = Longitude, y = Latitude), color = "red", size = 0.02)}+
      facet_wrap(temporal_grouping_column, nrow = ifelse(temporal_grouping_column=="Mois",1,1)) +
      my_theme() +
      theme(legend.position="none") +
      ggtitle(paste0(site," - by ", temporal_grouping_column))

  }

  if(map_type == "points"){

    df2 <- df %>%
      group_by(!!sym(temporal_grouping_column), trap_code) %>%
      summarise(nb_rec = n(), effectif_jour_mn = mean(effectif_jour, na.rm = T), effectif_jour_sd = sd(effectif_jour, na.rm = T), Latitude = mean(Latitude), Longitude = mean(Longitude))

    final_map <- ggmap(map) +
      #{if(temporal_grouping_column=="Year")geom_point(data = df2,aes(x = Longitude, y = Latitude), color = "red", size = 0.02)}+
      geom_point(aes(x = Longitude, y = Latitude), data = df2, size = 0.5, color = "black") +
      geom_point(aes(x = Longitude, y = Latitude, size = effectif_jour_mn), data = df2 %>% filter(effectif_jour_mn>0), colour = "darkred", alpha = 0.7) +
      scale_size_continuous(breaks = c(10,20,30,50,100,200), limits = c(0,200), range = c(1,10), name="Mean egg count / trap / day", labels=c("1-10","10-20","20-30","30-50","50-100",">100")) +
      facet_wrap(temporal_grouping_column, nrow = ifelse(temporal_grouping_column=="Mois",1,1)) +
      ggtitle(paste0(site," - by ", temporal_grouping_column)) +
      my_theme()
    #+ theme(legend.position="none")


  }

  if(map_type=="hexagrid"){

    # Use the function:
    map <- ggmap_bbox(map)

    df_sf <- st_as_sf(df, crs = "OGC:CRS84", coords = c("Longitude", "Latitude")) %>%
      st_transform(3857)

    g = st_make_grid(df_sf , square=FALSE, cellsize = 100)
    nc2 = st_sf(geom=g)
    nc2$ID=1:length(g)

    a= nc2 %>%
      st_join(df_sf, join = st_intersects,left = TRUE) %>%
      filter(!is.na(Year)) %>%
      group_by(ID,!!sym(temporal_grouping_column)) %>%
      summarise(effectif_jour_mn = mean(effectif_jour, na.rm = T))


    final_map <- ggmap(map) +
      coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
      geom_sf(data = a, aes(fill = effectif_jour_mn), inherit.aes = FALSE,alpha = .7,) +
      scale_fill_gradient(low="lightyellow", high="red", trans = "sqrt") +
      {if(temporal_grouping_column=="Year")geom_sf(data = df_sf %>% st_transform(3857), color = "red", size = 0.01, inherit.aes = FALSE)}+
      facet_wrap(temporal_grouping_column, nrow = ifelse(temporal_grouping_column=="Mois",2,1)) +
      ggtitle(paste0(site," - by ", temporal_grouping_column," - ", map_type)) +
      my_theme()


  }
  return(final_map)
}

Sites = data.frame(site = sites)

# points - par année et par mois
pl1 <- Sites %>%
  mutate(pl_year_points = purrr::map(.$site,~fun_get_map(df,.,"Year","hexagrid")))

