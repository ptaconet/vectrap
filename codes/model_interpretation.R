########################### Opening packages

library(tidyverse)
library(purrr)
library(patchwork)
library(landscapemetrics)

#source("functions.R")

#rf_univ <- read.csv("rf_univ_results.csv")
rf_univ <- read.csv("rf_univ_results_method1.csv")

rf_univ <- rf_univ %>%
  mutate(spatialscale = word(rf_univ$predictor, 1, sep = "_")) %>%
  mutate(predictor = sub(".*?_", "", predictor))

###########################
#########'landscape metrics with different buffers
###########################

lsm_data_dic_NG <- read.csv("landcover_ungrouped_veget_data_dic.csv", stringsAsFactors = F)
lsm_data_dic_G <- read.csv("landcover_grouped_veget_data_dic.csv", stringsAsFactors = F)

lsm_data_dic <- rbind(lsm_data_dic_NG,lsm_data_dic_G) %>%
  mutate(class = as.character(class)) %>%
  mutate(class_group = ifelse(class %in% c(1,2,3), "Land cover - other classes", "Land cover - vegetation")) %>%
  distinct()


rf_univ_lsm <- rf_univ %>%
  filter(grepl("lsm",predictor)) %>%
  mutate(function_name = ifelse(spatialscale == "buffer", sub("^(.*)_([^_]+_[^_]+_[^_]+)$", "\\1", predictor),sub("^(.*)_([^_]+_[^_]+)$", "\\1", predictor)),
         lc_source = ifelse(spatialscale == "buffer", word(predictor, -3, sep = "_"),word(predictor, -2, sep = "_")),
         buffer = ifelse(spatialscale == "buffer",word(predictor, -2, sep = "_"),spatialscale),
         class = word(predictor, -1, sep = "_")) %>%
  mutate(buffer = forcats::fct_relevel(buffer, c("5","10","parcel","20","50","100","bloc","250","500"))) %>%
  left_join(list_lsm()) %>%
  left_join(lsm_data_dic) %>% ## to join the landscape metrics interpretation
  mutate(label = paste0(class_label, " - ", name)) %>%
  mutate(name = forcats::fct_relevel(name, c("shannon's diversity index","total edge", "total (class) area", "percentage of landscape", "patch area", "edge density", "patch cohesion index", "number of patches","clumpiness index" , "connectance")))



ggplot(rf_univ_lsm, aes(buffer, name)) +
  geom_tile(aes(fill = rmse_diff_null_model), color = "white") +
  facet_grid(class_label~type, scales="free", space="free") +
  scale_fill_gradient(low = "#FEE0D2",high = "#DE2D26")




rf_univ_height <- rf_univ %>%
  filter(grepl("height",predictor)) %>%
  mutate(buffer = ifelse(spatialscale == "buffer",word(predictor, -1, sep = "_"),spatialscale),
         class = ifelse(spatialscale == "buffer",word(predictor, -2, sep = "_"),word(predictor, -1, sep = "_"))) %>%
  mutate(buffer = forcats::fct_relevel(buffer, c("5","10","parcel","20","50","100","bloc","250","500"))) %>%
  mutate(class_group = ifelse(grepl("vegetation",predictor),"Vegetation","Bati")) %>%
  mutate(label = paste0("Hauteur ",class_group, " - ",class))

ggplot(rf_univ_height, aes(buffer, label)) +
  geom_tile(aes(fill = imp_mean), color = "white") +
  facet_grid(class_group~., scales="free_y", space="free_y")



rf_univ_fil <- rf_univ %>%
  filter(grepl("FIL",predictor))



rf_univ_pop <- rf_univ %>%
  filter(grepl("POP",predictor)) %>%
  mutate(buffer = ifelse(spatialscale == "buffer",word(predictor, 2, sep = "_"),spatialscale))



rf_univ_plaques <- rf_univ %>%
  filter(grepl("PLAQUES",predictor)) %>%
  mutate(buffer = ifelse(spatialscale == "buffer",word(predictor, 1, sep = "_"),spatialscale), class = word(predictor, -1, sep = "_")) %>%
  mutate(buffer = forcats::fct_relevel(buffer, c("5","10","parcel","20","50","100","bloc","250","500")))

ggplot(rf_univ_plaques, aes(buffer, class)) +
  geom_tile(aes(fill = rmse_diff_null_model), color = "white") +
  #facet_grid(class_group~., scales="free_y", space="free_y")+
  scale_fill_continuous(type = "viridis")




# rf_univ_texture <- rf_univ %>%
#   filter(grepl("texture",predictor)) %>%
#   mutate(buffer = ifelse(spatialscale == "buffer",word(predictor, -1, sep = "_"),spatialscale),
#          label = ifelse(spatialscale == "buffer",word(predictor, -2, sep = "_"),word(predictor, -1, sep = "_"))) %>%
#   mutate(buffer = forcats::fct_relevel(buffer, c("5","10","parcel","20","50","100","bloc","250","500"))) %>%
#   mutate(label = paste0("textures - ", label))
#
#
# ggplot(rf_univ_texture, aes(buffer, label)) +
#   geom_tile(aes(fill = imp_mean), color = "white") +
#   scale_fill_continuous(type = "viridis")

