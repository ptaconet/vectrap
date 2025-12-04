########################### Opening packages

library(tidyverse) ## Version ‘2.0.0’
library(purrr)  ## Version ‘1.0.1’
library(patchwork) ## Version ‘1.2.0.9000’
library(landscapemetrics) ## Version ‘2.1.4’

source("03_Analysis/modelling_adults/functions.R") #### Need of functiosn which allow the interpretation of

###########################
#########'landscape metrics with different buffers
###########################

landcover_grouped_veget_data_dict <- read.csv("02_Data/processed_data/03_Landcover_Data/landcover_grouped_veget_data_dic.csv", stringsAsFactors = F) %>% mutate(lc_source = "LCG")
data_dic <- read.csv("02_Data/processed_data/01_Adults_Abundance/data_dictionary.csv", stringsAsFactors = F)

lsm_data_dic <- landcover_grouped_veget_data_dict ## to create data dictionnary for landscape metrics to interprate different class
mutate(class = as.character(class))


univ_glmm_lsm <- glmm_univ_presence %>%
  bind_rows(glmm_univ_abundance) %>%
  filter(grepl("lsm",term)) %>% ## to put together presence and abundance models
  filter(grepl("np|te|c_area_mn|pland|shdi",term)) %>%
  mutate(function_name = sub("\\_L.*", "", term), lc_source = word(term, -3, sep = "_"), buffer = word(term, -2, sep = "_"), class = word(term, -1, sep = "_")) %>%
  mutate(buffer = forcats::fct_relevel(buffer, c("0","20","50","100","250"))) %>%## different buffer : 20 m, 50 m, 100 and 250 m
  left_join(list_lsm()) %>%
  left_join(lsm_data_dic) %>% ## to join the landscape metrics interpretation
  rename(correlation = estimate) %>%
  mutate(label = ifelse(level=="landscape","landscape",label)) %>%
  mutate(label = paste0(label, " - ", metric," - ",name)) %>%
  mutate(type=ifelse(grepl("Vegetation", label), "Vegetation variables", "Land cover variables"))%>%
  nest(-lc_source)

plots_univ_glmm_spatial <- univ_glmm_lsm %>%
  #mutate(univ_spatial = pmap(list(data,lc_source,type), ~fun_plot_tile_univ_spatial(correlation_df = ..1, metric_name = "glmm", indicator = ..1$indicator, lc_source = ..2, type = ..3))) %>%
  mutate(univ_spatial = pmap(list(data,lc_source), ~fun_plot_tile_univ_spatial_r2(correlation_df = ..1, metric_name = "glmm", indicator = ..1$indicator, lc_source = ..2, type = "", xlabel = "buffer radius around the collection site (meters)"))) %>%
  dplyr::select(-data)


plots_univ_glmm_spatial <- univ_glmm_lsm %>%
  mutate(univ_spatial = pmap(list(data,lc_source), ~fun_plot_tile_univ_spatial_r2(correlation_df = ..1, metric_name = "glmm", indicator = ..1$indicator, lc_source = ..2, type = "", xlabel = "buffer radius around the collection site (meters)"))) %>% ## to use the function to interprate graphically the GLMMs
  dplyr::select(-data)

pmap(list(plots_univ_glmm_spatial$lc_source,plots_univ_glmm_spatial$univ_spatial),~ggsave(filename = paste0("02_Data/processed_data/",..1,".pdf"),plot = ..2, device = "pdf", width = 7,height = 14)) ## to save
