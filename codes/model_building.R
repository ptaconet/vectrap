########################### Opening packages

library(tidyverse) ## Version ‘2.0.0’
library(glmmTMB) ## Version ‘1.1.9’
library(purrr) ## Version ‘1.0.1’
library(furrr) ## Version ‘0.3.1’
library(correlation) ## Version ‘0.8.5’
library(caret) ## Version ‘6.0.94’
library(performance) ## Version ‘0.12.3’

########################### Open dataset containing the dependent variables
trapping_data <- read.csv("entomo_data/fusion_data/data_mosquitoes.csv")
trapping_data$X <- NULL
trapping_data$year <- year(trapping_data$date_releve)

trapping_data <- trapping_data %>%
  mutate(week = as.factor(week))

# trapping_data <- trapping_data %>%
#   group_by(trap_code,projet,year,site) %>%
#   summarise(nb_femelles_median=median(nb_femelles_jour, na.rm = T),
#             nb_males_median=median(nb_males_jour, na.rm = T),
#             nb_moustiques_median=median(nb_moustiques_jour, na.rm = T),
#             nb_femelles_mean=mean(nb_femelles_jour, na.rm = T),
#             nb_males_mean=mean(nb_males_jour, na.rm = T),
#             nb_moustiques_mean=mean(nb_moustiques_jour, na.rm = T),
#             nb_femelles_coefvar=sd(nb_femelles_jour, na.rm = T)/mean(nb_femelles_jour, na.rm = T),
#             nb_males_coefvar=sd(nb_males_jour, na.rm = T)/mean(nb_males_jour, na.rm = T),
#             nb_moustiques_coefvar=sd(nb_moustiques_jour, na.rm = T)/mean(nb_moustiques_jour, na.rm = T)) %>%
#   ungroup()

## First week of detection for each trap
# trapping_data = trapping_data %>%
#   mutate(
#     date_releve = ymd(date_releve),
#     year = year(date_releve)
#   ) %>%
#   filter(nb_tot > 0) %>%                       # keep only detections
#   arrange(trap_code, year, date_releve) %>%    # sort correctly
#   group_by(trap_code, year) %>%                # per trap & year
#   slice(1) %>%                                  # keep first detection
#   ungroup()

########################### Open dataset containing the independent variables
predictors <- read.csv("data/df_model.csv")
predictors$X <- NULL

predictors <- predictors %>%
  mutate(across(
    contains("lsm_c_area_mn_LCG"),
    ~ . / 0.0001
  ))

########################### combine both datasets
df_model <- trapping_data %>%
  left_join(predictors)


### model

df_model <- df_model %>% filter(projet %in% c("vectrap","saussan","saussan_fabregues_pignan"))

df_glmm <- df_model %>%
  mutate(exposure_days = as.character(exposure_days), nb_femelles=as.character(nb_femelles), nb_males=as.character(nb_males), nb_tot=as.character(nb_tot),nb_femelles_jour=as.character(nb_femelles_jour), nb_males_jour=as.character(nb_males_jour), nb_moustiques_jour=as.character(nb_moustiques_jour)) %>%
  mutate_if(is.numeric, ~scale(., center = TRUE, scale = FALSE)) %>%  # we center and scale to be able to compare the magnitudes (centering also helps with allowing easier interpretation of variable coefficients associated with different magnitudes, e.g. when one regressor is measured on a very small order, while another on a very large order.  )
  mutate(exposure_days = as.numeric(exposure_days), nb_femelles=as.numeric(nb_femelles), nb_males=as.numeric(nb_males), nb_tot=as.numeric(nb_tot),nb_femelles_jour=as.numeric(nb_femelles_jour), nb_males_jour=as.numeric(nb_males_jour), nb_moustiques_jour=as.numeric(nb_moustiques_jour))


fun_compute_glmm_univ <- function(df_glmm,indicator, null_model, tolerance){

  # if(indicator == "presence"){ ## For presence models
  #   func <- function(x){
  #     ret <- glmmTMB(as.formula(paste0("PRES_ALBO ~ ",x," + (1|AREA/ID_PIEGE) + (1|num_session)")), data = df_glmm, family = binomial(link = "logit")) ## Realization of GLMM with binomial distribution for each predictor and with 2 random effects
  #     return(ret)
  #   }
  # } else if (indicator == "abundance"){ ## For abundance models
  #   func <- function(x){
  #     ret <- glmmTMB(as.formula(paste0("NB_ALBO_F ~ ",x," + (1|AREA/ID_PIEGE) + (1|num_session)")), data = df_glmm, family = truncated_nbinom2) ## Realization of GLMM with negative binomial distribution zero truncared for each predictor and with 2 random effects
  #     return(ret)
  #   }
  # }
  func <- function(x){
  ret <- glmmTMB(
    as.formula(paste0("nb_femelles ~ ",x," +
      (1 | site/trap_code) +
      (1 | year/week) +
      (1 | projet) +
      offset(log(exposure_days))")),  # nombre de jours entre installation et relevé,
    ziformula = ~ 1,         # zero-inflation component
    family = nbinom2,
    data = df_glmm
  )
  return(ret)
  }

  possible_a <- possibly(func, otherwise = NA_real_) ## Allows to return a numeric missing value NA in stead of an error message for the function func

  glmms_univs <- future_map(colnames(df_glmm[15:ncol(df_glmm)]), possible_a) ## Allows to apply the function possible_a to every part of the data frame df_glmm[5:ncol(df_glmm)]: returns a list of GLMMs for presence and abundance for every predictors

  ## Allows to delete the missing and the empty models on the list
  glmm_to_rm <- NULL
  glmm_to_rm2 <- NULL

  for(i in 1:length(glmms_univs)){
    ifelse(is.na(glmms_univs[[i]]), glmm_to_rm <- c(glmm_to_rm,i),
           ifelse(is.na(summary(glmms_univs[[i]])$AICtab[1]),c(glmm_to_rm2,i),glmms_univs[[i]]))
  }
  glmm_to_rm <- c(glmm_to_rm,glmm_to_rm2)
  glmms_univs <- glmms_univs[-glmm_to_rm]

  ## Function which allows to put graphically and in a tidy way the results of the glmms in a form of dataframe,
  func2 <- function(x){
    #ret <- broom.mixed::tidy(x, conf.int = TRUE,  exponentiate = ifelse(indicator == "abundance",FALSE,TRUE))
    ret <- broom.mixed::tidy(x, conf.int = TRUE,  exponentiate = FALSE)
    ret$r2<-performance::r2_nakagawa(x, tolerance = tolerance, null_model = null_model)$R2_marginal ## Adding the value for the marginal R2 to evaluate the proportion of the variance explained by the fixed effect
    return(ret)
  }

  possible_b <- possibly(func2, otherwise = NULL) ## function which allows to return NULL in stead of an error message
  glmms_univs <- future_map(glmms_univs, possible_b) ## return a list of table with the results of the glmms

  glmms_univs<-do.call(rbind.data.frame, glmms_univs) %>% ## Allows to create a data frame which every line is the results of a model
    filter(effect == "fixed" & term!="(Intercept)") ## Select only the fixed effect on the results of models

  return(glmms_univs)
}


null_model <- glmmTMB(
  nb_femelles ~
    1 +
    (1 | site/trap_code) +
    (1 | year/week) +
    (1 | projet) +
    offset(log(exposure_days)),  # nombre de jours entre installation et relevé,
  ziformula = ~ 1,         # zero-inflation component
  family = nbinom2,
  data = df_glmm
)

null_model <- randomForest::randomForest(nb_femelles_jour ~ site+trap_code+year+week+projet, data = df_model)

oopts <- options(future.globals.maxSize = 1.0 * 1e9)  ## 1.0 GB
glmm_univ_abundance <- fun_compute_glmm_univ(df_glmm, "abundance", null_model, tolerance= 1e-12)
oopts <- options(future.globals.maxSize = 0.5 * 1e9)  ## 500 MB

write.csv(glmm_univ_abundance,'glmm_univ_abundance.csv', row.names = F)
