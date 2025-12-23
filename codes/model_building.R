########################### Opening packages

library(tidyverse)
library(furrr)
library(randomForest)
library(caret)

########################### Open dataset containing the dependent variables
trapping_data <- read.csv("entomo_data/fusion_data/data_mosquitoes.csv")
trapping_data$X <- NULL
trapping_data$year <- year(trapping_data$date_releve)
trapping_data$month <- month(trapping_data$date_releve)

trapping_data <- trapping_data %>%
  mutate(week = as.factor(week)) %>%
  mutate(year = as.factor(year))%>%
  mutate(month = as.factor(month))


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

# remove columns with more than 30% NA values.

predictors_clean <- predictors %>%
  #mutate_if(is.numeric, ~scale(., center = TRUE, scale = TRUE)[,1]) %>%
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(is.nan(.x) | is.infinite(.x), NA, .x)
    )
  ) %>%
  select(
    where(~ mean(is.na(.x)) <= 0.25)
  )


########################### combine both datasets
df_model <- trapping_data %>%
  left_join(predictors_clean)

predictors_clean <- predictors_clean %>%
  dplyr::select(-c("trap_code","site","X","X.1"))

predictors <- predictors %>%
  dplyr::select(-c("trap_code","site","X","X.1"))


df_model <- df_model %>% filter(!is.na(nb_femelles_jour))

df_model$nb_femelles_jour = log(df_model$nb_femelles_jour+1)




## Random forest v1

cv_col <- "site"

# null model
indices_cv <- CAST::CreateSpacetimeFolds(df_model, spacevar = cv_col,k = length(unique(unlist(df_model[,cv_col]))))

tr <- trainControl(
  method = "cv",
  index = indices_cv$index,
  indexOut = indices_cv$indexOut
)

null_model <- caret::train(x = df_model[,c("trap_code","year","month","projet")], y = df_model$nb_femelles_jour, method = "rf", tuneLength = 3, trControl = tr)

rmse_null_model = min(null_model$results$RMSE)
Rsquared_null_model = max(null_model$results$Rsquared)
MAE_null_model = min(null_model$results$MAE)


plan(multisession, workers = 20)

rf_univ_results <- furrr::future_map_dfr(
  colnames(predictors_clean),
  possibly(
    function(th_predictor) {

      th_df_model <- df_model %>% filter(!is.na({{th_predictor}}))
      indices_cv <- CAST::CreateSpacetimeFolds(th_df_model, spacevar = cv_col,k = length(unique(unlist(th_df_model[,cv_col]))))
      tr <- trainControl(
        method = "cv",
        index = indices_cv$index,
        indexOut = indices_cv$indexOut
      )
      th_model <-  caret::train(x = th_df_model[,c("trap_code","year","month","projet",th_predictor)], y = th_df_model$nb_femelles_jour, method = "rf", tuneLength = 3, trControl = tr, preProcess = c("center","scale"))

      tibble(
        predictor = th_predictor,
        rmse = min(th_model$results$RMSE),
        Rsquared = max(th_model$results$Rsquared),
        MAE = min(th_model$results$MAE)
      )
    },
    otherwise = tibble(
      predictor = th_predictor,
      rmse = NA_real_,
      Rsquared = NA_real_,
      MAE = NA_real_
    )
  )
)



rf_univ_results <- rf_univ_results %>%
  mutate(rmse_diff_null_model = rmse_null_model - rmse ,
         Rsquared_diff_null_model = Rsquared - Rsquared_null_model,
         MAE_diff_null_model = MAE_null_model - MAE)

rf_univ_results <- rf_univ_results %>%
  filter(!is.na(rmse)) %>%
  filter(rmse_diff_null_model>0)

write.csv(rf_univ_results,'rf_univ_results_method1.csv', row.names = F)




## Random forest v2


plan(multisession)

n_runs <- 10

rf_univ_results <- future_map_dfr(
  colnames(predictors_clean),
  possibly(
    function(th_predictor) {

      imp_runs <- map_dbl(
        1:n_runs,
        function(i) {

          set.seed(1000 + i)  # reproducible but different runs

          th_model <- randomForest(
            as.formula(
              paste0(
                "nb_femelles_jour ~ trap_code + year + month + projet + ",
                th_predictor
              )
            ),
            data = df_model,
            na.action = na.omit,
            importance = TRUE
          )

          imp <- importance(th_model, type = 1)
          imp <- data.frame(imp, predictor = rownames(imp))
          imp <- imp %>% filter(predictor == th_predictor)
          imp$X.IncMSE
        }
      )

      tibble(
        predictor = th_predictor,
        imp_mean = mean(imp_runs, na.rm = TRUE),
        imp_sd   = sd(imp_runs, na.rm = TRUE)
        )
    },
    otherwise = function(th_predictor) {
      tibble(
        predictor = th_predictor,
        imp_mean = NA_real_,
        imp_sd   = NA_real_      )
    }
  )
)

rf_univ_results <- rf_univ_results %>%
  mutate(imp_z = scale(imp_mean, center = F)[,1])


write.csv(rf_univ_results,'rf_univ_results.csv', row.names = F)

















### glmm

# df_glmm <- df_model %>%
#   mutate(exposure_days = as.character(exposure_days), nb_femelles=as.character(nb_femelles), nb_males=as.character(nb_males), nb_tot=as.character(nb_tot),nb_femelles_jour=as.character(nb_femelles_jour), nb_males_jour=as.character(nb_males_jour), nb_moustiques_jour=as.character(nb_moustiques_jour)) %>%
#   mutate_if(is.numeric, ~scale(., center = TRUE, scale = FALSE)) %>%  # we center and scale to be able to compare the magnitudes (centering also helps with allowing easier interpretation of variable coefficients associated with different magnitudes, e.g. when one regressor is measured on a very small order, while another on a very large order.  )
#   mutate(exposure_days = as.numeric(exposure_days), nb_femelles=as.numeric(nb_femelles), nb_males=as.numeric(nb_males), nb_tot=as.numeric(nb_tot),nb_femelles_jour=as.numeric(nb_femelles_jour), nb_males_jour=as.numeric(nb_males_jour), nb_moustiques_jour=as.numeric(nb_moustiques_jour))


fun_compute_glmm_univ <- function(df_glmm,indicator, null_model, tolerance){

  func <- function(x){
    ret <- glmmTMB(
      as.formula(paste0("nb_femelles_jour ~ ",x," +
      (1 | trap_code) +
      (1 | year/month) +
      (1 | projet)")),
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
  nb_femelles_jour ~
    1 +
    (1 | trap_code) +
    (1 | year/month) +
    (1 | projet),
  ziformula = ~ 1,         # zero-inflation component
  family = nbinom2,
  data = df_glmm
)




oopts <- options(future.globals.maxSize = 1.0 * 1e9)  ## 1.0 GB
glmm_univ_abundance <- fun_compute_glmm_univ(df_glmm, "abundance", null_model, tolerance= 1e-12)
oopts <- options(future.globals.maxSize = 0.5 * 1e9)  ## 500 MB

write.csv(glmm_univ_abundance,'glmm_univ_abundance.csv', row.names = F)



