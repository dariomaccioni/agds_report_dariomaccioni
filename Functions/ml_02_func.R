## Loading the Datasets into R

eco_flux_davos <- readr::read_csv("../data/FLX_CH-Dav_FLUXNET2015_FULLSET_DD_1997-2014_1-3.csv") # Loading the dataset (.csv) into R and creating a table

eco_flux_laegern <- readr::read_csv("../data/FLX_CH-Lae_FLUXNET2015_FULLSET_DD_2004-2014_1-4.csv") # Loading the dataset (.csv) into R and creating a table

## Cleaning and splitting the Davos Dataset

library(lubridate)
library(dplyr)

eco_flux_davos <- readr::read_csv("../data/FLX_CH-Dav_FLUXNET2015_FULLSET_DD_1997-2014_1-3.csv") |>  
  
  # select only the variables we are interested in
  dplyr::select(TIMESTAMP,
                GPP_NT_VUT_REF,    # the dependent variable
                ends_with("_QC"),  # quality control info
                ends_with("_F"),   # includes all meteorological covariates
                -contains("JSB")   # irrelevant variable
  ) |>
  
  # convert TIMESTAMP to a better date mode
  dplyr::mutate(TIMESTAMP = ymd(TIMESTAMP)) |>
  
  # set all -9999 to NA for further analysis or elimination
  mutate(across(where(is.numeric), ~na_if(., -9999))) |>
  
  # retaining all data with at least or more than 80% of good-quality measurements and converting the other data (bad-data) into NA values
  dplyr::mutate(GPP_NT_VUT_REF = ifelse(NEE_VUT_REF_QC < 0.8, NA, GPP_NT_VUT_REF),
                TA_F           = ifelse(TA_F_QC        < 0.8, NA, TA_F),
                SW_IN_F        = ifelse(SW_IN_F_QC     < 0.8, NA, SW_IN_F),
                LW_IN_F        = ifelse(LW_IN_F_QC     < 0.8, NA, LW_IN_F),
                VPD_F          = ifelse(VPD_F_QC       < 0.8, NA, VPD_F),
                PA_F           = ifelse(PA_F_QC        < 0.8, NA, PA_F),
                P_F            = ifelse(P_F_QC         < 0.8, NA, P_F),
                WS_F           = ifelse(WS_F_QC        < 0.8, NA, WS_F)) |> 
  
  # drop quality control variables (no longer needed)
  dplyr::select(-ends_with("_QC"))

# splitting the data in a training (80%) and test set (20%)

set.seed(1982) # just for reproducibility
split <- rsample::initial_split(eco_flux_davos, prop = 0.8)
flux_davos_train <- rsample::training(split)
flux_davos_test <- rsample::testing(split)

# Cleaning and splitting the Laegern Dataset

library(lubridate)
library(dplyr)
eco_flux_laegern <- readr::read_csv("../data/FLX_CH-Lae_FLUXNET2015_FULLSET_DD_2004-2014_1-4.csv") |>  
  
  # select only the variables we are interested in
  dplyr::select(TIMESTAMP,
                GPP_NT_VUT_REF,    # the dependent variable
                ends_with("_QC"),  # quality control info
                ends_with("_F"),   # includes all all meteorological covariates
                -contains("JSB")   # irrelevant variable
  ) |>
  
  # convert TIMESTAMP to a better date mode
  dplyr::mutate(TIMESTAMP = ymd(TIMESTAMP)) |>
  
  # set all -9999 to NA for further analysis or elimination
  mutate(across(where(is.numeric), ~na_if(., -9999))) |>
  
  # retaining all data with at least or more than 80% of good-quality measurements and converting the other data (bad-data) into NA values
  dplyr::mutate(GPP_NT_VUT_REF = ifelse(NEE_VUT_REF_QC < 0.8, NA, GPP_NT_VUT_REF),
                TA_F           = ifelse(TA_F_QC        < 0.8, NA, TA_F),
                SW_IN_F        = ifelse(SW_IN_F_QC     < 0.8, NA, SW_IN_F),
                LW_IN_F        = ifelse(LW_IN_F_QC     < 0.8, NA, LW_IN_F),
                VPD_F          = ifelse(VPD_F_QC       < 0.8, NA, VPD_F),
                PA_F           = ifelse(PA_F_QC        < 0.8, NA, PA_F),
                P_F            = ifelse(P_F_QC         < 0.8, NA, P_F),
                WS_F           = ifelse(WS_F_QC        < 0.8, NA, WS_F)) |> 
  
  # drop quality control variables (no longer needed)
  dplyr::select(-ends_with("_QC"))

# splitting the data in a training (80%) and test set (20%)

set.seed(1982) # just for reproducibility
split <- rsample::initial_split(eco_flux_laegern, prop = 0.8)
flux_laegern_train <- rsample::training(split)
flux_laegern_test <- rsample::testing(split)

## Deleting irrelevant data

# Deleting LW_IN_F for all train and test sets

flux_davos_test$LW_IN_F <- NULL # Deleting the LW_IN_F row for the Davos test set

flux_davos_train$LW_IN_F <- NULL # Deleting the LW_IN_F row for the Davos train set

flux_laegern_test$LW_IN_F <- NULL # Deleting the LW_IN_F row for the Laegern test set

flux_laegern_train$LW_IN_F <- NULL # Deleting the LW_IN_F row for the Laegern train set

# Deleting P_F for the Laegern Dataset

flux_davos_test$P_F <- NULL # Deleting the P_F row for the Davos test set

flux_davos_train$P_F <- NULL # Deleting the P_F row for the Davos train set

flux_laegern_test$P_F <- NULL # Deleting the P_F row for the Laegern test set

flux_laegern_train$P_F <- NULL # Deleting the P_F row for the Laegern train set

# Deleting all missing values for both datasets

flux_davos_test <- stats::na.omit(flux_davos_test) # Deleting all rows with at least one value which equals "NA"

flux_davos_train <- stats::na.omit(flux_davos_train) # Deleting all rows with at least one value which equals "NA"

flux_laegern_test <- stats::na.omit(flux_laegern_test) # Deleting all rows with at least one value which equals "NA"

flux_laegern_train <- stats::na.omit(flux_laegern_train) # Deleting all rows with at least one value which equals "NA"

## Defining a model for both training sets

# model formulation for the davos trainset
model_davos_train <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + PA_F + WS_F + TA_F, 
                                     data = flux_davos_train) |> 
  recipes::step_center(all_numeric(), -all_outcomes()) |>
  recipes::step_scale(all_numeric(), -all_outcomes())

# model formulation for the laegern trainset
model_laegern_train <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + PA_F + WS_F + TA_F, 
                                       data = flux_laegern_train) |> 
  recipes::step_center(all_numeric(), -all_outcomes()) |>
  recipes::step_scale(all_numeric(), -all_outcomes())

## Using the caret function to find the best fitting model for KNN and Lm

# Caret function for Davos dataset (KNN and lm)

library(tidyverse)
library(recipes)
library(caret)

set.seed(1982)
caret_knn_davos_train <- caret::train(model_davos_train, 
                                      data = flux_davos_train |> drop_na(), 
                                      method = "knn",
                                      trControl = caret::trainControl(method = "cv", number = 10),
                                      tuneGrid = data.frame(k = c(2, 5, 10, 15, 20, 25, 30, 35, 40, 60, 100)),
                                      metric = "MAE")

caret_lm_davos_train <- caret::train(model_davos_train, 
                                     data = flux_davos_train |> drop_na(), 
                                     trControl = caret::trainControl(method = "none"),
                                     method = "lm"
)

# Caret Function for Laegern dataset (KNN and lm)

caret_knn_laegern_train <- caret::train(model_laegern_train, 
                                        data = flux_laegern_train |> drop_na(), 
                                        method = "knn",
                                        trControl = caret::trainControl(method = "cv", number = 10),
                                        tuneGrid = data.frame(k = c(2, 5, 10, 15, 20, 25, 30, 35, 40, 60, 100)),
                                        metric = "MAE")

caret_lm_laegern_train <- caret::train(model_laegern_train, 
                                       data = flux_laegern_train |> drop_na(), 
                                       trControl = caret::trainControl(method = "none"),
                                       method = "lm"
)

## evaluation of the model created by putting the training and test set into the formula. In this case for better visualization creation of a function which can contain two seperate models and their training and test sets. the Result is a 2x2 plot.

eval_model_2 <- function(mod_1, df_train, df_test, mod_2, df_train2, df_test2){
  # loading all packages needed
  library(dplyr)
  library(ggplot2)
  library(yardstick)
  library(cowplot)
  
  # add predictions to the data frames
  df_train <- df_train |> 
    drop_na()
  df_train$fitted <- predict(mod_1, newdata = df_train)
  
  df_test <- df_test |> 
    drop_na()
  df_test$fitted <- predict(mod_1, newdata = df_test)
  
  df_train2 <- df_train2 |> 
    drop_na()
  df_train2$fitted <- predict(mod_2, newdata = df_train2)
  
  df_test2 <- df_test2 |> 
    drop_na()
  df_test2$fitted <- predict(mod_2, newdata = df_test2)
  
  # get the table of the metrics
  metrics_train <- df_train |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  metrics_test <- df_test |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  metrics_train2 <- df_train2 |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  metrics_test2 <- df_test2 |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  # extract values from metrics tables
  rmse_train <- metrics_train |> 
    filter(.metric == "rmse") |> 
    pull(.estimate)
  rsq_train <- metrics_train |> 
    filter(.metric == "rsq") |> 
    pull(.estimate)
  
  rmse_test <- metrics_test |> 
    filter(.metric == "rmse") |> 
    pull(.estimate)
  rsq_test <- metrics_test |> 
    filter(.metric == "rsq") |> 
    pull(.estimate)
  
  rmse_train2 <- metrics_train2 |> 
    filter(.metric == "rmse") |> 
    pull(.estimate)
  rsq_train2 <- metrics_train2 |> 
    filter(.metric == "rsq") |> 
    pull(.estimate)
  
  rmse_test2 <- metrics_test2 |> 
    filter(.metric == "rmse") |> 
    pull(.estimate)
  rsq_test2 <- metrics_test2 |> 
    filter(.metric == "rsq") |> 
    pull(.estimate)
  
  # visualise as scatterplots
  # adding information of metrics as sub-titles
  plot_1 <- ggplot(data = df_train, aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq_train, digits = 2)) ~~
                              RMSE == .(format(rmse_train, digits = 3))),
         title = "Training set KNN") +
    theme_classic()
  
  plot_2 <- ggplot(data = df_test, aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq_test, digits = 2)) ~~
                              RMSE == .(format(rmse_test, digits = 3))),
         title = "Test set KNN") +
    theme_classic()
  
  plot_3 <- ggplot(data = df_train2, aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq_train2, digits = 2)) ~~
                              RMSE == .(format(rmse_train2, digits = 3))),
         title = "Training set lm") +
    theme_classic()
  
  plot_4 <- ggplot(data = df_test2, aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq_test2, digits = 2)) ~~
                              RMSE == .(format(rmse_test2, digits = 3))),
         title = "Test set lm") +
    theme_classic()
  
  out <- cowplot::plot_grid(plot_1, plot_2, plot_3, plot_4, ncol = 2)
  
  return(out)
}

## Task 4 - Pooling

### Task 4.1 - Pooling the training sets and the test sets together


pool_dav_lae_train <- rbind(flux_davos_train, flux_laegern_train)


pool_dav_lae_test <- rbind(flux_davos_test, flux_laegern_test)


### Task 4.2 - Creating a model for the pooled training sets


model_pool_train <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + PA_F + WS_F + TA_F, 
                                    data = pool_dav_lae_train) |> 
  recipes::step_center(all_numeric(), -all_outcomes()) |>
  recipes::step_scale(all_numeric(), -all_outcomes())


### Task 4.3 - Training the best lm model for pooled training data


caret_lm_pool_train <- caret::train(model_pool_train, 
                                    data = pool_dav_lae_train |> drop_na(), 
                                    method = "lm",
                                    trControl = caret::trainControl(method = "none"))

                