get_mae <- function(k) {
  # Data splitting
  set.seed(1982)  # for reproducibility
  split <- rsample::initial_split(half_hourly_fluxes_fixed, prop = 0.7, strata = "VPD_F")
  half_hourly_fluxes_train <- rsample::training(split)
  half_hourly_fluxes_test <- rsample::testing(split)
  
  # Model and pre-processing formulation, use all variables but LW_IN_F
  pp <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, 
                        data = half_hourly_fluxes_train |> drop_na()) |> 
    recipes::step_BoxCox(all_predictors()) |> 
    recipes::step_center(all_numeric(), -all_outcomes()) |>
    recipes::step_scale(all_numeric(), -all_outcomes())
  
  # Fit KNN model
  mod_knn <- caret::train(
    pp, 
    data = half_hourly_fluxes_train, 
    method = "knn",
    trControl = caret::trainControl(method = "none"),
    tuneGrid = data.frame(k = k),
    metric = "MAE"
  )
  
  # Evaluate model on test set
  new_data <- drop_na(half_hourly_fluxes_test)
  pred <- predict(mod_knn, newdata = new_data)
  obs <- new_data$GPP_NT_VUT_REF
  mae <- caret::MAE(pred, obs)
  
  # Evaluate model on training set
  pred_train <- predict(mod_knn, newdata = drop_na(half_hourly_fluxes_train))
  obs_train <- drop_na(half_hourly_fluxes_train)$GPP_NT_VUT_REF
  mae_train <- caret::MAE(pred_train, obs_train)
  
  return(list("mae_train" = mae_train, "mae_test" = mae))
  return(mae)
}
