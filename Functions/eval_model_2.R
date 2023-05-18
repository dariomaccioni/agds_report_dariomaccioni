
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
  
  # get metrics tables
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

                