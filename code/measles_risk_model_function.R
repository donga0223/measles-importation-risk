
library(xgboost)
library(caret)
library(Matrix)
library(tidycensus)
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(sf)
library(ggplot2)
library(tibble)
library(tigris)
library(lubridate)
library(purrr)
library(pROC)
library(precrec)
library(randomForest)

lag_sum_cal_G <- function(gravity_input_measles, delta, alpha, alpha_d, alpha_c){
  gravity_input_measles <- gravity_input_measles %>%
    mutate(across(where(is.numeric), ~replace_na(., 0)))
  df <- gravity_input_measles %>%
    dplyr::mutate(
      log_pop_x = log1p(population_x),
      log_visits = log1p(visits + 1e-5),
      log_dist = log1p(dist_km + 1e-5),
      log_catchment_flow = log1p(catchment_flow + 1e-5),
      
      
      G_lag_mob = (inc_lag_sum * log_pop_x^delta) / (log_visits^alpha),
      G_lag_dist = (inc_lag_sum * log_pop_x^delta) / (log_dist^alpha_d),
      G_lag_catchment = (inc_lag_sum * log_pop_x^delta) / (log_catchment_flow^alpha_c)
    ) %>%
    dplyr::group_by(county_x, as_of, first_onset, vaccine,
                    pct_age_0_4, pct_age_5_9, pct_age_10_17,
                    pct_age_18_64, pct_age_65_plus, mennonite) %>%
    dplyr::summarise(
      G_mob = sum(G_lag_mob, na.rm = TRUE),
      G_dist = sum(G_lag_dist, na.rm = TRUE),
      G_catchment = sum(G_lag_catchment, na.rm = TRUE),
      .groups = "drop"
    )
  return(df)
}


gbm_fitting <- function(gravity_input_measles, delta, alpha, alpha_d, alpha_c, gravity_test_measles, data_filter = NULL, G_only = FALSE){
  df_train <- lag_sum_cal_G(gravity_input_measles, delta, alpha, alpha_d, alpha_c) %>%
    drop_na()
  
  df_test <- lag_sum_cal_G(gravity_test_measles, delta, alpha, alpha_d, alpha_c) %>%
    drop_na()
  
  if(!is.null(data_filter)){
    df_train <- df_train %>%
      mutate(as_of = as.Date(as_of)) %>%  # 문자열을 날짜로 변환
      group_by(county_x) %>%
      slice_max(as_of, n = 1) %>%
      ungroup()
    
    df_test <- df_test %>%
      mutate(as_of = as.Date(as_of)) %>%  # 문자열을 날짜로 변환
      group_by(county_x) %>%
      slice_max(as_of, n = 1) %>%
      ungroup()
  }
  
  
  if(G_only){
    df_train_scaled <-df_train %>%
      dplyr::mutate(across(starts_with("G"), scale))
    
    train_X <- as.matrix(df_train_scaled %>% select(G_mob, G_dist, G_catchment))
    train_y <- df_train_scaled$first_onset
    dtrain <- xgb.DMatrix(data = train_X, label = train_y)
    
    df_test_scaled <-df_test %>%
      dplyr::mutate(across(starts_with("G"), scale))
    
    test_X <- as.matrix(df_test_scaled %>% select(G_mob, G_dist, G_catchment))
    test_y <- df_test_scaled$first_onset
    dtest <- xgb.DMatrix(data = test_X, label = test_y)
    
  }else{
    df_test_scaled <- df_test %>%
      dplyr::mutate(across(
        .cols = -c(county_x, as_of, first_onset),
        .fns = scale
      ))
    
    df_train_scaled <- df_train %>%
      dplyr::mutate(across(
        .cols = -c(county_x, as_of, first_onset),
        .fns = scale
      ))
    
    train_X <- as.matrix(df_train_scaled %>% select(-county_x, -as_of, -first_onset))
    train_y <- df_train$first_onset
    dtrain <- xgb.DMatrix(data = train_X, label = train_y)
    
    test_X <- as.matrix(df_test_scaled %>% select(-county_x, -as_of, -first_onset))
    test_y <- df_test$first_onset
    dtest <- xgb.DMatrix(data = test_X, label = test_y)
    
  }
  
  
  scale_pos_weight <- length(which(train_y==0)) / length(which(train_y==1))
  xgb_fit <- xgb.train(
    params = list(
      objective = "binary:logistic",
      eval_metric = "logloss",
      max_depth = 4,
      eta = 0.1,
      scale_pos_weight = scale_pos_weight
    ),
    data = dtrain,
    nrounds = 30,
    verbose = 0
  )
  
  preds <- predict(xgb_fit, dtest)
  return(list(preds = preds, df_test_scaled = df_test_scaled, test_y = test_y, xgb_fit = xgb_fit))
}

loss_fn_gbm <- function(params, gravity_input_measles, gravity_test_measles, data_filter = NULL) {
  delta <- params[1]
  alpha <- params[2]
  alpha_d <- params[3]
  alpha_c <- params[4]
  
  gmb_res <- gbm_fitting(gravity_input_measles, delta, alpha, alpha_d, alpha_c, gravity_test_measles, data_filter = NULL, G_only = TRUE)
  preds = gmb_res$preds
  test_y = gmb_res$test_y
  
  logloss <- -mean(test_y * log(preds + 1e-6) + (1 - test_y) * log(1 - preds + 1e-6))
  
  return(logloss)
}


ROC_PRC_plot <- function(df_test_scaled, model, thresholds_to_plot = c(0.5, 0.4, 0.3, 0.2, 0.1, 0.05)){
  if (length(unique(df_test_scaled$first_onset)) == 2) {
    roc_obj <- pROC::roc(df_test_scaled$first_onset, df_test_scaled$predicted_prob)
    print(roc_obj)
    
    pROC::auc(roc_obj)
    
    plot(roc_obj, col = "blue", lwd = 2, main = paste("ROC Curve for ", model, " model", sep = ""))
    
    for (thr in thresholds_to_plot) {
      coords_point <- coords(roc_obj, x = thr, input = "threshold", ret = c("specificity", "sensitivity"))
      fpr <- 1 - coords_point["specificity"]
      tpr <- coords_point["sensitivity"]
      
      points(1-fpr, tpr, col = "red", pch = 19)
      text(1-fpr, tpr, labels = paste0("t=", thr), pos = 4, cex = 0.8)
    }
    
    actual <- df_test_scaled$first_onset
    predicted_prob <- df_test_scaled$predicted_prob
    
    # precrec용 데이터 객체 생성
    precrec_obj <- evalmod(scores = predicted_prob, labels = actual)
    
    # PR curve 그리기
    p <- autoplot(precrec_obj, "PR") +
      ggtitle("Precision-Recall Curve") +
      scale_x_reverse() + 
      theme_minimal()
    
    print(p)
    
    print(precrec::auc(precrec_obj))
  }else{
    message("Skipping ROC computation: `first_onset` has only one class.")
  }
  
  print(table(df_test_scaled$first_onset, df_test_scaled$threshold_0.5))
  print(table(df_test_scaled$first_onset, df_test_scaled$threshold_0.2))
  print(table(df_test_scaled$first_onset, df_test_scaled$threshold_0.1))
  
}



gbm_fitting_final <- function(train_date, test_date, data_filter = NULL, 
                              G_only = FALSE, test_3weeks = FALSE){
  gravity_input_measles <- gravity_input %>%
    mutate(inc_lag_sum = rowSums(across(c(inc_lag1, inc_lag2, inc_lag3)), na.rm = TRUE)) %>%
    filter(as_of <= train_date)
  if(test_3weeks){
    gravity_test_measles <- gravity_input %>%
      mutate(inc_lag_sum = rowSums(across(c(inc_lag1, inc_lag2, inc_lag3)), na.rm = TRUE)) %>%
      filter(as_of >= test_date, as_of <= as.Date(test_date) + 14)
  }else{
    gravity_test_measles <- gravity_input %>%
      mutate(inc_lag_sum = rowSums(across(c(inc_lag1, inc_lag2, inc_lag3)), na.rm = TRUE)) %>%
      filter(as_of >= test_date)
  }
  
  
  result <- optim(par = c(0.2, 0.2, 0.2, 0.2), 
                  fn = function(par) loss_fn_gbm(par, gravity_input_measles,
                                                 gravity_test_measles), 
                  method = "L-BFGS-B",
                  lower = c(-10, -10, -5, -5), upper = c(5,10, 10, 10))
  print(result)
  
  gbm_res <- gbm_fitting(gravity_input_measles, delta = result$par[1], alpha = result$par[2], 
                         alpha_d = result$par[3], alpha_c = result$par[4], gravity_test_measles,
                         data_filter = data_filter, G_only = FALSE)
  preds = gbm_res$preds
  df_test_scaled = gbm_res$df_test_scaled
  xgb_fit = gbm_res$xgb_fit
  
  
  df_test_scaled_gbm <- df_test_scaled %>%
    mutate(predicted_prob = preds,
           threshold_0.5 = ifelse(predicted_prob < 0.5, 0, 1),
           threshold_0.2 = ifelse(predicted_prob < 0.2, 0, 1),
           threshold_0.1 = ifelse(predicted_prob < 0.1, 0, 1))
  

  ROC_PRC_plot(df_test_scaled_gbm, model = "GBM", thresholds_to_plot = c(0.5, 0.4, 0.3, 0.2, 0.1, 0.05))
    
  xgb.importance(model = xgb_fit) %>% 
    xgb.plot.importance()
  
  return(list(df_test_scaled_gbm = df_test_scaled_gbm, result = result))
  
}


rf_fit <- function(train_date, test_date,
                   delta, alpha, alpha_d, alpha_c,
                   data_filter = NULL, test_3weeks = FALSE){
  gravity_input_measles <- gravity_input %>%
    mutate(inc_lag_sum = rowSums(across(c(inc_lag1, inc_lag2, inc_lag3)), na.rm = TRUE)) %>%
    filter(as_of <= train_date)
  if(test_3weeks){
    gravity_test_measles <- gravity_input %>%
      mutate(inc_lag_sum = rowSums(across(c(inc_lag1, inc_lag2, inc_lag3)), na.rm = TRUE)) %>%
      filter(as_of >= test_date, as_of <= as.Date(test_date) + 14)
  }else{
    gravity_test_measles <- gravity_input %>%
      mutate(inc_lag_sum = rowSums(across(c(inc_lag1, inc_lag2, inc_lag3)), na.rm = TRUE)) %>%
      filter(as_of >= test_date)
  }
  
  df_train <- lag_sum_cal_G(gravity_input_measles, delta = delta, alpha = alpha, 
                            alpha_d = alpha_d, alpha_c = alpha_c) %>%
    drop_na()
  
  df_test <- lag_sum_cal_G(gravity_test_measles, 
                           delta = delta, alpha = alpha, 
                           alpha_d = alpha_d, alpha_c = alpha_c) %>%
    drop_na()
  
  
  if(!is.null(data_filter)){
    df_train <- df_train %>%
      mutate(as_of = as.Date(as_of)) %>%  
      group_by(county_x) %>%
      slice_max(as_of, n = 1) %>%
      ungroup()
    
    df_test <- df_test %>%
      mutate(as_of = as.Date(as_of)) %>%  
      group_by(county_x) %>%
      slice_max(as_of, n = 1) %>%
      ungroup()
  }
  
  df_train_scaled <- df_train %>%
    dplyr::mutate(across(
      .cols = -c(county_x, as_of, first_onset),
      .fns = scale
    ))
  
  df_test_scaled <- df_test %>%
    dplyr::mutate(across(
      .cols = -c(county_x, as_of, first_onset),
      .fns = scale
    ))
  
  
  rf_fit <- randomForest(
    factor(first_onset) ~ G_mob + G_dist + G_catchment + vaccine + 
      pct_age_0_4 + pct_age_5_9 + pct_age_10_17 + pct_age_18_64 + pct_age_65_plus + 
      mennonite,
    data = df_train_scaled,
    ntree = 500,
    sampsize = c(length(which(df_train_scaled$first_onset==0)), length(which(df_train_scaled$first_onset==1)))  # 0 class: 100, 1 class: 20
  )
  
  preds_rf <- predict(rf_fit, newdata = df_test_scaled, type = "prob")[,2]
  
  df_test_scaled_rf <- df_test_scaled %>%
    mutate(predicted_prob = preds_rf,
           threshold_0.5 = ifelse(predicted_prob < 0.5, 0, 1),
           threshold_0.2 = ifelse(predicted_prob < 0.2, 0, 1),
           threshold_0.1 = ifelse(predicted_prob < 0.1, 0, 1))
  
  ROC_PRC_plot(df_test_scaled_rf, model = "RF", thresholds_to_plot = c(0.5, 0.4, 0.3, 0.2, 0.1, 0.05))
  
  return(df_test_scaled_rf)
}

