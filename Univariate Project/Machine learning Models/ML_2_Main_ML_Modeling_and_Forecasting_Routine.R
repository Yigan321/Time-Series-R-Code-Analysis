#############################################################################
# Advanced Forecasting System                       
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ML_2_Main_ML_Modeling_and_Forecasting_Routine
#############################################################################

# Remove all objects from Global Environment.
rm(list=ls())

# Supress warning messages.
options(warn=-1)

# Set a working directory
setwd("C:/Users/Derek/Documents/R Scripts/Forecasting System/R Scripts/")

####################################################################################
# Establish Connection to SQL Server
####################################################################################


library(RODBC)

SQLServer_Forecast <- odbcConnect(dsn="Forecast_SQL", uid="Derek", pwd="password")

##################################################################################
# Machine Learning - Forecasting Methodologies & Main Routine
##################################################################################


library(date) #1.2-39
library(lubridate) #1.7.9
library(dplyr) #1.0.1 
library(xgboost) #1.1.1.1 
library(DMwR) #0.4.1
library(zoo) #1.8-8
library(tensorflow) #1.14.0
library(keras) # 2.2.4
#library(neuralnet)

library(reticulate)


use_condaenv(condaenv = "tf_test", conda = "C:/Users/derek/Anaconda3/envs/tf_test/python.exe")
Sys.setenv(RETICULATE_PYTHON = "C:/Users/derek/Anaconda3/envs/tf_test/python.exe")
Sys.setenv(TENSORFLOW_PYTHON="C:/Users/derek/Anaconda3/envs/tf_test/python.exe")


# TensorFlow Dependencies
# install.packages("reticulate") #1.16
# install.packages("tfruns") #1.4

# Keras Dependencies
# install.packages("zeallot") #0.1.0


input_data <- sqlFetch(SQLServer_Forecast, "rvfc.datainput_ml_factor")
modelrun <- sqlFetch(SQLServer_Forecast, "rvfc.result_modelruntimeseries")
table_result_scenarios <- sqlFetch(SQLServer_Forecast, "rvfc.result_scenarios")


# Fix the factor to the chracter format
input_data$Dimension1 <- as.character(input_data$Dimension1)
input_data$Dimension2 <- as.character(input_data$Dimension2)
input_data$Dataset <- as.character(input_data$Dataset)

modelrun$Master_Run_ID <- as.character(modelrun$Master_Run_ID)
modelrun$Run_ID <- as.character(modelrun$Run_ID)
modelrun$Scenario <- as.character(modelrun$Scenario)
modelrun$Technique <- as.character(modelrun$Technique)
modelrun$Method <- as.character(modelrun$Method)
modelrun$Model_Run_Date <- as.character(modelrun$Model_Run_Date)
modelrun$Dimension1 <- as.character(modelrun$Dimension1)
modelrun$Dimension2 <- as.character(modelrun$Dimension2)
modelrun$Threshold <- as.character(modelrun$Threshold)
modelrun$Train_Start_Month <- as.character(modelrun$Train_Start_Month)
modelrun$Train_End_Month <- as.character(modelrun$Train_End_Month)
modelrun$Test_Start_Month <- as.character(modelrun$Test_Start_Month)
modelrun$Test_End_Month <- as.character(modelrun$Test_End_Month)
modelrun$Local_Currency <- as.character(modelrun$Local_Currency)


table_result_scenarios$Master_Run_ID <- as.character(table_result_scenarios$Master_Run_ID)
table_result_scenarios$Run_ID <- as.character(table_result_scenarios$Run_ID)
table_result_scenarios$Dimension1 <- as.character(table_result_scenarios$Dimension1)
table_result_scenarios$Dimension2 <- as.character(table_result_scenarios$Dimension2)
table_result_scenarios$Scenario <- as.character(table_result_scenarios$Scenario)
table_result_scenarios$Currency <- as.character(table_result_scenarios$Currency)


dim_index = as.data.frame(input_data[,c("Dimension1", "Dimension2")] %>% dplyr:::group_by(Dimension1,Dimension2) %>% dplyr:::distinct())
dim_index$ind = seq(1:nrow(dim_index))


# Define the error metric functions

RMSE = function(m, o){
  sqrt(mean((m-o)^2))
}

mapefunc <- function(train, test){
  train <- ifelse(train == 0, 0.01, train)
  mape <- abs((train - test) / train)
  mape <- ifelse(mape > 1, 1, mape)
  finalmape <- mean(mape)
  return(finalmape)
}

########################################################################
# XGBOOST Modeling
########################################################################

### add error handeling
apply_xgb <- function(k){
  
  tryCatch({
    dim_id_1 <- dim_index[dim_index$ind == k, 'Dimension1']
    dim_id_2 <- dim_index[dim_index$ind == k, 'Dimension2']
    
    set.seed(12345)
    
    #k <- match(dim_id, unique(input_data$Dimension1))
    ###model run
    model_run <- modelrun[modelrun$Dimension1 == dim_id_1 & modelrun$Dimension2 == dim_id_2 & modelrun$Scenario == 'Revenue', ][1, ]
    Master_Run_ID <- model_run$Master_Run_ID
    Run_ID <- paste('M', as.character(format((Sys.time() + seconds(k)), '%Y%m%d%H%M%S')), '01', sep='')
    model_run$Run_ID <- Run_ID
    model_run$Technique <- 'Machine Learning'
    model_run$Model_Run_Date <- as.character(Sys.Date())
    model_run$Method <- 'XGBoost'
    
    train = input_data[input_data$Dataset == "TrainSet" & input_data$Dimension1 == dim_id_1 & input_data$Dimension2 == dim_id_2,]
    val = input_data[input_data$Dataset == "ValSet" & input_data$Dimension1 == dim_id_1 & input_data$Dimension2 == dim_id_2,]
    test = input_data[input_data$Dataset == "TestSet" & input_data$Dimension1 == dim_id_1 & input_data$Dimension2 == dim_id_2,]
    
    train <- train[,!is.na(train[1,])]
    val <- val[,!is.na(val[1,])]
    test <- test[,!is.na(test[1,])]
    
    dtrain <- xgb.DMatrix(as.matrix(train[, !names(train) %in% c('Dimension1', 'Dimension2', 'Dataset', 'Sale')]), label=as.matrix(train[, 'Sale']))
    dval <- xgb.DMatrix(as.matrix(val[, !names(val) %in% c('Dimension1', 'Dimension2', 'Dataset', 'Sale')]), label=as.matrix(val[, 'Sale']))
    dtest <- xgb.DMatrix(as.matrix(test[, !names(test) %in% c('Dimension1', 'Dimension2', 'Dataset', 'Sale')]), label=as.matrix(test[, 'Sale']))
    
    param <- list(booster = "gbtree", 
                  eval_metric = c("rmse"),
                  eta=0.01, gamma=0, max_depth=3, min_child_weight=1, 
                  subsample=0.6, colsample_bytree=0.6)
    
    bst <- xgb.train(params = param,
                     data = dtrain,
                     nrounds = 200,
                     maximize = FALSE
    )
    
    tr_pred = predict(bst, dtrain)
    val_pred = predict(bst, dval)
    te_pred = predict(bst, dtest)
    
    ####ERROR_MATRIX
    Error_matrix <- data.frame(Master_Run_ID = character(0), Run_ID = character(0), Technique = character(0), Method = character(0), 
                               Dataset = character(0), Dimension1 = character(0), Dimension2 = character(0),
                               MAE = numeric(0), RMSE = numeric(0), MAPE = numeric(0))
    
    Error_matrix_train <- data.frame(Master_Run_ID, Run_ID)
    Error_matrix_train$Dataset <- 'TrainSet'
    Error_matrix_train$MAE <- regr.eval(getinfo(dtrain, 'label'), tr_pred, stats='mae')
    Error_matrix_train$RMSE <- RMSE(getinfo(dtrain, 'label'), tr_pred)
    Error_matrix_train$MAPE <- mapefunc(getinfo(dtrain, 'label'), tr_pred)
    
    Error_matrix_test <- data.frame(Master_Run_ID, Run_ID)
    Error_matrix_test$Dataset <- 'TestSet'
    Error_matrix_test$MAE <- regr.eval(getinfo(dval, 'label'), val_pred, stats='mae')
    Error_matrix_test$RMSE <- RMSE(getinfo(dval, 'label'), val_pred)
    Error_matrix_test$MAPE <- mapefunc(getinfo(dval, 'label'), val_pred)
    
    Error_matrix <- rbind(Error_matrix_train, Error_matrix_test)
    Error_matrix$Technique <- rep('MachineLearning', 2)
    Error_matrix$Method <- rep('XGBoost', 2)
    Error_matrix$Dimension1 <- rep(dim_id_1, 2)
    Error_matrix$Dimension2 <- rep(dim_id_2, 2)
    Error_matrix <- Error_matrix[, c('Master_Run_ID', 'Run_ID', 'Technique', 'Method', 'Dataset', 'Dimension1', 'Dimension2',
                                     'MAE', 'RMSE', 'MAPE')]
    
    
    ####FORECAST
    test_forecast <- data.frame(Master_Run_ID = character(12), Run_ID = character(12),
                                Dimension1 = character(12), Dimension2 = character(12),
                                Period = character(12), Point_Forecast = numeric(12), Lower_Level_1 = numeric(12),
                                Upper_Level_1 =numeric(12), Lower_Level_2=numeric(12), Upper_Level_2=numeric(12)) 
    
    test_forecast$Master_Run_ID = rep(Master_Run_ID, 12)
    test_forecast$Run_ID = rep(Run_ID, 12)
    test_forecast$Dimension1 = rep(dim_id_1, 12)
    test_forecast$Dimension2 = rep(dim_id_2, 12)
    test_forecast$Period = seq(1, 12, 1)
    test_forecast$Point_Forecast = te_pred
    test_forecast$Lower_Level_1 <- te_pred * (1 - 0.05)
    test_forecast$Upper_Level_1 <- te_pred * (1 + 0.05)
    test_forecast$Lower_Level_2 <- te_pred * (1 - 0.2)
    test_forecast$Upper_Level_2 <- te_pred * (1 + 0.2)
    
    ####VALIDATION RESULTS
    Validation <- data.frame(Run_ID = character(0), Method = character(0), Dataset = character(0), Dimension1 = character(0), Dimension2 = character(0),
                             Point_forecast = numeric(0), Actual_value = numeric(0))
    Validation <- data.frame(val[, 'Sale'], val_pred )
    names(Validation) <- c('Actual_value', 'Point_forecast')
    Validation$Run_ID <- rep(Run_ID, nrow(Validation))
    Validation$Method <- rep('XGBoost', nrow(Validation))
    Validation$Dataset <- rep('TestSet', nrow(Validation))
    Validation$Dimension1 <- rep(dim_id_1, nrow(Validation))
    Validation$Dimension2 <- rep(dim_id_2, nrow(Validation))
    Validation <- Validation[, c('Run_ID', 'Method', 'Dataset', 'Dimension1', 'Dimension2', 'Point_forecast', 'Actual_value')]
    
    ######feature importance
    imp <- xgb.importance(feature_names = colnames(dtrain), model = bst)
    imp_Gain <- imp[, c('Feature', 'Gain')]
    imp_Gain$Master_Run_ID <- rep(Master_Run_ID, nrow(imp_Gain))
    imp_Gain$Run_ID <- rep(Run_ID, nrow(imp_Gain))
    imp_Gain$Dimension1 <- rep(model_run$Dimension1, nrow(imp_Gain))
    imp_Gain$Dimension2 <- rep(model_run$Dimension2, nrow(imp_Gain))
    imp_Gain <- imp_Gain[, c('Master_Run_ID', 'Run_ID', 'Dimension1', 'Dimension2', 'Feature', 'Gain')]
    
    rm(bst, imp, Error_matrix_train, Error_matrix_test)
    return(list(Error_matrix, imp_Gain, test_forecast, Validation, as.data.frame(model_run)))
  },
  error = function(e){
    return (list(NULL, NULL, NULL, NULL, NULL))
    #return (list(e, e, e, e, e))
  })
}

########################################################################
# Neural Network (AI) Modeling
########################################################################

nn_modeling <- function(k) {
  tryCatch({
    dim_id_1 <- dim_index[dim_index$ind == k, 'Dimension1']
    dim_id_2 <- dim_index[dim_index$ind == k, 'Dimension2']
    
    set.seed(12345)
    
    model_run <- modelrun[modelrun$Dimension1 == dim_id_1 & modelrun$Dimension2 == dim_id_2 & modelrun$Scenario == 'Revenue', ][1, ]
    Master_Run_ID <- model_run$Master_Run_ID
    Run_ID <- paste('M', as.character(format((Sys.time() + seconds(k)), '%Y%m%d%H%M%S')), '02', sep='')
    model_run$Run_ID <- Run_ID
    model_run$Technique <- 'Machine Learning'
    model_run$Model_Run_Date <- as.character(Sys.Date())
    model_run$Method <- 'Neural Network'
    
    train = input_data[input_data$Dataset == "TrainSet" & input_data$Dimension1 == dim_id_1 & input_data$Dimension2 == dim_id_2,]
    val = input_data[input_data$Dataset == "ValSet" & input_data$Dimension1 == dim_id_1 & input_data$Dimension2 == dim_id_2,]
    test = input_data[input_data$Dataset == "TestSet" & input_data$Dimension1 == dim_id_1 & input_data$Dimension2 == dim_id_2,]
    
    train <- train[,!is.na(train[1,])]
    val <- val[,!is.na(val[1,])]
    test <- test[,!is.na(test[1,])]
    
    X_train <- as.matrix(train[, !names(train) %in% c('Dimension1', 'Dimension2', 'Dataset', 'Sale')])
    y_train <- as.vector(train[, 'Sale'])
    X_val <- as.matrix(val[, !names(train) %in% c('Dimension1', 'Dimension2', 'Dataset', 'Sale')])
    y_val <- as.vector(val[, 'Sale'])
    X_test <- as.matrix(test[, !names(train) %in% c('Dimension1', 'Dimension2', 'Dataset', 'Sale')])
    
    tf$set_random_seed(12345)
    use_session_with_seed(12345)
    
    model <- keras_model_sequential() %>%
      layer_dense(units = 16, activation = "relu", input_shape = dim(X_train)[2], kernel_regularizer=regularizer_l1(0.01), 
                  kernel_initializer=initializer_random_uniform(minval = -0.25, maxval = 0.25, seed = 12345)) %>% 
      #          layer_dense(units = 8, activation = "relu", kernel_initializer=initializer_random_uniform(minval = -0.25, maxval = 0.25, seed = 12345)) %>%
      
      layer_dense(units = 1, kernel_regularizer=regularizer_l1(0.01), kernel_initializer=initializer_random_uniform(minval = -0.25, maxval = 0.25, seed = 12345))
    
    
    model %>% compile(
      loss = "mse",
      optimizer = optimizer_adam(),
      #       optimizer = optimizer_rmsprop(),
      metrics = "mean_absolute_error"
    )
    
    epochs <- 100
    # Fit the model and store training stats
    model %>% fit(
      as.matrix(X_train),
      as.vector(y_train),
      epochs = epochs,
      callbacks = list(callback_early_stopping()),
      validation_data = list(X_val, y_val),
      verbose = 0
    )
    
    tr_pred <- as.vector(model %>% predict(X_train))
    val_pred <- as.vector(model %>% predict(X_val))
    te_pred <- as.vector(model %>% predict(X_test))
    
    ####ERROR_MATRIX
    Error_matrix <- data.frame(Master_Run_ID = character(0), Run_ID = character(0), Technique = character(0), Method = character(0), 
                               Dataset = character(0), Dimension1 = character(0), Dimension2 = character(0),
                               MAE = numeric(0), RMSE = numeric(0), MAPE = numeric(0))
    
    Error_matrix_train <- data.frame(Master_Run_ID, Run_ID)
    Error_matrix_train$Dataset <- 'TrainSet'
    Error_matrix_train$MAE <- regr.eval(train$Sale, tr_pred, stats='mae')
    Error_matrix_train$RMSE <- RMSE(train$Sale, tr_pred)
    Error_matrix_train$MAPE <- mapefunc(train$Sale, tr_pred)
    
    Error_matrix_test <- data.frame(Master_Run_ID, Run_ID)
    Error_matrix_test$Dataset <- 'TestSet'
    Error_matrix_test$MAE <- regr.eval(val$Sale, val_pred, stats='mae')
    Error_matrix_test$RMSE <- RMSE(val$Sale, val_pred)
    Error_matrix_test$MAPE <- mapefunc(val$Sale, val_pred)
    
    
    Error_matrix <- rbind(Error_matrix_train, Error_matrix_test)
    Error_matrix$Technique <- rep('MachineLearning', 2)
    Error_matrix$Method <- rep('NeuralNet', 2)
    Error_matrix$Dimension1 <- rep(dim_id_1, 2)
    Error_matrix$Dimension2 <- rep(dim_id_2, 2)
    Error_matrix <- Error_matrix[, c('Master_Run_ID', 'Run_ID', 'Technique', 'Method', 'Dataset', 'Dimension1', 'Dimension2',
                                     'MAE', 'RMSE', 'MAPE')]
    
    
    ####FORECAST
    test_forecast <- data.frame(Master_Run_ID = character(12), Run_ID = character(12),
                                Dimension1 = character(12), Dimension2 = character(12),
                                Period = character(12), Point_Forecast = numeric(12), Lower_Level_1 = numeric(12),
                                Upper_Level_1 =numeric(12), Lower_Level_2=numeric(12), Upper_Level_2=numeric(12)) 
    
    test_forecast$Master_Run_ID = rep(Master_Run_ID, 12)
    test_forecast$Run_ID = rep(Run_ID, 12)
    test_forecast$Dimension1 = rep(dim_id_1, 12)
    test_forecast$Dimension2 = rep(dim_id_2, 12)
    test_forecast$Period = seq(1, 12, 1)
    test_forecast$Point_Forecast = te_pred
    test_forecast$Lower_Level_1 <- te_pred * (1 - 0.05)
    test_forecast$Upper_Level_1 <- te_pred * (1 + 0.05)
    test_forecast$Lower_Level_2 <- te_pred * (1 - 0.2)
    test_forecast$Upper_Level_2 <- te_pred * (1 + 0.2)
    
    ####VALIDATION RESULTS
    Validation <- data.frame(Run_ID = character(0), Method = character(0), Dataset = character(0), Dimension1 = character(0), Dimension2 = character(0),
                             Point_forecast = numeric(0), Actual_value = numeric(0))
    Validation <- data.frame(val[, 'Sale'], val_pred)
    names(Validation) <- c('Actual_value', 'Point_forecast')
    Validation$Run_ID <- rep(Run_ID, nrow(Validation))
    Validation$Method <- rep('NeuralNet', nrow(Validation))
    Validation$Dataset <- rep('TestSet', nrow(Validation))
    Validation$Dimension1 <- rep(dim_id_1, nrow(Validation))
    Validation$Dimension2 <- rep(dim_id_2, nrow(Validation))
    Validation <- Validation[, c('Run_ID', 'Method', 'Dataset', 'Dimension1', 'Dimension2', 'Point_forecast', 'Actual_value')]
    
    rm(Error_matrix_train, Error_matrix_test)
    return(list(Error_matrix, test_forecast, Validation, as.data.frame(model_run)))
  }, 
  error = function(e){
    return (list(NULL, NULL, NULL, NULL))
    #return (list(e, e, e, e))
  })
}  

# Apply XGBOOST

set.seed(20195)

output <- sapply(1:nrow(dim_index), apply_xgb)
# output <- sapply(1:3, apply_xgb)


Error_Matrix <- do.call(rbind, output[1, ])
Feature_Importance <- do.call(rbind, output[2, ])
Test_Forecast <- do.call(rbind, output[3, ])
Validation <- do.call(rbind, output[4, ])
model_run <- do.call(rbind, output[5, ])
rownames(Error_Matrix) <- NULL
rownames(Test_Forecast) <- NULL
rownames(Validation) <- NULL
rownames(Feature_Importance) <- NULL


# Apply Neural Net

#set.seed(20195)

output_nn <- sapply(1:nrow(dim_index), nn_modeling)
# output_nn <- sapply(1:3, nn_modeling)

Error_Matrix_nn <- do.call(rbind, output_nn[1, ])
Test_Forecast_nn <- do.call(rbind, output_nn[2, ])
Validation_nn <- do.call(rbind, output_nn[3, ])
model_run_nn <- do.call(rbind, output_nn[4, ])
rownames(Error_Matrix_nn) <- NULL
rownames(Test_Forecast_nn) <- NULL
rownames(Validation_nn) <- NULL
rownames(model_run_nn) <- NULL



# output_nn %>% head(100)
model_run %>% head(10000)
#dim_index %>% head(100)


Error_Matrix = rbind(Error_Matrix, Error_Matrix_nn)
Validation = rbind(Validation, Validation_nn)
model_run = rbind(model_run, model_run_nn)

# Bring in additional table

table_predictiontimeseries <- sqlFetch(SQLServer_Forecast, "rvfc.work_predictiontimeseries")

table_predictiontimeseries$Master_Run_ID <- as.character(table_predictiontimeseries$Master_Run_ID)
table_predictiontimeseries$Run_ID <- as.character(table_predictiontimeseries$Run_ID)
table_predictiontimeseries$Dimension1 <- as.character(table_predictiontimeseries$Dimension1)
table_predictiontimeseries$Dimension2 <- as.character(table_predictiontimeseries$Dimension2)
table_predictiontimeseries$Period <- as.character(table_predictiontimeseries$Period)


match_period <- function(df){
  d1 <- df$Dimension1[1]
  d2 <- df$Dimension2[1]
  period <- table_predictiontimeseries[table_predictiontimeseries$Dimension1 == d1 & table_predictiontimeseries$Dimension2 == d2, 'Period'][1:12]     # [1:12] For 12 month prediction runs
  df$Period <- period
  return (df)
}

# Test_Forecast %>% head(100)

Test_Forecast_X <- as.data.frame(Test_Forecast %>%
                                   dplyr:::group_by(Dimension1, Dimension2) %>%
                                   dplyr:::do(match_period(.)))

Test_Forecast_U <- as.data.frame(Test_Forecast_nn %>%
                                   dplyr:::group_by(Dimension1, Dimension2) %>%
                                   dplyr:::do(match_period(.)))

Test_Forecast_T = rbind(Test_Forecast_X, Test_Forecast_U)


# Bring in additional table

result_scenarios <- sqlFetch(SQLServer_Forecast, "rvfc.result_scenarios")

result_scenarios$Master_Run_ID <- as.character(result_scenarios$Master_Run_ID)
result_scenarios$Run_ID <- as.character(result_scenarios$Run_ID)
result_scenarios$Scenario <- as.character(result_scenarios$Scenario)
result_scenarios$Dimension1 <- as.character(result_scenarios$Dimension1)
result_scenarios$Dimension2 <- as.character(result_scenarios$Dimension2)
result_scenarios$Currency <- as.character(result_scenarios$Currency)


##############################################################
# Revised code from D.Kane

sqlSave(SQLServer_Forecast, Feature_Importance, tablename = "rvfc.result_xgboostfeatureimportance", append = TRUE, rownames = FALSE, fast = FALSE)


# Original Code
# if (sum(is.na(result_scenarios$Business_Intuition)) == nrow(result_scenarios)) {
#   sqlSave(SQLServer_Forecast, Feature_Importance, tablename = "rvfc.result_xgboostfeatureimportance", append = TRUE, rownames = FALSE, fast = FALSE)}



##############################################################

convert_date <- function(df){
  df$Fiscal_year <- format(as.yearmon(df$Period), '%Y')
  df$Posting_period <- format(as.yearmon(df$Period), '%m')
  return(df)
}


pick_zero <- function(k){
  tryCatch({
    dim_id_1 <- dim_index[dim_index$ind == k, 'Dimension1']
    dim_id_2 <- dim_index[dim_index$ind == k, 'Dimension2']
    
    set.seed(12345)
    
    #k <- match(dim_id, unique(input_data$Dimension1))
    error_df <- Error_Matrix[Error_Matrix$Dimension1 == dim_id_1 & Error_Matrix$Dimension2 == dim_id_2, ]
    error_df = error_df[error_df$Dataset == 'TestSet',]
    if (nrow(error_df[error_df$MAPE == min(error_df$MAPE),]) > 1) {
      best_master_run_id = error_df[1,'Master_Run_ID']
      best_run_id = error_df[1,'Run_ID']
    } else {
      best_master_run_id = error_df[error_df$MAPE == min(error_df$MAPE),'Master_Run_ID']
      best_run_id = error_df[error_df$MAPE == min(error_df$MAPE),'Run_ID']
    }
    best_master_run_id = error_df[error_df$MAPE == min(error_df$MAPE),'Master_Run_ID']
    best_run_id = error_df[error_df$MAPE == min(error_df$MAPE),'Run_ID']
    
    error_df = error_df[error_df$Master_Run_ID == best_master_run_id & error_df$Run_ID == best_run_id,]
    
    forecast_df = Test_Forecast_T[Test_Forecast_T$Dimension1 == dim_id_1 & Test_Forecast_T$Dimension2 == dim_id_2, ]
    forecast_df = forecast_df[forecast_df$Master_Run_ID == best_master_run_id & forecast_df$Run_ID == best_run_id,]
    
    val_df = Validation[Validation$Dimension1 == dim_id_1 & Validation$Dimension2 == dim_id_2, ]
    val_df = val_df[val_df$Run_ID == best_run_id,]
    
    #     model_run_df = model_run[model_run$Dimension1 == dim_id_1 & model_run$Dimension2 == dim_id_2, ]
    #     model_run_df = model_run_df[model_run_df$Master_Run_ID == best_master_run_id & model_run_df$Run_ID == best_run_id,]
    
    
    return(list(error_df, forecast_df, val_df))
  }, 
  error = function(e){
    return (list(NULL,NULL,NULL))
    #return (list(e, e, e, e))
  })
}


##############################################################
# Revised code from D.Kane

Test_Forecast_T$Rank = rep(0, nrow(Test_Forecast_T))
Test_Forecast_T$Scenario = rep("Revenue", nrow(Test_Forecast_T))
Test_Forecast_T <- convert_date(Test_Forecast_T)
Test_Forecast_T <- Test_Forecast_T[, c('Master_Run_ID', 'Run_ID', 'Scenario', 'Dimension1', 'Dimension2', 
                                         'Period', 'Posting_period', 'Fiscal_year','Point_Forecast', 
                                         'Lower_Level_1', 'Upper_Level_1', 'Lower_Level_2', 'Upper_Level_2','Rank')]

sqlSave(SQLServer_Forecast, Test_Forecast_T, tablename = "rvfc.result_finalprediction", append = TRUE, rownames = FALSE, fast = FALSE)


# Original Code
# if (sum(is.na(table_result_scenarios$Business_Intuition)) == dim(table_result_scenarios)[1]) {
#   Test_Forecast_T <- Test_Forecast_T[, c('Master_Run_ID', 'Run_ID', 'Dimension1', 'Dimension2', 'Period', 'Point_Forecast', 
#                                          'Lower_Level_1', 'Upper_Level_1', 'Lower_Level_2', 'Upper_Level_2')]
#   sqlSave(SQLServer_Forecast, Test_Forecast_T, tablename = "rvfc.work_predictiontimeseries", append = TRUE, rownames = FALSE, fast = FALSE)} else {
#     
#     select_z = sapply(1:nrow(dim_index), pick_zero)
#     
#     Error_Matrix = do.call(rbind, select_z[1, ])
#     Test_Forecast_T = do.call(rbind, select_z[2, ])
#     Validation = do.call(rbind, select_z[3, ])
#     #   model_run = do.call(rbind, select_z[4, ])
#     
#     rownames(Error_Matrix_nn) <- NULL
#     rownames(Test_Forecast_nn) <- NULL
#     rownames(Validation_nn) <- NULL
#     rownames(model_run_nn) <- NULL
#     
#     Test_Forecast_T$Rank = rep(0, nrow(Test_Forecast_T))
#     Test_Forecast_T$Scenario = rep("Revenue", nrow(Test_Forecast_T))
#     Test_Forecast_T <- convert_date(Test_Forecast_T)
#     Test_Forecast_T <- Test_Forecast_T[, c('Master_Run_ID', 'Run_ID', 'Scenario', 'Dimension1', 'Dimension2', 
#                                            'Period', 'Posting_period', 'Fiscal_year','Point_Forecast', 
#                                            'Lower_Level_1', 'Upper_Level_1', 'Lower_Level_2', 'Upper_Level_2','Rank')]
#     sqlSave(SQLServer_Forecast, Test_Forecast_T, tablename = "rvfc.result_finalprediction", append = TRUE, rownames = FALSE, fast = FALSE)}




#################################################################
# Update the ML Table
#################################################################

# Update Additional Datasets

sqlSave(SQLServer_Forecast, Error_Matrix, tablename = "rvfc.work_statisticalperformance", append = TRUE, rownames = FALSE, fast = FALSE)
sqlSave(SQLServer_Forecast, Validation, tablename = "rvfc.work_validationtimeseries", append = TRUE, rownames = FALSE, fast = FALSE)
sqlSave(SQLServer_Forecast, model_run, tablename = "rvfc.result_modelruntimeseries", append = TRUE, rownames = FALSE, fast = FALSE)
