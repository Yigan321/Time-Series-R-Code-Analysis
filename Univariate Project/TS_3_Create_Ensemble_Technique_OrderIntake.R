#############################################################################
# Advanced Forecasting System                       Date: 06/May/2020
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TS_3_Create_Ensemble_Technique
#############################################################################

# Remove all objects from Global Environment.
rm(list=ls())

# Supress warning messages.
options(warn=-1)

# Set a working directory
setwd("D:/R Scripts/")

####################################################################################
# Establish Connection to SQL Server
####################################################################################


library(RODBC)

SQLServer_Forecast <- odbcConnect(dsn="ForecastDSN", uid="soothsayersql@ssasqlserver", pwd="SoothDez@413")

##################################################################################
# Time Series Statistical - Prepare the Input Data for the Main Dataset
##################################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Run Global initialization script
source("DS_2_Global_Initilization_Script.r")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Importing lookup table

table_statisticstimeseries <- sqlFetch(SQLServer_Forecast, "rvfc.work_statisticalperformance")
table_predictiontimeseries <- sqlFetch(SQLServer_Forecast, "rvfc.work_predictiontimeseries")
table_validationtimeseries <- sqlFetch(SQLServer_Forecast, "rvfc.work_validationtimeseries")
table_modelruntimeseries <- sqlFetch(SQLServer_Forecast, "rvfc.result_modelruntimeseries")
table_forecastscenario <- sqlFetch(SQLServer_Forecast, "rvfc.control_forecastscenario")


table_modelruntimeseries <- as.data.frame(table_modelruntimeseries)
table_forecastscenario <- as.data.frame(table_forecastscenario)
table_statisticstimeseries <- as.data.frame(table_statisticstimeseries)
table_predictiontimeseries <- as.data.frame(table_predictiontimeseries)
table_validationtimeseries <- as.data.frame(table_validationtimeseries)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Fix the factor to the character format
table_validationtimeseries$Dimension1 <- as.character(table_validationtimeseries$Dimension1)
table_validationtimeseries$Dimension2 <- as.character(table_validationtimeseries$Dimension2)
table_validationtimeseries$Dataset <- as.character(table_validationtimeseries$Dataset)
table_validationtimeseries$Run_ID <- as.character(table_validationtimeseries$Run_ID)
table_validationtimeseries$Method <- as.character(table_validationtimeseries$Method)
table_validationtimeseries$Time_Dimension <- as.character(table_validationtimeseries$Time_Dimension)

table_statisticstimeseries$Master_Run_ID <- as.character(table_statisticstimeseries$Master_Run_ID)
table_statisticstimeseries$Run_ID <- as.character(table_statisticstimeseries$Run_ID)
table_statisticstimeseries$Technique <- as.character(table_statisticstimeseries$Technique)
table_statisticstimeseries$Method <- as.character(table_statisticstimeseries$Method)
table_statisticstimeseries$Dataset <- as.character(table_statisticstimeseries$Dataset)
table_statisticstimeseries$Dimension1 <- as.character(table_statisticstimeseries$Dimension1)
table_statisticstimeseries$Dimension2 <- as.character(table_statisticstimeseries$Dimension2)
table_statisticstimeseries$Time_Dimension <- as.character(table_statisticstimeseries$Time_Dimension)

table_predictiontimeseries$Master_Run_ID <- as.character(table_predictiontimeseries$Master_Run_ID)
table_predictiontimeseries$Run_ID <- as.character(table_predictiontimeseries$Run_ID)
table_predictiontimeseries$Period <- as.character(table_predictiontimeseries$Period)
table_predictiontimeseries$Dimension1 <- as.character(table_predictiontimeseries$Dimension1)
table_predictiontimeseries$Dimension2 <- as.character(table_predictiontimeseries$Dimension2)
table_predictiontimeseries$Time_Dimension <- as.character(table_predictiontimeseries$Time_Dimension)

table_modelruntimeseries$Master_Run_ID <- as.character(table_modelruntimeseries$Master_Run_ID)
table_modelruntimeseries$Run_ID <- as.character(table_modelruntimeseries$Run_ID)
table_modelruntimeseries$Technique <- as.character(table_modelruntimeseries$Technique)
table_modelruntimeseries$Method <- as.character(table_modelruntimeseries$Method)
table_modelruntimeseries$Model_Run_Date <- as.character(table_modelruntimeseries$Model_Run_Date)
table_modelruntimeseries$Dimension1 <- as.character(table_modelruntimeseries$Dimension1)
table_modelruntimeseries$Dimension2 <- as.character(table_modelruntimeseries$Dimension2)
table_modelruntimeseries$Threshold <- as.character(table_modelruntimeseries$Threshold)
table_modelruntimeseries$Train_Start_Date <- as.character(table_modelruntimeseries$Train_Start_Date)
table_modelruntimeseries$Test_Start_Date <- as.character(table_modelruntimeseries$Test_Start_Date)
table_modelruntimeseries$Train_End_Date <- as.character(table_modelruntimeseries$Train_End_Date)
table_modelruntimeseries$Test_End_Date <- as.character(table_modelruntimeseries$Test_End_Date)
table_modelruntimeseries$UoM <- as.character(table_modelruntimeseries$UoM)
table_modelruntimeseries$Validation <- as.character(table_modelruntimeseries$Validation)
table_modelruntimeseries$Forecast_Period <- as.character(table_modelruntimeseries$Forecast_Period)
table_modelruntimeseries$Lower_Prediction_Interval <- as.character(table_modelruntimeseries$Lower_Prediction_Interval)
table_modelruntimeseries$Upper_Prediction_Interval <- as.character(table_modelruntimeseries$Upper_Prediction_Interval)
table_modelruntimeseries$Time_Dimension <- as.character(table_modelruntimeseries$Time_Dimension)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

KPI_lookup <- function(){
  kpi <- table_forecastscenario[table_forecastscenario$Active == 'X', 'Scenario']
  return (kpi)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (length(unique(data.prediction$Scenario)) == 1) {
  kpi <- unique(data.prediction$Scenario)
}
else {
  kpi <- KPI_lookup()
  if (length(kpi) == 0){
    kpi <- "Order_Intake"
  }
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TimeDimension_lookup <- function() {
  time_dimension <- names(control_timescenario)[apply(control_timescenario, 1, function(i) which(i == 'X'))]
  return(time_dimension)                                                    
}
Time_Dimension = TimeDimension_lookup()     
print(Time_Dimension)     

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Define MAPE Evaluation metrics\
mapefunc <- function(train, test){
  train <- ifelse(train == 0, 0.01, train)
  mape <- abs((train - test) / train)
  mape <- ifelse(mape > 1, 1, mape)
  finalmape <- mean(mape)
  return(finalmape)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

############################################################################################
# Prepare the data for Ensemble Technique
############################################################################################
Prediction_Ensemble <- data.frame(Master_Run_ID = character(0), Run_ID = character(0), Dimension1 = character(0), Dimension2 = character(0),
                                  Posting_period = character(0), Fiscal_year = character(0), Point_Forecast = numeric(0), Lower_Level_1 = numeric(0),
                                  Upper_Level_1 = numeric(0), Lower_Level_2 = numeric(0), Upper_Level_2 = numeric(0),Time_Dimension = character(0)) 
Statistics_Ensemble <- data.frame(Master_Run_ID = character(0), Run_ID = character(0), Technique = character(0), Method = character(0), 
                                  Dataset = character(0), Dimension1 = character(0), Dimension2 = character(0),
                                  MAE = numeric(0), RMSE = numeric(0), MAPE = numeric(0), Time_Dimension = character(0))
Statistics_Agg <- data.frame(Master_Run_ID = character(0), Run_ID = character(0), Technique = character(0), Method = character(0), 
                             Dataset = character(0), Dimension1 = character(0), Dimension2 = character(0),
                             MAE = numeric(0), RMSE = numeric(0), MAPE = numeric(0), Final_Rank = numeric(0), Time_Dimension = character(0)) 
Model_run_Ensemble <- data.frame(Master_Run_ID = character(0), Run_ID =character(0), Technique = character(0), Method = character(0), 
                                 Model_Run_Date = character(0), Dimension1 = character(0), Dimension2 = character(0),
                                 Threshold = character(0), Train_Start_Date = character(0), 
                                 Train_End_Date = character(0), Test_Start_Date =character(0), 
                                 Test_End_Date = character(0), Validation = numeric(0), 
                                 Forecast_Period = numeric(0), Lower_Prediction_Interval = numeric(0),
                                 Upper_Prediction_Interval = numeric(0), Time_Dimension = character(0), UoM = character(0))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Fix the column formats
Prediction_Ensemble$Master_Run_ID <- as.character(Prediction_Ensemble$Master_Run_ID)
Prediction_Ensemble$Run_ID <- as.character(Prediction_Ensemble$Run_ID)
Prediction_Ensemble$Period <- as.character(Prediction_Ensemble$Period)
Prediction_Ensemble$Dimension1 <- as.character(Prediction_Ensemble$Dimension1)
Prediction_Ensemble$Dimension2 <- as.character(Prediction_Ensemble$Dimension2)
Prediction_Ensemble$Time_Dimension <- as.character(Prediction_Ensemble$Time_Dimension)

Statistics_Agg$Master_Run_ID <- as.character(Statistics_Agg$Master_Run_ID)
Statistics_Agg$Run_ID <- as.character(Statistics_Agg$Run_ID)
Statistics_Agg$Technique <- as.character(Statistics_Agg$Technique)
Statistics_Agg$Method <- as.character(Statistics_Agg$Method)
Statistics_Agg$Dataset <- as.character(Statistics_Agg$Dataset)
Statistics_Agg$Dimension1 <- as.character(Statistics_Agg$Dimension1)
Statistics_Agg$Dimension2 <- as.character(Statistics_Agg$Dimension2)
Statistics_Agg$Time_Dimension <- as.character(Statistics_Agg$Time_Dimension)

Statistics_Ensemble$Master_Run_ID <- as.character(Statistics_Ensemble$Master_Run_ID)
Statistics_Ensemble$Run_ID <- as.character(Statistics_Ensemble$Run_ID)
Statistics_Ensemble$Technique <- as.character(Statistics_Ensemble$Technique)
Statistics_Ensemble$Method <- as.character(Statistics_Ensemble$Method)
Statistics_Ensemble$Dataset <- as.character(Statistics_Ensemble$Dataset)
Statistics_Ensemble$Dimension1 <- as.character(Statistics_Ensemble$Dimension1)
Statistics_Ensemble$Dimension2 <- as.character(Statistics_Ensemble$Dimension2)
Statistics_Ensemble$Time_Dimension <- as.character(Statistics_Ensemble$Time_Dimension)

Model_run_Ensemble$Master_Run_ID <- as.character(Model_run_Ensemble$Master_Run_ID)
Model_run_Ensemble$Run_ID <- as.character(Model_run_Ensemble$Run_ID)
Model_run_Ensemble$Scenario <- as.character(Model_run_Ensemble$Scenario)
Model_run_Ensemble$Technique <- as.character(Model_run_Ensemble$Technique)
Model_run_Ensemble$Method <- as.character(Model_run_Ensemble$Method)
Model_run_Ensemble$Model_Run_Date <- as.character(Model_run_Ensemble$Model_Run_Date)
Model_run_Ensemble$Dimension1 <- as.character(Model_run_Ensemble$Dimension1)
Model_run_Ensemble$Dimension2 <- as.character(Model_run_Ensemble$Dimension2)
Model_run_Ensemble$Threshold <- as.character(Model_run_Ensemble$Threshold)
Model_run_Ensemble$Train_Start_Date <- as.character(Model_run_Ensemble$Train_Start_Date)
Model_run_Ensemble$Test_Start_Date <- as.character(Model_run_Ensemble$Test_Start_Date)
Model_run_Ensemble$Train_End_Date <- as.character(Model_run_Ensemble$Train_End_Date)
Model_run_Ensemble$Test_End_Date <- as.character(Model_run_Ensemble$Test_End_Date)
Model_run_Ensemble$UoM <- as.character(Model_run_Ensemble$UoM)
Model_run_Ensemble$Time_Dimension <- as.character(Model_run_Ensemble$Time_Dimension)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

prediciton_agg <- function(master_runid){
  # Run_ID
  Run_ID <- paste(substr(master_runid, 1, nchar(as.character(master_runid)) - 2), '99', sep='')
  
  ### Model_run df
  Model_run_Ensemble <- table_modelruntimeseries[table_modelruntimeseries$Master_Run_ID == master_runid, ][1, ]
  Model_run_Ensemble$Run_ID <- Run_ID
  Model_run_Ensemble$Method <- 'Ensemble'
  
  # Get best 2 model Run_ID
  Error_Matrix <- table_statisticstimeseries[table_statisticstimeseries$Master_Run_ID == master_runid, ]
  Result_RMSE <- reshape:::cast(Error_Matrix, Dimension1 + Dimension2 + Dataset ~ Run_ID, value="RMSE")
  Result_RMSE <- Result_RMSE[Result_RMSE$Dataset == "TestSet", ]
  
  if (dim(Result_RMSE)[2]<5){
    Models = as.data.frame(t(colnames(Result_RMSE[1,4:ncol(Result_RMSE), drop = FALSE])))
    Models$V2 = Models$V1
    Best_Model = Models
  } else{
    Models <- as.data.frame(t(colnames(sort(Result_RMSE[1, 4:ncol(Result_RMSE)]))))
    Best_Model <- as.data.frame(t(colnames(sort(Result_RMSE[1, 4:ncol(Result_RMSE)]))[1:2]))
  }
  
  Model_id_1 <- as.character(Best_Model$V1[1]) 
  Model_id_2 <- as.character(Best_Model$V2[1])
  
  #####
  ## Train & Validation
  #####
  # Look up point_forecast in validationtimeseries table
  Actual_value_test <- table_validationtimeseries[(table_validationtimeseries$Run_ID == Model_id_1) & (table_validationtimeseries$Dataset == 'TestSet'), 
                                                  'Actual_value']    
  Point_forecast_test_1 <- table_validationtimeseries[(table_validationtimeseries$Run_ID == Model_id_1) & (table_validationtimeseries$Dataset == 'TestSet'), 
                                                      'Point_forecast']
  Point_forecast_test_2 <- table_validationtimeseries[(table_validationtimeseries$Run_ID == Model_id_2) & (table_validationtimeseries$Dataset == 'TestSet'), 
                                                      'Point_forecast']  
  
  #####
  ## Prediction
  #####
  # Look up point_forecast in predictiontimeseries table
  Point_forecast_1 <- table_predictiontimeseries[table_predictiontimeseries$Run_ID == Model_id_1, 'Point_Forecast']
  Point_forecast_2 <- table_predictiontimeseries[table_predictiontimeseries$Run_ID == Model_id_2, 'Point_Forecast']
  Upper_1_r1 <- table_predictiontimeseries[table_predictiontimeseries$Run_ID == Model_id_1, 'Upper_Level_1']
  Lower_1_r1 <- table_predictiontimeseries[table_predictiontimeseries$Run_ID == Model_id_1, 'Lower_Level_1']
  Upper_2_r1 <- table_predictiontimeseries[table_predictiontimeseries$Run_ID == Model_id_1, 'Upper_Level_2']
  Lower_2_r1 <- table_predictiontimeseries[table_predictiontimeseries$Run_ID == Model_id_1, 'Lower_Level_2']
  Upper_1_r2 <- table_predictiontimeseries[table_predictiontimeseries$Run_ID == Model_id_2, 'Upper_Level_1']
  Lower_1_r2 <- table_predictiontimeseries[table_predictiontimeseries$Run_ID == Model_id_2, 'Lower_Level_1']
  Upper_2_r2 <- table_predictiontimeseries[table_predictiontimeseries$Run_ID == Model_id_2, 'Upper_Level_2']
  Lower_2_r2 <- table_predictiontimeseries[table_predictiontimeseries$Run_ID == Model_id_2, 'Lower_Level_2']  
  
  # Ensemble
  if (ncol(Models) > 2) {
    Best_Model_3 <- as.data.frame(t(colnames(sort(Result_RMSE[1, 4:ncol(Result_RMSE)]))[1:3]))
    Model_id_3 <- as.character(Best_Model_3$V3[1])
    Point_forecast_test_3 <- table_validationtimeseries[(table_validationtimeseries$Run_ID == Model_id_3) & (table_validationtimeseries$Dataset == 'TestSet'), 'Point_forecast']
    Point_forecast_3 <- table_predictiontimeseries[table_predictiontimeseries$Run_ID == Model_id_3, 'Point_Forecast']
    
    Upper_1_r3 <- table_predictiontimeseries[table_predictiontimeseries$Run_ID == Model_id_3, 'Upper_Level_1']
    Lower_1_r3 <- table_predictiontimeseries[table_predictiontimeseries$Run_ID == Model_id_3, 'Lower_Level_1']
    Upper_2_r3 <- table_predictiontimeseries[table_predictiontimeseries$Run_ID == Model_id_3, 'Upper_Level_2']
    Lower_2_r3 <- table_predictiontimeseries[table_predictiontimeseries$Run_ID == Model_id_3, 'Lower_Level_2']  
    Point_forecast_test_ensemble <- rowMeans(cbind(Point_forecast_test_1, Point_forecast_test_2, Point_forecast_test_3))
    Point_forecast_ensemble <- rowMeans(cbind(Point_forecast_1, Point_forecast_2, Point_forecast_3))
    
    Upper_1_e <- rowMeans(cbind(Upper_1_r1, Upper_1_r2, Upper_1_r3))
    Lower_1_e <- rowMeans(cbind(Lower_1_r1, Lower_1_r2, Lower_1_r3))
    Upper_2_e <- rowMeans(cbind(Upper_2_r1, Upper_2_r2, Upper_2_r3))
    Lower_2_e <- rowMeans(cbind(Lower_2_r1, Lower_2_r2, Lower_2_r3))
  }
  else{
    Point_forecast_test_ensemble <- rowMeans(cbind(Point_forecast_test_1, Point_forecast_test_2))
    Point_forecast_ensemble <- rowMeans(cbind(Point_forecast_1, Point_forecast_2))
    
    Upper_1_e <- rowMeans(cbind(Upper_1_r1, Upper_1_r2))
    Lower_1_e <- rowMeans(cbind(Lower_1_r1, Lower_1_r2))
    Upper_2_e <- rowMeans(cbind(Upper_2_r1, Upper_2_r2))
    Lower_2_e <- rowMeans(cbind(Lower_2_r1, Lower_2_r2))
  }
  
  # Output table    
  Statistics_Ensemble_test <- as.data.frame(master_runid)
  names(Statistics_Ensemble_test) <- 'Master_Run_ID'
  Statistics_Ensemble_test$Run_ID <- Run_ID
  Statistics_Ensemble_test$Technique <- 'TimeSeries'    
  Statistics_Ensemble_test$Method <- 'Ensemble'
  Statistics_Ensemble_test$Dataset <- 'TestSet'
  Statistics_Ensemble_test$Dimension1 <- table_predictiontimeseries[table_predictiontimeseries$Master_Run_ID == master_runid, 'Dimension1'][1]
  Statistics_Ensemble_test$Dimension2 <- table_predictiontimeseries[table_predictiontimeseries$Master_Run_ID == master_runid, 'Dimension2'][1]
  Statistics_Ensemble_test$MAE <- regr.eval(Actual_value_test, Point_forecast_test_ensemble, stats='mae')
  Statistics_Ensemble_test$RMSE <- regr.eval(Actual_value_test, Point_forecast_test_ensemble, stats='rmse')
  Statistics_Ensemble_test$MAPE <- mapefunc(Actual_value_test, Point_forecast_test_ensemble) 
  Statistics_Ensemble_test$Time_Dimension = Time_Dimension
  
  Statistics_Ensemble <- rbind(Statistics_Ensemble, Statistics_Ensemble_test)
  
  
  # Build output prediction table
  Predictionensemble <- table_predictiontimeseries[table_predictiontimeseries$Run_ID == Model_id_1, c('Dimension1', 'Dimension2', 'Posting_period', 'Fiscal_year')]
  Predictionensemble$Master_Run_ID <- rep(master_runid, nrow(Predictionensemble))
  Predictionensemble$Run_ID <- rep(Run_ID, nrow(Predictionensemble))
  Predictionensemble <- Predictionensemble[, c(5:6, 1:4)]
  Predictionensemble$Point_Forecast <- Point_forecast_ensemble
  ### Ensemble CI
  Predictionensemble$Lower_Level_1 <- Lower_1_e
  Predictionensemble$Upper_Level_1 <- Upper_1_e
  Predictionensemble$Lower_Level_2 <- Lower_2_e
  Predictionensemble$Upper_Level_2 <- Upper_2_e   
  
  Prediction_Ensemble <- rbind(Prediction_Ensemble, Predictionensemble)
  
  
  #####
  ##Aggregating 2 statistics tables 
  #####
  statistics_agg <- rbind(Error_Matrix, Statistics_Ensemble_test)
  Rank_test <- rank(statistics_agg[statistics_agg$Dataset == 'TestSet', 'RMSE'], ties.method = 'random')
  statistics_agg$Final_Rank[statistics_agg$Dataset == 'TestSet'] <- Rank_test
  statistics_agg$Final_Rank <- as.character(na.locf(zoo(statistics_agg$Final_Rank), fromLast = TRUE))
  Statistics_Agg <- rbind(Statistics_Agg, statistics_agg)
  
  
  rm(Predictionensemble, Statistics_Ensemble_train, Statistics_Ensemble_test, statistics_agg)
  return (list(Statistics_Ensemble, Prediction_Ensemble, Statistics_Agg, Model_run_Ensemble))
}

###########################################################################################
# Apply the function
###########################################################################################

set.seed(20195)

output <- sapply(unique(table_statisticstimeseries$Master_Run_ID), prediciton_agg)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Validation_E <- do.call(rbind, output[1, ])
Prediction_E <- do.call(rbind, output[2, ])
Statistics_Agg <- do.call(rbind, output[3, ])
Model_run_Ensemble <- do.call(rbind, output[4, ])
row.names(Validation_E) <- NULL
row.names(Prediction_E) <- NULL
row.names(Statistics_Agg) <- NULL
row.names(Model_run_Ensemble) <- NULL

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Statistics_Agg$Scenario <- rep(kpi, nrow(Statistics_Agg))

Statistics_Agg <- Statistics_Agg[, c('Master_Run_ID', 'Run_ID', 'Scenario', 'Technique', 'Method', 'Dataset', 'Dimension1', 'Dimension2', 
                                     'MAE', 'RMSE', 'MAPE', 'Time_Dimension', 'Final_Rank')]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Model_run_Ensemble$Scenario <- rep(kpi, nrow(Model_run_Ensemble))

Model_run_Ensemble <- Model_run_Ensemble[, c('Master_Run_ID', 'Run_ID', 'Scenario', 'Technique', 'Method', 'Model_Run_Date', 'Dimension1', 
                                             'Dimension2', 'Threshold', 'Train_Start_Date', 'Train_End_Date', 'Test_Start_Date', 
                                             'Test_End_Date', 'Validation', 'Forecast_Period', 'Lower_Prediction_Interval', 
                                             'Upper_Prediction_Interval', 'UoM', 'Time_Dimension')]


##################################################################################
# Output
##################################################################################
Prediction_E$Time_Dimension = Time_Dimension
Prediction_E_r <- Prediction_E %>% dplyr:::distinct()

Validation_E_r <- Validation_E %>% dplyr:::distinct()
Statistics_Agg_r <- Statistics_Agg %>% dplyr:::distinct()
Model_run_Ensemble_r <- Model_run_Ensemble %>% dplyr:::distinct()


##################################################################################
# Write Results back to SQL Server
##################################################################################


sqlSave(SQLServer_Forecast, Prediction_E_r, tablename = "rvfc.work_predictionensemble", append = TRUE, rownames = FALSE)
sqlSave(SQLServer_Forecast, Validation_E_r, tablename = "rvfc.work_statisticsensemble", append = TRUE, rownames = FALSE)
sqlSave(SQLServer_Forecast, Statistics_Agg_r, tablename = "rvfc.result_finalranktimeseries", append = TRUE, rownames = FALSE)
sqlSave(SQLServer_Forecast, Model_run_Ensemble_r, tablename = "rvfc.result_modelruntimeseries", append = TRUE, rownames = FALSE)
