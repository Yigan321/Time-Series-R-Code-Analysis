#############################################################################
# Advanced Forecasting System                       
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ML_3_Main_All_Ensemble_and_Model_Selection_Routine
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


library(zoo)
library(reshape)
library(date)
library(DMwR)
library(dplyr)
library(lubridate)



#Importing lookup table

table_statisticalperformance <- sqlFetch(SQLServer_Forecast, "rvfc.work_statisticalperformance")
table_predictiontimeseries <- sqlFetch(SQLServer_Forecast, "rvfc.work_predictiontimeseries")
table_validationtimeseries <- sqlFetch(SQLServer_Forecast, "rvfc.work_validationtimeseries")
modelrun <- sqlFetch(SQLServer_Forecast, "rvfc.result_modelruntimeseries")



# Fix the factor to the chracter format
table_validationtimeseries$Dimension1 <- as.character(table_validationtimeseries$Dimension1)
table_validationtimeseries$Dimension2 <- as.character(table_validationtimeseries$Dimension2)
table_validationtimeseries$Dataset <- as.character(table_validationtimeseries$Dataset)
table_validationtimeseries$Run_ID <- as.character(table_validationtimeseries$Run_ID)
table_validationtimeseries$Method <- as.character(table_validationtimeseries$Method)

table_statisticalperformance$Master_Run_ID <- as.character(table_statisticalperformance$Master_Run_ID)
table_statisticalperformance$Run_ID <- as.character(table_statisticalperformance$Run_ID)
table_statisticalperformance$Technique <- as.character(table_statisticalperformance$Technique)
table_statisticalperformance$Method <- as.character(table_statisticalperformance$Method)
table_statisticalperformance$Dataset <- as.character(table_statisticalperformance$Dataset)
table_statisticalperformance$Dimension1 <- as.character(table_statisticalperformance$Dimension1)
table_statisticalperformance$Dimension2 <- as.character(table_statisticalperformance$Dimension2)

table_predictiontimeseries$Master_Run_ID <- as.character(table_predictiontimeseries$Master_Run_ID)
table_predictiontimeseries$Run_ID <- as.character(table_predictiontimeseries$Run_ID)
table_predictiontimeseries$Period <- as.character(table_predictiontimeseries$Period)
table_predictiontimeseries$Dimension1 <- as.character(table_predictiontimeseries$Dimension1)
table_predictiontimeseries$Dimension2 <- as.character(table_predictiontimeseries$Dimension2)

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
modelrun$Test_Start_Month <- as.character(modelrun$Test_Start_Month)
modelrun$Train_End_Month <- as.character(modelrun$Train_End_Month)
modelrun$Test_End_Month <- as.character(modelrun$Test_End_Month)
modelrun$Local_Currency <- as.character(modelrun$Local_Currency)

### Define MAPE Evaluation metrics\
mapefunc <- function(train, test){
  train <- ifelse(train == 0, 0.01, train)
  mape <- abs((train - test) / train)
  mape <- ifelse(mape > 1, 1, mape)
  finalmape <- mean(mape)
  return(finalmape)
}

Prediction_Ensemble <- data.frame(Master_Run_ID = character(0), Run_ID = character(0), Dimension1 = character(0), Dimension2 = character(0),
                                  Period = character(0), Point_Forecast = numeric(0), Lower_Level_1 = numeric(0), Upper_Level_1 = numeric(0),
                                  Lower_Level_2 = numeric(0), Upper_Level_2 = numeric(0)) 
Statistics_Ensemble <- data.frame(Master_Run_ID = character(0), Run_ID = character(0), Technique = character(0), Method = character(0), 
                                  Dataset = character(0), Dimension1 = character(0), Dimension2 = character(0),
                                  MAE = numeric(0), RMSE = numeric(0), MAPE = numeric(0))
Statistics_Agg <- data.frame(Master_Run_ID = character(0), Run_ID = character(0), Technique = character(0), Method = character(0), 
                             Dataset = character(0), Dimension1 = character(0), Dimension2 = character(0),
                             MAE = numeric(0), RMSE = numeric(0), MAPE = numeric(0), Final_Rank = numeric(0)) 


# Fix the column formats
Prediction_Ensemble$Master_Run_ID <- as.character(Prediction_Ensemble$Master_Run_ID)
Prediction_Ensemble$Run_ID <- as.character(Prediction_Ensemble$Run_ID)
Prediction_Ensemble$Period <- as.character(Prediction_Ensemble$Period)
Prediction_Ensemble$Dimension1 <- as.character(Prediction_Ensemble$Dimension1)
Prediction_Ensemble$Dimension2 <- as.character(Prediction_Ensemble$Dimension2)

Statistics_Agg$Master_Run_ID <- as.character(Statistics_Agg$Master_Run_ID)
Statistics_Agg$Run_ID <- as.character(Statistics_Agg$Run_ID)
Statistics_Agg$Technique <- as.character(Statistics_Agg$Technique)
Statistics_Agg$Method <- as.character(Statistics_Agg$Method)
Statistics_Agg$Dataset <- as.character(Statistics_Agg$Dataset)
Statistics_Agg$Dimension1 <- as.character(Statistics_Agg$Dimension1)
Statistics_Agg$Dimension2 <- as.character(Statistics_Agg$Dimension2)

Statistics_Ensemble$Master_Run_ID <- as.character(Statistics_Ensemble$Master_Run_ID)
Statistics_Ensemble$Run_ID <- as.character(Statistics_Ensemble$Run_ID)
Statistics_Ensemble$Technique <- as.character(Statistics_Ensemble$Technique)
Statistics_Ensemble$Method <- as.character(Statistics_Ensemble$Method)
Statistics_Ensemble$Dataset <- as.character(Statistics_Ensemble$Dataset)
Statistics_Ensemble$Dimension1 <- as.character(Statistics_Ensemble$Dimension1)
Statistics_Ensemble$Dimension2 <- as.character(Statistics_Ensemble$Dimension2)


prediciton_agg <- function(master_id){
  # Run_ID
  tryCatch({
    Run_ID <- paste('E', substr(master_id, 2, nchar(as.character(master_id)) - 2), '99', sep='')
    
    model_run <- modelrun[modelrun$Master_Run_ID == master_id & modelrun$Scenario == 'Revenue', ][1, ] 
    model_run$Run_ID <- Run_ID
    model_run$Technique <- 'Ensemble'
    model_run$Method <- 'Ensemble'
    
    # Get best 2 model Run_ID
    Error_Matrix <- table_statisticalperformance[table_statisticalperformance$Master_Run_ID == master_id, ]
    Error_Matrix <- Error_Matrix[!is.na(Error_Matrix$Master_Run_ID),]
    Result_RMSE <- reshape:::cast(Error_Matrix, Dimension1 + Dimension2 + Dataset ~ Run_ID, value="RMSE")
    Result_RMSE <- Result_RMSE[Result_RMSE$Dataset == "TestSet", ]
    
    Models <- as.data.frame(t(colnames(sort(Result_RMSE[1, 4:ncol(Result_RMSE)]))))
    Best_Model <- as.data.frame(t(colnames(sort(Result_RMSE[1, 4:ncol(Result_RMSE)]))[1:2]))
    Model_id_1 <- as.character(Best_Model$V1[1]) 
    Model_id_2 <- as.character(Best_Model$V2[1])
    
    #####
    ##Validation
    #####
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
      Point_forecast_train_3 <- table_validationtimeseries[(table_validationtimeseries$Run_ID == Model_id_3) & (table_validationtimeseries$Dataset == 'TrainSet'), 'Point_forecast']
      Point_forecast_test_3 <- table_validationtimeseries[(table_validationtimeseries$Run_ID == Model_id_3) & (table_validationtimeseries$Dataset == 'TestSet'), 'Point_forecast']
      Point_forecast_3 <- table_predictiontimeseries[table_predictiontimeseries$Run_ID == Model_id_3, 'Point_Forecast']
      
      Upper_1_r3 <- table_predictiontimeseries[table_predictiontimeseries$Run_ID == Model_id_3, 'Upper_Level_1']
      Lower_1_r3 <- table_predictiontimeseries[table_predictiontimeseries$Run_ID == Model_id_3, 'Lower_Level_1']
      Upper_2_r3 <- table_predictiontimeseries[table_predictiontimeseries$Run_ID == Model_id_3, 'Upper_Level_2']
      Lower_2_r3 <- table_predictiontimeseries[table_predictiontimeseries$Run_ID == Model_id_3, 'Lower_Level_2']  
      #Point_forecast_train_ensemble <- rowMeans(cbind(Point_forecast_train_1, Point_forecast_train_2, Point_forecast_train_3))
      Point_forecast_test_ensemble <- rowMeans(cbind(Point_forecast_test_1, Point_forecast_test_2, Point_forecast_test_3))
      Point_forecast_ensemble <- rowMeans(cbind(Point_forecast_1, Point_forecast_2, Point_forecast_3))
      
      Upper_1_e <- rowMeans(cbind(Upper_1_r1, Upper_1_r2, Upper_1_r3))
      Lower_1_e <- rowMeans(cbind(Lower_1_r1, Lower_1_r2, Lower_1_r3))
      Upper_2_e <- rowMeans(cbind(Upper_2_r1, Upper_2_r2, Upper_2_r3))
      Lower_2_e <- rowMeans(cbind(Lower_2_r1, Lower_2_r2, Lower_2_r3))
    }
    else{
      #Point_forecast_train_ensemble <- rowMeans(cbind(Point_forecast_train_1, Point_forecast_train_2))
      Point_forecast_test_ensemble <- rowMeans(cbind(Point_forecast_test_1, Point_forecast_test_2))
      Point_forecast_ensemble <- rowMeans(cbind(Point_forecast_1, Point_forecast_2))
      
      Upper_1_e <- rowMeans(cbind(Upper_1_r1, Upper_1_r2))
      Lower_1_e <- rowMeans(cbind(Lower_1_r1, Lower_1_r2))
      Upper_2_e <- rowMeans(cbind(Upper_2_r1, Upper_2_r2))
      Lower_2_e <- rowMeans(cbind(Lower_2_r1, Lower_2_r2))
    }
    
    # Output table
    Statistics_Ensemble_test <- as.data.frame(master_id)
    names(Statistics_Ensemble_test) <- 'Master_Run_ID'
    Statistics_Ensemble_test$Run_ID <- Run_ID
    Statistics_Ensemble_test$Technique <- 'Ensemble'    
    Statistics_Ensemble_test$Method <- 'Ensemble'
    Statistics_Ensemble_test$Dataset <- 'TestSet'
    Statistics_Ensemble_test$Dimension1 <- model_run$Dimension1
    Statistics_Ensemble_test$Dimension2 <- model_run$Dimension2
    Statistics_Ensemble_test$MAE <- regr.eval(Actual_value_test, Point_forecast_test_ensemble, stats='mae')
    Statistics_Ensemble_test$RMSE <- regr.eval(Actual_value_test, Point_forecast_test_ensemble, stats='rmse')
    Statistics_Ensemble_test$MAPE <- mapefunc(Actual_value_test, Point_forecast_test_ensemble)    
    
    Statistics_Ensemble <- rbind(Statistics_Ensemble, Statistics_Ensemble_test)
    
    
    # Build output prediction table
    Predictionensemble <- table_predictiontimeseries[table_predictiontimeseries$Run_ID == Model_id_1, c('Dimension1', 'Dimension2', 'Period')]
    Predictionensemble$Master_Run_ID <- rep(master_id, nrow(Predictionensemble))
    Predictionensemble$Run_ID <- rep(Run_ID, nrow(Predictionensemble))
    Predictionensemble <- Predictionensemble[, c(4:5, 1:3)]
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
    #statistics_agg <- rbind(Error_Matrix, Statistics_Ensemble_train, Statistics_Ensemble_test)
    Rank_test <- rank(statistics_agg[statistics_agg$Dataset == 'TestSet', 'RMSE'], ties.method = 'random')
    statistics_agg$Final_Rank[statistics_agg$Dataset == 'TestSet'] <- Rank_test
    statistics_agg$Final_Rank <- as.character(na.locf(zoo(statistics_agg$Final_Rank), fromLast = TRUE))
    Statistics_Agg <- rbind(Statistics_Agg, statistics_agg)
    
    
    rm(Predictionensemble, Statistics_Ensemble_test, statistics_agg)
    return(list(Statistics_Ensemble, Prediction_Ensemble, Statistics_Agg, model_run))
  },
  error = function(e){
    return (list(NULL, NULL, NULL, NULL))
    #return (list(e, e, e, e))
  })
}


set.seed(20195)

output <- sapply(unique(modelrun$Master_Run_ID[modelrun$Scenario == 'Revenue']), prediciton_agg)
#output <- sapply(unique(modelrun[modelrun$Scenario == 'Revenue' & substring(modelrun$Master_Run_ID, 0, 1) == "T", 'Master_Run_ID']), prediciton_agg)

Validation_E <- do.call(rbind, output[1, ])
Prediction_E <- do.call(rbind, output[2, ])
Statistics_Agg <- do.call(rbind, output[3, ])
model_run <- do.call(rbind, output[4, ])
row.names(Validation_E) <- NULL
row.names(Prediction_E) <- NULL
row.names(Statistics_Agg) <- NULL
row.names(model_run) <- NULL


Best_model_selection_prediction_cap <- function() {
  
  prediction_agg <- rbind(table_predictiontimeseries, Prediction_E)
  
  Add_rank <- function(k){
    ID <- prediction_agg[k, 'Run_ID']
    rank_col <- Statistics_Agg[Statistics_Agg$Run_ID == ID, 'Final_Rank'][1]
    return (rank_col)
  }
  rank_list <- sapply(1:nrow(prediction_agg), Add_rank)
  prediction_agg$Rank <- rank_list
  
  return(prediction_agg)
}

Prediction_Agg = Best_model_selection_prediction_cap()

Prediction_Agg$Scenario = rep("Revenue", nrow(Prediction_Agg))

convert_date <- function(df){
  df$Fiscal_year <- format(as.yearmon(df$Period), '%Y')
  df$Posting_period <- format(as.yearmon(df$Period), '%m')
  return(df)
}


Prediction_Agg <- convert_date(Prediction_Agg)
Prediction_Agg <- Prediction_Agg[, c('Master_Run_ID', 'Run_ID', 'Scenario', 'Dimension1', 'Dimension2', 
                                     'Period', 'Posting_period', 'Fiscal_year','Point_Forecast', 
                                     'Lower_Level_1', 'Upper_Level_1', 'Lower_Level_2', 'Upper_Level_2','Rank')]

Best_model_selection <- function(){
  
  prediction_agg <- table_predictiontimeseries
  best_run_id <- unique(Statistics_Agg[Statistics_Agg$Final_Rank == 1, 'Run_ID'])
  
  Filter <- function(id){
    return(prediction_agg[prediction_agg$Run_ID == id, ])
  }
  return((lapply(best_run_id, Filter)) %>% bind_rows())
}

Best_model_prediction <- Best_model_selection()

Best_model_prediction$Scenario <- rep("Revenue", nrow(Best_model_prediction))


# Check this section

Best_model_prediction <- convert_date(Best_model_prediction)
Best_model_prediction <- Best_model_prediction[, c('Master_Run_ID', 'Run_ID', 'Scenario', 'Dimension1', 'Dimension2', 'Period', 
                                                   'Posting_period', 'Fiscal_year', 'Point_Forecast', 'Lower_Level_1', 'Upper_Level_1', 
                                                   'Lower_Level_2', 'Upper_Level_2')]
Best_model_prediction = Best_model_prediction[complete.cases(Best_model_prediction),]

# write.csv(Prediction_Agg, file='C:/Users/Derek/Documents/mydata.csv', row.names=F)

# Remove the NULL Ranks from the list
Prediction_Agg <- Prediction_Agg[!is.na(Prediction_Agg$Rank),]

Prediction_Agg$Rank <- as.integer(Prediction_Agg$Rank)
Prediction_Agg$Posting_period <- as.integer(Prediction_Agg$Posting_period)
Prediction_Agg$Fiscal_year <- as.integer(Prediction_Agg$Fiscal_year)

#########################################################################
# Post Processing
#########################################################################

# Delete Query - DELETE FROM rvfc.result_finalprediction WHERE Scenario = 'Revenue'

sqlQuery(SQLServer_Forecast, "EXEC dbo.sp_Delete_Query1")

Prediction_Agg_spark <- Prediction_Agg %>% dplyr:::distinct()
sqlSave(SQLServer_Forecast, Prediction_Agg_spark, tablename = "rvfc.result_finalprediction", append = TRUE, rownames = FALSE, fast = FALSE)

sqlSave(SQLServer_Forecast, model_run, tablename = "rvfc.result_modelruntimeseries", append = TRUE, rownames = FALSE, fast = FALSE)


# Delete Query2 - DELETE FROM rvfc.result_prediction WHERE Scenario = 'Revenue'
sqlQuery(SQLServer_Forecast, "EXEC dbo.sp_Delete_Query2")


sqlSave(SQLServer_Forecast, Best_model_prediction, tablename = "rvfc.result_prediction", append = TRUE, rownames = FALSE, fast = FALSE)


# Delete Query3 = DELETE FROM rvfc.result_finalranktimeseries WHERE Scenario = 'Revenue'
sqlQuery(SQLServer_Forecast, "EXEC dbo.sp_Delete_Query3")


Statistics_Agg <- Statistics_Agg %>% dplyr:::distinct()
Statistics_Agg$Scenario <- rep('Revenue', nrow(Statistics_Agg))
Statistics_Agg <- Statistics_Agg[, c('Master_Run_ID', 'Run_ID', 'Scenario', 'Technique', 'Method', 'Dataset', 'Dimension1', 'Dimension2', 'MAE', 'RMSE', 'MAPE', 'Final_Rank')]

sqlSave(SQLServer_Forecast, Statistics_Agg, tablename = "rvfc.result_finalranktimeseries", append = TRUE, rownames = FALSE, fast = FALSE)
