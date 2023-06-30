#############################################################################
# Advanced Forecasting System                       Date: 06/May/2020
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TS_3_Create_Ensemble_Technique_Revenue
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


# Fix the factor to the chracter format
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
table_modelruntimeseries$Scenario <- as.character(table_modelruntimeseries$Scenario)
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
table_modelruntimeseries$UoM <- as.character(table_modelruntimeseries$Uom)
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
}

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

Statistics_Agg <- data.frame(Master_Run_ID = character(0), Run_ID = character(0), Technique = character(0), Method = character(0), 
                             Dataset = character(0), Dimension1 = character(0), Dimension2 = character(0),
                             MAE = numeric(0), RMSE = numeric(0), MAPE = numeric(0), Time_Dimension = character(0), Final_Rank = numeric(0)) 

Statistics_Agg$Master_Run_ID <- as.character(Statistics_Agg$Master_Run_ID)
Statistics_Agg$Run_ID <- as.character(Statistics_Agg$Run_ID)
Statistics_Agg$Technique <- as.character(Statistics_Agg$Technique)
Statistics_Agg$Method <- as.character(Statistics_Agg$Method)
Statistics_Agg$Dataset <- as.character(Statistics_Agg$Dataset)
Statistics_Agg$Dimension1 <- as.character(Statistics_Agg$Dimension1)
Statistics_Agg$Dimension2 <- as.character(Statistics_Agg$Dimension2)

prediciton_agg <- function(master_runid){
  Error_Matrix <- table_statisticstimeseries[table_statisticstimeseries$Master_Run_ID == master_runid, ]
  statistics_agg <- Error_Matrix
  Rank_test <- rank(statistics_agg[statistics_agg$Dataset == 'TestSet', 'RMSE'], ties.method = 'random')
  statistics_agg$Final_Rank[statistics_agg$Dataset == 'TestSet'] <- Rank_test
  statistics_agg$Final_Rank <- as.character(na.locf(zoo(statistics_agg$Final_Rank), fromLast = TRUE))
  Statistics_Agg <- rbind(Statistics_Agg, statistics_agg)
  
  rm(Error_Matrix, statistics_agg)
  return (list(Statistics_Agg))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


output <- sapply(unique(table_statisticstimeseries$Master_Run_ID), prediciton_agg)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Statistics_Agg <- do.call(rbind, output)
row.names(Statistics_Agg) <- NULL
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

convert_date <- function(df){
  df$Fiscal_year <- format(as.yearmon(df$Period), '%Y')
  df$Posting_period <- format(as.yearmon(df$Period), '%m')
  return(df)
}


Statistics_Agg$Scenario <- rep(kpi, nrow(Statistics_Agg))
Statistics_Agg <- Statistics_Agg[, c('Master_Run_ID', 'Run_ID', 'Scenario', 'Technique', 'Method', 'Dataset', 'Dimension1', 'Dimension2', 'MAE', 'RMSE', 'MAPE','Time_Dimension','Final_Rank')]

##################################################################################
# Write Results back to SQL Server
##################################################################################

Statistics_Agg_r <- Statistics_Agg %>% dplyr:::distinct()

sqlSave(SQLServer_Forecast, Statistics_Agg_r, tablename = "rvfc.result_finalranktimeseries", append = TRUE, rownames = FALSE)
