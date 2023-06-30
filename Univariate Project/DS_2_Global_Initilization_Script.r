#############################################################################
# Advanced Forecasting System                       Date:  06/May/2021
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DS_2_Global_Initilization_Script
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

# Library initialization
##########################################################
library(rlang)
library(DMwR)
library(zoo)
library(dplyr)
library(reshape)
#dyn.load('/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/server/libjvm.so')
#library(rJava)
#library(RJDBC)
#drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver")
#conn <- dbConnect(drv,jdbcUrl)
library(date)
library(lubridate)
library(TTR)
library(smooth)
library(caret)
library(xgboost)
library(tensorflow)
library(keras)
library(forecast, lib.loc("/databricks/spark/R/lib"))
library(prophet)
##########################################################


control_timescenario <- sqlFetch(SQLServer_Forecast, "rvfc.control_timescenario")
table_forecastscenario <- sqlFetch(SQLServer_Forecast, "rvfc.control_forecastscenario")
table_timethreshold <- sqlFetch(SQLServer_Forecast, "rvfc.control_timethreshold")
table_timeseriescontrol <- sqlFetch(SQLServer_Forecast, "rvfc.control_parameters")
data.prediction <- sqlFetch(SQLServer_Forecast, "rvfc.datainput_forecast")
finalprediction_table <- sqlFetch(SQLServer_Forecast, "rvfc.result_finalprediction")
finalrankTS_table <- sqlFetch(SQLServer_Forecast, "rvfc.result_finalranktimeseries")
modelrunTS_table <- sqlFetch(SQLServer_Forecast, "rvfc.result_modelruntimeseries")
prediction_table <- sqlFetch(SQLServer_Forecast, "rvfc.result_prediction")
table_scenario <- sqlFetch(SQLServer_Forecast, "rvfc.result_scenarios")
xgboost_scenario <- sqlFetch(SQLServer_Forecast, "rvfc.result_xgboostfeatureimportance")
decomposition_table <- sqlFetch(SQLServer_Forecast, "rvfc.result_timeseriesdecomposition")
datainput_timescenario <- sqlFetch(SQLServer_Forecast, "rvfc.work_datainput_timescenario")