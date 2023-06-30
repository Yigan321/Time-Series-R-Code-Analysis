#############################################################################
# Advanced Forecasting System                       Date : 06/May/2020
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TS_0A_PreRun_Preparations
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

con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "SQL Server",
                      Server   = "ssarfdb.eastus.cloudapp.azure.com",
                      Database = "Forecast",
                      UID      = "soothsayersql@ssasqlserver",
                      PWD      ="SoothDez@413",
                      Port     = 1433,
                      trusted_connection = TRUE)

con <- DBI::dbConnect(odbc::odbc(),
                      dsn = "ForecastDSN",
                      Driver   = "SQL Server",
                      Server   = "ssarfdb.eastus.cloudapp.azure.com",
                      Database = "Forecast",
                      UID      = "soothsayersql@ssasqlserver",
                      PWD      ="SoothDez@413",
                      Port     = 1433,
                      trusted_connection = "Yes")


con <- DBI::dbConnect(odbc::odbc(), "ForecastDSN", UID = "soothsayersql@ssasqlserver", PWD ="SoothDez@413", trusted_connection = TRUE)
con <- DBI::dbConnect(odbc::odbc(), "ForecastDSN", UID = "soothsayersql@ssasqlserver", PWD ="SoothDez@413", trusted_connection = TRUE)

                     
con2 <-  DBI::dbConnect(odbc::odbc(),dsn="ForecastDSN", uid="soothsayersql@ssasqlserver", pwd="SoothDez@413", trusted_connection = TRUE)                      

#SQLServer_Forecast <- RODBC::odbcConnect(dsn="ForecastDSN", uid="soothsayersql@ssasqlserver", pwd="SoothDez@413", trusted_connection = "Yes")

con1 <- odbcConnect("ForecastDSN", uid = "soothsayersql@ssasqlserver", pwd = "SoothDez@413",  trusted_connection = TRUE)

odbcGetInfo(con1)
sqlTables(con1, tableName = "rvfc.datainput_forecast")
sqlFetch(con1, "rvfc.datainput_forecast")

list <- dbListTables(con1)
View(list)
###########################################################
# Run the Preparations needed before Routine is launched
###########################################################

# dbSendUpdate(conn, "TRUNCATE TABLE rvfc.control_forecastscenario")

# dbSendUpdate(conn, "INSERT INTO rvfc.control_forecastscenario (Scenario, Active) 
#                    VALUES ('Order_Intake', 'X'),
#                           ('Revenue', '')")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Run Global initialization script
source("DS_2_Global_Initilization_Script.r")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Reading the result tables to load history result tables

finalprediction_table <- sqlFetch(con, "rvfc.result_finalprediction")
finalrankTS_table <- sqlFetch(SQLServer_Forecast, "rvfc.result_finalranktimeseries")
modelrunTS_table <- sqlFetch(SQLServer_Forecast, "rvfc.result_modelruntimeseries")
prediction_table <- sqlFetch(SQLServer_Forecast, "rvfc.result_prediction")
table_forecastscenario <- sqlFetch(SQLServer_Forecast, "rvfc.control_forecastscenario")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# table_forecastscenario <- as.data.frame(table_forecastscenario) 

table_scenario <- sqlFetch(SQLServer_Forecast, "rvfc.result_scenarios")
xgboost_scenario <- sqlFetch(SQLServer_Forecast, "rvfc.result_xgboostfeatureimportance")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#kpi <- table_forecastscenario[table_forecastscenario$Active == 'X', 'Scenario']

KPI_lookup <- function(){
  kpi <- table_forecastscenario[table_forecastscenario$Active == 'X', 'Scenario']
  return (kpi)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating a function to select Scenario option as KPI 
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

#Adding timestamp to the existing result table dataframe
time_stamp = Sys.time()

finalprediction_table$Time_stamp = time_stamp
finalrankTS_table$Time_stamp = time_stamp
modelrunTS_table$Time_stamp = time_stamp
prediction_table$Time_stamp = time_stamp
xgboost_scenario$Time_stamp = time_stamp
decomposition_table$Time_stamp = time_stamp

###############################################################################
# KPI Selection Criteria
###############################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if ((length(unique(data.prediction$Scenario)) == 1) || ((length(unique(data.prediction$Scenario)) == 2) && (kpi == "Order_Intake"))) {
  table_scenario$Time_stamp = time_stamp
  
  sqlSave(SQLServer_Forecast, table_scenario, tablename = "rvfc.result_scenarios_history", append = TRUE, rownames = FALSE)
}


###############################################################################
# Writing the results to Result History tables with Time stamp - Append
###############################################################################

sqlSave(SQLServer_Forecast, finalprediction_table, tablename = "rvfc.result_finalprediction_history", append = TRUE, rownames = FALSE)
sqlSave(SQLServer_Forecast, finalrankTS_table, tablename = "rvfc.result_finalranktimeseries_history", append = TRUE, rownames = FALSE)
sqlSave(SQLServer_Forecast, modelrunTS_table, tablename = "rvfc.result_modelruntimeseries_history", append = TRUE, rownames = FALSE)
sqlSave(SQLServer_Forecast, prediction_table, tablename = "rvfc.result_prediction_history", append = TRUE, rownames = FALSE)
sqlSave(SQLServer_Forecast, xgboost_scenario, tablename = "rvfc.result_xgboostfeatureimportance_history", append = TRUE, rownames = FALSE)
sqlSave(SQLServer_Forecast, decomposition_table, tablename = "rvfc.result_timeseriesdecomposition_history", append = TRUE, rownames = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Purging the dataframes for efficient memory utilization
rm(finalprediction_table, finalrankTS_table, modelrunTS_table, prediction_table)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Truncating existing tables

sqlQuery(SQLServer_Forecast, "TRUNCATE TABLE rvfc.work_datainput_timescenario")
sqlQuery(SQLServer_Forecast, "TRUNCATE TABLE rvfc.work_statisticalperformance")
sqlQuery(SQLServer_Forecast, "TRUNCATE TABLE rvfc.work_predictiontimeseries")
sqlQuery(SQLServer_Forecast, "TRUNCATE TABLE rvfc.work_validationtimeseries")
sqlQuery(SQLServer_Forecast, "TRUNCATE TABLE rvfc.work_predictionensemble")
sqlQuery(SQLServer_Forecast, "TRUNCATE TABLE rvfc.work_statisticsensemble")
sqlQuery(SQLServer_Forecast, "TRUNCATE TABLE rvfc.work_finalranktimeseries")
sqlQuery(SQLServer_Forecast, "TRUNCATE TABLE rvfc.result_xgboostfeatureimportance")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Truncating the Result tables
if ((length(unique(data.prediction$Scenario)) == 1) || ((length(unique(data.prediction$Scenario)) == 2) && (kpi == "Order_Intake"))){
  sqlQuery(SQLServer_Forecast, "TRUNCATE TABLE rvfc.output_dashboardtool")
  sqlQuery(SQLServer_Forecast, "TRUNCATE TABLE rvfc.result_scenarios")
  sqlQuery(SQLServer_Forecast, "TRUNCATE TABLE rvfc.result_finalprediction")
  sqlQuery(SQLServer_Forecast, "TRUNCATE TABLE rvfc.result_finalranktimeseries")
  sqlQuery(SQLServer_Forecast, "TRUNCATE TABLE rvfc.result_modelruntimeseries")
  sqlQuery(SQLServer_Forecast, "TRUNCATE TABLE rvfc.result_prediction")
  sqlQuery(SQLServer_Forecast, "TRUNCATE TABLE rvfc.eda_timeseriesdecomposition")
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~