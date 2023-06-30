#############################################################################
# Advanced Forecasting System                       Date: 06/May/2021
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TS_5_Final_Output
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
# Create the final output tables for downstream analysis
##################################################################################

#Run Global initialization script
source("DS_2_Global_Initilization_Script.r")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

table_forecastscenario <- sqlFetch(SQLServer_Forecast, "rvfc.control_forecastscenario")


KPI_lookup <- function(df){
  kpi <- table_forecastscenario[table_forecastscenario$Active == 'X', 'Scenario']
  return (kpi)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


TimeDimension_lookup <-function() {
  time_dimension <- names(control_timescenario)[apply(control_timescenario, 1, function(i) which(i == 'X'))]
  return(time_dimension)                                                    
}
Time_Dimension = TimeDimension_lookup()  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##################################################################################
# Run SQL Statements
##################################################################################

#delete all from rvfc.work_dashboardtool
SQL_Query_Truncate <- "TRUNCATE TABLE rvfc.work_dashboardtool"
sqlQuery(SQLServer_Forecast, SQL_Query_Truncate)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# append in work_dashboardtool

SQL_Query_Insert1 <- "INSERT INTO rvfc.work_dashboardtool (Version, Dimension1, Dimension2, [UoM], Date, Point_Forecast )
SELECT 'Actual' AS Expr1, rvfc.work_datainput_timescenario.[Dimension1], rvfc.work_datainput_timescenario.Dimension2, rvfc.work_datainput_timescenario.UoM, rvfc.work_datainput_timescenario.Date, rvfc.work_datainput_timescenario.Value
FROM rvfc.work_datainput_timescenario"


sqlQuery(SQLServer_Forecast, SQL_Query_Insert1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SQL_Query_Insert2 <- "INSERT INTO rvfc.work_dashboardtool (Version, Dimension1, Dimension2, Date, Point_Forecast, Lower_Level_1, Upper_Level_1, Lower_Level_2, Upper_Level_2, Time_Dimension, Rank, Master_Run_ID, Run_ID )
SELECT 'Forecast' AS Expr1, rvfc.result_finalprediction.Dimension1, rvfc.result_finalprediction.Dimension2, rvfc.result_finalprediction.Date, rvfc.result_finalprediction.Point_Forecast, rvfc.result_finalprediction.Lower_Level_1, rvfc.result_finalprediction.Upper_Level_1, rvfc.result_finalprediction.Lower_Level_2, rvfc.result_finalprediction.Upper_Level_2, rvfc.result_finalprediction.Time_Dimension, rvfc.result_finalprediction.Rank, rvfc.result_finalprediction.[Master_Run_ID], rvfc.result_finalprediction.Run_ID
FROM rvfc.result_finalprediction"

sqlQuery(SQLServer_Forecast, SQL_Query_Insert2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SQL_Query_Update1 <- "UPDATE  rvfc.work_dashboardtool 
SET rvfc.work_dashboardtool.[UoM] = rvfc.result_modelruntimeseries.[UoM]
FROM rvfc.result_modelruntimeseries
INNER JOIN rvfc.work_dashboardtool
ON (rvfc.work_dashboardtool.[Master_Run_ID] = rvfc.result_modelruntimeseries.[Master_Run_ID]) AND (rvfc.work_dashboardtool.Run_ID = rvfc.result_modelruntimeseries.Run_ID) WHERE (((rvfc.work_dashboardtool.Version)='Forecast'))"

sqlQuery(SQLServer_Forecast, SQL_Query_Update1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (length(unique(data.prediction$Scenario)) == 1) {
  kpi <- unique(data.prediction$Scenario)
  SQL_Query_Insert3 <- "UPDATE rvfc.work_dashboardtool SET rvfc.work_dashboardtool.Scenario = ", "'", kpi, "'", " WHERE (((rvfc.work_dashboardtool.Scenario) IS NULL))"
  sqlQuery(SQLServer_Forecast, SQL_Query_Insert3)
}

else {
  kpi <- KPI_lookup()
  if (length(kpi) == 0){
    kpi <- "Order_Intake"
  }
  if (kpi == 'Revenue') {
    SQL_Query_Insert4 <- "UPDATE rvfc.work_dashboardtool SET rvfc.work_dashboardtool.Scenario = 'Revenue' WHERE (((rvfc.work_dashboardtool.Scenario) IS NULL))"
    sqlQuery(SQLServer_Forecast, SQL_Query_Insert4)
  } 
  if (kpi == 'Order_Intake') {
    SQL_Query_Insert5 <- "UPDATE rvfc.work_dashboardtool SET rvfc.work_dashboardtool.Scenario = 'Order_Intake' WHERE (((rvfc.work_dashboardtool.Scenario) IS NULL))"
    sqlQuery(SQLServer_Forecast, SQL_Query_Insert5)
  }
  
} 



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
