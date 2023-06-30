#############################################################################
# Advanced Forecasting System                       Date:  06/May/2021
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TS_1_Prepare_TimeSeries_InputData_AdHoc
#############################################################################

# Remove all objects from Global Environment.
rm(list=ls())

# Suppress warning messages.
options(warn=-1)

# Set a working directory
setwd("D:/R Scripts/")

####################################################################################
# Establish Connection to SQL Server
####################################################################################


library(RODBC)

#SQLServer_Forecast <- odbcConnect(dsn="Forecast_SQL", uid="Derek", pwd="password")
#SQLServer_Forecast <- RODBC::odbcConnect(dsn="Forecast", uid="soothsayersql@ssasqlserver", pwd="SoothDez@413")


con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "SQL Server",
                      Server   = "ssarfdb.eastus.cloudapp.azure.com",
                      Database = "Forecast",
                      UID      = "soothsayersql@ssasqlserver",
                      PWD      ="SoothDez@413",
                      Port     = 1433)

sqlTables(con)

##################################################################################
# Time Series Statistical - Prepare the Input Data for the Main Dataset
##################################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Run Global initialization script
source("DS_2_Global_Initilization_Script.r")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


control_timescenario <- sqlFetch(SQLServer_Forecast, "rvfc.control_timescenario")
control_timescenario <- as.data.frame(control_timescenario)

# Fix the factor to the character format
control_timescenario$Start_Date <- as.character(control_timescenario$Start_Date)
control_timescenario$End_Date <- as.character(control_timescenario$End_Date)

table_forecastscenario <- sqlFetch(SQLServer_Forecast, "rvfc.control_forecastscenario")
table_forecastscenario <- as.data.frame(table_forecastscenario)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

KPI_lookup <- function(){
  kpi <- table_forecastscenario[table_forecastscenario$Active == 'X', 'Scenario']
  return (kpi)
}


# Creating a function to select Scenario option as KPI 
if (length(unique(data.prediction$Scenario)) == 1) {
  kpi <- unique(data.prediction$Scenario)
}
else {
  kpi <- KPI_lookup()
  if (length(kpi) == 0){
    kpi <- "Order_Intake"
  }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
data.prediction <- sqlFetch(SQLServer_Forecast, "rvfc.datainput_forecast")
data.prediction <- as.data.frame(data.prediction)

  
data.prediction = data.prediction[data.prediction$Scenario == kpi,]
data.prediction <- data.prediction %>% dplyr:::distinct()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This is where the control time series table is filtereing based on the start and end date.

#start_date = as.numeric(unlist(strsplit(control_timescenario[1,1], "/")))
#end_date = as.numeric(unlist(strsplit(control_timescenario[1,2], "/")))

#start_date = as.yearmon(paste(start_date[2], start_date[1]), "%Y %m")
#end_date = as.yearmon(paste(end_date[2], end_date[1]), "%Y %m")

start_date <- control_timescenario[1,1]
end_date <- control_timescenario[1,2]

data.prediction <- as.data.frame(data.prediction %>%
                                   dplyr:::filter(as.Date(Date, "%m/%d/%Y") >= as.Date(start_date, "%m/%d/%Y") & as.Date(Date, "%m/%d/%Y") <= as.Date(end_date,"%m/%d/%Y")))
                                                                                                                                                      
head(data.prediction) 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Selecting the data without any missing values. Complete cases gives data with all variables filled. 
data.prediction = data.prediction[complete.cases(data.prediction),]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Write Dataframe back to SQL Server

sqlSave(SQLServer_Forecast, data.prediction, tablename = "rvfc.datainput_forecast", append = TRUE, rownames = FALSE)