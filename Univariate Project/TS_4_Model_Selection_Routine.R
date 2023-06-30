#############################################################################
# Advanced Forecasting System                       Date:  06/May/2021
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TS_4_Model_Selection_Routine
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
# Time Series Statistical - Forecasting Methodologies & Main Routine
##################################################################################

#Run Global initialization script
source("DS_2_Global_Initilization_Script.r")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

table_predictionensemble <- sqlFetch(SQLServer_Forecast, "rvfc.work_predictionensemble")
table_predictiontimeseries <- sqlFetch(SQLServer_Forecast, "rvfc.work_predictiontimeseries")
table_finalranktimeseries <- sqlFetch(SQLServer_Forecast, "rvfc.result_finalranktimeseries")
table_forecastscenario <- sqlFetch(SQLServer_Forecast, "rvfc.control_forecastscenario")

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

# Fix the factor to the chracter format
table_finalranktimeseries$Master_Run_ID <- as.character(table_finalranktimeseries$Master_Run_ID)
table_finalranktimeseries$Run_ID <- as.character(table_finalranktimeseries$Run_ID)
table_finalranktimeseries$Dimension1 <- as.character(table_finalranktimeseries$Dimension1)
table_finalranktimeseries$Dimension2 <- as.character(table_finalranktimeseries$Dimension2)
table_finalranktimeseries$Technique <- as.character(table_finalranktimeseries$Technique)
table_finalranktimeseries$Scenario <- as.character(table_finalranktimeseries$Scenario)
table_finalranktimeseries$Method <- as.character(table_finalranktimeseries$Method)
table_finalranktimeseries$Dataset <- as.character(table_finalranktimeseries$Dataset)

table_predictionensemble$Master_Run_ID <- as.character(table_predictionensemble$Master_Run_ID)
table_predictionensemble$Run_ID <- as.character(table_predictionensemble$Run_ID)
table_predictionensemble$Dimension1 <- as.character(table_predictionensemble$Dimension1)
table_predictionensemble$Dimension2 <- as.character(table_predictionensemble$Dimension2)
table_predictionensemble$Period <- as.character(table_predictionensemble$Period)

table_predictiontimeseries$Master_Run_ID <- as.character(table_predictiontimeseries$Master_Run_ID)
table_predictiontimeseries$Run_ID <- as.character(table_predictiontimeseries$Run_ID)
table_predictiontimeseries$Dimension1 <- as.character(table_predictiontimeseries$Dimension1)
table_predictiontimeseries$Dimension2 <- as.character(table_predictiontimeseries$Dimension2)
table_predictiontimeseries$Period <- as.character(table_predictiontimeseries$Period)

##################################################################################
# Best Model Selection Routine
##################################################################################

Best_model_selection <- function(){
  if ((length(unique(data.prediction$Scenario)) == 1) || ((length(unique(data.prediction$Scenario)) == 2) && (kpi == "Order_Intake"))) {
    prediction_agg <- rbind(table_predictiontimeseries, table_predictionensemble)
    best_run_id <- unique(table_finalranktimeseries[table_finalranktimeseries$Final_Rank == 1, 'Run_ID'])
  } else {
    prediction_agg <- table_predictiontimeseries
    best_run_id <- unique(table_finalranktimeseries[table_finalranktimeseries$Final_Rank == 1, 'Run_ID'])
  }
  
  Filter <- function(id){
    return(prediction_agg[prediction_agg$Run_ID == id, ])
  }
  return((lapply(best_run_id, Filter)) %>% bind_rows())
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Best_model_prediction <- Best_model_selection()

Best_model_prediction$Scenario <- rep(kpi, nrow(Best_model_prediction))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Best_model_selection_prediction_cap <- function() {
  if ((length(unique(data.prediction$Scenario)) == 1) || ((length(unique(data.prediction$Scenario)) == 2) && (kpi == "Order_Intake"))) {
    prediction_agg <- rbind(table_predictiontimeseries, table_predictionensemble)
  } else {
    prediction_agg <- table_predictiontimeseries
  }
  
  Add_rank <- function(k){
    ID <- prediction_agg[k, 'Run_ID']
    rank_col <- table_finalranktimeseries[table_finalranktimeseries$Run_ID == ID, 'Final_Rank'][1]
    return (rank_col)
  }
  rank_list <- sapply(1:nrow(prediction_agg), Add_rank)
  prediction_agg$Rank <- rank_list
  
  return(prediction_agg)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Best_model_prediction_with_cap <- Best_model_selection_prediction_cap()
Best_model_prediction_with_cap = Best_model_prediction_with_cap[complete.cases(Best_model_prediction_with_cap),]

Best_model_prediction_with_cap$Scenario <- rep(kpi, nrow(Best_model_prediction_with_cap))

##################################################################################
# Date Conversion and Upload
##################################################################################

convert_date <- function(df, Posting_period, Fiscal_year){
  if (Time_Dimension=='Day'){
    Date = as.Date(as.numeric(Posting_period), origin = (as.Date(as.character(paste(Fiscal_year, 1, 1)), "%Y%m%d"))-days(1))
    Date = format(strptime(Date, format = "%Y-%m-%d"), "%m/%d/%Y")
  }
  else if (Time_Dimension=='Week'){
    Date = as.Date(paste(Fiscal_year, Posting_period, 1, sep="-"), "%Y-%U-%u")-weeks(1)
    Date = format(strptime(Date, format = "%Y-%m-%d"), "%m/%d/%Y")
  }
  else if (Time_Dimension=='Month'){
    Date = as.Date(as.character(paste(Fiscal_year, Posting_period, 1)), "%Y%m%d")
    Date = format(strptime(Date, format = "%Y-%m-%d"), "%m/%d/%Y")
  }
  else if (Time_Dimension=='Quarter'){
    Date = as.Date(as.yearqtr(paste0(Fiscal_year,'-',Posting_period)))
    Date = format(strptime(Date, format = "%Y-%m-%d"), "%m/%d/%Y")
  }
  else {
    Date = as.Date(paste(Fiscal_year, 1, 1, sep="-"), "%Y-%m-%d")
    Date = format(strptime(Date, format = "%Y-%m-%d"), "%m/%d/%Y")
  }
  
  return(Date)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TimeDimension_lookup <-function() {
  time_dimension <- names(control_timescenario)[apply(control_timescenario, 1, function(i) which(i == 'X'))]
  return(time_dimension)                                                    
}
Time_Dimension = TimeDimension_lookup() 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Best_model_prediction$Date <- convert_date(Best_model_prediction, Best_model_prediction$Posting_period, Best_model_prediction$Fiscal_year)
Best_model_prediction <- Best_model_prediction[, c('Master_Run_ID', 'Run_ID', 'Scenario', 'Dimension1', 'Dimension2', 'Date', 
                                                   'Point_Forecast', 'Lower_Level_1', 'Upper_Level_1', 
                                                   'Lower_Level_2', 'Upper_Level_2','Time_Dimension')]

Best_model_prediction_with_cap$Date <- convert_date(Best_model_prediction_with_cap, Best_model_prediction_with_cap$Posting_period, 
                                                    Best_model_prediction_with_cap$Fiscal_year)
Best_model_prediction_with_cap <- Best_model_prediction_with_cap[, c('Master_Run_ID', 'Run_ID', 'Scenario', 'Dimension1', 'Dimension2', 
                                                                     'Date', 'Point_Forecast', 
                                                                     'Lower_Level_1', 'Upper_Level_1', 'Lower_Level_2',
                                                                     'Upper_Level_2','Time_Dimension','Rank')]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# prepare the data for the upload

Best_model_prediction_r <- Best_model_prediction %>% dplyr:::distinct()
Best_model_prediction_with_cap_r <- Best_model_prediction_with_cap %>% dplyr:::distinct()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Fix the factor to the chracter format
Best_model_prediction_r$Master_Run_ID <- as.character(Best_model_prediction_r$Master_Run_ID)
Best_model_prediction_r$Run_ID <- as.character(Best_model_prediction_r$Run_ID)
Best_model_prediction_r$Scenario <- as.character(Best_model_prediction_r$Scenario)
Best_model_prediction_r$Dimension1 <- as.character(Best_model_prediction_r$Dimension1)
Best_model_prediction_r$Dimension2 <- as.character(Best_model_prediction_r$Dimension2)
Best_model_prediction_r$Date <- as.character(Best_model_prediction_r$Date)
Best_model_prediction_r$Time_Dimension <- as.numeric(Best_model_prediction_r$Time_Dimension)


Best_model_prediction_with_cap_r$Master_Run_ID <- as.character(Best_model_prediction_with_cap_r$Master_Run_ID)
Best_model_prediction_with_cap_r$Run_ID <- as.character(Best_model_prediction_with_cap_r$Run_ID)
Best_model_prediction_with_cap_r$Scenario <- as.character(Best_model_prediction_with_cap_r$Scenario)
Best_model_prediction_with_cap_r$Dimension1 <- as.character(Best_model_prediction_with_cap_r$Dimension1)
Best_model_prediction_with_cap_r$Dimension2 <- as.character(Best_model_prediction_with_cap_r$Dimension2)
Best_model_prediction_with_cap_r$Date <- as.character(Best_model_prediction_with_cap_r$Period)
Best_model_prediction_with_cap_r$Time_Dimension <- as.numeric(Best_model_prediction_with_cap_r$Time_Dimension)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sqlSave(SQLServer_Forecast, Best_model_prediction_r, tablename = "rvfc.result_prediction", append = TRUE, rownames = FALSE, fast = FALSE)
sqlSave(SQLServer_Forecast, Best_model_prediction_with_cap_r, tablename = "rvfc.result_finalprediction", append = TRUE, rownames = FALSE, fast = FALSE)