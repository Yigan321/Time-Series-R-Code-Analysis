#############################################################################
# Advanced Forecasting System                       Date: 06/May/2020
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TS_6_Update_Control_Table
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Run Global initialization script
source("DS_2_Global_Initilization_Script.r")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##################################################################################
# Update the Control table
##################################################################################

table_forecastscenario <- sqlFetch(SQLServer_Forecast, "rvfc.control_forecastscenario")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

KPI_lookup <- function(){
  kpi <- table_forecastscenario[table_forecastscenario$Active == 'X', 'Scenario']
  return (kpi)
}

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


output_dashboardtool_table <- sqlFetch(SQLServer_Forecast, "rvfc.work_dashboardtool")

# Create the If statement for the order intake

if ((length(unique(data.prediction$Scenario)) == 1) || ((length(unique(data.prediction$Scenario)) == 2) && (kpi == "Order_Intake"))){
  scenario_df <- output_dashboardtool_table[(output_dashboardtool_table$Version == 'Forecast') & (output_dashboardtool_table$Scenario == kpi) &
                                              (output_dashboardtool_table$Rank == 1)
                                            ,c("Master_Run_ID", "Run_ID", "Scenario", "Dimension1", "Dimension2", "Date", "UoM", "Point_Forecast",
                                               "Time_Dimension")]
  
  names(scenario_df) <- c("Master_Run_ID", "Run_ID", "Scenario", "Dimension1", "Dimension2", "Date", "UoM", "Base_Forecast", "Time_Dimension")
  
  #   scenario_df$Business_Intuition <- NA  
  #   scenario_df$Composite_Forecast <- ifelse(scenario_df$Business_Intuition == 0, scenario_df$Base_Forecast, scenario_df$Business_Intuition)
  scenario_df$Composite_Forecast <- scenario_df$Base_Forecast
}


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Fix the factor to the chracter format
  scenario_df$Master_Run_ID <- as.character(scenario_df$Master_Run_ID)
  scenario_df$Run_ID <- as.character(scenario_df$Run_ID)
  scenario_df$Scenario <- as.character(scenario_df$Scenario)
  scenario_df$Dimension1 <- as.character(scenario_df$Dimension1)
  scenario_df$Dimension2 <- as.character(scenario_df$Dimension2)
  scenario_df$Uom <- as.character(scenario_df$Uom)
  scenario_df$Time_Dimension <- as.numeric(scenario_df$Time_Dimension)
  
  # Add the business intuition column
  scenario_df$Business_Intuition <- NA
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Append OrderIntake Values into the result Scenarios Table


  

  if ((length(unique(data.prediction$Scenario)) == 1) || ((length(unique(data.prediction$Scenario)) == 2) && (kpi == "Order_Intake"))){
    scenario_df <- as.DataFrame(scenario_df)
    sqlSave(SQLServer_Forecast, scenario_df, tablename = "rvfc.result_scenarios", append = TRUE, rownames = FALSE, fast = FALSE)
  }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# Update Additional Datasets

sqlSave(SQLServer_Forecast, output_dashboardtool_table, tablename = "rvfc.output_dashboardtool", append = TRUE, rownames = FALSE, fast = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Apply a timestamp

time_stamp2 = Sys.time()

output_dashboardtool_table$Time_stamp <- time_stamp2

# Create the output history file

sqlSave(SQLServer_Forecast, output_dashboardtool_table, tablename = "rvfc.output_dashboardtool_history", append = TRUE, rownames = FALSE, fast = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

control_table_update <- function(kpi){ 
  if (length(unique(data.prediction$Scenario)) == 2) {
    dbSendUpdate(conn, "TRUNCATE TABLE rvfc.control_forecastscenario")
    if (kpi == 'Order_Intake') {
      dbSendUpdate(conn, "INSERT INTO rvfc.control_forecastscenario (Scenario, Active)
      VALUES ('Order_Intake', ''),
      ('Revenue', 'X')")
    }
    if (kpi == 'Revenue') {
      dbSendUpdate(conn, "INSERT INTO rvfc.control_forecastscenario (Scenario, Active)
      VALUES ('Order_Intake', 'X'),
      ('Revenue', '')")
    }
  }
  
}
