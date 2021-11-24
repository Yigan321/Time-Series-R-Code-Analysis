#############################################################################
# Advanced Forecasting System                       Author: Derek Kane 8/2020
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ML_0A_PreRun_Preparations
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


###########################################################
# Drop the ML Feature Table
###########################################################


# DROP rvfc.datainput_ml_factor
SQL_Query_Truncate <- "TRUNCATE TABLE rvfc.datainput_ml_factor"
# SQL_Query_Truncate <- "DROP TABLE rvfc.datainput_ml_factor"
sqlQuery(SQLServer_Forecast, SQL_Query_Truncate)