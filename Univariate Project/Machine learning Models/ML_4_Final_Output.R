#############################################################################
# Advanced Forecasting System                       
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ML_4_Final_Output
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
# Machine Learning - Prepare the Final Output Tables
##################################################################################



sqlQuery(SQLServer_Forecast, "EXEC dbo.sp_Delete_Query4")

#delete all from rvfc.work_dashboardtool
SQL_Query_Truncate <- "TRUNCATE TABLE rvfc.work_dashboardtool"
sqlQuery(SQLServer_Forecast, SQL_Query_Truncate)

#delete all from rvfc.work_currencyconversion
SQL_Query_Truncate2 <- "TRUNCATE TABLE rvfc.work_currencyconversion"
sqlQuery(SQLServer_Forecast, SQL_Query_Truncate2)


# Append datainput into rvfc.work_currencyconversion
SQL_Query_Insert <- "INSERT INTO rvfc.work_currencyconversion (Dimension1, Dimension2, [Currency], Posting_period, Fiscal_Year, [Value], [Value2])
SELECT rvfc.datainput_forecast.[Dimension1], rvfc.datainput_forecast.Dimension2, rvfc.datainput_forecast.Currency, rvfc.datainput_forecast.Posting_period, rvfc.datainput_forecast.Fiscal_year, rvfc.datainput_forecast.Value, rvfc.datainput_forecast.Value_Euro
FROM rvfc.datainput_forecast"

sqlQuery(SQLServer_Forecast, SQL_Query_Insert)


# update currency conversion
sqlQuery(SQLServer_Forecast, "EXEC dbo.sp_Update_Query1")


#Update FX Rate 1
SQL_Query_Update2 <- "UPDATE rvfc.work_currencyconversion SET rvfc.work_currencyconversion.FX_Rate = 1
WHERE (((rvfc.work_currencyconversion.Currency)='EUR'))"

sqlQuery(SQLServer_Forecast, SQL_Query_Update2)

# truncate rvfc.work_currencyconversionpredict
SQL_Query_Truncate3 <- "TRUNCATE TABLE rvfc.work_currencyconversionpredict"
sqlQuery(SQLServer_Forecast, SQL_Query_Truncate3)

# append data to currencyconversionpredict
sqlQuery(SQLServer_Forecast, "EXEC dbo.sp_Insert_Query_2")


# update currency type
SQL_Query_Update3 <- "UPDATE rvfc.work_currencyconversionpredict 
SET rvfc.work_currencyconversionpredict.[Currency] = [Local_Currency]
FROM rvfc.result_modelruntimeseries
INNER JOIN rvfc.work_currencyconversionpredict
ON (rvfc.work_currencyconversionpredict.[Master_Run_ID] = rvfc.result_modelruntimeseries.[Master_Run_ID]) AND (rvfc.work_currencyconversionpredict.Run_ID = rvfc.result_modelruntimeseries.Run_ID)"

sqlQuery(SQLServer_Forecast, SQL_Query_Update3)

# update currency temporary
sqlQuery(SQLServer_Forecast, "EXEC dbo.sp_Update_Query2")

# update period

SQL_Query_Update4 <- "UPDATE rvfc.work_currencyconversionpredict
SET Posting_period = C.Posting_period, Fiscal_Year = C.Fiscal_Year
FROM rvfc.control_dateconversion C 
INNER JOIN rvfc.work_currencyconversionpredict
ON C.Period = rvfc.work_currencyconversionpredict.Period"

sqlQuery(SQLServer_Forecast, SQL_Query_Update4)


# update exchange rate

sqlQuery(SQLServer_Forecast, "EXEC dbo.sp_Update_Query3")
sqlQuery(SQLServer_Forecast, "EXEC dbo.sp_Update_Query4")

SQL_Query_Update5 <- "UPDATE rvfc.work_currencyconversionpredict 
SET rvfc.work_currencyconversionpredict.FX_Rate = 1
WHERE (((rvfc.work_currencyconversionpredict.Currency)='EUR'))"

sqlQuery(SQLServer_Forecast, SQL_Query_Update5)

# update conversion

SQL_Query_Update6 <- "UPDATE rvfc.work_currencyconversionpredict 
SET rvfc.work_currencyconversionpredict.Point_Forecast2 = [Point_Forecast]/[FX_Rate], rvfc.work_currencyconversionpredict.Lower_Level2_1 = [Lower_Level_1]/[FX_Rate], rvfc.work_currencyconversionpredict.Upper_Level2_1 = [Upper_Level_1]/[FX_Rate], rvfc.work_currencyconversionpredict.Lower_Level2_2 = [Lower_Level_2]/[FX_Rate], rvfc.work_currencyconversionpredict.Upper_Level2_2 = [Upper_Level_2]/[FX_Rate]"

sqlQuery(SQLServer_Forecast, SQL_Query_Update6)

# append in outputdashboardtool

SQL_Query_Insert2 <- "INSERT INTO rvfc.work_dashboardtool (Type, Version, Dimension1, Dimension2, [Currency], FX_Rate, Period, [Year], Point_Forecast )
SELECT 'LC' AS Expr3, 'Actual' AS Expr1, rvfc.work_currencyconversion.[Dimension1], rvfc.work_currencyconversion.Dimension2, rvfc.work_currencyconversion.Currency, 1 AS Expr2, rvfc.work_currencyconversion.Posting_period, rvfc.work_currencyconversion.Fiscal_Year, rvfc.work_currencyconversion.Value
FROM rvfc.work_currencyconversion"

sqlQuery(SQLServer_Forecast, SQL_Query_Insert2)


SQL_Query_Insert3 <- "INSERT INTO rvfc.work_dashboardtool (Type, Version, Dimension1, Dimension2, [Currency], FX_Rate, Period, [Year], Point_Forecast )
SELECT 'EUR' AS Expr3, 'Actual' AS Expr1, rvfc.work_currencyconversion.[Dimension1], rvfc.work_currencyconversion.Dimension2, rvfc.work_currencyconversion.Currency, rvfc.work_currencyconversion.FX_Rate, rvfc.work_currencyconversion.Posting_period, rvfc.work_currencyconversion.Fiscal_Year, rvfc.work_currencyconversion.Value2
FROM rvfc.work_currencyconversion"

sqlQuery(SQLServer_Forecast, SQL_Query_Insert3)


SQL_Query_Insert4 <- "INSERT INTO rvfc.work_dashboardtool (Type, Version, Dimension1, Dimension2, [Currency], FX_Rate, Period, [Year], Point_Forecast, Lower_Level_1, Upper_Level_1, Lower_Level_2, Upper_Level_2, Rank, Master_Run_ID, Run_ID )
SELECT 'LC' AS Expr3, 'Forecast' AS Expr1, rvfc.work_currencyconversionpredict.[Dimension1], rvfc.work_currencyconversionpredict.Dimension2, rvfc.work_currencyconversionpredict.Currency, 1 AS Expr2, rvfc.work_currencyconversionpredict.Posting_period, rvfc.work_currencyconversionpredict.Fiscal_Year, rvfc.work_currencyconversionpredict.Point_Forecast, rvfc.work_currencyconversionpredict.Lower_Level_1, rvfc.work_currencyconversionpredict.Upper_Level_1, rvfc.work_currencyconversionpredict.Lower_Level_2, rvfc.work_currencyconversionpredict.Upper_Level_2, rvfc.work_currencyconversionpredict.Rank, rvfc.work_currencyconversionpredict.[Master_Run_ID], rvfc.work_currencyconversionpredict.Run_ID
FROM rvfc.work_currencyconversionpredict"

sqlQuery(SQLServer_Forecast, SQL_Query_Insert4)

SQL_Query_Insert5 <- "INSERT INTO rvfc.work_dashboardtool (Type, Version, Dimension1, Dimension2, [Currency], FX_Rate, Period, [Year], Point_Forecast, Lower_Level_1, Upper_Level_1, Lower_Level_2, Upper_Level_2, Rank, Master_Run_ID, Run_ID )
SELECT 'EUR' AS Expr3, 'Forecast' AS Expr1, rvfc.work_currencyconversionpredict.[Dimension1], rvfc.work_currencyconversionpredict.Dimension2, rvfc.work_currencyconversionpredict.Currency, rvfc.work_currencyconversionpredict.FX_Rate, rvfc.work_currencyconversionpredict.Posting_period, rvfc.work_currencyconversionpredict.Fiscal_Year, rvfc.work_currencyconversionpredict.Point_Forecast2, rvfc.work_currencyconversionpredict.Lower_Level2_1, rvfc.work_currencyconversionpredict.Upper_Level2_1, rvfc.work_currencyconversionpredict.Lower_Level2_2, rvfc.work_currencyconversionpredict.Upper_Level2_2, rvfc.work_currencyconversionpredict.Rank, rvfc.work_currencyconversionpredict.[Master_Run_ID], rvfc.work_currencyconversionpredict.Run_ID
FROM rvfc.work_currencyconversionpredict"

sqlQuery(SQLServer_Forecast, SQL_Query_Insert5)

sqlQuery(SQLServer_Forecast, "EXEC dbo.sp_Update_Query6")


#####################################################################################
# Bring in the Final Table
#####################################################################################

output_dashboardtool_table <- sqlFetch(SQLServer_Forecast, "rvfc.work_dashboardtool")
output_dashboardtool_table <- as.data.frame(output_dashboardtool_table)


output_dashboardtool_table = output_dashboardtool_table[!is.na(output_dashboardtool_table$Year),]

sqlSave(SQLServer_Forecast, output_dashboardtool_table, tablename = "rvfc.output_dashboardtool", append = TRUE, rownames = FALSE, fast = FALSE)


