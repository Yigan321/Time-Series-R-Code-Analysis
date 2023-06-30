#############################################################################
# Advanced Forecasting System                       Date: 06/May/2020
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Master Routine - Adhoc
#############################################################################


# Set the Timer
start.time <- Sys.time()

# Remove all objects from Global Environment.
rm(list=ls())

# Supress warning messages.
options(warn=-1)

# Set a working directory
setwd("D:/R Scripts/")



#############################################################################
# Prerequisties - Install the Libraries
#############################################################################

# MUST DO THIS BEFORE RUNNING ROUTINE

source("DS_1_Library_Installation.r")
#source("DS_1_Library_Installation.r") for DS_2

#############################################################################
# Prerequisties - Reset the Tables & Purge Data 
#############################################################################

source("DF_Eureka_Data_Operations_Reset_Platform_Tables.r")

#############################################################################
# Prepare the Routine
#############################################################################

# Stage 1
run1 <- parse("TS_0A_PreRun_Preparations.r")
eval(run1)
run2 <- parse("TS_1_Prepare_TimeSeries_InputData_AdHoc.r")
eval(run2)
run3 <- parse("TS_2_Main_TimeSeries_Routine.r")
eval(run3)
run4 <- parse("TS_3_Create_Ensemble_Technique_OrderIntake.r")
eval(run4)
run5 <- parse("TS_4_Model_Selection_Routine.r") #TS_4_Model_Selection_Routine_adhoc
eval(run5)
run6 <- parse("TS_5_Final_Output.r")
eval(run6)
run7 <- parse("TS_6_Update_Control_Table.r")
eval(run7)

# Stage 2
run8 <- parse("TS_0A_PreRun_Preparations.r")
eval(run8)
run9 <- parse("TS_1_Prepare_TimeSeries_InputData_AdHoc.r")
eval(run9)
run10 <- parse("TS_2_Main_TimeSeries_Routine.r")
eval(run10)
run11 <- parse("TS_3_Create_Ensemble_Technique_Revenue.r")
eval(run11)
run12 <- parse("TS_4_Model_Selection_Routine.r") #TS_4_Model_Selection_Routine_adhoc
eval(run12)
run13 <- parse("TS_5_Final_Output.r")
eval(run13)
run14 <- parse("TS_6_Update_Control_Table.r")
eval(run14)

# Stage 3
run15 <- parse("ML_0A_PreRun_Preparations.r")
eval(run15)
run16 <- parse("ML_1_Prepare_ML_InputData_AdHoc.r")
eval(run16)
run17 <- parse("ML_2_Main_ML_Modeling_and_Forecasting_Routine.r")
eval(run17)
run18 <- parse("ML_3_Main_All_Ensemble_and_Model_Selection_Routine.r")
eval(run18)
run19 <- parse("ML_4_Final_Output.r")
eval(run19)


#############################################################################
# Calculate the Run Time
#############################################################################

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
