#############################################################################
# Advanced Forecasting System                       Date: 06/May/2021
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TS_2_Main_TimeSeries_Routine
# Time Series Statistical - Forecasting Methodologies & Main Routine
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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Run Global initialization script
source("DS_2_Global_Initilization_Script.r")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Bring in the forecast dataset

df <- sqlFetch(SQLServer_Forecast, "rvfc.datainput_forecast")


# Fix the factor to the chracter format
df$Dimension1 <- as.character(df$Dimension1)
df$Dimension2 <- as.character(df$Dimension2)
df$Scenario <- as.character(df$Scenario)
df$UoM <- as.character(df$UoM)


data.prediction <- as.data.frame(df)


data.prediction = DataFrame[complete.cases(data.prediction),]
data.prediction$Date = as.Date(data.prediction$Date, "%m/%d/%Y")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TimeDimension_lookup <- function() {
  time_dimension <- names(control_timescenario)[apply(control_timescenario, 1, function(i) which(i == 'X'))]
  return(time_dimension)                                                    
}
Time_Dimension = TimeDimension_lookup()     
print(Time_Dimension)                                                      

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Group the data.prediction to find min/max date
data.monthyear <- as.data.frame(data.prediction %>% 
                                  dplyr:::group_by(Dimension1, Dimension2) %>% 
                                  dplyr:::summarise("min_date"=min(Date), "max_date" = max(Date)))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Importing lookup table

table_timethreshold <- sqlFetch(SQLServer_Forecast, "rvfc.control_timethreshold")
table_timeseriescontrol <- sqlFetch(SQLServer_Forecast, "rvfc.control_parameters")
table_forecastscenario <- sqlFetch(SQLServer_Forecast, "rvfc.control_forecastscenario")


table_forecastscenario <- as.data.frame(table_forecastscenario)
table_timeseriescontrol <- as.data.frame(table_timeseriescontrol)
table_timethreshold <- as.data.frame(table_timethreshold)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Look up THRESHOLD from imported SQL table
Threshold_func <- function(diff){
  table_timethreshold <- table_timethreshold[table_timethreshold$Time_Dimension == Time_Dimension,  ]
  for (row in 1:nrow(table_timethreshold)){
    if ((diff >= table_timethreshold[row, 'Lower']) & (diff <= table_timethreshold[row, 'Upper'])){
      threshold <- table_timethreshold[row, 'Threshold']
      break
    }
  }
  return (threshold)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MAX_DATE <- max(data.monthyear$max_date)
data.monthyear$set_max <- MAX_DATE


 

data.monthyear$set_diff = Set_Diff()

# Remove data.monthyear with only 1 data point
data.monthyear <- data.monthyear[data.monthyear$set_diff > 1, ]

data.monthyear$type <- lapply(as.integer(data.monthyear$set_diff), Threshold_func)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data.ts <- data.monthyear[order(data.monthyear$set_diff, decreasing = T),]
rownames(data.ts) <-  1:nrow(data.ts)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Look up INPUT VARIABLES for prediction from imported SQL table
Timeseriescontrol_func <- function(threshold){
  table_timeseriescontrol = table_timeseriescontrol[table_timeseriescontrol$Time_Dimension == Time_Dimension, ]
  data_control <- as.data.frame(table_timeseriescontrol[(table_timeseriescontrol$Threshold == threshold) & 
                                                          (table_timeseriescontrol$Technique == 'Time_Series'), ])
  ###
  method <- data_control[data_control$Activation == 'Y', 'Method']
  ###
  validation_step <- data_control[1, 'Validation']
  forecast_step <- data_control[1, 'Forecast_Period']
  lower_ci <- data_control[1, 'Lower_Prediction_Interval']
  upper_ci <- data_control[1, 'Upper_Prediction_Interval']
  return(list(validation_step, forecast_step, lower_ci, upper_ci, method))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##################################################################################
#  Define MAPE Evaluation metrics
##################################################################################

# "mapefunc" is the Function written to set the upper cape of MAPE to "1" 
# when actual values are zero, since acutal value zero results in INf mape and thus  being misleading 

mapefunc <- function(train, test){
  train <- ifelse(train == 0, 0.01, train)
  mape <- abs((train - test) / train)
  mape <- ifelse(mape > 1, 1, mape)
  finalmape <- mean(mape)
  return(finalmape)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## find the missing values between Min date and Max date and impute it with "0" 
datafinal <- function(data, dimension1, dimension2, data.ts, k, time_dimension){
  data = data[((data$Dimension1 == dimension1) & (data$Dimension2 == dimension2)), ]
  y = data.ts[k, ]    
  if (time_dimension == 'Day'){
    data$Posting_period = as.numeric(strftime(data$Date, format = "%j"))
    t = as.data.frame(seq(as.Date(y$min_date[1]),as.Date(y$set_max[1]), by = 'day'))
    names(t) = c('Date') 
    t$Posting_period <- as.numeric(strftime(t$Date, format = "%j"))
  }
  else if (time_dimension == 'Week'){
    data$Posting_period = as.numeric(strftime(data$Date, format = "%V"))
    t = as.data.frame(seq(as.Date(y$min_date[1]),as.Date(y$set_max[1]), by = 'week'))
    names(t) = c('Date') 
    t$Posting_period <- as.numeric(strftime(t$Date, format = "%V"))
  }
  else if (time_dimension == 'Quarter'){
    data$Posting_period = as.numeric(substr(as.character(as.yearqtr(as.Date(data$Date, "%Y-%m-%d" ))), 7,7))
    t = as.data.frame(seq(as.Date(y$min_date[1]),as.Date(y$set_max[1]), by = 'quarter'))
    names(t) = c('Date') 
    t$Posting_period <- as.numeric(substr(as.character(as.yearqtr(as.Date(t$Date, "%Y-%m-%d" ))), 7,7))
  }
  else if (time_dimension == 'Month'){
    data$Posting_period = as.numeric(strftime(data$Date, format = "%m")) 
    t = as.data.frame(seq(as.Date(y$min_date[1]),as.Date(y$set_max[1]), by = 'month'))
    names(t) = c('Date') 
    t$Posting_period <- as.numeric(strftime(t$Date, format = "%m"))
  }
  else {
    data$Posting_period = 1
    t = as.data.frame(seq(as.Date(y$min_date[1]),as.Date(y$set_max[1]), by = 'year'))
    names(t) = c('Date') 
    t$Posting_period <- 1
  }
  data$Fiscal_year <- as.numeric(strftime(data$Date, format = "%Y")) 
  
  t$Fiscal_year <- as.numeric(strftime(t$Date, format = "%Y")) 
  t$Dimension1 <- dimension1
  t$Dimension2 <- dimension2
  t$Scenario = data$Scenario[1]
  t$UoM = data$UoM[1]
  
  
  agg_data <- as.data.frame(data %>% 
                              dplyr:::group_by(Posting_period, Fiscal_year) %>% 
                              dplyr:::summarize('Value' = sum(Value)))
  names(agg_data) = c('Posting_period', 'Fiscal_year', 'Value')
  
  ### Merge it with  data to populate missing values
  agg_data = merge(t, agg_data, all=T)
  agg_data = agg_data[order(agg_data$Date), ]
  agg_data$Value <- ifelse(is.na(agg_data$Value), 0, agg_data$Value)
  agg_data$type <- y$type
  agg_data = agg_data[, c('Dimension1', 'Dimension2', 'Scenario', 'UoM', 'Posting_period', 'Fiscal_year', 'Value','type')]  
  # Selecting only distinct value after Sorting
  agg_data = agg_data %>% dplyr:::distinct()  
  rm(t,y)
  return(agg_data)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##################################################################################
# Get model_run, error_matrix, prediction dataframes
##################################################################################

##################################################################################
# Get model_run, error_matrix, prediction dataframes
##################################################################################

Model_Info <- function(Model_type, freq=365, Master_Run_ID, Run_ID, dimension1, dimension2, type, traindata, testdata, validation_step, forecast_step, lower_ci, upper_ci, New_data, Model_run_timeseries, Error_Matrix, Prediction_M, Validation_M, Model, MA = 'NO'){
  
  ##### Model_run_info
  model_run <- as.data.frame(Master_Run_ID)
  model_run$Run_ID <- Run_ID
  model_run$Technique <- 'Time Series'
  model_run$Method <- Model_type
  model_run$Model_Run_Date <- as.character(format(strptime(Sys.Date(), format = "%Y-%m-%d"), "%m/%d/%Y")) 
  model_run$Dimension1 <- dimension1
  model_run$Dimension2 <- dimension2
  model_run$Threshold <- type
  
  model_run$Train_Start_Posting_period <- as.character(traindata$Posting_period[1])
  model_run$Train_Start_Fiscal_year <- as.character(traindata$Fiscal_year[1])
  model_run$Train_End_Posting_period <- as.character(traindata$Posting_period[nrow(traindata)])
  model_run$Train_End_Fiscal_year <- as.character(traindata$Fiscal_year[nrow(traindata)])
  model_run$Test_Start_Posting_period <- as.character(testdata$Posting_period[1])
  model_run$Test_Start_Fiscal_year <- as.character(testdata$Fiscal_year[1])
  model_run$Test_End_Posting_period <- as.character(testdata$Posting_period[nrow(testdata)])
  model_run$Test_End_Fiscal_year <- as.character(testdata$Fiscal_year[nrow(testdata)])
  
  model_run$Validation <- validation_step
  model_run$Forecast_Period <- forecast_step
  model_run$Lower_Prediction_Interval <- lower_ci
  model_run$Upper_Prediction_Interval <- upper_ci
  model_run$UoM <- New_data$UoM[1]
  Model_run_timeseries <- rbind(Model_run_timeseries, model_run)
  
  
  # Validation & Error_M
  Test_pred <- data.frame(forecast(Model, h = validation_step))
  Validation_test <- data.frame(Test_pred$Point.Forecast, testdata$Value)
  names(Validation_test) <- c('Point_forecast', 'Actual_value')
  Validation_test$Run_ID <- rep(Run_ID, nrow(Validation_test))
  Validation_test$Method <- rep(Model_type, nrow(Validation_test))
  Validation_test$Dataset <- rep('TestSet', nrow(Validation_test))
  Validation_test$Dimension1 <- rep(dimension1, nrow(Validation_test))
  Validation_test$Dimension2 <- rep(dimension2, nrow(Validation_test))
  Validation_test <- Validation_test[, c(3:7, 1:2)]
  
  Test_Error <- as.data.frame(Master_Run_ID)
  Test_Error$Run_ID <- Run_ID
  Test_Error$Technique <- 'TimeSeries'
  Test_Error$Method <- Model_type
  Test_Error$Dataset <- "TestSet"
  Test_Error$Dimension1 <- dimension1
  Test_Error$Dimension2 <- dimension2
  Test_Error$MAE <- regr.eval(testdata$Value, Test_pred$Point.Forecast, stats='mae')
  Test_Error$RMSE <- regr.eval(testdata$Value, Test_pred$Point.Forecast, stats='rmse')
  Test_Error$MAPE <- mapefunc(testdata$Value, Test_pred$Point.Forecast)
  
  
  if (MA == 'NO'){
    Train_pred <- as.data.frame(as.vector(fitted(Model)))
    colnames(Train_pred) <- c("Point.Forecast")
    
    Validation_train <- data.frame(Train_pred$Point.Forecast, traindata$Value)
    names(Validation_train) <- c('Point_forecast', 'Actual_value')
    Validation_train$Run_ID <- rep(Run_ID, nrow(Validation_train))
    Validation_train$Method <- rep(Model_type, nrow(Validation_train))
    Validation_train$Dataset <- rep('TrainSet', nrow(Validation_train))
    Validation_train$Dimension1 <- rep(dimension1, nrow(Validation_train))
    Validation_train$Dimension2 <- rep(dimension2, nrow(Validation_train))
    Validation_train <- Validation_train[, c(3:7, 1:2)]
    
    Validation_M <- rbind(Validation_M, Validation_train, Validation_test)
    
    Train_Error <- as.data.frame(Master_Run_ID)
    Train_Error$Run_ID <- Run_ID
    Train_Error$Technique <- 'TimeSeries'
    Train_Error$Method <- Model_type
    Train_Error$Dataset <- "TrainSet"
    Train_Error$Dimension1 <- dimension1
    Train_Error$Dimension2 <- dimension2
    Train_Error$MAE <- regr.eval(traindata$Value, Train_pred$Point.Forecast, stats='mae')
    Train_Error$RMSE <- regr.eval(traindata$Value, Train_pred$Point.Forecast, stats='rmse')
    Train_Error$MAPE <- mapefunc(traindata$Value, Train_pred$Point.Forecast)
    
    Error_Matrix <- rbind(Error_Matrix, Train_Error, Test_Error)
  }
  else{
    Validation_M <- rbind(Validation_M, Validation_test)
    Error_Matrix <- rbind(Error_Matrix, Test_Error)
  }
  
  
  ### Predict on test set   
  prediction_test <- Model_Building_Final(New_data, Model_type, dimension1, dimension2, Master_Run_ID, Run_ID, forecast_step, upper_ci, lower_ci, freq)       
  Prediction_M <- rbind(Prediction_M, prediction_test)
  
  rm(Train_pred, Model, Test_pred, Train_Error, Test_Error, prediction_test, Validation_train, Validation_test, model_run)
  
  
  return(list(Error_Matrix, Model_run_timeseries, Prediction_M, Validation_M))
}

##################################################################################
# Modeling with various methods
##################################################################################
### Modeling with various methods
Model_Building <- function(New_data, dimension1, dimension2, type, k, freq=365){
  
  Master_Run_ID <- paste('T', as.character(format((Sys.time() + seconds(k)), '%Y%m%d%H%M%S')), '00', sep='')
  # look up input ts prediction info
  prediction_info <- Timeseriescontrol_func(type) 
  validation_step <- as.numeric(prediction_info[1][[1]])
  forecast_step <- as.numeric(prediction_info[2][[1]])
  lower_ci <- as.numeric(prediction_info[3][[1]])
  upper_ci <- as.numeric(prediction_info[4][[1]])
  method <- as.character(prediction_info[[5]])
  
  ### Create a Place Holders for the Outputs of the Function. 
  Model_run_timeseries <- data.frame(Master_Run_ID = character(0), Run_ID =character(0), Technique = character(0), 
                                     Method = character(0), 
                                     Model_Run_Date = character(0), Dimension1 = character(0), Dimension2 = character(0),
                                     Threshold = character(0), Train_Start_Posting_period = character(0), Train_Start_Fiscal_year = character(0), 
                                     Train_End_Posting_period = character(0), Train_End_Fiscal_year = character(0), Test_Start_Posting_period = character(0),
                                     Test_Start_Fiscal_year = character(0), Test_End_Posting_period = character(0), Test_End_Fiscal_year = character(0),
                                     Validation = numeric(0), 
                                     Forecast_Period = numeric(0), Lower_Prediction_Interval = numeric(0),
                                     Upper_Prediction_Interval = numeric(0))
  
  Error_Matrix <- data.frame(Master_Run_ID = character(0), Run_ID = character(0), Technique = character(0), 
                             Method = character(0), Dataset = character(0), 
                             Dimension1 = character(0), Dimension2 = character(0),
                             MAE = numeric(0), RMSE = numeric(0), MAPE = numeric(0))
  
  Prediction_M <- data.frame(Master_Run_ID = character(0), Run_ID = character(0), Dimension1 = character(0), 
                             Dimension2 = character(0),
                             Posting_period = numeric(0), Fiscal_year = numeric(0), Point_Forecast = numeric(0), 
                             Lower_Level_1 = numeric(0), Upper_Level_1 = numeric(0), 
                             Lower_Level_2 = numeric(0), Upper_Level_2 = numeric(0)) 
  Validation_M <- data.frame(Run_ID = character(0), Method = character(0), Dataset = character(0),
                             Dimension1 = character(0), Dimension2 = character(0),
                             Point_forecast = numeric(0), Actual_value = numeric(0))
  
  #########
  # Decomposition
  data_seq <- ts(New_data$Value, frequency = freq,
                 start = c(as.numeric(New_data$Fiscal_year[1]),as.numeric(New_data$Posting_period[1])))
  Decompose_TS <- as.data.frame(rep(Master_Run_ID, length(data_seq)))
  names(Decompose_TS) <- 'Master_Run_ID'
  Decompose_TS$Dimension1 <- rep(dimension1, length(data_seq))
  Decompose_TS$Dimension2 <- rep(dimension2, length(data_seq))
  Decompose_TS$Posting_period <- as.character(New_data$Posting_period)
  Decompose_TS$Fiscal_year <- as.character(New_data$Fiscal_year)
  Decompose_TS$UoM <- New_data$UoM
  Decompose_TS$Actual <- New_data$Value
  #Get Trend, Seasonality, Randomness
  Decompose_TS$Trend <- rep(0, length(data_seq))
  Decompose_TS$Seasonality <- rep(0, length(data_seq))
  Decompose_TS$Randomness <- rep(0, length(data_seq))
  tryCatch({
    #STL is package for timeseries decompose - acronym for "Seasonal and Trend decomposition 
    stl_decompose <- stl(data_seq, s.window='periodic')$time.series
    Decompose_TS$Trend <- as.numeric(stl_decompose[, 'trend'])
    Decompose_TS$Seasonality <- as.numeric(stl_decompose[, 'seasonal'])
    Decompose_TS$Randomness <- as.numeric(stl_decompose[, 'remainder'])},
    error = function(e){
      print(e)
    })
  
  
  # set train/validation dataset
  start <- 1
  end <- nrow(New_data) - validation_step
  traindata <- New_data[start: end, ]
  testdata <- New_data[((end + 1):nrow(New_data)), ]
  
  
  ### Preparing the data as time series object
  dataTimeSeries <- ts(traindata$Value, frequency = freq,
                       start = c(as.numeric(traindata$Fiscal_year[1]),as.numeric(traindata$Posting_period[1])))
  
  
  #ARIMA model
  Model_run <- function(){
    count <- 0
    for (method_name in method){
      if (method_name == 'ARIMA'){
        tryCatch({
          ### Model Building 
          Run_ID <- paste('T', as.character(format((Sys.time() + seconds(k)), '%Y%m%d%H%M%S')), '01', sep='')
          Model <- auto.arima(dataTimeSeries)
          
          output_table <- Model_Info(method_name, freq, Master_Run_ID, Run_ID, dimension1, dimension2, type, traindata, testdata, validation_step, forecast_step, lower_ci, upper_ci, New_data, Model_run_timeseries, Error_Matrix, Prediction_M, Validation_M, Model)
          
          Error_Matrix <- output_table[[1]]
          Model_run_timeseries <- output_table[[2]]
          Prediction_M <- output_table[[3]]
          Validation_M <- output_table[[4]]
        },
        error = function(e) {
          print(e)
          count=count+1}
        )
      }
      
      #TSLM model
      if (method_name == 'TSLM'){
        tryCatch({
          Run_ID <- paste('T', as.character(format((Sys.time() + seconds(k)), '%Y%m%d%H%M%S')), '02', sep='')
          Model <- tslm(dataTimeSeries ~ trend + season)
          
          output_table <- Model_Info(method_name, freq, Master_Run_ID, Run_ID, dimension1, dimension2, type, traindata, testdata, validation_step, forecast_step, lower_ci, upper_ci, New_data, Model_run_timeseries, Error_Matrix, Prediction_M, Validation_M, Model)
          
          Error_Matrix <- output_table[[1]]
          Model_run_timeseries <- output_table[[2]]
          Prediction_M <- output_table[[3]]
          Validation_M <- output_table[[4]]
        },
        error = function(e) {
          print(e)
          count=count+1})
      }
      
      
      #SMA3 model
      if (method_name == 'SMA3'){
        
        tryCatch({
          Run_ID <- paste('T', as.character(format((Sys.time() + seconds(k)), '%Y%m%d%H%M%S')), '03', sep='')
          Model <- sma(dataTimeSeries, order = 3)
          
          output_table <- Model_Info(method_name, freq, Master_Run_ID, Run_ID, dimension1, dimension2, type, traindata, testdata, validation_step, forecast_step, lower_ci, upper_ci, New_data, Model_run_timeseries, Error_Matrix, Prediction_M, Validation_M, Model)
          
          Error_Matrix <- output_table[[1]]
          Model_run_timeseries <- output_table[[2]]
          Prediction_M <- output_table[[3]]
          Validation_M <- output_table[[4]]
        },
        error = function(e) {
          print(e)
          count=count+1})
      }
      
      #SME6 model
      if (method_name == 'SMA6'){
        tryCatch({
          Run_ID <- paste('T', as.character(format((Sys.time() + seconds(k)), '%Y%m%d%H%M%S')), '04', sep='')
          Model <- sma(dataTimeSeries, order = 6)
          
          output_table <- Model_Info(method_name, freq,Master_Run_ID, Run_ID, dimension1, dimension2, type, traindata, testdata, validation_step, forecast_step, lower_ci, upper_ci, New_data, Model_run_timeseries, Error_Matrix, Prediction_M, Validation_M, Model)
          
          Error_Matrix <- output_table[[1]]
          Model_run_timeseries <- output_table[[2]]
          Prediction_M <- output_table[[3]]
          Validation_M <- output_table[[4]]
        },
        error = function(e) {
          print(e)
          count=count+1})
      }
      
      #SMA9 model
      if (method_name == 'SMA9'){
        tryCatch({
          Run_ID <- paste('T', as.character(format((Sys.time() + seconds(k)), '%Y%m%d%H%M%S')), '05', sep='')
          Model <- sma(dataTimeSeries, order = 9)
          
          output_table <- Model_Info(method_name, freq,Master_Run_ID, Run_ID, dimension1, dimension2, type, traindata, testdata, validation_step, forecast_step, lower_ci, upper_ci, New_data, Model_run_timeseries, Error_Matrix, Prediction_M, Validation_M, Model)
          
          Error_Matrix <- output_table[[1]]
          Model_run_timeseries <- output_table[[2]]
          Prediction_M <- output_table[[3]]
          Validation_M <- output_table[[4]]
        },
        error = function(e) {
          print(e)
          count=count+1})
      }
      
      #WMA3 model
      if (method_name == 'WMA3'){
        tryCatch({
          Run_ID <- paste('T', as.character(format((Sys.time() + seconds(k)), '%Y%m%d%H%M%S')), '06', sep='')
          Model <- WMA(dataTimeSeries, order = 3)
          
          output_table <- Model_Info(method_name, freq,Master_Run_ID, Run_ID, dimension1, dimension2, type, traindata, testdata, validation_step, forecast_step, lower_ci, upper_ci, New_data, Model_run_timeseries, Error_Matrix, Prediction_M, Validation_M, Model, 'M')
          
          Error_Matrix <- output_table[[1]]
          Model_run_timeseries <- output_table[[2]]
          Prediction_M <- output_table[[3]]
          Validation_M <- output_table[[4]]
        },
        error = function(e) {
          print(e)
          count=count+1})
      }      
      
      #WMA6 model
      if (method_name == 'WMA6'){
        tryCatch({
          Run_ID <- paste('T', as.character(format((Sys.time() + seconds(k)), '%Y%m%d%H%M%S')), '07', sep='')
          Model <- WMA(dataTimeSeries, order = 6)
          
          output_table <- Model_Info(method_name, freq,Master_Run_ID, Run_ID, dimension1, dimension2, type, traindata, testdata, validation_step, forecast_step, lower_ci, upper_ci, New_data, Model_run_timeseries, Error_Matrix, Prediction_M, Validation_M, Model, 'M')
          
          Error_Matrix <- output_table[[1]]
          Model_run_timeseries <- output_table[[2]]
          Prediction_M <- output_table[[3]]
          Validation_M <- output_table[[4]]
        },
        error = function(e) {
          print(e)
          count=count+1})
      }            
      
      #WMA9 model
      if (method_name == 'WMA9'){
        tryCatch({
          Run_ID <- paste('T', as.character(format((Sys.time() + seconds(k)), '%Y%m%d%H%M%S')), '08', sep='')
          Model <- WMA(dataTimeSeries, order = 9)
          
          output_table <- Model_Info(method_name, freq,Master_Run_ID, Run_ID, dimension1, dimension2, type, traindata, testdata, validation_step, forecast_step, lower_ci, upper_ci, New_data, Model_run_timeseries, Error_Matrix, Prediction_M, Validation_M, Model, 'M')
          
          Error_Matrix <- output_table[[1]]
          Model_run_timeseries <- output_table[[2]]
          Prediction_M <- output_table[[3]]
          Validation_M <- output_table[[4]]
        },
        error = function(e) {
          print(e)
          count=count+1})
      }            
      
      #EMA3 model
      if (method_name == 'EMA3'){
        tryCatch({
          Run_ID <- paste('T', as.character(format((Sys.time() + seconds(k)), '%Y%m%d%H%M%S')), '09', sep='')
          Model <- EMA(dataTimeSeries, 3)
          
          output_table <- Model_Info(method_name, freq,Master_Run_ID, Run_ID, dimension1, dimension2, type, traindata, testdata, validation_step, forecast_step, lower_ci, upper_ci, New_data, Model_run_timeseries, Error_Matrix, Prediction_M, Validation_M, Model, 'M')
          
          Error_Matrix <- output_table[[1]]
          Model_run_timeseries <- output_table[[2]]
          Prediction_M <- output_table[[3]]
          Validation_M <- output_table[[4]]
        },
        error = function(e) {
          print(e)
          count=count+1})
      }      
      
      #EMA6 model
      if (method_name == 'EMA6'){
        tryCatch({
          Run_ID <- paste('T', as.character(format((Sys.time() + seconds(k)), '%Y%m%d%H%M%S')), '10', sep='')
          Model <- EMA(dataTimeSeries, 6)
          
          output_table <- Model_Info(method_name, freq,Master_Run_ID, Run_ID, dimension1, dimension2, type, traindata, testdata, validation_step, forecast_step, lower_ci, upper_ci, New_data, Model_run_timeseries, Error_Matrix, Prediction_M, Validation_M, Model, 'M')
          
          Error_Matrix <- output_table[[1]]
          Model_run_timeseries <- output_table[[2]]
          Prediction_M <- output_table[[3]]
          Validation_M <- output_table[[4]]
        },
        error = function(e) {
          print(e)
          count=count+1})
      }            
      
      #EMA9 model
      if (method_name == 'EMA9'){
        tryCatch({
          Run_ID <- paste('T', as.character(format((Sys.time() + seconds(k)), '%Y%m%d%H%M%S')), '11', sep='')
          Model <- EMA(dataTimeSeries, 9)
          
          output_table <- Model_Info(method_name, freq, Master_Run_ID, Run_ID, dimension1, dimension2, type, traindata, testdata, validation_step, forecast_step, lower_ci, upper_ci, New_data, Model_run_timeseries, Error_Matrix, Prediction_M, Validation_M, Model, 'M')
          
          Error_Matrix <- output_table[[1]]
          Model_run_timeseries <- output_table[[2]]
          Prediction_M <- output_table[[3]]
          Validation_M <- output_table[[4]]
        },
        error = function(e) {
          print(e)
          count=count+1})
      }            
      
      #Holt-Winter model
      if (method_name == 'Holt-Winter'){
        tryCatch({
          Run_ID <- paste('T', as.character(format((Sys.time() + seconds(k)), '%Y%m%d%H%M%S')), '12', sep='')
          Model <- HoltWinters(dataTimeSeries)
          
          output_table <- Model_Info(method_name, freq, Master_Run_ID, Run_ID, dimension1, dimension2, type, traindata, testdata, validation_step, forecast_step, lower_ci, upper_ci, New_data, Model_run_timeseries, Error_Matrix, Prediction_M, Validation_M, Model, 'M')
          
          Error_Matrix <- output_table[[1]]
          Model_run_timeseries <- output_table[[2]]
          Prediction_M <- output_table[[3]]
          Validation_M <- output_table[[4]]
        },
        error = function(e) {
          print(e)
          count=count+1})
      }
      
      #Prophet model
      if (method_name == 'Prophet'){
        tryCatch({
          Run_ID <- paste('T', as.character(format((Sys.time() + seconds(k)), '%Y%m%d%H%M%S')), '13', sep='')
          
          model_run <- as.data.frame(Master_Run_ID)
          model_run$Run_ID <- Run_ID
          model_run$Technique <- 'Time Series'
          model_run$Method <- method_name
          model_run$Model_Run_Date <- as.character(format(strptime(Sys.Date(), format = "%Y-%m-%d"), "%m/%d/%Y")) 
          model_run$Dimension1 <- dimension1
          model_run$Dimension2 <- dimension2
          model_run$Threshold <- type
          
          model_run$Train_Start_Posting_period <- as.character(traindata$Posting_period[1])
          model_run$Train_Start_Fiscal_year <- as.character(traindata$Fiscal_year[1])
          model_run$Train_End_Posting_period <- as.character(traindata$Posting_period[nrow(traindata)])
          model_run$Train_End_Fiscal_year <- as.character(traindata$Fiscal_year[nrow(traindata)])
          model_run$Test_Start_Posting_period <- as.character(testdata$Posting_period[1])
          model_run$Test_Start_Fiscal_year <- as.character(testdata$Fiscal_year[1])
          model_run$Test_End_Posting_period <- as.character(testdata$Posting_period[nrow(testdata)])
          model_run$Test_End_Fiscal_year <- as.character(testdata$Fiscal_year[nrow(testdata)])
          
          model_run$Validation <- validation_step
          model_run$Forecast_Period <- forecast_step
          model_run$Lower_Prediction_Interval <- lower_ci
          model_run$Upper_Prediction_Interval <- upper_ci
          model_run$UoM <- New_data$UoM[1]
          Model_run_timeseries <- rbind(Model_run_timeseries, model_run)
          
          # Data processing
          if (freq==365){
            seq_start = as.Date(traindata$Posting_period[1], origin = (as.Date(as.character(paste(traindata$Fiscal_year[1], 1, 1)),"%Y%m%d"))-days(1))
            seq_end = as.Date(traindata$Posting_period[nrow(traindata)], origin = (as.Date(as.character(paste(traindata$Fiscal_year[nrow(traindata)], 1,
                                                                                                              1)),"%Y%m%d"))-days(1)) 
            seq_ts <- seq(from = seq_start, length.out = length(dataTimeSeries), by ='day') 
          }
          else if (freq==52) {
            seq_start = as.Date(paste(traindata$Fiscal_year[1], traindata$Posting_period[1], 1, sep="-"), "%Y-%U-%u")
            seq_end = as.Date(paste(traindata$Fiscal_year[nrow(traindata)], traindata$Posting_period[nrow(traindata)], 1, sep="-"), "%Y-%U-%u")
            seq_ts <- seq(from = seq_start, length.out = length(dataTimeSeries), by ='week') 
            
          }
          else if (freq==12) {
            seq_start = as.Date(as.character(paste(traindata$Fiscal_year[1], traindata$Posting_period[1], 1)), "%Y%m%d")  
            seq_end = as.Date(as.character(paste(traindata$Fiscal_year[nrow(traindata)], traindata$Posting_period[nrow(traindata)], 1)), "%Y%m%d")  
            seq_ts <- seq(from = seq_start, length.out = length(dataTimeSeries), by ='month') 
          }
          else if (freq==4) {
            seq_start = as.Date(as.yearqtr(paste0('Q',traindata$Posting_period[1], '/',traindata$Fiscal_year[1]), format = "Q%q/%y"))
            seq_end = as.Date(as.yearqtr(paste0('Q',traindata$Posting_period[nrow(traindata)], '/',traindata$Fiscal_year[nrow(traindata)]), 
                                         format = "Q%q/%y"))
            seq_ts <- seq(from = seq_start, length.out = length(dataTimeSeries), by ='quarter') 
          }
          else{
            seq_start = as.Date(as.character(paste(traindata$Fiscal_year[1], 1, 1)), "%Y%m%d") 
            seq_end = as.Date(as.character(paste(traindata$Fiscal_year[nrow(traindata)], 1, 1)), "%Y%m%d")  
            seq_ts <- seq(from = seq_start, length.out = length(dataTimeSeries), by ='year') 
          }
          
          
          data_p <- data.frame(ds=as.Date(seq_ts), y=melt(dataTimeSeries)$value)
          Model <- prophet(data_p, daily.seasonality=TRUE, weekly.seasonality=FALSE, yearly.seasonality=TRUE)
          future <- make_future_dataframe(Model, periods = validation_step)
          forecast_v <- predict(Model, future)
          
          Train_pred <- as.data.frame(forecast_v[1:end, 'yhat'])
          colnames(Train_pred) <- c("Point.Forecast")
          Validation_train <- data.frame(Train_pred$Point.Forecast, traindata$Value)
          names(Validation_train) <- c('Point_forecast', 'Actual_value')
          Validation_train$Run_ID <- rep(Run_ID, nrow(Validation_train))
          Validation_train$Method <- rep(method_name, nrow(Validation_train))
          Validation_train$Dataset <- rep('TrainSet', nrow(Validation_train))
          Validation_train$Dimension1 <- rep(dimension1, nrow(Validation_train))
          Validation_train$Dimension2 <- rep(dimension2, nrow(Validation_train))
          Validation_train <- Validation_train[, c(3:7, 1:2)]
          
          Test_pred <- forecast_v[(end + 1): nrow(New_data), ]
          Validation_test <- data.frame(Test_pred$yhat, testdata$Value)
          names(Validation_test) <- c('Point_forecast', 'Actual_value')
          Validation_test$Run_ID <- rep(Run_ID, nrow(Validation_test))
          Validation_test$Method <- rep(method_name, nrow(Validation_test))
          Validation_test$Dataset <- rep('TestSet', nrow(Validation_test))
          Validation_test$Dimension1 <- rep(dimension1, nrow(Validation_test))
          Validation_test$Dimension2 <- rep(dimension2, nrow(Validation_test))
          Validation_test <- Validation_test[, c(3:7, 1:2)]
          
          Validation_M <- rbind(Validation_M, Validation_train, Validation_test)
          Train_Error <- as.data.frame(Master_Run_ID)
          Train_Error$Run_ID <- Run_ID
          Train_Error$Technique <- 'TimeSeries'
          Train_Error$Method <- method_name
          Train_Error$Dataset <- "TrainSet"
          Train_Error$Dimension1 <- dimension1
          Train_Error$Dimension2 <- dimension2
          Train_Error$MAE <- regr.eval(traindata$Value, Train_pred$Point.Forecast, stats='mae')
          Train_Error$RMSE <- regr.eval(traindata$Value, Train_pred$Point.Forecast, stats='rmse')
          Train_Error$MAPE <- mapefunc(traindata$Value, Train_pred$Point.Forecast)
          
          Test_Error <- as.data.frame(Master_Run_ID)
          Test_Error$Run_ID <- Run_ID
          Test_Error$Technique <- 'TimeSeries'
          Test_Error$Method <- method_name
          Test_Error$Dataset <- "TestSet"
          Test_Error$Dimension1 <- dimension1
          Test_Error$Dimension2 <- dimension2
          Test_Error$MAE <- regr.eval(testdata$Value, Test_pred$yhat, stats='mae')
          Test_Error$RMSE <- regr.eval(testdata$Value, Test_pred$yhat, stats='rmse')
          Test_Error$MAPE <- mapefunc(testdata$Value, Test_pred$yhat)            
          Error_Matrix <- rbind(Error_Matrix, Train_Error, Test_Error)
          
          
          ### Predict on test set       
          prediction_test <- Model_Building_Final(New_data, method_name, dimension1, dimension2, Master_Run_ID, Run_ID, forecast_step, upper_ci, lower_ci, freq)       
          Prediction_M <- rbind(Prediction_M, prediction_test)
          
          rm(Train_pred, Model, Test_pred, Train_Error, Test_Error, prediction_test, Validation_train, Validation_test, model_run)          
        },
        error = function(e) {
          #print(e)
          count=count+1})
      }        
      
      
      #Previous_Period model
      if (method_name == 'Previous_Period'){
        tryCatch({
          Run_ID <- paste('T', as.character(format((Sys.time() + seconds(k)), '%Y%m%d%H%M%S')), '14', sep='')
          model_run <- as.data.frame(Master_Run_ID)
          model_run$Run_ID <- Run_ID
          model_run$Technique <- 'Time Series'
          model_run$Method <- method_name
          model_run$Model_Run_Date <- as.character(format(strptime(Sys.Date(), format = "%Y-%m-%d"), "%m/%d/%Y")) 
          model_run$Dimension1 <- dimension1
          model_run$Dimension2 <- dimension2
          model_run$Threshold <- type
          
          model_run$Train_Start_Posting_period <- as.character(traindata$Posting_period[1])
          model_run$Train_Start_Fiscal_year <- as.character(traindata$Fiscal_year[1])
          model_run$Train_End_Posting_period <- as.character(traindata$Posting_period[nrow(traindata)])
          model_run$Train_End_Fiscal_year <- as.character(traindata$Fiscal_year[nrow(traindata)])
          model_run$Test_Start_Posting_period <- as.character(testdata$Posting_period[1])
          model_run$Test_Start_Fiscal_year <- as.character(testdata$Fiscal_year[1])
          model_run$Test_End_Posting_period <- as.character(testdata$Posting_period[nrow(testdata)])
          model_run$Test_End_Fiscal_year <- as.character(testdata$Fiscal_year[nrow(testdata)])
          
          model_run$Validation <- validation_step
          model_run$Forecast_Period <- forecast_step
          model_run$Lower_Prediction_Interval <- lower_ci
          model_run$Upper_Prediction_Interval <- upper_ci
          model_run$UoM <- New_data$UoM[1]
          Model_run_timeseries <- rbind(Model_run_timeseries, model_run)
          
          ### Getting the fitted values
          Previous_period <- traindata[nrow(traindata), 'Value']  
          
          Train_pred <- as.data.frame(rep(Previous_period, nrow(traindata)))
          colnames(Train_pred) <- c("Point.Forecast")
          
          ### Binding it with the global Test_Forecast data frame
          Test_pred <- data.frame(rep(Previous_period, validation_step))
          colnames(Test_pred) <- c("Point.Forecast")
          
          Validation_train <- data.frame(Train_pred$Point.Forecast, traindata$Value)
          names(Validation_train) <- c('Point_forecast', 'Actual_value')
          Validation_train$Run_ID <- rep(Run_ID, nrow(Validation_train))
          Validation_train$Method <- rep(method_name, nrow(Validation_train))
          Validation_train$Dataset <- rep('TrainSet', nrow(Validation_train))
          Validation_train$Dimension1 <- rep(dimension1, nrow(Validation_train))
          Validation_train$Dimension2 <- rep(dimension2, nrow(Validation_train))
          Validation_train <- Validation_train[, c(3:7, 1:2)]
          
          Validation_test <- data.frame(Test_pred$Point.Forecast, testdata$Value)
          names(Validation_test) <- c('Point_forecast', 'Actual_value')
          Validation_test$Run_ID <- rep(Run_ID, nrow(Validation_test))
          Validation_test$Method <- rep(method_name, nrow(Validation_test))
          Validation_test$Dataset <- rep('TestSet', nrow(Validation_test))
          Validation_test$Dimension1 <- rep(dimension1, nrow(Validation_test))
          Validation_test$Dimension2 <- rep(dimension2, nrow(Validation_test))
          Validation_test <- Validation_test[, c(3:7, 1:2)]
          
          Validation_M <- rbind(Validation_M, Validation_train, Validation_test)  
          
          
          ### Binding it with the global Error_Matrix data frame
          Train_Error <- as.data.frame(Master_Run_ID)
          Train_Error$Run_ID <- Run_ID
          Train_Error$Technique <- 'TimeSeries'
          Train_Error$Method <- method_name
          Train_Error$Dataset <- "TrainSet"
          Train_Error$Dimension1 <- dimension1
          Train_Error$Dimension2 <- dimension2
          Train_Error$MAE <- regr.eval(traindata$Value, Train_pred$Point.Forecast, stats='mae')
          Train_Error$RMSE <- regr.eval(traindata$Value, Train_pred$Point.Forecast, stats='rmse')
          Train_Error$MAPE <- mapefunc(traindata$Value, Train_pred$Point.Forecast)
          
          Test_Error <- as.data.frame(Master_Run_ID)
          Test_Error$Run_ID <- Run_ID
          Test_Error$Technique <- 'TimeSeries'
          Test_Error$Method <- method_name
          Test_Error$Dataset <- 'TestSet'
          Test_Error$Dimension1 <- dimension1
          Test_Error$Dimension2 <- dimension2
          Test_Error$MAE <- regr.eval(testdata$Value, Test_pred$Point.Forecast, stats='mae')
          Test_Error$RMSE <- regr.eval(testdata$Value, Test_pred$Point.Forecast, stats='rmse')
          Test_Error$MAPE <- mapefunc(testdata$Value, Test_pred$Point.Forecast)
          
          Error_Matrix <- rbind(Error_Matrix, Train_Error, Test_Error)
          
          ### Predict on test set       
          prediction_test <- Model_Building_Final(New_data, method_name, dimension1, dimension2, Master_Run_ID, Run_ID, forecast_step, upper_ci, lower_ci, freq)       
          
          Prediction_M <- rbind(Prediction_M, prediction_test)
          
          rm(Train_pred, Model, Test_pred, Train_Error, Test_Error, prediction_test, Validation_train, Validation_test)
        },
        error = function(e) {
          print(e)
          count=count+1})
      }
      
      # TBATS model      
      if (method_name == 'TBATS'){
        tryCatch({
          Run_ID <- paste('T', as.character(format((Sys.time() + seconds(k)), '%Y%m%d%H%M%S')), '15', sep='')
          Model <- tbats(dataTimeSeries)
          
          output_table <- Model_Info(method_name, freq,Master_Run_ID, Run_ID, dimension1, dimension2, type, traindata, testdata, validation_step, forecast_step, lower_ci, upper_ci, New_data, Model_run_timeseries, Error_Matrix, Prediction_M, Validation_M, Model)
          
          Error_Matrix <- output_table[[1]]
          Model_run_timeseries <- output_table[[2]]
          Prediction_M <- output_table[[3]]
          Validation_M <- output_table[[4]]
        },
        error = function(e) {
          print(e)
          count=count+1})
      }      
      
    }
    return (list(Error_Matrix, count, Model_run_timeseries, Prediction_M, Validation_M, Decompose_TS))
  }
  
  return(Model_run())
  
}
##################################################################################
# Predict future n steps with specified model
##################################################################################

# output Point forecast and Confidence intervals
Future_Prediction <- function(Model, forecast_step, ci_upper, ci_lower, Test_Forecast, MA = 'NO'){
  
  if (MA != 'NO'){
    Test_pred_lower <- data.frame(forecast(Model, h = forecast_step, level = ci_lower/100))
    Test_pred_upper <- data.frame(forecast(Model, h = forecast_step, level = ci_upper/100))[, 2:3]
    Test_pred <- cbind(Test_pred_lower, Test_pred_upper)
    names(Test_pred) <- c('Point_Forecast', 'Lower_Level_1', 'Upper_Level_1', 'Lower_Level_2', 'Upper_Level_2')
    if (Test_pred$Lower_Level_1 < -2^31 | Test_pred$Upper_Level_1 > 2^31-1 | Test_pred$Lower_Level_2 < -2^31 | Test_pred$Upper_Level_2 > 2^31-1){
      Test_Pred$Lower_Level_1 = Test_Pred$Point_Forecast * 0.80
      Test_Pred$Upper_Level_1 = Test_Pred$Point_Forecast * 1.20
      Test_Pred$Lower_Level_2 = Test_Pred$Point_Forecast * 0.50
      Test_Pred$Upper_Level_2 = Test_Pred$Point_Forecast * 1.50
    } 
    Test_pred <- cbind(Test_Forecast, Test_pred)
  }
  else{
    Test_pred <- data.frame(forecast(Model, h = forecast_step, level = c(ci_lower, ci_upper)))
    names(Test_pred) <- c('Point_Forecast', 'Lower_Level_1', 'Upper_Level_1', 'Lower_Level_2', 'Upper_Level_2')
    if (Test_pred$Lower_Level_1 < -2^31 | Test_pred$Upper_Level_1 > 2^31-1 | Test_pred$Lower_Level_2 < -2^31 | Test_pred$Upper_Level_2 > 2^31-1){
      Test_Pred$Lower_Level_1 = Test_Pred$Point_Forecast * 0.80
      Test_Pred$Upper_Level_1 = Test_Pred$Point_Forecast * 1.20
      Test_Pred$Lower_Level_2 = Test_Pred$Point_Forecast * 0.50
      Test_Pred$Upper_Level_2 = Test_Pred$Point_Forecast * 1.50
    } 
    Test_pred <- cbind(Test_Forecast, Test_pred)
    return(Test_pred)
  }
  
}

##################################################################################
# Build model for predicting unseen data (test set)
##################################################################################
# Build model for predicting unseen data (test set)

Model_Building_Final <- function(New_data, model_name="Prophet", dimension1, dimension2, masterrunid, runid, forecast_step, CI_upper, CI_lower, freq=365){
  # Preparing the data as time series object
  
  dataTimeSeries <- ts(New_data$Value, frequency = freq, start = c(as.numeric(New_data$Fiscal_year[1]),as.numeric(New_data$Posting_period[1])))
  if (freq==365){      
    Train_end_date = as.Date(New_data$Posting_period[nrow(New_data)], origin = (as.Date(as.character(paste(New_data$Fiscal_year[nrow(New_data)], 1, 1)), "%Y%m%d"))-days(1))
    Forecast_start_date = Train_end_date+days(1)
    Test_Forecast <- as.data.frame(seq(as.Date(Forecast_start_date), by ='day', length.out = forecast_step))        
    names(Test_Forecast) <- 'Period'
    Test_Forecast$Posting_period <- as.numeric(strftime(Test_Forecast$Period, format = "%j"))
  }
  else if (freq==52){
    Train_end_date = as.Date(paste(New_data$Fiscal_year[nrow(New_data)], New_data$Posting_period[nrow(New_data)], 1, sep="-"), "%Y-%U-%u")
    Forecast_start_date = Train_end_date+weeks(1)
    Test_Forecast <- as.data.frame(seq(as.Date(Forecast_start_date), by ='week', length.out = forecast_step))        
    names(Test_Forecast) <- 'Period'
    Test_Forecast$Posting_period <- as.numeric(strftime(Test_Forecast$Period, format = "%V"))
    
  }
  else if (freq==12){
    Train_end_date = as.Date(as.character(paste(New_data$Fiscal_year[nrow(New_data)], New_data$Posting_period[nrow(New_data)], 1)), "%Y%m%d")  
    Forecast_start_date = Train_end_date+months(1)
    Test_Forecast <- as.data.frame(seq(as.Date(Forecast_start_date), by ='month', length.out = forecast_step))        
    names(Test_Forecast) <- 'Period'
    Test_Forecast$Posting_period <- as.numeric(strftime(Test_Forecast$Period, format = "%m"))
  }
  else if (freq==4){    
    Train_end_date = as.Date(as.yearqtr(paste0('Q',New_data$Posting_period[nrow(New_data)], '/',New_data$Fiscal_year[nrow(New_data)]), format = "Q%q/%y"))
    Forecast_start_date = Train_end_date+months(3)
    Test_Forecast <- as.data.frame(seq(as.Date(Forecast_start_date), by ='quarter', length.out = forecast_step)) 
    names(Test_Forecast) <- 'Period'
    Test_Forecast$Posting_period = as.numeric(substr(as.character(as.yearqtr(as.Date(Test_Forecast$Period, "%Y-%m-%d" ))), 7,7))
    
  }
  else {
    Train_end_date = as.Date(as.character(paste(New_data$Fiscal_year[nrow(New_data)], 1, 1)), "%Y%m%d")  
    Forecast_start_date = Train_end_date+years(1)
    Test_Forecast <- as.data.frame(seq(as.Date(Forecast_start_date), by ='year', length.out = forecast_step))        
    names(Test_Forecast) <- 'Period'
    Test_Forecast$Posting_period <- 1
  }
  
  
  Test_Forecast$Fiscal_year <- as.numeric(strftime(Test_Forecast$Period, format = "%Y")) 
  
  Test_Forecast$Master_Run_ID <- masterrunid
  Test_Forecast$Run_ID <- runid
  Test_Forecast$Dimension1 <- dimension1
  Test_Forecast$Dimension2 <- dimension2
  rownames(Test_Forecast) <-  1:nrow(Test_Forecast)
  Test_Forecast <- Test_Forecast[, c(4, 5, 6, 7, 2, 3)]
  
  ###autoarima
  if(model_name == "ARIMA"){
    Model <- auto.arima(dataTimeSeries)
    Test_pred <- Future_Prediction(Model, forecast_step, CI_upper, CI_lower, Test_Forecast)
  }
  else if(model_name == "TSLM"){
    Model <- tslm(dataTimeSeries ~ trend + season)
    Test_pred <- Future_Prediction(Model, forecast_step, CI_upper, CI_lower, Test_Forecast)
  }
  else if(model_name == "SMA3"){
    Model <- sma(dataTimeSeries, order = 3)
    Test_pred <- Future_Prediction(Model, forecast_step, CI_upper, CI_lower, Test_Forecast, 'MA')
  }    
  else if(model_name == "SMA6"){
    Model <- sma(dataTimeSeries, order = 6)
    Test_pred <- Future_Prediction(Model, forecast_step, CI_upper, CI_lower, Test_Forecast, 'MA')
  }       
  else if(model_name == "SMA9"){
    Model <- sma(dataTimeSeries, order = 9)
    Test_pred <- Future_Prediction(Model, forecast_step, CI_upper, CI_lower, Test_Forecast, 'MA')
  }  
  else if(model_name == "EMA3"){
    Model <- EMA(dataTimeSeries, 3)
    Test_pred <- Future_Prediction(Model, forecast_step, CI_upper, CI_lower, Test_Forecast)
  }  
  else if(model_name == "EMA6"){
    Model <- EMA(dataTimeSeries, order = 6)
    Test_pred <- Future_Prediction(Model, forecast_step, CI_upper, CI_lower, Test_Forecast)
  }  
  else if(model_name == "EMA9"){
    Model <- EMA(dataTimeSeries, order = 9)
    Test_pred <- Future_Prediction(Model, forecast_step, CI_upper, CI_lower, Test_Forecast)
  }  
  else if(model_name == "WMA3"){
    Model <- WMA(dataTimeSeries, 3)
    Test_pred <- Future_Prediction(Model, forecast_step, CI_upper, CI_lower, Test_Forecast)
  }  
  else if(model_name == "WMA6"){
    Model <- WMA(dataTimeSeries, order = 6)
    Test_pred <- Future_Prediction(Model, forecast_step, CI_upper, CI_lower, Test_Forecast)
  }  
  else if(model_name == "WMA9"){
    Model <- WMA(dataTimeSeries, order = 9)
    Test_pred <- Future_Prediction(Model, forecast_step, CI_upper, CI_lower, Test_Forecast)
  }                          
  else if(model_name == "Holt-Winter"){
    Model <- HoltWinters(dataTimeSeries)
    Test_pred <- Future_Prediction(Model, forecast_step, CI_upper, CI_lower, Test_Forecast)
  }
  else if(model_name == "Prophet"){
    
    if (freq==365){
      seq_start = as.Date(New_data$Posting_period[1], origin = (as.Date(as.character(paste(New_data$Fiscal_year[1], 1, 1)),"%Y%m%d"))-days(1))
      seq_end = as.Date(New_data$Posting_period[nrow(New_data)], origin = (as.Date(as.character(paste(New_data$Fiscal_year[nrow(New_data)], 1,
                                                                                                      1)),"%Y%m%d"))-days(1))
      seq_ts <- seq(from = seq_start, length.out = length(dataTimeSeries), by ='day') 
    }
    else if (freq==52) {
      seq_start = as.Date(paste(New_data$Fiscal_year[1], New_data$Posting_period[1], 1, sep="-"), "%Y-%U-%u")
      seq_end = as.Date(paste(New_data$Fiscal_year[nrow(New_data)], New_data$Posting_period[nrow(New_data)], 1, sep="-"), "%Y-%U-%u")
      seq_ts <- seq(from = seq_start, length.out = length(dataTimeSeries), by ='week') 
      
    }
    else if (freq==12) {
      seq_start = as.Date(as.character(paste(New_data$Fiscal_year[1], New_data$Posting_period[1], 1)), "%Y%m%d")  
      seq_end = as.Date(as.character(paste(New_data$Fiscal_year[nrow(New_data)], New_data$Posting_period[nrow(New_data)], 1)), "%Y%m%d")  
      seq_ts <- seq(from = seq_start, length.out = length(dataTimeSeries), by ='month') 
    }
    else if (freq==4) {
      seq_start = as.Date(as.yearqtr(paste0('Q',New_data$Posting_period[1], '/',New_data$Fiscal_year[1]), format = "Q%q/%y"))
      seq_end = as.Date(as.yearqtr(paste0('Q',New_data$Posting_period[nrow(New_data)], '/',New_data$Fiscal_year[nrow(New_data)]), 
                                   format = "Q%q/%y"))
      seq_ts <- seq(from = seq_start, length.out = length(dataTimeSeries), by ='quarter') 
    }
    else{
      seq_start = as.Date(as.character(paste(New_data$Fiscal_year[1], 1, 1)), "%Y%m%d") 
      seq_end = as.Date(as.character(paste(New_data$Fiscal_year[nrow(New_data)], 1, 1)), "%Y%m%d")  
      seq_ts <- seq(from = seq_start, length.out = length(dataTimeSeries), by ='year') 
    }
    
    data_p <- data.frame(ds=as.Date(seq_ts), y=melt(dataTimeSeries)$value)
    model <- prophet(data_p, weekly.seasonality=TRUE, daily.seasonality=TRUE, interval.width = CI_lower/100)
    future <- make_future_dataframe(model, periods = forecast_step)
    forecast_l <- predict(model, future)[(nrow(data_p)+1): (nrow(data_p)+forecast_step) , c('yhat', 'yhat_lower', 'yhat_upper')]
    model <- prophet(data_p, weekly.seasonality=TRUE, daily.seasonality=TRUE, interval.width = CI_upper/100)
    future <- make_future_dataframe(model, periods = forecast_step)
    forecast_u <- predict(model, future)[(nrow(data_p)+1): (nrow(data_p)+forecast_step), c('yhat_lower', 'yhat_upper')]
    Test_pred <- cbind(forecast_l, forecast_u)
    names(Test_pred) <- c('Point_Forecast', 'Lower_Level_1', 'Upper_Level_1', 'Lower_Level_2', 'Upper_Level_2')
    Test_pred <- cbind(Test_Forecast, Test_pred)
  }  
  
  else if(model_name == "Previous_Period"){
    Previous_period <- New_data[nrow(New_data), 'Value']
    Test_pred <- data.frame(rep(Previous_period, forecast_step))
    names(Test_pred) <- c('Point_Forecast')
    Test_pred$Lower_Level_1 <- Test_pred$Point_Forecast
    Test_pred$Upper_Level_1 <- Test_pred$Point_Forecast
    Test_pred$Lower_Level_2 <- Test_pred$Point_Forecast
    Test_pred$Upper_Level_2 <- Test_pred$Point_Forecast
    Test_pred <- cbind(Test_Forecast, Test_pred)  
  }
  else if(model_name == "TBATS"){
    Model <- tbats(dataTimeSeries)
    Test_pred <- Future_Prediction(Model, forecast_step, CI_upper, CI_lower, Test_Forecast)    
  }
  
  rm(Test_Forecast)
  return (Test_pred)
}
##################################################################################
# Main Routine
##################################################################################
# Main
### Creating Place holders for the final Data files

temp <- data.frame(Dimension1 = character(0), Dimension2 = character(0))


### Final File which contains Error metrics for test and train points
Error_M <- data.frame(Master_Run_ID = character(0), Run_ID = character(0), Technique = character(0), 
                      Method = character(0), Dataset = character(0), 
                      Dimension1 = character(0), Dimension2 = character(0), 
                      MAE = numeric(0), RMSE = numeric(0), MAPE = numeric(0))

Model_run_TS <- data.frame(Master_Run_ID = character(0), Run_ID =character(0), Technique = character(0), 
                           Method = character(0), 
                           Model_Run_Date = character(0), Dimension1 = character(0), Dimension2 = character(0),
                           Data_Threshold = character(0), Train_Start_Posting_period = character(0), Train_Start_Fiscal_year = character(0), 
                           Train_End_Posting_period = character(0), Train_End_Fiscal_year = character(0), Test_Start_Posting_period = character(0),
                           Test_Start_Fiscal_year = character(0), Test_End_Posting_period = character(0), Test_End_Fiscal_year = character(0),
                           Validation = numeric(0), 
                           Forecast_Period = numeric(0), Lower_Prediction_Interval = numeric(0),
                           Upper_Prediction_Interval = numeric(0))

Prediction_TS <- data.frame(Master_Run_ID = character(0), Run_ID = character(0), Dimension1 = character(0), 
                            Dimension2 = character(0),
                            Posting_period = numeric(0), Fiscal_year = numeric(0), Point_Forecast = numeric(0), 
                            Lower_Level_1 = numeric(0), Upper_Level_1 = numeric(0), 
                            Lower_Level_2 = numeric(0), Upper_Level_2 = numeric(0)) 

Validation_TS <- data.frame(Run_ID = character(0), Method = character(0), Dataset = character(0),
                            Dimension1 = character(0), Dimension2 = character(0),
                            Point_forecast = numeric(0), Actual_value = numeric(0)) 
Decompose_TS <- data.frame(Master_Run_ID = character(0), Dimension1 = character(0), Dimension2 = character(0),
                           Date = character(0), UoM = character(0),
                           Actual = numeric(0), Trend = numeric(0), Seasonality = numeric(0), Randomness = numeric(0))

Output_results <- function(k){
  pr <- tryCatch({
    dimension_1 <- as.character(data.ts$Dimension1[k])
    dimension_2 <- as.character(data.ts$Dimension2[k])
    time_dimension = Time_Dimension
    datapr <- datafinal(data.prediction, dimension_1, dimension_2, data.ts, k, time_dimension)
    row.names(datapr) <- seq(1, nrow(datapr))
    ##########################################
    t <- datapr$type[1][[1]]
    if (time_dimension=='Day'){
      freq = 365
    }
    else if (time_dimension=='Week'){
      freq = 52
    }
    else if (time_dimension=='Month'){
      freq = 12
    }
    else if (time_dimension=='Quarter'){
      freq = 4
    }
    else{
      freq = 1
    }    
    Final_result <- Model_Building(datapr, dimension_1, dimension_2, t, k, freq)
    error <- Final_result[[1]]
    count <- Final_result[[2]]
    model_run <- Final_result[[3]]
    prediction <- Final_result[[4]]
    validation <- Final_result[[5]]
    decompose <- Final_result[[6]]
    print('output results done')
    
    ### Error handling checks
    if(count == 13){ 
      temp1 <- data.frame(Dimension1 = dimension_1, Dimension2 = dimension_2)
      temp <- rbind(temp,temp1)
    }else{
      if(nrow(error)>0){
        Error_M <- rbind(Error_M, error)
      }
      if(nrow(model_run)>0){
        Model_run_TS <- rbind(Model_run_TS, model_run)
      }
      if(nrow(prediction)>0){
        Prediction_TS <- rbind(Prediction_TS, prediction)
      }
      if(nrow(validation)>0){
        Validation_TS <- rbind(Validation_TS, validation)
      }
      
      if(nrow(decompose)>0){
        Decompose_TS <- rbind(Decompose_TS, decompose)
      }
    }
    
    
    rm(Final_result, error, count, model_run, prediction, validation, decompose)
    
  },
  
  error = function(e) {print(e)
    temp1 = data.frame(Dimension1 = dimension_1, Dimension2 = dimension_2)
    temp = rbind(temp, temp1)}
  )
  return (list(Error_M, Model_run_TS, Prediction_TS, Validation_TS, Decompose_TS))
}

##################################################################################
# Output
##################################################################################

set.seed(20195)

output <- sapply(1:nrow(data.ts), Output_results)
#output <- sapply(1:3, Output_results)

Error_M <- do.call(rbind, output[1, ])
Model_run_TS <- do.call(rbind, output[2, ])
Prediction_TS <- do.call(rbind, output[3, ])
Validation_TS <- do.call(rbind, output[4, ])
Decompose_TS <- do.call(rbind, output[5, ])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert date

convert_date <- function(df, Posting_period, Fiscal_year){
  if (Time_Dimension=='Day'){
    Date = as.Date(as.numeric(Posting_period), origin = (as.Date(as.character(paste(Fiscal_year, 1, 1)), "%Y%m%d"))-days(1))
    Date = format(strptime(Date, format = "%Y-%m-%d"), "%m/%d/%Y")
  }
  else if (Time_Dimension=='Week'){
    Date = as.Date(paste(Fiscal_year, Posting_period, 1, sep="-"), "%Y-%U-%u")
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
Error_M$Time_Dimension = Time_Dimension

Error_M <- Error_M %>% dplyr:::distinct()

Error_M_r <- as.DataFrame(Error_M)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Model_run_TS$Train_Start_Date= convert_date(Model_run_TS, Model_run_TS$Train_Start_Posting_period, Model_run_TS$Train_Start_Fiscal_year)
Model_run_TS$Train_End_Date= convert_date(Model_run_TS, Model_run_TS$Train_End_Posting_period, Model_run_TS$Train_End_Fiscal_year)
Model_run_TS$Test_Start_Date= convert_date(Model_run_TS, Model_run_TS$Test_Start_Posting_period, Model_run_TS$Test_Start_Fiscal_year)
Model_run_TS$Test_End_Date= convert_date(Model_run_TS, Model_run_TS$Test_End_Posting_period, Model_run_TS$Test_End_Fiscal_year)

Model_run_TS$Scenario <- rep(kpi, nrow(Model_run_TS))
Model_run_TS$Time_Dimension = Time_Dimension

Model_run_TS <- Model_run_TS[, c('Master_Run_ID', 'Run_ID', 'Scenario', 'Technique', 'Method', 'Model_Run_Date', 'Dimension1', 
                                 'Dimension2', 'Threshold', 'Train_Start_Date', 'Train_End_Date', 'Test_Start_Date', 
                                 'Test_End_Date', 'Validation', 'Forecast_Period', 'Lower_Prediction_Interval', 
                                 'Upper_Prediction_Interval', 'Time_Dimension','UoM')] 
Model_run_TS <- Model_run_TS %>% dplyr:::distinct()
Model_run_TS_r <- as.DataFrame(Model_run_TS)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Prediction_TS = Prediction_TS[complete.cases(Prediction_TS), ]

Prediction_TS$Lower_Level_1 = round(Prediction_TS$Lower_Level_1,6)
Prediction_TS$Upper_Level_1 = round(Prediction_TS$Upper_Level_1,6)
Prediction_TS$Lower_Level_2 = round(Prediction_TS$Lower_Level_2,6)
Prediction_TS$Upper_Level_2 = round(Prediction_TS$Upper_Level_2,6)

Prediction_TS$Lower_Level_1[Prediction_TS$Lower_Level_1 < -999999] <- Prediction_TS$Point_Forecast
Prediction_TS$Lower_Level_2[Prediction_TS$Lower_Level_2 < -999999] <- Prediction_TS$Point_Forecast
Prediction_TS$Upper_Level_1[Prediction_TS$Upper_Level_1 > 999999] <- Prediction_TS$Point_Forecast
Prediction_TS$Upper_Level_2[Prediction_TS$Upper_Level_2 > 999999] <- Prediction_TS$Point_Forecast

Prediction_TS$Time_Dimension = Time_Dimension

Prediction_TS <- Prediction_TS %>% dplyr:::distinct()
Prediction_TS_r <- as.DataFrame(Prediction_TS)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Validation_TS$Time_Dimension = Time_Dimension

Validation_TS <- Validation_TS %>% dplyr:::distinct()
Validation_TS_r <- as.DataFrame(Validation_TS)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Decompose_TS <- Decompose_TS %>% dplyr:::distinct()

Decompose_TS$Date <- convert_date(Decompose_TS, Decompose_TS$Posting_period, Decompose_TS$Fiscal_year)
Decompose_TS$Scenario <- rep(kpi, nrow(Decompose_TS))
Decompose_TS$Time_Dimension = Time_Dimension

Decompose_TS <- Decompose_TS[, c('Master_Run_ID','Dimension1', 'Dimension2', 'Scenario', 'Date', 'UoM', 'Actual', 'Trend', 'Seasonality', 'Randomness', 'Time_Dimension')] 
Decompose_TS_r <- as.DataFrame(Decompose_TS)

##################################################################################
# Write Results back to SQL Server
##################################################################################

# New Section - Fix the factor to the chracter format
Model_run_TS_r$Master_Run_ID <- as.character(Model_run_TS_spark$Master_Run_ID)
Model_run_TS_r$Scenario <- as.character(Model_run_TS_spark$Scenario)
Model_run_TS_r$Threshold <- as.character(Model_run_TS_spark$Threshold)

Decompose_TS_r$Master_Run_ID <- as.character(Decompose_TS_spark$Master_Run_ID)
Decompose_TS_r$Scenario <- as.character(Decompose_TS_spark$Scenario)


sqlSave(SQLServer_Forecast, Error_M_r, tablename = "rvfc.work_statisticalperformance", append = TRUE, rownames = FALSE)
sqlSave(SQLServer_Forecast, Model_run_TS_r, tablename = "rvfc.result_modelruntimeseries", append = TRUE, rownames = FALSE)
sqlSave(SQLServer_Forecast, Prediction_TS_r, tablename = "rvfc.work_predictiontimeseries", append = TRUE, rownames = FALSE)
sqlSave(SQLServer_Forecast, Validation_TS_r, tablename = "rvfc.work_validationtimeseries", append = TRUE, rownames = FALSE)
sqlSave(SQLServer_Forecast, Decompose_TS_r, tablename = "rvfc.eda_timeseriesdecomposition", append = TRUE, rownames = FALSE)