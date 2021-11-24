#############################################################################
# Advanced Forecasting System                       Date:  06/May/2021
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Machine Learning - Prepare the Input Data for the Main Dataset
#############################################################################

# Remove all objects from Global Environment.
rm(list=ls())

# Suppress warning messages.
options(warn=-1)

# Set a working directory
setwd("D:/R Scripts/")

#Run Global intiliazation script
source("DS_2_Global_Initilization_Script.r")

#Set up connection to SQL Server
SQLServer_Forecast <- odbcDriverConnect('driver=SQL Server;server=ssarfdb.eastus.cloudapp.azure.com,1433;database=Forecast;uid=soothsayersql@ssasqlserver;pwd=SoothDez@413')





#Acknowledges time of command runs
start_time <- Sys.time()

#Read in Datainput_forecast table

pushdown_query = "rvfc.datainput_forecast"
data_prediction <- sqlFetch(jdbcUrl,table = pushdown_query)
data_prediction <- as.data.frame(data_prediction)

data_prediction_rev = data_prediction[data_prediction$Scenario == 'Revenue',]
data_prediction_oi = data_prediction[data_prediction$Scenario == 'Order_Intake',]

data_prediction_rev <- data_prediction_rev %>% dplyr:::distinct()
data_prediction_oi <- data_prediction_oi %>% dplyr:::distinct()

data_prediction_oi = data_prediction_oi[1:380, ]
data_prediction_rev$Date = data_prediction_oi$Date

data.prediction = merge(data_prediction_rev, data_prediction_oi, all=TRUE, by=c("Dimension1", "Dimension2", "UoM", "Date"))

data.prediction = data.prediction[,c("Dimension1", "Dimension2", "UoM", "Date", "Value.x", "Value.y")]
names(data.prediction) = c("Dimension1", "Dimension2", "UoM", "Date", "Sale", "Orderintake")

data.prediction = data.prediction[complete.cases(data.prediction),]

start_date <- control_timescenario[1,1]
end_date <- control_timescenario[1,2]

data.prediction <- as.data.frame(data.prediction %>%
                                   dplyr:::filter(as.Date(Date, "%m/%d/%Y") >= as.Date(start_date, "%m/%d/%Y") & as.Date(Date, "%m/%d/%Y") <= as.Date(end_date, "%m/%d/%Y")))

#data.prediction$Date = dmy(data.prediction$Date)

head(data.prediction)

#Time Dimension Lookup
TimeDimension_lookup <- function() {
  time_dimension <- names(control_timescenario)[apply(control_timescenario, 1, function(i) which(i == 'X'))]
  return(time_dimension)                                               
}
Time_Dimension = TimeDimension_lookup()     
print(Time_Dimension) 

#Adding Period function
Add_period <- function(df) {
  if (Time_Dimension=='Day'){
    temp_Date = as.Date(df$Date, "%m/%d/%Y")
    df$Posting_period <- as.numeric(strftime(temp_Date, format = "%j"))
    df$Fiscal_year <- as.numeric(strftime(temp_Date, format = "%Y")) 
  }
  else if (Time_Dimension=='Week'){
    temp_Date = as.Date(df$Date, "%m/%d/%Y")
    df$Posting_period <- as.numeric(strftime(temp_Date, format = "%V"))
    df$Fiscal_year <- as.numeric(strftime(temp_Date, format = "%Y")) 
  }
  else if (Time_Dimension=='Month'){
    temp_Date = as.Date(df$Date, "%m/%d/%Y")
    df$Posting_period <- as.numeric(strftime(temp_Date, format = "%m"))
    df$Fiscal_year <- as.numeric(strftime(temp_Date, format = "%Y")) 
  }
  else if (Time_Dimension=='Quarter'){
    temp_Date = as.Date(df$Date, "%m/%d/%Y")
    df$Posting_period = as.numeric(substr(as.character(as.yearqtr(as.Date(temp_Date, "%Y-%m-%d" ))), 7,7))
    df$Fiscal_year <- as.numeric(strftime(temp_Date, format = "%Y")) 
  }
  else{
    temp_Date = as.Date(df$Date, "%m/%d/%Y")
    df$Posting_period <- 1
    df$Fiscal_year <- as.numeric(strftime(temp_Date, format = "%Y")) 
  }
  return (df)
}

data.prediction = Add_period(data.prediction)

data.prediction<- as.data.frame(data.prediction %>%
                                  dplyr:::group_by(Dimension1, Dimension2, UoM, Posting_period, Fiscal_year)%>% 
                                  dplyr:::summarize('Sale' = sum(Sale), 'Orderintake' = sum(Orderintake)))

# Look up THRESHOLD from imported SQL table
Threshold_func <- function(diff){
  table_timethreshold <- table_timethreshold[table_timethreshold$Time_Dimension == Time_Dimension,  ]
  for (row in 1:nrow(table_timethreshold)){
    if ((diff >= table_timethreshold[row, 'Lower']) & (diff <= table_timethreshold[row, 'Upper'])){
      threshold <- table_timethreshold[row, 'Threshold']
      break
    } else {
      threshold <- "Drop"
    }
  }
  return (threshold)
}


# Look up INPUT VARIABLES for prediction from imported SQL table
MLcontrol_func <- function(threshold){
  table_timeseriescontrol = table_timeseriescontrol[table_timeseriescontrol$Time_Dimension == Time_Dimension, ]
  data_control <- as.data.frame(table_timeseriescontrol[(table_timeseriescontrol$Threshold == threshold) & 
                                                          (table_timeseriescontrol$Technique == 'Machine_Learning'), ])
  ###
  method <- data_control[data_control$Activation == 'Y', 'Method']
  ###
  validation_step <- data_control[1, 'Validation']
  forecast_step <- data_control[1, 'Forecast_Period']
  lower_ci <- data_control[1, 'Lower_Prediction_Interval']
  upper_ci <- data_control[1, 'Upper_Prediction_Interval']
  return(list(validation_step, forecast_step, lower_ci, upper_ci, method))
}

#combining data together to get a larger size of data
data.prediction = rbind(data.prediction,data.prediction)

thresh_temp <- as.data.frame(data.prediction %>%
                               dplyr:::add_count(Dimension1, Dimension2) %>%
                               dplyr:::filter(n>1))
thresh_temp$thresholds <- unlist(lapply(thresh_temp$n, Threshold_func))

head(thresh_temp,5)

#Train Validation Split
train_val_split <- function(sub_data){
  threshold <- as.character(sub_data %>% dplyr:::distinct(thresholds))
  validation_step <- MLcontrol_func(threshold)[[1]]
  data_set <- c(rep('TrainSet', nrow(sub_data) - validation_step), rep('ValSet', validation_step))
  sub_data$Dataset <- data_set
  return (sub_data)
}

df_train_val <- as.data.frame(thresh_temp %>%
                                dplyr:::group_by(Dimension1, Dimension2) %>%
                                dplyr:::do(train_val_split(.[-1])))

data.prediction <- df_train_val[, c('Dimension1', 'Dimension2', 'UoM', 'Posting_period', 'Fiscal_year', 'Dataset', 'Sale', 'Orderintake')]

forecast_step <- table_timeseriescontrol$Forecast_Period[1]

########
## read in order_intake forecast
########
pushdown_query = "rvfc.result_scenarios"
result_scenarios <- sqlFetch(SQLServer_Forecast,table = pushdown_query)
result_scenarios <- as.data.frame(result_scenarios)

head(result_scenarios)

############# Read in Business intuition
result_scenarios$Composite_Forecast <- ifelse(is.na(result_scenarios$Business_Intuition), result_scenarios$Base_Forecast, result_scenarios$Business_Intuition)
result_scenarios = Add_period(result_scenarios)
head(result_scenarios)

##############append result_scenarios to data.prediciton

result_scenarios$Dataset <- rep('TestSet', nrow(result_scenarios))
result_scenarios$Sale <- rep(0, nrow(result_scenarios))
orderintake_prediction <- result_scenarios[result_scenarios$Scenario == 'Order_Intake', c('Dimension1', 'Dimension2', 'UoM', 'Posting_period', 'Fiscal_year', 'Dataset', 'Sale', 'Composite_Forecast')]
names(orderintake_prediction)[names(orderintake_prediction) == 'Composite_Forecast'] <- 'Orderintake' 

# names(data.prediction)[names(data.prediction) == 'Fiscal_year'] <- 'Fiscal_Year'

# data.prediction$Posting_period <- as.numeric(data.prediction$Posting_period)

data.prediction <- rbind(data.prediction, orderintake_prediction)

# Adding lags and MA3
datainput_ml <- as.data.frame(data.prediction %>%
                                dplyr:::group_by(Dimension1, Dimension2) %>%
                                dplyr:::mutate(Orderintake_lag1 = dplyr:::lag(Orderintake, n=1, default= NA),
                                               Orderintake_lag2 = dplyr:::lag(Orderintake, n=2, default= NA),
                                               Orderintake_lag3 = dplyr:::lag(Orderintake, n=3, default= NA),
                                               Orderintake_lag4 = dplyr:::lag(Orderintake, n=4, default= NA),
                                               Orderintake_lag5 = dplyr:::lag(Orderintake, n=5, default= NA),
                                               Orderintake_lag6 = dplyr:::lag(Orderintake, n=6, default= NA),
                                               Orderintake_lag7 = dplyr:::lag(Orderintake, n=7, default= NA),
                                               Orderintake_lag8 = dplyr:::lag(Orderintake, n=8, default= NA),
                                               Orderintake_lag9 = dplyr:::lag(Orderintake, n=9, default= NA),
                                               Orderintake_lag10 = dplyr:::lag(Orderintake, n=10, default= NA),
                                               Orderintake_lag11 = dplyr:::lag(Orderintake, n=11, default= NA),
                                               Orderintake_lag12 = dplyr:::lag(Orderintake, n=12, default= NA),
                                               Orderintake_MA3 = SMA(Orderintake, 3)               
                                ))

# Function for number of NA's and datainput lags
na_number <- function(col){ 
  return (sum(is.na(col)))
}

datainput_lags <- function(sub_data){
  trainset_lag <- sub_data[sub_data$Dataset == 'TrainSet', ]
  total_num <- nrow(trainset_lag)
  na_num <- apply(trainset_lag, 2, na_number)
  return (sub_data[, na_num <= total_num])
}


###Fill in NAs
fill_na <- function(df){
  NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
  df[] <- lapply(df, NA2mean)
  return (df)
}

datainput_ml_agg <- as.data.frame(datainput_ml %>%
dplyr:::group_by(Dimension1, Dimension2) %>%
dplyr:::do(datainput_lags(.[-1])) %>%
dplyr:::do(fill_na(.[-1])))

#Convert Date Function

convert_date <- function(df, Posting_period, Fiscal_year){
  if (Time_Dimension=='Day'){
    Date = as.Date(as.numeric(Posting_period), origin = (as.Date(as.character(paste(Fiscal_year, 1, 1)), "%Y%m%d"))-days(1))
    Date = format(strptime(Date, format = "%Y-%m-%d"), "%m/%d/%Y")
  }
  else if (Time_Dimension=='Week'){
    Date = as.Date(paste(Fiscal_year, Posting_period, 1, sep="-"), "%Y-%U-%u")
    for (n in 1:length(Date)){
      if (is.na(Date[n])){
        Date[n] = as.Date((paste0(as.numeric(Fiscal_year[n]), '-12-31')))     
      }
    }
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

datainput_ml_agg$Date = convert_date(datainput_ml_agg, datainput_ml_agg$Posting_period, datainput_ml_agg$Fiscal_year)
head(datainput_ml_agg)

df_factor <- datainput_ml_agg[,  !names(datainput_ml_agg) %in% c('UoM', 'Orderintake', 'Posting_period', 'Fiscal_year')]
df_factor_spark <- as.DataFrame(df_factor)

#Write output back to control table
sqlSave(df_factor_spark, SQLServer_Forecast, table = "rvfc.work_machine_learning", mode = "append")

#Ending Script time
end_time <- Sys.time()
Time_taken_ML_1 = end_time - start_time
Time_taken_ML_1

