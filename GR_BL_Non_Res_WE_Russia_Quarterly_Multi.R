
#Composite Revenue Modeling Project.

#Created on Aug 02 2021 
#@author: Haritima Chauhan
#@email: haritima.chauhan@soothsayeranalytics.com

#This script creates anlaysis for Multi indicator models at regional level

# Importing libraries and setting working directory.----------------------------
# .rs.restartR()
options(device = "RStudioGD")
options(scipen=999,digits=3)
rm(list = ls())
library(dplyr)
library(tidyverse) #mutate
library(lubridate) #make_date
library(tseries) #CCF
library(aTSA) #3 levels adf #Haritima: later add urca for three level checks.
library(xts) #index
library(forecast) #ACF, PACF, ndiffs
library(urca)
library(TSA)
library(ggtext) # for title text
library(lmtest) #bgtest
library(ggplot2)
library(fable)
library(fabletools)
library(caret)

library(reshape2)
library(grDevices)
library(scales)
library(caret)
path = getwd()

#Speecify the region and SEUM details
eum <- "BL"  #BL
bus_segment <- "GR"  #GR
seum <- "Non Residential"
region <- "West Europe & Russian Fed" #this is going to be model segment

#Specify the time and frequency
frequency <- "Quarter"
drop_covid = FALSE # drops the data after Q1 2020
start_year = 2014 # this could be the first year of data too.
ccf_lag = 8 # we only need 8 lags for quarters
acf_lag = 12

#Read the data
df_data <- read.csv(file.path(path, "BL_Non_Residential_WE&Russia_Quarterly.csv"))

#Fix the column names to remove erros
names(df_data) = gsub("_", " ", names(df_data))
names(df_data) = gsub("\\.", " ", names(df_data))

names(df_data) = gsub("\\$", "D", names(df_data))
names(df_data) = gsub("\\&", "and", names(df_data))
names(df_data) = gsub("\\%", "percent", names(df_data))
names(df_data) = gsub("\\,", "", names(df_data))
names(df_data) = gsub("\\(", "", names(df_data))
names(df_data) = gsub("\\)", "", names(df_data))
names(df_data)<-sapply(names(df_data), function(name) paste(unlist(strsplit(name, ".", fixed=TRUE)), collapse = " "))

drop_list = c("Year","Quarter","Sales")
data = list(df_data)

# Extract the names of the columns assign to Sales and Indicator ---------------
# this can be done just by assigning col names to temp_indicators 
# but that will need an update for the whole code hence the for loop. temp_indicators = list()

temp_indicators = list()
for (col in colnames(df_data)){ 
  print(col)
  temp_indicators = c(temp_indicators, col)
}

#drop the columns not needed
temp_indicators = temp_indicators[temp_indicators %in% drop_list == FALSE] 

list_data = list(list(geo = region, sales="Sales", #H: Aug 12 add region instead of US
                      indicator = temp_indicators)
                
)

# Handling time ----------------------------------------------------------------
# set time and frequency variables
if (frequency == "Month") { 
  start_arg <- 1 # this can be soft coded as data$Month[1]
  ts_freq <- 12 # this can be soft coded as max(data$Month)
}

if (frequency == "Quarter") { 
  start_arg <- 1 # this can be soft coded as data$Quarter[1]
  ts_freq <- 4 # this can be soft coded as max(data$Quarter)
}

# setting time series-----------------------------------------------------------

for (lst_dt in data){
  #converts the data in time series
  f <- lapply(lst_dt, function(y) ts(y, start = c(start_year, start_arg), frequency=ts_freq))
  
  if (drop_covid) {
    if (frequency == "Quarter"){
      for (colname in names(f)) {
        f[[colname]] = window(f[[colname]], end=c(2020,1))
      }
    }
    if (frequency == "Month"){
      for (colname in names(f)) {
        f[[colname]] = window(f[[colname]], end=c(2020,3))
      }
    }
  }
}

#Read the selected indicators
selected_indicators = c("SA","Manufacturing Gross operating surplus  profits   Nominal USD",
                        "Housing starts",
                        "Consumption  government  nominal  share of GDP 1")


i=1
y = f[[list_data[[i]]$sales]]
name_y = list_data[[i]]$sales

length(selected_indicators)

x1 = f[[selected_indicators[1]]]
name_x1 =  selected_indicators[[1]]

x2 = f[[selected_indicators[2]]]
name_x2 =  selected_indicators[[2]]

x3 = f[[selected_indicators[3]]]
name_x3 =  selected_indicators[[3]]

#x4 = f[[selected_indicators[5]]]
#name_x4 =  selected_indicators[[5]]

crit_val= 1.96

x1_lag1 = stats::lag(x1,-1)
print(x1_lag1)
x2_lag3 = stats::lag(x2,-3)
print(x2_lag3)
x3_lag2 = stats::lag(x3,-2)
print(x3_lag2)
#x4_lag1 = stats::lag(x4,-1)
#print(x4_lag1)

#View the data to be used for modeling
dt_all = cbind(y, x1_lag1, x2_lag3, x3_lag2)
print(dt_all)
# remove the bottom (max lag) rows with y as na then 
# then remove the top (max lag) rows with x as na 

#select the maximum lag and slice to divide the data(dt_all) into modeling and forecast
max_lag = 3 #max(selected_indicators$Lags)
slice = 5 + max_lag 

#Slice and save the forecasting data
xreg_forecast = tail(dt_all, slice)[,-1]
xreg_forecast

xreg_forecast = head(xreg_forecast,-4) #3 
xreg_forecast

#slice and save the modeling data
#dt_mod = head(tail(dt_norm,-max_lag), -slice)
dt_mod = head(dt_all, -slice)
dt_mod

#normalizing plot[0,1]
library(caret)

normal <- preProcess(as.data.frame(dt_mod[,-1]), method=c("range"))
normal

train_norm <- predict(normal, as.data.frame(dt_mod[,-1]))
train_norm

test_norm <- predict(normal, as.data.frame(xreg_forecast))
test_norm

#Convert the normalized data as time series sequence 
start_time = as.yearqtr(head(time(dt_mod), 1))
start_time

end_time = as.yearqtr(tail(time(dt_mod), 1))
end_time

train_norm = ts(train_norm, start = start_time, end = end_time, frequency = 4)
train_norm

start_time_forecast = as.yearqtr(head(time(xreg_forecast), 1))
start_time_forecast

end_time_forecast = as.yearqtr(tail(time(xreg_forecast), 1))
end_time_forecast

test_norm = ts(test_norm, start = start_time_forecast, end = end_time_forecast, frequency = 4)
test_norm

#joining again y and normalized indicators
train_norm = cbind(y=dt_mod[,1], train_norm)
train_norm

train_norm = tail(train_norm,-max_lag)
colnames(train_norm) = gsub("dt_norm.", "", colnames(dt_mod))
train_norm

# Modeling
#Specify the respective model and ARMA erros 

#Auto Arima model
model = auto.arima(train_norm[,1], xreg = train_norm[, 2:4], ic ='aicc', test = 'kpss' )
summary(model)

#Model with ARMA error (1,0,1)
model = Arima(train_norm[,1], xreg = train_norm[, 2:4], order = c(1,0,1))
summary(model)

#Model with ARMA error (1,0,2)
model = Arima(train_norm[,1], xreg = train_norm[, 2:4], order = c(1,0,2))
summary(model)

#Generate the coefficients of the Arima model
x_t= model$coef/sqrt(diag(model$var.coef))
print(x_t)

#Get the p-value of indicators of Arima model
x_pval = (1 - pt(abs(x_t), model$nobs-1))*2
print(round(x_pval, 3))

#Specify the threshold - confidence level
threshold = c(0.20, 0.05)  # 80% confidence level / lower confidence, 95% confidence level

thresh <- function(x, threshold){
  if ((x <= threshold[1]) | x > threshold[2]) {x_out = "Less Important (lower confidence level)"}
  if (x <= threshold[2]) {x_out = "More Important"}
  if (x > threshold[1]) {x_out = "Not Important"}
  return (x_out)
}

#Generate the p-value
x1_out_pval = thresh(x_pval["x1_lag1"], threshold)
x2_out_pval = thresh(x_pval["x2_lag3"], threshold)
x3_out_pval = thresh(x_pval["x3_lag2"], threshold)

print(paste(name_x1 ,x1_out_pval))
print(paste(name_x2 ,x2_out_pval))
print(paste(name_x3 ,x3_out_pval))

# Check the residuals and print if they are random or not random
res_test = Box.test(model$residuals, lag = acf_lag, type = c("Ljung-Box"))
if (res_test$p.value > 0.05){res_out = "Model residuals are random"
} else {res_out="Model residuals are not random, needs another model"}
print(res_out)

#Forecast
forecast_result <- forecast(model, xreg = test_norm)
forecast_plot <- list()

#title = paste(name, lag)
filename = paste(bus_segment," ", eum," ", seum," ", region, ".png", sep="")
png(file= filename, width=800, height=500)

#Create forecast plot
forecast_plot <- autoplot(forecast_result, 
                          xlab = element_blank(), 
                          ylab = element_blank(), 
                          main = paste(filename, "","\nSales in   Millions (USD)")) +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size=24)) +
  scale_y_continuous(labels = label_number( scale = 1e-6))  # millions) 
print(forecast_plot)

#-------------------------------------------------------------------------------------------------------
