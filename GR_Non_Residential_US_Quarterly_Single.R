library(reshape2)
library(grDevices)
library(scales)
library(caret)

# Enter the order information
order = c(1,0,1)
seasonal = c(1,0,0)

# Read in selected indicator files from the working directory
selected_indicators <- read.xlsx(file.path(path, "selected_indicators.xlsx"), sheet="Sheet1",  colNames = FALSE)
colnames(selected_indicators) = c("Indicators", "Lags", "OCPick", "OCPickLags", "SSA4IndicatorResult", "SSALags")
names(selected_indicators) = gsub("_", " ", names(selected_indicators))
names(selected_indicators) = gsub("\\.", " ", names(selected_indicators))

#creates folder to save plots --------------------------------------------------
store_path_forecast_plots = paste(path,"/",folder_name, "/", "forecast_plots", sep="")
dir.create(store_path_forecast_plots)

y = f[[list_names$sales]]
name_y = list_names$sales

# Creating model data and exporting it for review ------------------------------
dt_all = y
for (i in 1:length(selected_indicators$Indicators))
{
  x = f[[selected_indicators$Indicators[i]]]
  name_x =  selected_indicators$Indicators[i]
  lag =  selected_indicators$Lags[i]
  print(name_x);   print(lag)

  x_lag = stats::lag(x,-lag) 
  dt_all = cbind(dt_all,x_lag)
  colnames(dt_all) = gsub("dt_all.", "", colnames(dt_all))
}

colnames(dt_all) = c("y", list(selected_indicators$Indicators)[[1]])
max_lag= max(selected_indicators$Lags)
dt = tail(head(dt_all,-max_lag), -max_lag)


#normalizing plot[0,1] ---------------------------------------------------------
library(caret)
normal <- preProcess(as.data.frame(dt[,-1]), method=c("range"))
dt_norm <- predict(normal, as.data.frame(dt[,-1]))

#Convert the normalized data as time series sequence
start_time = as.yearqtr(head(time(dt), 1))
end_time = as.yearqtr(tail(time(dt), 1))
dt_norm = ts(dt_norm, start = start_time, end = end_time, frequency = 4)

#joining again
dt_norm = cbind(y=dt[,1], dt_norm)
colnames(dt_norm) = gsub("dt_norm.", "", colnames(dt_norm))


# Get forecasting data---------------------------------------------

for (lst_dt in list(df_data)){
  #converts the data in time series
  f_new <- lapply(lst_dt, function(y) ts(y, start = c(start_year, start_arg), frequency=ts_freq))
  
  }
dt_test = f_new[[list_names$sales]]

for (i in 1:length(selected_indicators$Indicators))
{
  t = f_new[[selected_indicators$Indicators[i]]]
  name_t =  selected_indicators$Indicators[i]
  lag =  selected_indicators$Lags[i]
  print(name_t);   print(lag)
  
  t_lag = stats::lag(t,-lag) #print(x1_lag3)
  dt_test = cbind(dt_test,t_lag)
  colnames(dt_test) = gsub("dt_test.", "", colnames(dt_test))
}
colnames(dt_test) = c("y", list(selected_indicators$Indicators)[[1]])
print(dt_test)
#Max lag is the highest number for lags amongst all indicators
max_lag = 4 
#Slice takes the maximum lag plus the amount of rows needed to slice data to insure correct amount
#of rows are used for forecasting ideally 2020 Q3 to 2021 Q1
slice = 5 + max_lag

#x for forecast
xreg_forecast = tail(dt_test, slice)[,-1]
xreg_forecast = head(xreg_forecast,-5)  
test_norm <- predict(normal, as.data.frame(xreg_forecast))


#Model building ARIMA - c(p,d,q)[P,D,Q]4 ---------------------------------------------
#Builds Arima Model based on order and seasonal values implemented
arima_out <- function(y, xreg, xreg_fore, order, seasonal, name, lag, oc, ssa){

  model_m = Arima(y, xreg = xreg, order = order, seasonal=seasonal) 
  x_t= model_m$coef/sqrt(diag(model_m$var.coef)) 
  x_pval = (1 - pt(abs(x_t), model_m$nobs-1))*2 
  
  res_test = Box.test(model_m$residuals, lag = acf_lag, type = c("Ljung-Box"))
  if (res_test$p.value > 0.05){res_out = "Model residuals are random"
  } else {res_out="Model residuals are not random, needs another model"}
  
  mape_m = sum(abs((model_m$residuals/y)*100))/length(y)

  if (oc=="NA(Lag:NA)"){
    oc=""
    ssa=""
  }
  #Creates a table that incorporates ARIMA model parameters
  new_out = cbind(name, lag,  round(model_m$coef['xreg'],2), round(x_pval['xreg'],2), oc, ssa,
                  paste("(", toString(order), ")", "[", toString(seasonal), "]", "x4", sep=""), model_m$nobs,
                  round(model_m$aicc, 0), round(mape_m, 2), res_out, 
                  round(model_m$aic, 0), round(model_m$bic, 0), 
                  toString(round(head(model_m$coef, -1), 2)), 
                  toString(round(head(x_pval, -1), 2)))
  
  #Forecast future values
  forecast_result <- forecast(model_m, xreg = xreg_fore)
  forecast_plot <- list()
  
  title = paste(name, lag)
  filename = paste(store_path_forecast_plots,"/", title, ".png", sep="")
  png(file= filename, width=800, height=500)
  
  #Plot the forecast for future values
  forecast_plot <- autoplot(forecast_result, 
                            xlab = element_blank(), 
                            ylab = element_blank(), 
                            main = paste(name,lag, "","\nSales in   Millions (USD)")) +
    theme_light() + 
    theme(plot.title = element_text(hjust = 0.5), 
          text = element_text(size=24)) +
    scale_y_continuous(labels = label_number( scale = 1e-6))  # millions) 
  
  print(forecast_plot) 
  write.csv(forecast_result, file.path(store_path,"forecast_result.csv"))
  dev.off()
  
  return(new_out)
}
#Create Model Output that returns excel sheet with each SEUM Indicator modeled
model_output = array()
for (i in 2:ncol(dt_norm)){
  name = selected_indicators$Indicators[i-1]
  lag = selected_indicators$Lags[i-1]
  oc = paste(selected_indicators$OCPick[i-1], "(Lag:", selected_indicators$OCPickLags[i-1], ")", sep="")
  ssa = paste(selected_indicators$SSA4IndicatorResult[i-1], "(Lag:", selected_indicators$SSALags[i-1], ")", sep="")
  print(paste(i-1, name, lag))
  model_f = arima_out(y=dt_norm[,1], xreg = dt_norm[,i], xreg_fore = test_norm[,i-1], order = order, seasonal = seasonal, name, lag, oc, ssa)
  model_output = rbind(model_output, model_f)
}
model_output =  as.data.frame(model_output[-1,])

colnames(model_output) =  c("Indicator", "Lag", "Indicator Coefficient", "Indicator Significance", "OC Selection", "SSA 4 Indicator Results",
                            "Order", "n", "AICc", "Mape",  "Diagnostics", "AIC", "BIC", "Arima Coeff.", "Arima Significance" )

# ----------

# drop some indicators where:
# 1. the model is doing worse than the univariate
# 2. Errors are not iid.
# 3. Significance level is higher than 20%.
# continue to hold on to OC picks.
#Creates a file called One Indicator Output and sends the file into specific folder
title = paste(bus_segment, eum_seum, region, "One_Indicator_Output")
filename = paste("./", folder_name, "/", title, ".csv", sep = "")

write.csv(model_output, filename)
