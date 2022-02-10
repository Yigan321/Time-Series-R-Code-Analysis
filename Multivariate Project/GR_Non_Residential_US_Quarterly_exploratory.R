" 

"

# Importing libraries and setting working directory.----------------------------
options(device = "RStudioGD")
options(scipen = 999, digits = 2) 
rm(list = ls())
library(dplyr)
library(tidyverse) #mutate
library(lubridate) #make_date
library(tseries) #CCF
#library(tsa)
library(aTSA) #3 levels adf #Haritima: later add urca for three level checks.
library(xts) #index
library(forecast) #ACF, PACF, ndiffs
library(urca)
library(TSA)
library(ggtext) # for title text
library(lmtest) #bgtest
library(openxlsx)

path = getwd()

# Enter details for each EUM/SEUM and Region in this section, rest of the code is common
# Enter Dataset details ------------------------------------------------

bus_segment <- "GR"
eum_seum <- "Non Residential" # "BL_NonResidential"
region <- "United States" #this is going to be model segment

#df_data <- read.csv(file.path(path, "data/Dataset GR Quarterly/IF_WE_Quarterly.csv"))
df_data <- read.csv(file.path(path, "BL_Non_Residential_US_Quarterly.csv"))

frequency <- "Quarter"
drop_covid = TRUE # drops the data after Q1 2020
start_year = 2014 # this could be the first year of data too.
ccf_lag = 8 # we only need 8 lags for quarters equal to 2 years
acf_lag = 12 

truncate_beg_year = 2015
truncate_beg_quarter = 1 
truncated =  FALSE   #Since US Non Residential needs both Truncate and Not Truncate run each code with True and False
out_corr_threshold = 0.0
out_lag_threshold = 8

# Fixed the column names - please add more if you come across--------------------
#Need to fix this otherwise file name errors come. 
names(df_data) = gsub("_", " ", names(df_data))
names(df_data) = gsub("\\.", " ", names(df_data))
names(df_data)[names(df_data) == "Sales"] <- "Sales"


drop_indicator_list = c("Region","Country","Year","Quarter", "Month", "Sales", "SEUM") # we have to Sales here to remove from CCF
          
# ------------------------------------------------------------------------------
#-----------------Common Code and functions from here.--------------------------
# ------------------------------------------------------------------------------

# Extract the names of the columns assign to Sales and Indicator ---------------

final_indicators = list()
for (col in colnames(df_data)){ 
  print(col)
  final_indicators = c(final_indicators, col)
}
#drop the columns not needed
final_indicators = final_indicators[final_indicators %in% drop_indicator_list == FALSE] 

#creates list of names to be used later for looping over names
list_names = list(geo = region, sales="Sales", indicator = final_indicators)

# Handling time ----------------------------------------------------------------
# setting time and frequency variables
if (frequency == "Month") { 
  start_arg <- 1 # this can be soft coded as data$Month[1]
  ts_freq <- 12 # this can be soft coded as max(data$Month)
}

if (frequency == "Quarter") { 
  start_arg <- 1 # this can be soft coded as data$Quarter[1]
  ts_freq <- 4 # this can be soft coded as max(data$Quarter)
}

# setting time series-----------------------------------------------------------

for (lst_dt in list(df_data)){
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
  
  if(truncated==TRUE){
    for (colname in names(f)) {
      f[[colname]] = window(f[[colname]], start=c(truncate_beg_year ,truncate_beg_quarter))
    }
  }
}

# creates a folder to save files-------------------------------------------------

folder_name = paste(bus_segment,"_",eum_seum,"_",region, "_", frequency, sep="") 
store_path = paste(path,"/",folder_name, sep="")

dir.create(store_path)

# creates folder for indicator information--------------------------------------
if (truncated == FALSE){
  store_path_plots = paste(path,"/",folder_name, "/", "Plots", sep="")
  dir.create(store_path_plots) }

if (truncated == TRUE){
  store_path_plots = paste(path,"/",folder_name, "/", "Plots_Truncated_Data", sep="")
  dir.create(store_path_plots) }

# Sales Line plots ------------------------------------------------------------------

title = paste(bus_segment, eum_seum, region, frequency, "\n", list_names$sales, "(in Millions USD)")
if (truncated == FALSE){filename = paste(folder_name,"/", list_names$sales, "_line.png", sep = "")}
if (truncated == TRUE){filename = paste(folder_name,"/", list_names$sales, "_line_Truncated.png", sep = "") }

autoplot(f[[list_names$sales]]/1000000) + #plot in millions
  ggtitle(title) + xlab("Year") + ylab("") +
  theme_light() + theme(plot.title = element_textbox_simple())
ggsave(file=filename,width = 6, height=4)

# Sales ACF PACF ---------------------------------------------------------------------

title = paste(bus_segment, eum_seum, region, frequency, "\n", list_names$sales)
if (truncated == FALSE){filename = paste(folder_name,"/", list_names$sales, "_acf.png", sep = "")}
if (truncated == TRUE){filename = paste(folder_name,"/", list_names$sales, "_acf_Truncated.png", sep = "") }

jpeg(filename)
par(fig=c(0, 1, 0.5, 1), oma=c(0,1,0,1))
Acf(as.vector(f[[list_names$sales]]), lag.max = acf_lag, plot = TRUE, main= title, cex=1)
par(new=TRUE, fig=c(0, 1, 0, 0.5) )
Pacf(as.vector(f[[list_names$sales]]), lag.max = acf_lag, plot = TRUE, main="")
dev.off()

# Indicator Line plots ------------------------------------------------------------------

#Indicators
for (j in 1:length(list_names$indicator)) {

  indicator = list_names$indicator[[j]]
  print(indicator)
 

  ptitle1 = paste(bus_segment, eum_seum, region, frequency)
  ptitle2 = indicator
  filename = paste(store_path_plots, "/", j, "_", indicator, "_line.png", sep = "")

  autoplot(f[[list_names$indicator[[j]]]]) + 
    ggtitle(label = ptitle1, subtitle = ptitle2) +
    xlab("Year") + ylab("Unit") + 
    theme_light() +
    theme(plot.title = element_textbox_simple()) 
  ggsave(file=filename,width=6, height=4)

}
dev.off()

# Indicator  ACF PACF ---------------------------------------------------------------------

#Indicators
for (j in 1:length(list_names$indicator)) {

  indicator = list_names$indicator[[j]]
  print(indicator)
  #print(f[[list_names$indicator[[j]]]])

  ptitle1 = paste(bus_segment, eum_seum, region, frequency, "\n", indicator)
  filename = paste(store_path_plots, "/", j, "_", indicator, "_acf.png", sep = "")

  jpeg(filename)
  par(fig=c(0, 1, 0.5, 1), oma=c(0,0,3,0), mar=c(4,2,2,2))
  Acf(as.vector(f[[list_names$indicator[[j]]]]), lag.max = acf_lag, plot = TRUE, main="")
  par(new=TRUE, fig=c(0, 1, 0, 0.5) )
  Pacf(as.vector(f[[list_names$indicator[[j]]]]), lag.max = acf_lag , plot = TRUE, main="")
  mtext(ptitle1, side=3, line=0, adj=0.5, cex=1, outer=TRUE)
  dev.off()
}

# Stationarity -----------------------------------------------------------------

stationarity_month <- function(z, name, counter, y, y_state=FALSE) ##use this code as is.
{
  #the code checks for unit roots and stationarity, and also cointegration
  
  #Unit root and stationarity
  sig_level = 0.05 #twotailed 10%
  acf_cval_col = 3 #5% 1=1%, 3=10%
  kpss_cval_col = 2 #10%, 2=5%, 4=1%
  kpss_lags = 12 #use.lag = kpss_lags 
  
  kpss_level_stats = ur.kpss(z, type = "mu", lags="long") #level (just drift)
  if (abs(kpss_level_stats@cval[1,kpss_cval_col]) >= abs(kpss_level_stats@teststat)) {
    state_kpss_level = "KPSS Level stat."
  }else { state_kpss_level = "KPSS Level nonstat."}
  
  adf_drift = urca::ur.df(z, type="drift", selectlags = "AIC") #drift
  if (abs(adf_drift@cval[1,acf_cval_col]) >= abs(adf_drift@teststat[1])) { #tau2 testing H0
    if (abs(adf_drift@cval[2,acf_cval_col]) >= abs(adf_drift@teststat[2])) { #phi1 testing H0
      adf_none = urca::ur.df(z, type="none", selectlags = "AIC") #none
      if (abs(adf_none@cval[1,acf_cval_col]) >= abs(adf_none@teststat[1])) { #tau1 testing H0
        state_adf = "Unit Root (no trend, no drift)" #tau1 H0
      }else {state_adf = "No Unit Root (no trend, no drift)"} #tau1 Ha
    }else {#phi1 Ha
      if (abs(adf_drift@testreg$coefficients[2,4]) > sig_level) { #tau2 gamma testing H0
        state_adf = "Unit Root (with drift)" #tau 2 gamma H0
      }else {state_adf = "No Unit Root (with drift)"}} # tau 2 gamma Ha
  }else {state_adf = "No Unit Root (drift eq.)"} #tau2 Ha
  
  print(state_adf)
  #compare adf with drift 
  if (state_adf %in% c("No Unit Root (drift eq.)", "No Unit Root (with drift)", "No Unit Root (no trend, no drift)")) {
    final_state1 = "Stationary"
  }else {
    if (state_adf %in% c("Unit Root (with drift)", "Unit Root (no trend, no drift)")
        & state_kpss_level == "KPSS Level stat.") {
      final_state1 = "Stationary (less obs.)" }
    if (state_adf %in% c("Unit Root (with drift)", "Unit Root (no trend, no drift)")
        & state_kpss_level == "KPSS Level nonstat.") {
      final_state1 = "Non Stationary"}}
  
  #assigning final states
  if (final_state1 %in% c("Stationary (less obs.)", "Stationary"))  {
    final_state = "Stationary"}
  if (final_state1 %in% c("Non Stationary"))  {
    final_state = "Non Stationary" } 
  
  # Differencing
  seas_diff <- nsdiffs(z)[1]
  level_diff <- ndiffs(z, test="kpss")
  
  #print(y_state)
  if (counter !=0){
   
    if (bgtest(lm(y~z))$p.value > 0.05) {
      state_cointegrate = "Yes (consider Sales ADF state))"
    } else {state_cointegrate ="No"}  #library lmtest
  } else { state_cointegrate = "Not Needed" }
  
  out = cbind(counter, name, final_state, final_state1, state_adf, 
              state_kpss_level, level_diff, seas_diff, state_cointegrate)
  return (out)
}

stationarity_quarter <- function(z, name, counter, y, y_state=FALSE) ##use this code as is.
{
  #the code checks for unit roots and stationarity, and also cointegration
  
  #variation for checking dampening ACF
  z_acf = Acf(as.vector(z), lag.max = acf_lag)
  ct = abs(1.96/sqrt(z_acf$n.used)) #conservative
  
  if (all(abs(ct) >= abs(z_acf$acf[-1]))) {
    cv = "none significant"
    state_acf="Ignore stationary" 
  }else {
    diff_z = diff(z_acf$acf,1)
    cv <- sd(diff_z) / mean(diff_z) * 100
    
    state_acf = "Needs more checks"
  }
  
  #Unit root and stationarity
  
  sig_level = 0.05 #twotailed 10%
  cval_col = 2 #2=5% 1=1%, 3=10%
  if (abs(ct) >= abs(z_acf$acf[3]) | abs(ct) >= abs(z_acf$acf[4]) | abs(ct) >= abs(z_acf$acf[3]))
  {adf_lags = 4} else {adf_lags = 1} # if not using AIC
  
  kpss_cval_col = 2 #10%, 2=5%, 4=1%
  kpss_lags = 4 #for bartlett window 

  
  kpss_trend_stats = ur.kpss(z, type = "tau", use.lag = kpss_lags) #trend
  if (abs(kpss_trend_stats@cval[1,kpss_cval_col]) >= abs(kpss_trend_stats@teststat)) {
    state_kpss_trend = "KPSS Trend stat." 
  }else {state_kpss_trend = "KPSS Trend nonstat."}
  
  kpss_level_stats = ur.kpss(z, type = "mu", use.lag = kpss_lags) #level (just drift)
  if (abs(kpss_level_stats@cval[1,kpss_cval_col]) >= abs(kpss_level_stats@teststat)) {
    state_kpss_level = "KPSS Level stat."
  }else { state_kpss_level = "KPSS Level nonstat."}
  
  adf_all = urca::ur.df(z, type="trend", selectlags = "AIC") #trend
  if (abs(adf_all@cval[1,cval_col]) >= abs(adf_all@teststat[1])) { #tau3 testing H0
    if (abs(adf_all@cval[3,cval_col]) >= abs(adf_all@teststat[3])) { #phi3 testing H0
      adf_drift = urca::ur.df(z, type="drift", selectlags = "AIC") #drift
      if (abs(adf_drift@cval[1,cval_col]) >= abs(adf_drift@teststat[1])) { #tau2 testing H0
        if (abs(adf_drift@cval[2,cval_col]) >= abs(adf_drift@teststat[2])) { #phi1 testing H0
          adf_none = urca::ur.df(z, type="none", selectlags = "AIC") #none
          if (abs(adf_none@cval[1,cval_col]) >= abs(adf_none@teststat[1])) { #tau1 testing H0
            state_adf = "Unit Root (no trend, no drift)" #tau1 H0
          }else {state_adf = "No Unit Root (no trend, no drift)"} #tau1 Ha
        }else {#phi1 Ha
          if (abs(adf_drift@testreg$coefficients[2,4]) > sig_level) { #tau2 gamma testing H0
            state_adf = "Unit Root (with drift)" #tau 2 gamma H0
          }else {state_adf = "No Unit Root (with drift)"}} # tau 2 gamma Ha
      }else {state_adf = "No Unit Root (drift eq.)"} #tau2 Ha
    }else {
      if (abs(adf_all@testreg$coefficients[2,4]) > sig_level) { #phi3 gamma testing H0
        state_adf = "Unit Root (with trend)" #phi3 gamma H0
      }else {state_adf = "No Unit Root (with trend)"}} #phi3 gamma Ha
  }else {state_adf = "No Unit Root (trend eq.)"} # tau3 Ha
  
  #compare adf with trend
  
  if (state_adf %in% c("No Unit Root (trend eq.)", "No Unit Root (with trend)")) {
    final_state1 = "Stationary"
  }else {
    if (state_adf %in% c("Unit Root (with trend)")
        & state_kpss_trend == "KPSS Trend stat.") {
      final_state1 = "Stationary (inconclusive, less obs.)" }
    if (state_adf %in% c("Unit Root (with trend)")
        & state_kpss_trend == "KPSS Trend nonstat.") {
      final_state1 = "Non Stationary"}}
  
  #compare adf with drift 
  if (state_adf %in% c("No Unit Root (drift eq.)", "No Unit Root (with drift)", "No Unit Root (no trend, no drift)")) {
    final_state1 = "Stationary"
  }else {
    if (state_adf %in% c("Unit Root (with drift)", "Unit Root (no trend, no drift)") 
        & state_kpss_level == "KPSS Level stat.") {
      final_state1 = "Stationary (inconclusive, less obs.)" }
    if (state_adf %in% c("Unit Root (with drift)", "Unit Root (no trend, no drift)") 
        & state_kpss_level == "KPSS Level nonstat.") {
      final_state1 = "Non Stationary"}}
  
  #assigning final
  if (final_state1 %in% c("Stationary (inconclusive, less obs.)", "Stationary") | 
      state_acf %in% c("Stationary",  "Ignore stationary"))  {
    final_state = "Stationary"
  } else {final_state = "Non Stationary"}
  
  
  # Differencing
  seas_diff <- nsdiffs(z)[1]

  level_diff <- ndiffs(z, test="kpss")
  
  #print(y_state)
  if (counter !=0){
   
    if (bgtest(lm(y~z))$p.value > 0.05) {
      state_cointegrate = "Yes (consider Sales ADF state))"
    } else {state_cointegrate ="No"}  #library lmtest
  } else { state_cointegrate = "Not Needed" }
  
  out = cbind(counter, name, final_state, cv, state_acf, final_state1, state_adf, 
              state_kpss_trend, state_kpss_level, level_diff, seas_diff, state_cointegrate)
  return (out)
}

if (frequency == "Month") { 
  
  #testing for stationary of all the series in the list
  output = array()
  #Sales
  out =  stationarity_month(f[[list_names$sales]], list_names$sales, i-1, f[[list_names$sales]])
  sales_state_adf = out[7]
  output = rbind(output, out)
  
  #Indicators
  for (j in 1:length(list_names$indicator)) {
    
    indicator =  list_names$indicator[[j]]
    print(indicator)
    out = stationarity_month(f[[list_names$indicator[[j]]]], indicator, j, f[[list_names$sales]], sales_state_adf)
    output = rbind(output, out)
  }
  
  output =  as.data.frame(output[-1, ])
  colnames(output) <- c('Id', 'name','final_state','CV',
                        'state_ACF', 'state_from tests', 
                        'state_ADF', 'state_KPSS_Trend', 'state_KPSS_Level',
                        'level_diff', 'seas_diff', 'state_cointegrate')
}

if (frequency == "Quarter") { 
  #testing for stationary of all the series in the list
  output = array()
  #Sales
  out =  stationarity_quarter(f[[list_names$sales]], list_names$sales, 0, f[[list_names$sales]])
  sales_state_adf = out[7]
  output = rbind(output, out)
  
  #Indicators
  for (j in 1:length(list_names$indicator)) {
    
    indicator =  list_names$indicator[[j]]
    print(indicator)
    out = stationarity_quarter(f[[list_names$indicator[[j]]]], indicator, j, f[[list_names$sales]], sales_state_adf)
    output = rbind(output, out)
  }

  output =  as.data.frame(output[-1, ])
  colnames(output) <- c('Id', 'name','final_state','CV',
                                  'state_ACF', 'state_from tests', 
                                  'state_ADF', 'state_KPSS_Trend', 'state_KPSS_Level',
                                  'level_diff', 'seas_diff', 'state_cointegrate')
  
  
}

# CCF---------------------------------------------------------------------------
#used in some testing.
if (frequency == "Quarter") {ccf_diff=4} 
if (frequency == "Month") {ccf_diff=12}

ccf_output <- function(z, n, counter, sales_name, indicator_name){
  
  ct = abs(1.96/sqrt(n))
  colnames(z) <- c('lag','ccf')
  
  lags = subset(z, lag<0)
  sig_lag = lags[abs(lags$ccf) > ct,]$lag
  if (length(sig_lag) != 0) {
    imp_lag = "Yes"
    condition_lags = ((abs(lags$ccf) >= ct) 
                 & (abs(lags$ccf)>= out_corr_threshold) 
                 & (abs(lags$lag)<= out_lag_threshold))
    above_threshold_lag = lags[condition_lags,]$lag
    sig_cor_lag = round(lags[condition_lags,]$ccf, 2)
    if (length(above_threshold_lag) != 0){sig_lag = above_threshold_lag}
    if (length(above_threshold_lag) == 0){
      sig_lag = paste("None above ", out_corr_threshold, " or are significant above ", out_lag_threshold, " lags", sep="")
      sig_cor_lag = paste("None above ", out_corr_threshold, " or are significant above ", out_lag_threshold, " lags", sep="")}
    
  } else {
    imp_lag = "No"
    sig_lag = "None"
    sig_cor_lag = "None"}
  
  leads = subset(z, lag>0)
  sig_leads = leads[abs(leads$ccf) > ct,]$lag
  if (length(sig_leads) != 0) {
    imp_lead = "Yes"
    condition_leads = ((abs(leads$ccf) >= ct) 
                     & (abs(leads$ccf)>= out_corr_threshold) 
                     & (abs(leads$lag)<= out_lag_threshold))
    above_threshold_lead = leads[condition_leads,]$lag
    sig_cor_lead = round(leads[condition_leads,]$ccf, 2)
    if (length(above_threshold_lead) == 0){
      sig_lead = paste("None above ", out_corr_threshold, " or are significant above ", out_lag_threshold, " lags", sep="")
      sig_cor_lead = paste("None above ", out_corr_threshold, " or are significant above ", out_lag_threshold, " lags", sep="")}
    if (length(above_threshold_lead) != 0) {sig_lead = above_threshold_lead}
  } else {
    imp_lead = "No"
    sig_lead = "None"
    sig_cor_lead = "None"}
  
  if ((z[z$lag == 0, ]$ccf >= ct)& (z[z$lag == 0, ]$ccf >= out_corr_threshold)) 
    { sig_lag0 = "Important"} else{sig_lag0 ="Not important"}
  
  region_sales = paste(region, sales_name, sep = " ") #so that the table has Region
  out = cbind(counter, region_sales, indicator_name, 
              imp_lag, paste("'", paste(sig_lag, collapse=" "), sep="" ), 
              paste("'", paste(sig_cor_lag, collapse=" "), sep="" ),
              imp_lead, paste("'", paste(sig_lead, collapse=" "), sep="" ), 
              paste("'", paste(sig_cor_lead, collapse=" "), sep="" ),
              sig_lag0, paste(frequency, "/", drop_covid, sep = " "))
  return (out)
  
}

ccf_plots <- function(){
  #all variables used are global except indicator_tbl, sales_name
  
  indicator_tbl = array()
  sales_name = list_names$sales
    
  for (j in 1:length(list_names$indicator)) {
    title = paste(bus_segment, eum_seum, region, list_names$sales, "and", "\n", list_names$indicator[[j]], frequency)
    
  
    filename_part = paste(list_names$indicator[[j]], "_CCF", sep = "")
    filename = paste(store_path_plots, "/", j, "_", filename_part, ".png", sep = "")
    
    indicator_name = list_names$indicator[[j]]
    print(indicator_name)
    
    jpeg(filename)
   
    
    y = f[[sales_name]]
    x = f[[indicator_name]]
    
    if (output$final_state[output$name == sales_name] == "Non Stationary" & 
        output$final_state[output$name == indicator_name] == "Non Stationary"){

    # print("1")
      if (output$state_cointegrate[output$name == indicator_name] == "Yes (consider Sales ADF state))") {
        sub_title = cbind("Non transformed data showing overall patterns \n Investigate Cointegration")
        
        par(oma=c(2,0,0,0))
        save_ccf = ccf(as.vector(y),as.vector(x), ylab="CCF",lag.max=ccf_lag, xaxp=c(-ccf_lag,ccf_lag,ccf_lag/2),
                       main=title)#, sub=sub_title[1])
        mtext(sub_title[1], side=1, line=1, adj=0.5, cex=1, outer=TRUE)
        out = ccf_output(data.frame(col1 = save_ccf[["lag"]], col2 = save_ccf[["acf"]]),
                         length(y), j, sales_name, indicator_name)
        indicator_tbl = rbind(indicator_tbl, out)
        
        
      }else{
        sub_title = cbind("Top figure: Non transformed data","Bottom figure: Both series first differenced")
        y = f[[sales_name]]
        x = f[[indicator_name]]
        par(fig=c(0, 1, 0.5, 1), oma=c(2,0,0,0))
        ccf(as.vector(y),as.vector(x), ylab="CCF",lag.max=ccf_lag, xaxp=c(-ccf_lag,ccf_lag,ccf_lag/2),
                       main=title, sub=sub_title[1])
        par(new=TRUE, fig=c(0, 1, 0, 0.5) )
        save_ccf = prewhiten(as.vector(y), as.vector(x), ylab='CCF', main="", sub=sub_title[2])
        # create table
        out = ccf_output(data.frame(col1 = save_ccf$ccf$lag, col2 = save_ccf$ccf$acf), 
                         length(y), j, sales_name, indicator_name)
        indicator_tbl = rbind(indicator_tbl, out)}
      
    }
    
    if (output$final_state[output$name == sales_name] == "Non Stationary" & 
        output$final_state[output$name == indicator_name] == "Stationary"){

      if (output$state_cointegrate[output$name == indicator_name] == "Yes (consider Sales ADF state))") {
        sub_title = cbind("Non transformed data showing overall patterns \n 
                        (Cointegration and/or Sales modeling transformation)")
      }else{
        sub_title = cbind("Non transformed data showing overall patterns \n 
                        (Sales modeling transformation)")}
 
      par(oma=c(2,0,0,0))
      save_ccf = ccf(as.vector(y),as.vector(x), ylab="CCF",lag.max=ccf_lag, xaxp=c(-ccf_lag,ccf_lag,ccf_lag/2),
                     main=title)#, sub=sub_title[1])
      mtext(sub_title[1], side=1, line=1, adj=0.5, cex=1, outer=TRUE)
      
      # create table
      out = ccf_output(data.frame(col1 = save_ccf[["lag"]], col2 = save_ccf[["acf"]]),
                       length(y), j, sales_name, indicator_name)
      indicator_tbl = rbind(indicator_tbl, out)
      
    }
    
    if (output$final_state[output$name == sales_name] == "Stationary" & 
        output$final_state[output$name == indicator_name] == "Non Stationary"){
      # print("3")
      if (output$state_cointegrate[output$name == indicator_name] == "Yes (consider Sales ADF state))") {
      sub_title = cbind("Non transformed data showing overall patterns \n (Cointegration and/or Indicator modeling transformation)")
      }else{
        sub_title = cbind("Non transformed data showing overall patterns \n (Indicator modeling transformation)")
        }
      
      par(oma=c(2,0,0,0))
      save_ccf = ccf(as.vector(y),as.vector(x), ylab="CCF",lag.max=ccf_lag, xaxp=c(-ccf_lag,ccf_lag,ccf_lag/2),
                     main=title)#, sub=sub_title[1])
      mtext(sub_title[1], side=1, line=1, adj=0.5, cex=1, outer=TRUE)
      
      # create table
      out = ccf_output(data.frame(col1 = save_ccf[["lag"]], col2 = save_ccf[["acf"]]), 
                       length(y), j, sales_name, indicator_name)
      indicator_tbl = rbind(indicator_tbl, out)
    }
    
    if (output$final_state[output$name == sales_name] == "Stationary" & 
        output$final_state[output$name == indicator_name] == "Stationary"){
      
      sub_title = cbind("No transformation, both series stationary")
      par(oma=c(2,2,2,2))
      save_ccf = ccf(as.vector(y),as.vector(x), ylab="CCF",lag.max=ccf_lag, xaxp=c(-ccf_lag,ccf_lag,ccf_lag/2),
                     main=title)#, sub=sub_title[1])
      mtext(sub_title[1], side=1, line=1, adj=0.5, cex=1, outer=TRUE)
      
      # create table
      out = ccf_output(data.frame(col1 = save_ccf[["lag"]], col2 = save_ccf[["acf"]]), 
                       length(y), j, sales_name, indicator_name)
      indicator_tbl = rbind(indicator_tbl, out)
      
    }
  dev.off()}
  return (indicator_tbl)}

if (truncated == FALSE){
  internal_file = "Internal_SSA_Analysis"
  corr_file =  "Indicator_Correlations_Table"}
if (truncated == TRUE){
  internal_file = "Internal_SSA_Analysis_Truncated" 
  corr_file =  "Indicator_Correlations_Table_Truncated"}


#generate CCF plots
indicator_output = as.data.frame(ccf_plots())
indicator_output[1,] = "" #add one row
colnames(indicator_output) <- c('Id', 'Sales Series','Indicator', 
                                'Lags Important', 'List Lags', 'Correlation' ,
                                'Leads Important', "List Leads", 'Correlation', 
                                "Current Correlation", "Frequency / Drop post Q1 2020")
title = paste(bus_segment, eum_seum, region, internal_file)
filename = paste("./", folder_name, "/", title, ".csv", sep = "")
write.table(cbind(output, indicator_output), file = filename,  sep="," ,col.names = T, row.names = F)

#output for OC
indicator_output =  indicator_output[-1,]
title = paste(bus_segment, eum_seum, region,corr_file)
filename = paste("./", folder_name, "/", title, ".csv", sep = "")
write.table(indicator_output, file = filename,  sep="," ,col.names = T, row.names = F)

# Correlations ------------------------------------------------------------------

if (truncated == FALSE){corr_file = "correlation_indicator.csv"}
if (truncated == TRUE){corr_file = "correlation_indicator_Truncated.csv"}

cor_data = f[[list_names$indicator[[1]]]]
for (i in list_names$indicator[-1]){cor_data = ts.union(cor_data, f[[i]], dframe=TRUE)}
colnames(cor_data) = list_names$indicator
ind_cor = round(cor(cor_data),2)
write.csv(ind_cor,paste(store_path, "/", corr_file, sep=""))

# Listing the top correlated for easy review------------------------------------
library(qpcR)

if (truncated == FALSE){corr_file = "correlation_specific.csv"}
if (truncated == TRUE){corr_file = "correlation_specific_Truncated.csv"}

corr_thres = 0.9
name = row.names(ind_cor)[1]
col = as.data.frame(ind_cor[,1])
colnames(col) = name
col <- cbind(indicator= rownames(col), col)
row.names(col) <- NULL
high_cor = col[abs(col[name])>corr_thres,]
high_cor = high_cor[order(high_cor[name]),]
ind_cor_specific =  high_cor

for (i in 2:nrow(ind_cor)){
  name = row.names(ind_cor)[i]
  col = as.data.frame(ind_cor[,i])
  colnames(col) = name
  col <- cbind(indicator= rownames(col), col)
  row.names(col) <- NULL
  high_cor = col[abs(col[name])>corr_thres,]
  high_cor = high_cor[order(high_cor[name]),]
  ind_cor_specific =  qpcR:::cbind.na(ind_cor_specific, high_cor)
  #print(names(row))
}

ind_cor_specific[is.na(ind_cor_specific)] <- ""
write.csv(ind_cor_specific, paste(store_path, "/", corr_file, sep=""))

# Cleanup -----------------------------------------------------------------------
rm(ind_cor_specific, high_cor, col, cor_data, ind_cor)
rm(lst_dt, indicator_output, out)
rm(colname, corr_file, corr_thres, i, j, indicator, name, ptitle1, ptitle2, sales_state_adf)


