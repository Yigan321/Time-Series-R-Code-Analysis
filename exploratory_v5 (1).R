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

# Updates in v2: Added month and quarter data checks, fixed the handling and setting time series section 
# and stationarity function for unit root error. 

# Enter the following details---------------------------------------------------
#setwd("C:/Users/haritima/Desktop/21_OC/code_R")
path <- getwd()
eum <- "BL"
bus_segment <- "GR West Europe"
seum <- "Residential"

# Month dataset check ----------------------------------------------------------
# we should be having datasets for each EUM. Maybe Further for each region/modeling segment.
# For example: 1. AM_Infra, 2. EU_Infra. 
# Each indicator should have the name of main region. For example: US_Construction NSA
AM_IF <- read.csv(file.path(path,"/Downloads/USA_Quarter.csv"),stringsAsFactors = FALSE)
EU_IF <- read.csv(file.path(path, "/yiga/EU_IF_Monthly.csv"),stringsAsFactors = FALSE)
data = list(AM_IF)#, EU_IF)

frequency <- "Month" # change to Quarter 
drop_covid = TRUE # drops the data after March 2020
start_year = 2014 # this could be the first year of data too.

list_AM = list(list(geo = "US", sales="US.Sales",
                    indicator = list("Infrastructure..NSA.",
                                     "Oil.Rigs", "Gas.Rigs", "Misc.Rigs", "Total.Rigs",
                                     "DUCS", "SE.Rovings", "ME.Rovings","CSM", "CFM", "WUCS"))
               # ,list(geo = "RNA", sales="Rest.North.America.Sales",
               #      indicator2 = list("Infrastructure..NSA.",
               #                    "Oil.Rigs", "Gas.Rigs", "Misc.Rigs", "Total.Rigs",
               #                    "DUCS", "SE.Rovings", "ME.Rovings","CSM", "CFM", "WUCS"))
)

# list_EU = list(list(geo = "WestEU", sales="US.Sales",
#                     indicator = list("Infrastructure..NSA.",
#                                      "Oil.Rigs", "Gas.Rigs", "Misc.Rigs", "Total.Rigs",
#                                      "DUCS", "SE.Rovings", "ME.Rovings","CSM", "CFM", "WUCS"))
#                ,list(geo = "RusFed", sales="Rest.North.America.Sales",
#                     indicator2 = list(""))
# )



# # Quarter dataset Check----------------------------------------------------------
AM_list <- read.csv(file.path(path, "/Downloads/WE_BL_Residential_Quarter.csv"),stringsAsFactors = FALSE);
data = list(AM_list)
frequency <- "Quarter"
drop_covid = TRUE # drops the data after Q1 2020
start_year = 2014 # this could be the first year of data too.
colnames(AM_list)
#temp <- paste(dQuote(colnames(AM_list)), collapse = ", ")
list_AM = list(list(geo = "EU", sales="West.Europe.Sales",
                     indicator = list("SA","NSA","composites_industry_value","DUCS","WUCS","T30","Rovings",
                                      "Mats","WE.Construction.Gross.output..sales...Real.USD","WE.Construction.Production.index","WE.Glass.Gross.operating.surplus..profits...Nominal.USD",
                                      "WE.Glass.Gross.output..sales...Nominal.USD","WE.Glass.Investment..Nominal.USD","WE.Glass.Production.index","WE.Glass.Value.added.output..As.a.percent.of.GDP",
                                      "WE.Glass.Value.added.output..As.a.percent.of.manufacturing","WE.Glass.Value.added.output..As.a.percent.of.world.total","WE.Industrial.Production.Gross.operating.surplus..profits...Nominal.USD",
                                      "WE.Industrial.Production.Gross.output..sales...Nominal.USD","WE.Glass.Investment..Nominal.USD","WE.Glass.Production.index","WE.Glass.Value.added.output..As.a.percent.of.GDP","WE.Glass.Value.added.output..As.a.percent.of.manufacturing",
                                      "WE.Glass.Value.added.output..As.a.percent.of.world.total","WE.Industrial.Production.Gross.operating.surplus..profits...Nominal.USD","WE.Industrial.Production.Gross.output..sales...Nominal.USD","WE.Industrial.Production.Production.index",
                                      "WE.Industrial.Production.Value.added.output..As.a.percent.of.GDP","WE.Industrial.Production.Value.added.output..As.a.percent.of.world.total","WE.Manufacturing.Gross.operating.surplus..profits...Nominal.USD","WE.Manufacturing.Gross.output..sales...Nominal.USD",
                                      "WE.Manufacturing.Investment..Nominal.USD","WE.Manufacturing.Production.index","WE.Manufacturing.Value.added.output..As.a.percent.of.GDP","WE.Manufacturing.Value.added.output..As.a.percent.of.world.total","WE.Current.account.of.balance.of.payments.in.US...share.of.GDP",
                                      "WE.Employment..total.1","WE.External.debt..total..US.","WE.Foreign.direct.investment..US.","WE.GDP.per.capita..nominal..US.","WE.GDP..nominal..US.","WE.Government.balance..share.of.GDP","WE.Population..total","WE.Reserves..foreign.exchange..US.",
                                      "WE.Reserves..months.of.import.cover","WE.Stockbuilding..real..share.of.GDP","WE.Visible.trade.balance..share.of.GDP","WE.Consumer.price.index","WE.Gross.government.debt..as.a...of.GDP.","WE.Industrial.production.index","WE.Interest.rate..short.term",
                                      "WE.Interest.rate..Yield.on.10.year.Government.Debt.Securities....per.annum.","WE.Services.balance..as...of.GDP","WE.Share.price.index","WE.Unemployment.rate","WE.Capacity.utilisation","WE.Consumption..government..PPP.exchange.rate..nominal..US.","WE.Consumption..government..nominal..US.",
                                      "WE.Consumption..government..nominal..share.of.GDP.1","WE.Consumption..private..PPP.exchange.rate..nominal..US.","WE.Exports..goods...services..constant.prices.and.exchange.rate..US.....of.World","WE.GDP..industry..real","WE.GVA.Agriculture.share.of.GVA","WE.GVA.Industry.share.of.GVA",
                                      "WE.GVA.Manufacturing.of.GVA","WE.GVA.Services..share.of.GVA","WE.Gross.value.added.in.construction..real","WE.Gross.value.added.in.services..real","WE.Imports..goods...services..constant.prices.and.exchange.rate..US.....of.World","WE.Imports..goods..PPP.exchange.rate..nominal..US.",
                                      "WE.Industrial.production.index.1","WE.Investment..government..nominal","WE.Investment..machinery...equipment..nominal","WE.Investment..private..non.residential.structures..nominal","WE.Investment..total.fixed.investment..nominal..US.",
                                      "WE.Investment..total.fixed..nominal..share.of.GDP","WE.Net.investment..nominal..US.","WE.Output.gap","WE.Productivity..trend","WE.Stockbuilding..nominal..US.",
                                      "WE.Stockbuilding..nominal..share.of.GDP","WE.Stockbuilding..real..annual.contribution.to.growth","WE.Trend.productivity.target","WE.World.trade.index","WE.House.price.index","WE.Housing.starts","WE.Interest.rate.on.building.society.mortgages","WE.Market.value.of.housing.stock..LCU",
                                      "WE.Residential.property.transactions","WE.Stock.of.owner.occupied.houses","WE.Consumers..expenditure..durables..nominal","WE.Financial.liabilities..household.sector..as.a...of.disposable.income","WE.Liabilities..debt.other.than.loans..households","WE.Personal.consumer.credit",
                                      "WE.Retail.sales..value.index","WE.Retail.sales..volume.index","WE.Savings..personal.sector.ratio")))
                # ,list(geo = "RNA", sales="Rest.North.America.Sales",
                #      indicator2 = list("Infrastructure..NSA.",
               #                    "Oil.Rigs", "Gas.Rigs", "Misc.Rigs", "Total.Rigs",
                #                    "DUCS", "SE.Rovings", "ME.Rovings","CSM", "CFM", "WUCS"))
 

# creates a folder to save files-------------------------------------------------
# For now I am just keeping it one for each business segment and eum. Feel free to modify. But stay consistent.
eum <- "BL"
bus_segment <- "West Europe"
seum <- "Residential"

#Replace periods with spaces
#install.packages('rrapply')
#library(rrapply)
#list_AM <- rrapply(list_AM1, f = function(x) gsub(".", " ", x, fixed = TRUE))
#list_AM

folder_name = paste(bus_segment,"_",eum, sep="")
store_path = paste(path,"/",folder_name, sep="")
dir.create(store_path)

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

#Remove columns that have 0
SelectVar[, colSums(SelectVar != 0) > 0]

# Line plots ------------------------------------------------------------------
#TODO_Later TEAM add them for indicators.

# Stationarity ----------------------------------------------------------------
stationarity <- function(x, name) ##use this code as is.
{
  # 1. Seasonal 
  seas_diff <- nsdiffs(x)[1]
  if (seas_diff == 0) {final_seasonal_state = "Stationary"}
  if (seas_diff == 1) {final_seasonal_state = "First Seasonal Difference"}
  if (seas_diff >1) {final_seasonal_state = "Higher Seasonal Difference - check"}
  
  # 2. KPSS and ADF
  
  #if (seas_diff == 1) {x = diff(x, ts_freq)}
  
  adf_none = urca::ur.df(x, type="none") 
  unit_root = FALSE
  if (abs(adf_none@cval[1,2]) > abs(adf_none@teststat[1])) { #none
    unit_root = TRUE }
  
  adf_all = urca::ur.df(x, type="trend")
  if (abs(adf_all@cval[1,2]) > abs(adf_all@teststat[1])) { #all
    if (abs(adf_all@cval[3,2]) > abs(adf_all@teststat[3])) { #trend
      adf_drift = urca::ur.df(x, type="drift")
      if (abs(adf_drift@cval[1,2]) > abs(adf_all@teststat[1])) {
        if (abs(adf_drift@cval[2,2]) > abs(adf_all@teststat[2])) { #drift
          if (unit_root){
            state_adf = "unit root"
          } else {state_adf = "No unit root"; final_state="Stationary"}
        } else if (unit_root){state_adf = "unit root"} else{state_adf = "No unit root"; final_state="Stationary"}
      } else {state_adf = "No unit root"; final_state="Stationary"}
    } else if (unit_root){state_adf = "unit root no trend"} else{state_adf = "No unit root"; final_state="Stationary"}
  } else {state_adf = "No unit root"; final_state="Stationary"}
  
  state_kpss = "Not needed"
  if (state_adf == "unit root"){
    kpss_level_stats = ur.kpss(x, type = "mu", use.lag=8) #trend
    if (abs(kpss_level_stats@cval[1,2]) < abs(kpss_level_stats@teststat)) {
      state_kpss = "KPSS Level stat." #Ho
      final_state = "Stationary"
    } else state_kpss = "KPSS Level nonstat."; final_state = "Non Stationary"}
  
  if (state_adf == "unit root no trend"){
    kpss_trend_stats = ur.kpss(x, type = "tau", use.lag=8) #trend
    if (abs(kpss_trend_stats@cval[1,2]) < abs(kpss_trend_stats@teststat)) {
      state_kpss = "KPSS trend stat." #Ho
      final_state = "Stationary"
    } else state_kpss = "KPSS Trend nonstat."; final_state = "Non Stationary"}
  
  out = cbind(name, final_state, state_adf, state_kpss, final_seasonal_state, seas_diff) 
  return (out)
}

#testing for stationary of all the series in the list
output = array()
for (i in 1:length(list_AM)) {
  #Sales
  out =  stationarity(f[[list_AM[[i]]$sales]], list_AM[[i]]$sales)
  output = rbind(output, out)
  
  #Indicators
  for (j in 1:length(list_AM[[i]]$indicator)) {
    indicator =  list_AM[[i]]$indicator[[j]]
    print(indicator)
    out = stationarity(f[[list_AM[[i]]$indicator[[j]]]], indicator)
    output = rbind(output, out)
  }
}
output =  as.data.frame(output[-1, ])
title = paste(bus_segment, eum, seum, "Output")
filename = paste("./", folder_name, "/", title, ".csv", sep = "")
write.table(output, file = filename,  sep="," ,col.names = T, row.names = F)


# ACF PACF ---------------------------------------------------------------------
for (i in 1:length(list_AM)) {
  title = paste(bus_segment, eum, seum, list_AM[[i]]$sales)
  filename = paste("./", folder_name, "/", "ACF_PACF_", title, ".png", sep = "")
  jpeg(filename)
  par(fig=c(0, 1, 0.5, 1), oma=c(0,0,0,0))
  Acf(as.vector(f[[list_AM[[i]]$sales]]), lag.max = 40, plot = TRUE, main= title, cex=1.5)
  par(new=TRUE, fig=c(0, 1, 0, 0.5) )
  Pacf(as.vector(f[[list_AM[[i]]$sales]]), lag.max = 40, plot = TRUE, main="")
  dev.off()
  
}

# Cointegration ----------------------------------------------------------------
#TODO Haritima

# CCF---------------------------------------------------------------------------

ccf_output <- function(z, n){
 
  ct = abs(1.96/sqrt(n))
  
  colnames(z) <- c('lag','ccf')
  
  lags = subset(z, lag<0)
  sig_lag = lags[abs(lags$ccf) > ct,]$lag
  if (length(sig_lag) != 0) {imp_lag = "Yes"} else {imp_lag = "No"; sig_lag = "None"}
  
  leads = subset(z, lag>0)
  sig_leads = leads[abs(leads$ccf) > ct,]$lag
  if (length(sig_leads) != 0) {imp_leads = "Yes"} else {imp_leads = "No"; sig_leads = "None"}
  
  if (z[z$lag == 0, ]$ccf > ct) { sig_lag0 = "Important"} else{sig_lag0 ="Not important"}
  
  out = cbind(sales_name, indicator_name, imp_lag, paste("'", paste(sig_lag, collapse=" "), sep="" ), 
              imp_leads, paste(sig_leads, collapse=" "), 
              sig_lag0)
  return (out)
  
}

indicator_tbl = array()
for (i in 1:length(list_AM)) {
  for (j in 1:length(list_AM[[i]]$indicator)) {
    title = paste(bus_segment, eum, seum, list_AM[[i]]$sales, "&", list_AM[[i]]$indicator[[j]])
    filename = paste("./", folder_name, "/", "CCF_", title, ".png", sep = "")
        
    
    #checking for stationarity combinations
    sales_name = list_AM[[i]]$sales
    indicator_name = list_AM[[i]]$indicator[[j]]
    
    print(indicator_name)
    
    jpeg(filename)
    #par(oma=c(0,0,1,1))
    if (output$final_state[output$name == sales_name] == "Non Stationary" & 
        output$final_state[output$name == indicator_name] == "Non Stationary"){
      
      sub_title = cbind("Non transformed data","Both series first differenced")
      y = f[[sales_name]]
      x = f[[indicator_name]]
      par(fig=c(0, 1, 0.5, 1), oma=c(0,0,0,0))
      ccf(as.vector(y),as.vector(x), ylab="CCF",lag.max=24, xaxp=c(-24,24,12),
          main=title, sub=sub_title[1])
      par(new=TRUE, fig=c(0, 1, 0, 0.5) )
      save_ccf = prewhiten(as.vector(y), as.vector(x),ylab='CCF', main=title, sub=sub_title[2])  
      
      # create table
      out = ccf_output(data.frame(col1 = save_ccf$ccf$lag, col2 = save_ccf$ccf$acf), length(y))
      indicator_tbl = rbind(indicator_tbl, out)
      
      }
    
    if (output$final_state[output$name == sales_name] == "Stationary" & 
        output$final_state[output$name == indicator_name] == "Non Stationary"){
  
      sub_title = cbind("Non transformed data","Indicator first differenced (non stationary)")
      y = f[[sales_name]]
      x = f[[indicator_name]]
      par(fig=c(0, 1, 0.5, 1), oma=c(0,0,0,0))
      ccf(as.vector(y),as.vector(x), ylab="CCF",lag.max=24, xaxp=c(-24,24,12),
          main=title, sub=sub_title[1])
      par(new=TRUE, fig=c(0, 1, 0, 0.5) )
      save_ccf = ccf(as.vector(y),as.vector(diff(x,1)), ylab="CCF",lag.max=24, xaxp=c(-24,24,12),
          main=title, sub=sub_title[2])
      
      # create table
      out = ccf_output(data.frame(col1 = save_ccf[["lag"]], col2 = save_ccf[["acf"]]), length(y))
      indicator_tbl = rbind(indicator_tbl, out)
    }
    
    if (output$final_state[output$name == sales_name] == "Non Stationary" & 
        output$final_state[output$name == indicator_name] == "Stationary"){
      
      sub_title = cbind("Non transformed data","Sales first differenced (non stationary)")
      y = f[[sales_name]]
      x = f[[indicator_name]]
      par(fig=c(0, 1, 0.5, 1), oma=c(0,0,0,0))
      ccf(as.vector(y),as.vector(x), ylab="CCF",lag.max=24, xaxp=c(-24,24,12),
          main=title, sub=sub_title[1])
      par(new=TRUE, fig=c(0, 1, 0, 0.5) )
      save_ccf = ccf(as.vector(diff(y,1)),as.vector(x), ylab="CCF",lag.max=24, xaxp=c(-24,24,12),
          main=title, sub=sub_title[2])
      
      # create table
      out = ccf_output(data.frame(col1 = save_ccf[["lag"]], col2 = save_ccf[["acf"]]), length(y))
      indicator_tbl = rbind(indicator_tbl, out)
      
    }
    
    if (output$final_state[output$name == sales_name] == "Stationary" & 
        output$final_state[output$name == indicator_name] == "Stationary"){
      
      sub_title = cbind("No transformation, both series stationary")
      y = f[[sales_name]]
      x = f[[indicator_name]]
      save_ccf = ccf(as.vector(y),as.vector(x), ylab="CCF",lag.max=24, xaxp=c(-24,24,12),
          main=title, sub=sub_title[1])
      
      # create table
      out = ccf_output(data.frame(col1 =save_ccf[["lag"]], col2 = save_ccf[["acf"]]), length(y))
      indicator_tbl = rbind(indicator_tbl, out)

    }
    dev.off()}}



indicator_output =  as.data.frame(indicator_tbl[-1,])
colnames(indicator_output) <- c('Sales Series','Indicator', 'Lags Important', 
                                'Significant', 'Leads Important', "Significant", 
                                "Current Lag Important")
title = paste(bus_segment, eum, seum, "Indicator_Table")
filename = paste("./", folder_name, "/", title, ".csv", sep = "")
write.table(indicator_output, file = filename,  sep="," ,col.names = T, row.names = F)

y = f[[sales_name]]
x = f[[indicator_name]]
p = prewhiten(as.vector(y), as.vector(x),ylab='CCF', main=title, sub=sub_title[2]) 

z = data.frame(col1 = p$ccf$lag, col2 = p$ccf$acf)
colnames(z) <- c('lag','ccf')

update.packages(ask = FALSE, repos = 'http://cran.rstudio.org')
install.packages('knitr', repos = c('http://rforge.net', 'http://cran.rstudio.org'),
                 type = 'source')

install.packages("xfun")
library(knitr)

