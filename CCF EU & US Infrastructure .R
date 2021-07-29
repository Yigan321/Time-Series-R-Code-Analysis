#Load Packages
library(ggplot2)
library(ggthemes)
library(forecast)
library(tseries)
library(fUnitRoots)
library(lmtest)
library(zoo)
library(car)
library(astsa)
library(dplyr)
library(tidyverse)
library(lubridate)
library(TSA)
library(corrplot)
library(cowplot)

#Load datasets
US_IF <- read.csv("US_RNA_IF_Monthly.csv",stringsAsFactors = FALSE)
EU_IF <- read.csv("EU_IF_Monthly.csv",stringsAsFactors = FALSE)

head(US_IF)

#Create Date Column for each dataset
US_IF<- US_IF %>%
  mutate(date = make_date(Year, Month))
head(US_IF)


EU_IF<- EU_IF %>%
  mutate(date = make_date(Year, Month))
head(EU_IF)

#Remove NA's
US_IF_subset <- US_IF[ , c("WUCS","composites_industry_value","DUCS")]
US_IFF<- US_IF[complete.cases(US_IF_subset), ]

EU_IF_subset <- EU_IF[ , c("WUCS","composites_industry_value","DUCS")]
EU_IFF<- EU_IF[complete.cases(EU_IF_subset), ]

#Transform US Infrastructure to Time Series
US_IF$Region <- NULL
US_IF$End.Use.Market <- NULL
US_IF$Year <- NULL
US_IF$Month <- NULL
head(US_IF)

#Create function to turn all column objects into timeseries
USIF_list <- list(US_IFF)
lst_US_IF <- lapply(USIF_list, function(x) {
  x[] <- lapply(x, function(y) ts(y, start = min(as.numeric(row.names(x))),
                                  end = max(as.numeric(row.names(x)))))
  x})
head(lst_US_IF)

#CCF Plots
Sales_USIF <- lst_US_IF[[1]][[1]]


#Infrastruct US Construction
ccf(lst_US_IF[[1]][[4]],Sales_USIF,lag.max=24 , main="GR US Construction IF & US Construction Sales")

#Dickey Fuller US Construction Infrastructure
library(tseries)
tseries::adf.test(lst_US_IF[[1]][[1]],k=4) #ADF on Sales
tseries::adf.test(lst_US_IF[[1]][[4]],k=4) #ADF on NSA

#Since both NSA and Sales are non stationary we will prewhiten
library(TSA)
prewhiten(US_IFF$US.Sales,US_IFF$Infrastructure..NSA.,ylab='CCF', main="GR US Infrastructure US Sales & US Construction NSA ", sub=" ")

#Infrastructure EU Construction

EU_IFF$Region <- NULL
EU_IFF$End.Use.Market <- NULL
EU_IFF$Year <- NULL
EU_IFF$Month <- NULL
head(EU_IFF)

EUIF_list <- list(EU_IFF)
lst_EU_IF <- lapply(EUIF_list, function(x) {
  x[] <- lapply(x, function(y) ts(y, start = min(as.numeric(row.names(x))),
                                  end = max(as.numeric(row.names(x)))))
  x})
head(lst_EU_IF)

Sales_EUIFW <- lst_EU_IF[[1]][[1]]
Sales_EUIFR <- lst_EU_IF[[1]][[2]]

#Infrastructure WEST EU Sales

ccf(lst_EU_IF[[1]][[4]],Sales_EUIFW,lag.max=24 , main="GR EU Construction IF & West EU Construction Sales")

#Infrastructure Russian Sales

ccf(lst_EU_IF[[1]][[4]],Sales_EUIFR,lag.max=24 , main="GR EU Construction IF & Russian Fed Construction Sales")

#Dickey Fuller EU both West and Russian Construction Infrastructure
library(tseries)
tseries::adf.test(Sales_EUIFW,k=4) #ADF on Sales
tseries::adf.test(lst_EU_IF[[1]][[4]],k=4) #ADF on NSA
tseries::adf.test(Sales_EUIFR,k=4)

#Prewhiten
prewhiten(EU_IFF$West.Europe.Sales,EU_IFF$NSA,ylab='CCF', main="GR US Infrastructure West EU Sales & EU Construction NSA ", sub=" ")
prewhiten(EU_IFF$Russian.Fed..Sales,EU_IFF$NSA,ylab='CCF', main="GR US Infrastructure Russian Fed Sales & Construction NSA ", sub=" ")
