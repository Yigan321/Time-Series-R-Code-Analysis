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
US_IF_APFE <- read.csv("US_RNA_IF_Monthly.csv",stringsAsFactors = FALSE)
EU_IF_APFE <- read.csv("EU_IF_Monthly.csv",stringsAsFactors = FALSE)

head(EU_IF_APFE)

#Create Date Column for each dataset
US_IF_APFE<- US_IF_APFE %>%
  mutate(date = make_date(Year, Month))
head(US_IF_APFE)


EU_IF_APFE<- EU_IF_APFE %>%
  mutate(date = make_date(Year, Month))
head(EU_IF_APFE)

#Remove NA's
US_IF_APFE_subset <- US_IF_APFE[ , c("WUCS","SE.Rovings",'ME.Rovings','CSM','CFM',"DUCS")]
US_IFF_APFE<- US_IF_APFE[complete.cases(US_IF_APFE_subset), ]

EU_IF_APFE_subset <- EU_IF_APFE[ , c("WUCS","composites_industry_value","DUCS")]
EU_IFF_APFE<- EU_IF_APFE[complete.cases(EU_IF_APFE_subset), ]

#Transform US Infrastructure to Time Series
US_IFF_APFE$Region <- NULL
US_IFF_APFE$End.Use.Market <- NULL
US_IFF_APFE$Year <- NULL
US_IFF_APFE$Month <- NULL
US_IFF_APFE$composites_industry_value <- NULL
US_IFF_APFE$date
head(US_IFF_APFE)

#Create function to turn all column objects into timeseries
USIFA_list <- list(US_IFF_APFE)
lst_US_IFA <- lapply(USIFA_list, function(x) {
  x[] <- lapply(x, function(y) ts(y, start = min(as.numeric(row.names(x))),
                                  end = max(as.numeric(row.names(x)))))
  x})
head(lst_US_IFA)

#CCF Plots
Sales_USIFA <- lst_US_IFA[[1]][[1]]


#Infrastruct US APFE
ccf(lst_US_IFA[[1]][[5]],Sales_USIFA,lag.max=24 , main="GR US APFE Sales IF & Oil Rigs") #Oil Rigs
ccf(lst_US_IFA[[1]][[6]],Sales_USIFA,lag.max=24 , main="GR US APFE IF & Gas Rigs") #Gas Rigs
ccf(diff(lst_US_IFA[[1]][[7]]),Sales_USIFA,lag.max=24 , main="GR US APFE IF & Misc Rigs") #Misc Rigs we difference because of Misc Rigs being stationary
ccf(lst_US_IFA[[1]][[8]],Sales_USIFA,lag.max=24 , main="GR US APFE IF & US  Total Rigs") #TotalRigs
ccf(lst_US_IFA[[1]][[10]],Sales_USIFA,lag.max=24 , main="GR US APFE IF & US  DUCS") #DUCS
ccf(lst_US_IFA[[1]][[11]],Sales_USIFA,lag.max=24 , main="GR US APFE IF & SE Rovings") #SE Rovings
ccf(lst_US_IFA[[1]][[12]],Sales_USIFA,lag.max=24 , main="GR US APFE IF & ME Rovings") #ME Rovings
ccf(lst_US_IFA[[1]][[13]],Sales_USIFA,lag.max=24 , main="GR US APFE IF & CSM") #CSM
ccf(lst_US_IFA[[1]][[14]],Sales_USIFA,lag.max=24 , main="GR US APFE IF & CFM") #CFM
ccf(lst_US_IFA[[1]][[15]],Sales_USIFA,lag.max=24 , main="GR US APFE IF & WUCS") #WUCS


#Dickey Fuller US APFE Infrastructure
library(tseries)
tseries::adf.test(lst_US_IFA[[1]][[1]],k=4) #ADF on Sales
tseries::adf.test(lst_US_IFA[[1]][[5]],k=4) #ADF on Oil Rigs
tseries::adf.test(lst_US_IFA[[1]][[6]],k=4) #ADF on Gas Rigs
tseries::adf.test(lst_US_IFA[[1]][[7]],k=4) #ADF on Misc Rigs
tseries::adf.test(lst_US_IFA[[1]][[8]],k=4) #ADF on Total  Rigs
tseries::adf.test(lst_US_IFA[[1]][[9]],k=4) #ADF on Ducs
tseries::adf.test(lst_US_IFA[[1]][[10]],k=4) #ADF on SE Rovings
tseries::adf.test(lst_US_IFA[[1]][[11]],k=4) #ADF on ME Rovings
tseries::adf.test(lst_US_IFA[[1]][[12]],k=4) #ADF on CSM
tseries::adf.test(lst_US_IFA[[1]][[13]],k=4) #ADF on CFM
tseries::adf.test(lst_US_IFA[[1]][[14]],k=4) #ADF on WUCS

#Prehiten if both variables are non stationary
library(TSA)
prewhiten(US_IF_APFE$US.Sales,US_IF_APFE$Oil.Rigs,ylab='CCF', main="GR US Infrastructure US Sales & Oil Rigs ", sub=" ")
prewhiten(US_IF_APFE$US.Sales,US_IF_APFE$Gas.Rigs,ylab='CCF', main="GR US Infrastructure US Sales & Gas Rigs ", sub=" ")
prewhiten(US_IF_APFE$US.Sales,US_IF_APFE$Misc.Rigs,ylab='CCF', main="GR US Infrastructure US Sales & Misc Rigs ", sub=" ")
prewhiten(US_IF_APFE$US.Sales,US_IF_APFE$Total.Rigs,ylab='CCF', main="GR US Infrastructure US Sales & Total Rigs ", sub=" ")
prewhiten(US_IF_APFE$US.Sales,US_IF_APFE$DUCS,ylab='CCF', main="GR US Infrastructure US Sales & DUCS ", sub=" ")
prewhiten(US_IF_APFE$US.Sales,US_IF_APFE$SE.Rovings,ylab='CCF', main="GR US Infrastructure US Sales & SE Rovings ", sub=" ")
prewhiten(US_IF_APFE$US.Sales,US_IF_APFE$ME.Rovings,ylab='CCF', main="GR US Infrastructure US Sales & ME Rovings ", sub=" ")
prewhiten(US_IF_APFE$US.Sales,US_IF_APFE$CSM,ylab='CCF', main="GR US Infrastructure US Sales & CSM ", sub=" ")
prewhiten(US_IF_APFE$US.Sales,US_IF_APFE$CFM,ylab='CCF', main="GR US Infrastructure US Sales & CFM ", sub=" ")
prewhiten(US_IF_APFE$US.Sales,US_IF_APFE$WUCS,ylab='CCF', main="GR US Infrastructure US Sales & WUCS ", sub=" ")

#Infrastructure EU Construction

EU_IFF_APFE$Region <- NULL
EU_IFF_APFE$End.Use.Market <- NULL
EU_IFF_APFE$Year <- NULL
EU_IFF_APFE$Month <- NULL
head(EU_IFF_APFE)

EUIFA_list <- list(EU_IFF_APFE)
lst_EU_IFA <- lapply(EUIFA_list, function(x) {
  x[] <- lapply(x, function(y) ts(y, start = min(as.numeric(row.names(x))),
                                  end = max(as.numeric(row.names(x)))))
  x})
head(lst_EU_IFA)

Sales_EUIFWA <- lst_EU_IF[[1]][[1]]
Sales_EUIFRA <- lst_EU_IF[[1]][[2]]

#Infrastructure WEST EU Sales

ccf(diff(lst_EU_IFA[[1]][[5]]),Sales_EUIFWA,lag.max=24 , main="GR West EU Sales IF & Rig Count")
ccf(lst_EU_IFA[[1]][[6]],Sales_EUIFWA,lag.max=24 , main="GR West EU Sales IF & DUCS")
ccf(lst_EU_IFA[[1]][[7]],Sales_EUIFWA,lag.max=24 , main="GR West EU Sales IF & WUCS")
ccf(lst_EU_IFA[[1]][[8]],Sales_EUIFWA,lag.max=24 , main="GR West EU Sales IF & T30")
ccf(lst_EU_IFA[[1]][[9]],Sales_EUIFWA,lag.max=24 , main="GR West EU Sales IF & Rovings")
ccf(lst_EU_IFA[[1]][[10]],Sales_EUIFWA,lag.max=24 , main="GR West EU Sales IF & Mats")

#Infrastructure Russian Sales

ccf(diff(lst_EU_IFA[[1]][[5]]),Sales_EUIFRA,lag.max=24 , main="GR Russian Fed Sales IF & Rig Count")
ccf(lst_EU_IFA[[1]][[6]],Sales_EUIFRA,lag.max=24 , main="GR Russian Fed Sales IF & DUCS")
ccf(lst_EU_IFA[[1]][[7]],Sales_EUIFRA,lag.max=24 , main="GR Russian Fed Sales IF & WUCS")
ccf(lst_EU_IFA[[1]][[8]],Sales_EUIFRA,lag.max=24 , main="GR Russian Fed Sales IF & T30")
ccf(lst_EU_IFA[[1]][[9]],Sales_EUIFRA,lag.max=24 , main="GR Russian Fed Sales IF & Rovings")
ccf(lst_EU_IFA[[1]][[10]],Sales_EUIFRA,lag.max=24 , main="GR Russian Fed  Sales IF & Mats")

#Dickey Fuller EU both West and Russian Construction Infrastructure
library(tseries)
#West EU
tseries::adf.test(Sales_EUIFWA,k=4) #ADF on Sales
tseries::adf.test(lst_EU_IFA[[1]][[5]],k=4) #ADF on Rig Count
tseries::adf.test(lst_EU_IFA[[1]][[6]],k=4) #ADF on DUCS
tseries::adf.test(lst_EU_IFA[[1]][[7]],k=4) #ADF on WUCS
tseries::adf.test(lst_EU_IFA[[1]][[8]],k=4) #ADF on T30
tseries::adf.test(lst_EU_IFA[[1]][[9]],k=4) #ADF on Rovings
tseries::adf.test(lst_EU_IFA[[1]][[10]],k=4) #ADF on Mats
tseries::adf.test(Sales_EUIFRA,k=4) # ADF on Russian Fed Sales


#Prewhiten West Europe
prewhiten(EU_IFF_APFE$West.Europe.Sales,EU_IFF_APFE$DUCS,ylab='CCF', main="GR US Infrastructure West EU Sales & DUCS ", sub=" ")
prewhiten(EU_IFF_APFE$West.Europe.Sales,EU_IFF_APFE$WUCS,ylab='CCF', main="GR US Infrastructure West EU Sales & WUCS ", sub=" ")
prewhiten(EU_IFF_APFE$West.Europe.Sales,EU_IFF_APFE$Rovings,ylab='CCF', main="GR US Infrastructure West EU Sales & Rovings ", sub=" ")
prewhiten(EU_IFF_APFE$West.Europe.Sales,EU_IFF_APFE$T30,ylab='CCF', main="GR US Infrastructure West EU Sales & T30 ", sub=" ")
prewhiten(EU_IFF_APFE$West.Europe.Sales,EU_IFF_APFE$Mats,ylab='CCF', main="GR US Infrastructure West EU Sales & Mats", sub=" ")

#Prewhite Russian Fed
prewhiten(EU_IFF_APFE$Russian.Fed..Sales,EU_IFF_APFE$DUCS,ylab='CCF', main="GR US Infrastructure Russian Fed Sales & DUCS ", sub=" ")
prewhiten(EU_IFF_APFE$Russian.Fed..Sales,EU_IFF_APFE$WUCS,ylab='CCF', main="GR US Infrastructure Russian Fed Sales & WUCS ", sub=" ")
prewhiten(EU_IFF_APFE$Russian.Fed..Sales,EU_IFF_APFE$Rovings,ylab='CCF', main="GR US Infrastructure Russian Fed Sales & Rovings ", sub=" ")
prewhiten(EU_IFF_APFE$Russian.Fed..Sales,EU_IFF_APFE$T30,ylab='CCF', main="GR US Infrastructure Russian Fed Sales & T30 ", sub=" ")
prewhiten(EU_IFF_APFE$Russian.Fed..Sales,EU_IFF_APFE$Mats,ylab='CCF', main="GR US Infrastructure Russian Fed Sales & Mats ", sub=" ")
