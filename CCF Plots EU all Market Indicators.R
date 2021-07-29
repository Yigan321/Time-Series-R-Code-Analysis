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

#Read Dataset
resi <- read.csv("EU_BL_Residential_Monthly.csv",stringsAsFactors = FALSE)
nonresi <- read.csv("EU_BL_Non_Residential_Monthly (1).csv",stringsAsFactors = FALSE)
head(nonresi)


#Create Date Column for each dataset
resi<- resi %>%
  mutate(date = make_date(Year, Month))
head(resi)


nonresi<- nonresi %>%
  mutate(date = make_date(Year, Month))
head(nonresi)

#Subset data for missing values

#Residential
resi_subset <- resi[ , c("WUCS","composites_industry_value","DUCS")]
resii <- resi[complete.cases(resi_subset), ]
resi1 <- resi[complete.cases(resi_subset), ]

#Non Residential
nonresi_subset <- nonresi[ , c("DUCS","composites_industry_value","Russian.Fed..Sales", "West.Europe.Sales")]
nonresii <- nonresi[complete.cases(nonresi_subset), ]

#Make a copy of the Nonresi dataframe
library(data.table)
resi1<- copy(resi)
head(resi1)

nonresi1<- copy(nonresi)
head(nonresi1)


#Transform every column into a timeseries ( RESIDENTIAL)

resii$Year <- NULL
resii$Month <- NULL
resii$SEUM <- NULL

resii_list <- list(resii)
lst2 <- lapply(resii_list, function(x) {
  x[] <- lapply(x, function(y) ts(y, start = min(as.numeric(row.names(x))),
                                  end = max(as.numeric(row.names(x)))))
  x})
head(lst2)
str(lst2)



Sales <- lst2[[1]][[2]] #Russian Fed
Sales1 <- lst2[[1]][[1]] #West EU
head(lst2)
#Russian Fed Sales
ccf(lst2[[1]][[6]],Sales,lag.max=24 , main="GR Residential Russian Fed Sales & DUCS")
ccf(lst2[[1]][[7]],Sales,lag.max=24 , main="GR Residential Russian Fed Sales & WUCS")
ccf(lst2[[1]][[9]],Sales,lag.max=24 , main="GR Residential Russian Fed Sales & Rovings")
ccf(lst2[[1]][[8]],Sales,lag.max=24 , main="GR Residential Russian Fed Sales & T30")
ccf(lst2[[1]][[4]],Sales,lag.max=24 , main="GR Residential Russian Fed Sales & NSA")
ccf(lst2[[1]][[10]],Sales,lag.max=24 , main="GR Residential Russian Fed Sales & Mats")

#West Europe Sales
ccf(lst2[[1]][[6]],Sales1,lag.max=24 , main="GR Residential West EU Sales & DUCS")
ccf(lst2[[1]][[7]],Sales1,lag.max=24 , main="GR Residential West EU Sales & WUCS")
ccf(lst2[[1]][[9]],Sales1,lag.max=24 , main="GR Residential West EU Sales & Rovings")
ccf(lst2[[1]][[8]],Sales1,lag.max=24 , main="GR Residential West EU Sales & T30")
ccf(lst2[[1]][[4]],Sales1,lag.max=24 , main="GR Residential West EU Sales & NSA")
ccf(lst2[[1]][[10]],Sales,lag.max=24 , main="GR Residential West EU Sales & Mats")


#Dickey Fuller Test
library(tseries)
tseries::adf.test(lst2[[1]][[6]],Sales,k=4) #nonstationary
tseries::adf.test(lst2[[1]][[7]],k=4) #nonstationary
tseries::adf.test(lst2[[1]][[8]],k=4) #nonstationary
tseries::adf.test(lst2[[1]][[9]],k=4) #nonstationary
tseries::adf.test(lst2[[1]][[10]],k=4) #nonstationary
tseries::adf.test(lst2[[1]][[4]],k=4) #nonstationary
tseries::adf.test(lst2[[1]][[1]],k=4) #adf of West Europe Sales nonstationary
tseries::adf.test(lst2[[1]][[2]],k=4) #adf of Russian Fed Sales nonstationary

#Prewhiten Russian Fed
library(TSA)
prewhiten(resii$Russian.Fed..Sales,resii$DUCS,ylab='CCF', main="GR Residential Russian Fed EU Sales & DUCS ", sub=" ")
prewhiten(resii$Russian.Fed..Sales,resii$WUCS,ylab='CCF', main="GR Residential Russian Fed EU Sales & WUCS ", sub=" ")
prewhiten(resii$Russian.Fed..Sales,resii$T30,ylab='CCF', main="GR Residential Russian Fed EU Sales & T30 ", sub=" ")
prewhiten(resii$Russian.Fed..Sales,resii$NSA,ylab='CCF', main="GR Residential Russian Fed  Sales & NSA ", sub=" ")
prewhiten(resii$Russian.Fed..Sales,resii$Mats,ylab='CCF', main="Russian Fed EU Sales & Mats ", sub=" ")
prewhiten(resii$Russian.Fed..Sales,resii$Rovings,ylab='CCF', main="Russian Fed EU Sales & Rovings ", sub=" ")

#Prewhiten West EU
prewhiten(resii$West.Europe.Sales,resii$DUCS,ylab='CCF', main=" GR Residential West EU Sales & DUCS ", sub=" ")
prewhiten(resii$West.Europe.Sales,resii$WUCS,ylab='CCF', main="GR Residential West EU Sales & WUCS ", sub=" ")
prewhiten(resii$West.Europe.Sales,resii$T30,ylab='CCF', main="GR Residential West EU Sales & T30 ", sub=" ")
prewhiten(resii$West.Europe.Sales,resii$NSA,ylab='CCF', main="GR Residential West EU Sales & NSA ", sub=" ")
prewhiten(resii$West.Europe.Sales,resii$Mats,ylab='CCF', main="GR Residential West Fed EU Sales & Mats ", sub=" ")
prewhiten(resii$West.Europe.Sales,resii$Rovings,ylab='CCF', main=" GR Residential West Fed EU Sales & Rovings ", sub=" ")



#Non Residential

#Detele unwanted columns
nonresii$Year <- NULL
nonresii$Month <- NULL
nonresii$SEUM <- NULL
head(nonresii)

#Transform every column into a timeseries (Non RESIDENTIAL)
nonresi_list <- list(nonresii)
lst3 <- lapply(nonresi_list, function(x) {
  x[] <- lapply(x, function(y) ts(y, start = min(as.numeric(row.names(x))),
                                  end = max(as.numeric(row.names(x)))))
  x})
str(lst3)
head(lst3)
head(lst2)
warnings(lst3)


#Pairs for ccf function plot cant figure it out right now
#tspairs <- CJ(1:24, 1:24)
head(tspairs)
try2 <- mapply(function(x, y){ccf(x, y)}, tspairs[1, ], tspairs[2,])


Sales2 <- lst3[[1]][[1]]
Sales3 <- lst3[[1]][[2]]

#Russian Fed Sales
ccf(lst3[[1]][[6]],Sales3,lag.max=24 , main="GR Non Residential Russian Fed Sales & DUCS")
ccf(lst3[[1]][[7]],Sales3,lag.max=24 , main="GR Non Residential Russian Fed Sales & WUCS")
ccf(lst3[[1]][[9]],Sales3,lag.max=24 , main="GR Non Residential Russian Fed Sales & Rovings")
ccf(lst3[[1]][[8]],Sales3,lag.max=24 , main="GR Non Residential Russian Fed Sales & T30")
ccf(lst3[[1]][[4]],Sales3,lag.max=24 , main="GR Non Residential Russian Fed Sales & NSA")
ccf(lst3[[1]][[10]],Sales3,lag.max=24 , main="GR Non Residential Russian Fed Sales & Mats")

#West Europe Sales
ccf(lst3[[1]][[6]],Sales2,lag.max=24 , main="GR Non Residential West EU Sales & DUCS")
ccf(lst3[[1]][[7]],Sales2,lag.max=24 , main="GR Non Residential West EU Sales & WUCS")
ccf(lst3[[1]][[9]],Sales2,lag.max=24 , main="GR Non Residential West EU Sales & Rovings")
ccf(lst3[[1]][[8]],Sales2,lag.max=24 , main="GR Non Residential West EU Sales & T30")
ccf(lst3[[1]][[4]],Sales2,lag.max=24 , main="GR Non Residential West EU Sales & NSA")
ccf(lst3[[1]][[10]],Sales2,lag.max=24 , main="GR Non Residential West EU Sales & Mats")


#Dickey Fuller Test
library(tseries)
tseries::adf.test(lst3[[1]][[6]],k=4) #nonstationary
tseries::adf.test(lst3[[1]][[7]],k=4) #nonstationary
tseries::adf.test(lst3[[1]][[8]],k=4) #nonstationary
tseries::adf.test(lst3[[1]][[9]],k=4) #nonstationary
tseries::adf.test(lst3[[1]][[10]],k=4) #nonstationary
tseries::adf.test(lst3[[1]][[4]],k=4) # NSA nonstationary
tseries::adf.test(Sales2,k=4) # West Europ Non Resi Non Stationary
tseries::adf.test(Sales3,k=4) #Russian Fed Non Resi Non Stationary
#Prewhiten Russian Fed
library(TSA)
prewhiten(nonresii$Russian.Fed..Sales,nonresii$DUCS,ylab='CCF', main="GR Non Residential Russian Fed Sales & DUCS ", sub=" ")
prewhiten(nonresii$Russian.Fed..Sales,nonresii$WUCS,ylab='CCF', main="GR Non Residential Russian Fed EU Sales & WUCS ", sub=" ")
prewhiten(nonresii$Russian.Fed..Sales,nonresii$T30,ylab='CCF', main="GR Non Residential Russian Fed EU Sales & T30 ", sub=" ")
prewhiten(nonresii$Russian.Fed..Sales,nonresii$NSA,ylab='CCF', main="GR Non Residential Russian Fed EU Sales & NSA ", sub=" ")
prewhiten(nonresii$Russian.Fed..Sales,nonresii$Mats,ylab='CCF', main="GR Non Residential Russian Fed EU Sales & Mats ", sub=" ")
prewhiten(nonresii$Russian.Fed..Sales,nonresii$Rovings,ylab='CCF', main="GR Non Residential Russian Fed EU Sales & Rovings ", sub=" ")

#Prewhiten West EU
prewhiten(nonresii$West.Europe.Sales,nonresii$DUCS,ylab='CCF', main="GR Non Residential West EU Sales & DUCS ", sub=" ")
prewhiten(nonresii$West.Europe.Sales,nonresii$WUCS,ylab='CCF', main="GR Non Residential West EU Sales & WUCS ", sub=" ")
prewhiten(nonresii$West.Europe.Sales,nonresii$T30,ylab='CCF', main="GR Non Residential West EU Sales & T30 ", sub=" ")
prewhiten(nonresii$West.Europe.Sales,nonresii$NSA,ylab='CCF', main="GR Non Residential West EU Sales & NSA ", sub=" ")
prewhiten(nonresii$West.Europe.Sales,nonresii$Mats,ylab='CCF', main="GR Non Residential West Fed EU Sales & Mats ", sub=" ")
prewhiten(nonresii$West.Europe.Sales,nonresii$Rovings,ylab='CCF', main="GR Non Residential West Fed EU Sales & Rovings ", sub=" ")






