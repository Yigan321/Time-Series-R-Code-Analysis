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
resi <- read.csv("EU_BL_Residential.csv",stringsAsFactors = FALSE)
nonresi <- read.csv("EU_BL_Non_Residential_Monthly.csv",stringsAsFactors = FALSE)
head(resi)

#Create Date Column for each dataset
resi<- resi %>%
  mutate(date = make_date(Year, Month))
head(resi)


nonresi<- nonresi %>%
  mutate(date = make_date(Year, Month))
head(nonresi)


#EU Region

# EU BL Residential 

#Make a copy of the Nonresi dataframe
library(data.table)
resi1<- copy(resi)
head(resi1)

#Delete unwanted columns
resi1$Region <- NULL
resi1$Country <- NULL
resi1$Year <- NULL
resi1$Month <- NULL
resi1$SEUM <- NULL
resi1$China.Rate <- NULL
resi1$Egypt.Rate <-NULL
resi1$date <-NULL

#Change from int to numeric for corrplot
resi1 <- sapply(resi1, as.numeric)
head(resi1)
str(resi1)

#ACF,EACF,PACF
resi_SalesEU <- ts(resi$Sales, start=c(2014),frequency=1)
resi_SAEU <-  ts(resi$SA, start=c(2014),frequency=1)
resi_NSAEU <-  ts(resi$NSA, start=c(2014),frequency=1)

acf(resi_SalesEU)
pacf(resi_SalesEU)
eacf(resi_SalesEU)

acf(resi_SAEU)
pacf(resi_SAEU)
eacf(resi_SAEU)

acf(resi_NSAEU)
pacf(resi_NSAEU)
eacf(resi_NSAEU)

#Correlation plot Residential
library(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(resi1), method="number",shade.col=NA, tl.col="black", tl.srt=25,
         addCoef.col = "black") 

#Lag Plots and CCF Plots
lag2.plot(resi_SAEU,resi_SalesEU, 12)
lag2.plot(resi_NSAEU,resi_SalesEU,12)

ccf(resi_SAEU,resi_SalesEU,lag.max = 24)
ccf(resi_NSAEU,resi_SalesEU,lag.max = 24)

#Dickey fuller Non Residentail 
library(tseries)
tseries::adf.test(resi_SalesEU)
tseries::adf.test(resi_SalesEU, k=0)
tseries::adf.test(resi_SAEU)
tseries::adf.test(resi_SAEU, k=0)
tseries::adf.test(resi_NSAEU)
tseries::adf.test(resi_NSAEU, k=0)



#BL Non Residential

#BL Non Residential

#Make a copy of the Nonresi dataframe
library(data.table)
nonresi1<- copy(nonresi)
head(nonresi1)
nonresi2 <- copy(nonresi)
head(nonresi2)


#Rename column Names
names(nonresi1)[names(nonresi) == 'Non.Residential..Commercial.Buildings..NSA.'] <- "NSA"
names(nonresi1)[names(nonresi) == 'Non.Residential..Commercial.Buildings..SAAR.'] <- "SA"


#Delete unwanted columns
nonresi1$Region <- NULL
nonresi1$Country <- NULL
nonresi1$Year <- NULL
nonresi1$Month <- NULL
nonresi1$SEUM <- NULL
nonresi1$China.Rate <- NULL
nonresi1$Egypt.Rate <-NULL
nonresi1$date <-NULL

#Change from int to numeric for corrplot
nonresi1 <- sapply(nonresi1, as.numeric)
head(nonresi1)
str(nonresi1)

#Correlation plot Nonresi
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(nonresi1,use="pairwise.complete.obs"), method="number",shade.col=NA, tl.col="black", tl.srt=90,
         addCoef.col = "black", tl.cex = .5,number.cex = .6,rect.lwd = 1)

#Lag Plots and CCF Plots
non_resi <- ts(nonresii$Sales, start=c(2014),frequency=1)
non_resi1 <- ts(nonresii$WUCS, start=c(2014),frequency=1)
non_resi2 <- ts(nonresii$T30, start = c(2014),frequency=1)

#Filter out post March 2020 dates
#resi_4 <- filter(resi, between(date, as.Date("2014-01-01"), as.Date("2020-02-01")))
#resi_44<-  ts(resi_4$WUCS, start=c(2014),frequency=1)
#tseries::adf.test(resi_44)
#tseries::adf.test(resi_44, k=0)


nonresi_subset <- nonresi[ , c("WUCS","T30")]
nonresii <- nonresi[complete.cases(nonresi_subset), ]


lag2.plot(non_resi1,non_resi, 12)
lag2.plot(non_resi2,non_resi, 12)

ccf(non_resi1,non_resi,lag.max = 24)
ccf(non_resi2,non_resi,lag.max = 24)

#ACF PACF and EACF
acf(non_resi)
pacf(non_resi)

acf(non_resi1)
pacf(non_resi1)

acf(non_resi2)
pacf(non_resi2)


#Dickey fuller Non Residental 
library(tseries)
tseries::adf.test(non_resi)
tseries::adf.test(non_resi,k=4)
tseries::adf.test(non_resi1)
tseries::adf.test(non_resi1, k=4)
tseries::adf.test(non_resi2)
tseries::adf.test(non_resi2, k=4)

#Line Plot
#Since composities industry has only values up to 2019 we need to filter 

nonresi2 <- nonresi2[nonresi2$Year == '2019',]
head(nonresi2)

nrsale <- ggplot(nonresi, aes(x = nonresi$date, y = nonresi$Sales)) +
  geom_line() + theme_bw()+ ggtitle("CFM , SE. Rovings and Sales") +
  xlab("Date") + ylab("Sales")

nrciv <- ggplot(nonresi, aes(x = nonresi$date, y = nonresi$WUCS)) +
  geom_line() + theme_bw()+ xlab("Date") + ylab("WUCS")

nrser <- ggplot(nonresi, aes(x = nonresi$date, y = nonresi$T30)) +
  geom_line() + theme_bw()+ xlab("Date") + ylab("T30")

plot_grid(nrsale,nrciv, nrser, labels = "AUTO",ncol = 1)
