setwd("C:/Users/sumedh/Desktop/assignment")
wtdata=read.csv("wt_data.csv")
wtdata=na.omit(wtdata)
library(MASS)
library(tidyverse)
library(knitr)
library(viridis)
library(lubridate)
wtdata<-mutate(wtdata,ws_norm=windspeed/max(windspeed))
wtdata$ttimestamplocal=dmy_hm(wtdata$ttimestamplocal)
wtdata$year=year(wtdata$ttimestamplocal)
wtdata$month=month(wtdata$ttimestamplocal)
wtdata$day=day(wtdata$ttimestamplocal)
yr=wtdata$year
mth=wtdata$month
dy=wtdata$day
windspeed=wtdata$windspeed
ws_norm=wtdata$ws_norm
tf=data.frame(yr,mth,dy,windspeed,ws_norm)
View(tf)
library(imputeTS)
library(fpp2)
library(forecast)

windspeed_d <- ts(tf[,4], start=c(2013,6), end=c(2014,1), frequency=31)
plot(windspeed_d)
ws_norm_d <- ts(tf[,5], start=c(2013,6), end=c(2014,1), frequency=31)
plot(ws_norm_d)
Acf(windspeed_d)
Acf(ws_norm_d)
Pacf(windspeed_d)
Pacf(ws_norm_d)

windspeed_w <- ts(tf[,4], start=c(2013,6), end=c(2014,1), frequency=7)
plot(windspeed_w)
ws_norm_w <- ts(tf[,5], start=c(2013,6), end=c(2014,1), frequency=7)
plot(ws_norm_w)

Acf(windspeed_w)
Acf(ws_norm_w)
Pacf(windspeed_w)
Pacf(ws_norm_w)

windspeed_m <- ts(tf[,4], start=c(2013,6), end=c(2014,1), frequency=12)
plot(windspeed_m)
ws_norm_m <- ts(tf[,5], start=c(2013,6), end=c(2014,1), frequency=12)
plot(ws_norm_m)

Acf(windspeed_m)
Acf(ws_norm_m)
Pacf(windspeed_m)
Pacf(ws_norm_m)


