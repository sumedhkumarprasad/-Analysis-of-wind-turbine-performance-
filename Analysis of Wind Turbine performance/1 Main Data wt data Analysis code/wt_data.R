# Setting the Working Directory
setwd("C:/Users/sumedh/Desktop/assignment")
# Loading the DataSet
wtdata=read.csv("wt_data.csv")
library(WindCurves)# For drawing the Power curves
#libraries
library(MASS)
library(tidyverse)
library(knitr)
library(viridis)
library(dplyr)
library(scales)
View(wtdata)
#Checking the Structure of the data set
str(wtdata)
# By seeing the structure of the dataset ttimestamplocal is in factor , It should be change into the Date Time Format
summary(wtdata)
## Exploratory Data analysis (EDA) using the DataExplorer package
library(DataExplorer)# Data Explorer package is called for data analysis
library(data.table) # supporting library for the Data Explorer
introduce(wtdata)
plot_str(wtdata) # Plot the attribute datatype.
plot_bar(wtdata)
# From the graph it is clear that number of observation for Wind turbine (WT) is almost equal
# Wind turbine of "OK" stage is having the highest frequency.
plot_missing(wtdata)
# There are 3 missing values having column name is gen_rpm,rtr_rpm,amb_temp,windspeed, power
plot_histogram(wtdata)
# Power is spread is from 0 to 2000.
# Wind Speed is distributed like Normal Distribution curve.
# Air Density also distributed like Normal Distribution curve.
# amb_temp is distributed like Normal Distribution.
# nac_direction is right skewed which means that most of the values is in between 50 to 150.
# rotor and generator rpm follow the left skew distribution.
# Most of the blade pitch angle between -1 to +25 degree.

plot_boxplot(wtdata,by="wtg_state")
wtdata=na.omit(wtdata)# Ommiting the missing value in the dataset because in the wtg_state "service" and "get not conn" condition data was not generated.
plot_correlation(wtdata)
# power with windspeed is highly correlated after that generator and rotar speed.
# With Windspeed also rotor and generator rpm is  correlated
# Wtg_state_ok is  correlated with the rotor and generator rpm.
plot_density(wtdata)
# The density curve also the same pattern as th histogram curve is showing.
plot(wtdata$windspeed,wtdata$power) # Scatter plot between windspeed vs power which is looks like S shape curve.
plot(wtdata$rtr_rpm,wtdata$gen_rpm)# rotor rmp and generator rpm is correlated.
plot(wtdata$airdensity,wtdata$windspeed)# As Air density increases wind speed descreases
plot(wtdata$amb_temp,wtdata$windspeed)# Atmosphere temp. in between 25 to 30 show good result for wind speed.

library(data.table)
wtdata_dt=data.table(wtdata)

a=group_category(wtdata_dt,"wtg_state",threshold = 0.05)
print(a)
#From this it is clear that "ok" percentage of data is around 71.85% in the whole dataset
output=split_columns(wtdata)
# There are three discrete columns and 8 continous columns and zero missing column
output$num_discrete
output$num_continuous
output$num_all_missing

## Converting the timestamp data in character format ot data time format with the help lubridate package.
library(lubridate)
wtdata<-mutate(wtdata,ws_norm=windspeed/max(windspeed),power_norm=power/max(power))
wtdata$ttimestamplocal=dmy_hm(wtdata$ttimestamplocal)
library(ggplot2)
ggplot(wtdata,aes(ttimestamplocal,windspeed))+geom_line()+theme_minimal()
#### From the graph it is clear that Avg. wind speed is desceasing from June to October and then started increasing from October to January.
wtdata$month=month(wtdata$ttimestamplocal)
wtdata$year=year(wtdata$ttimestamplocal)
wtdata$day=day(wtdata$ttimestamplocal)
wtdata$date=date(wtdata$ttimestamplocal)
wtdata$weekday=weekdays.POSIXt(wtdata$ttimestamplocal)

ggplot(wtdata,aes(factor(day),windspeed))+
  geom_boxplot()+
  theme_minimal()+
  labs(x='day')

#ggplot(wtdata,aes(day,windspeed))+geom_line()+theme_minimal()
###############################################
ggplot(wtdata,aes(factor(month),windspeed))+
  geom_boxplot()+
  theme_minimal()+
  labs(x='month')
#ggplot(wtdata,aes(month,windspeed))+geom_line()+theme_minimal()
################################################

ggplot(wtdata,aes(factor(weekday),windspeed))+
  geom_boxplot()+
  theme_minimal()+
  labs(x='weekday')

#ggplot(wtdata,aes(weekday,windspeed))+geom_line()+theme_minimal()

#Ques 3 Ques 3 Use the fitted curve to create the difference measured data and the fit.
# A) Is there any change point over time?
# B) Can you Estimate the Point of Change?
## For June data.
june=wtdata %>% filter(month==6) %>% select(month,day,windspeed)%>% group_by(day)%>% summarize(mean_day_wind=mean(windspeed))
       
ggplot(june,aes(day,mean_day_wind))+
  geom_line()+theme_minimal()+labs(x='day')+labs(x='June Month')+scale_x_continuous(breaks=seq(1,30,1))+scale_y_continuous(breaks=seq(1,12,1))

# June month graph is clear that there is two sudden dip on 8th June and 27th June.
# On 8th June value is around average windspeed 7.5m/s on 27th 6.2 m/s.

## For July data.
july=wtdata %>% filter(month==7) %>% select(month,day,windspeed)%>% group_by(day)%>% summarize(mean_day_wind=mean(windspeed))

ggplot(july,aes(day,mean_day_wind))+
  geom_line()+theme_minimal()+labs(x='day')+labs(x='July Month')+scale_x_continuous(breaks=seq(1,31,1))+scale_y_continuous(breaks=seq(1,12,1))

## In July Month THere is a sudden dip on 14th July and 29th July and mean windspeed values are in between 3 m/s to 4 m/s.

## For August data.
aug=wtdata %>% filter(month==8) %>% select(month,day,windspeed)%>% group_by(day)%>% summarize(mean_day_wind=mean(windspeed))
ggplot(aug,aes(day,mean_day_wind))+
  geom_line()+theme_minimal()+labs(x='day')+labs(x='August Month')+scale_x_continuous(breaks=seq(1,31,1))+scale_y_continuous(breaks=seq(1,12,1))
## There is the sudden dip in the mean windspeed on 5th and 30th day of August Month. On 5th value is around 6.5m/s and on 30th around 5.4  m/s 

## For September data.
sep=wtdata %>% filter(month==9) %>% select(month,day,windspeed)%>% group_by(day)%>% summarize(mean_day_wind=mean(windspeed))
ggplot(sep,aes(day,mean_day_wind))+
  geom_line()+theme_minimal()+labs(x='day')+labs(x='September Month')+scale_x_continuous(breaks=seq(1,30,1))+scale_y_continuous(breaks=seq(1,12,1))
# There is a descresing trends for whole September month and  on 28th of September lowest mean day wind. 

## For October data.
oct=wtdata %>% filter(month==10) %>% select(month,day,windspeed)%>% group_by(day)%>% summarize(mean_day_wind=mean(windspeed))
ggplot(oct,aes(day,mean_day_wind))+
  geom_line()+theme_minimal()+labs(x='day')+labs(x='October Month')+scale_x_continuous(breaks=seq(1,30,1))+scale_y_continuous(breaks=seq(1,12,1))
# From 1st Oct to 06th Oct mean wind speed is descreasing fro 6th oct to 12th Oct increasing from there again descreasing 17th oct then increases till 24 t Oct.

## For November data.
nov=wtdata %>% filter(month==11) %>% select(month,day,windspeed)%>% group_by(day)%>% summarize(mean_day_wind=mean(windspeed))
ggplot(nov,aes(day,mean_day_wind))+
  geom_line()+theme_minimal()+labs(x='day')+labs(x='Novomber Month')+scale_x_continuous(breaks=seq(1,30,1))+scale_y_continuous(breaks=seq(1,12,1))

# In November on 6th and 17 th mean wind speed is lowest in whole month of mean_day_wind

## For December Data
dec=wtdata %>% filter(month==12) %>% select(month,day,windspeed)%>% group_by(day)%>% summarize(mean_day_wind=mean(windspeed))
ggplot(dec,aes(day,mean_day_wind))+
  geom_line()+theme_minimal()+labs(x='day')+labs(x='December Month')+scale_x_continuous(breaks=seq(1,31,1))+scale_y_continuous(breaks=seq(1,12,1))


# On 3rd and 25th of December month there is a sudden dip in the data and the values in between 2 and 3 m/s of avg. day of the wind.


#== Ques 1.Evaluate the Power curve for all States and state "ok"==#

########### wtg_state=='ok'##############
ok=wtdata[wtdata$wtg_state=='ok',]
s_ok=ok$windspeed
p_ok=ok$power
da_ok=data.frame(s_ok,p_ok)
x_ok=fitcurve(da_ok)
validate.curve(x_ok)
plot(x_ok)
plot(ok$windspeed,ok$power, main="Power Curve for Ok State")

s_okk=ok$ws_norm
p_okk=ok$power_norm
da_okk=data.frame(s_okk,p_okk)
x_okk=fitcurve(da_okk)
validate.curve(x_okk)
plot(x_okk)
plot(ok$ws_norm,ok$power_norm, main="Power Curve for Ok State after normalization")
######### wtg_state=='Curtailed' #########
curtailed=wtdata[wtdata$wtg_state=='curtailed',]
s_curtailed=curtailed$windspeed
p_curtailed=curtailed$power
da_curtailed=data.frame(s_curtailed,p_curtailed)
x_curtailed=fitcurve(da_curtailed)
validate.curve(x_curtailed)
plot(x_curtailed)
plot(curtailed$windspeed,curtailed$power, main="Power Curve for curtailed state")

s_curtailedd=curtailed$ws_norm
p_curtailedd=curtailed$power_norm
da_curtailedd=data.frame(s_curtailedd,p_curtailedd)
x_curtailedd=fitcurve(da_curtailedd)
validate.curve(x_curtailedd)
plot(x_curtailedd)
plot(curtailed$ws_norm,curtailed$power_norm, main="Power Curve for curtailed state after nomalization ")

####### wtd_state=='wind low' ##########
windlow=wtdata[wtdata$wtg_state=='wind low',]
s_windlow=windlow$windspeed
p_windlow=windlow$power
da_windlow=data.frame(s_windlow,p_windlow)
x_windlow=fitcurve(da_windlow)
validate.curve(x_windlow)
plot(x_windlow)
plot(windlow$windspeed,windlow$power,main="Power curve for windlow state")

s_windloww=windlow$ws_norm
p_windloww=windlow$power_norm
da_windloww=data.frame(s_windloww,p_windloww)
x_windloww=fitcurve(da_windloww)
validate.curve(x_windloww)
plot(x_windloww)
plot(windlow$ws_norm,windlow$power_norm,main="Power curve for windlow state after normalization")

####### wtd_stage=='high wind cut-out' ######
windhigh=wtdata[wtdata$wtg_state=='high wind cut-out',]
s_windhigh=windhigh$windspeed
p_windhigh=windhigh$power
da_windhigh=data.frame(s_windhigh,p_windhigh)
x_windhigh=fitcurve(da_windhigh)
validate.curve(x_windhigh)
plot(x_windhigh)
plot(windhigh$windspeed,windhigh$power,main="Power curve for windhigh state")

s_windhighh=windhigh$ws_norm
p_windhighh=windhigh$power_norm
da_windhighh=data.frame(s_windhighh,p_windhighh)
x_windhighh=fitcurve(da_windhighh)
validate.curve(x_windhighh)
plot(x_windhighh)
plot(windhigh$windspeed,windhigh$power,main="Power curve for windhigh state after normalization")

####### wtd_stage=='data error' ######
#dataerror=wtdata[wtdata$wtg_state=='data error',]
#s_dataerror=dataerror$windspeed
#p_dataerror=dataerror$power
#da_dataerror=data.frame(s_dataerror,p_dataerror)
#x_dataerror=fitcurve(da_dataerror)
#validate.curve(x_dataerror)
#plot(x_dataerror) 
#" Could not able to plot the fitcurve because observation is too low i.e. 3"

#### Ques 2 Fit the curve to the two data and compare the two turbines.####

####### unitlocation=='WTG01' ######

wtg01=wtdata[wtdata$unitlocation=='WTG01',]
s_wtg01=wtg01$windspeed
p_wtg01=wtg01$power
da_wtg01=data.frame(s_wtg01,p_wtg01)
x_wtg01=fitcurve(da_wtg01)
validate.curve(x_wtg01)
plot(x_wtg01)
g=wtg01 %>%filter(wtg_state=='ok')%>%select(windspeed,power,wtg_state)
plot(g$windspeed,g$power,main='Power Curve for WTG01 with ok state of turbine')
s_g=g$windspeed
p_g=g$power
da_g=data.frame(s_g,p_g)
x_g=fitcurve(da_g)
validate.curve(x_g)
plot(x_g)

s_wtg011=wtg01$ws_norm
p_wtg011=wtg01$power_norm
da_wtg011=data.frame(s_wtg011,p_wtg011)
x_wtg011=fitcurve(da_wtg011)
validate.curve(x_wtg011)
plot(x_wtg011)
####### unitlocation=='WTG02' ######

wtg02=wtdata[wtdata$unitlocation=='WTG02',]
s_wtg02=wtg02$windspeed
p_wtg02=wtg02$power
da_wtg02=data.frame(s_wtg02,p_wtg02)
x_wtg02=fitcurve(da_wtg02)
validate.curve(x_wtg02)
plot(x_wtg02)

h=wtg02 %>%filter(wtg_state=='ok')%>%select(windspeed,power,wtg_state)
plot(h$windspeed,h$power,main='Power Curve for WTg02 with ok state of turbine')
s_h=h$windspeed
p_h=h$power
da_h=data.frame(s_h,p_h)
x_h=fitcurve(da_h)
validate.curve(x_h)
plot(x_h)

s_wtg022=wtg02$ws_norm
p_wtg022=wtg02$power_norm
da_wtg022=data.frame(s_wtg022,p_wtg022)
x_wtg022=fitcurve(da_wtg022)
validate.curve(x_wtg022)
plot(x_wtg022)

####With Normalized Data
#### From the boxplot it is clear that Avg. wind speed is desceasing from June to October and then started increasing from October to January.

#libraries
library(MASS)
library(tidyverse)
library(knitr)
library(viridis)
library(dplyr)
# binwise power curve function which we'll use a few times
calc_binwise_pc<-function(df){
  df %>%
    mutate(ws_bin=factor(round(ws_norm,2))) %>%
    group_by(ws_bin) %>%
    summarise(n=n(),power_norm=mean(power_norm)) %>%
    mutate(ws_bin_real=as.numeric(as.character(ws_bin)))
}

#tidy

#applying filtering techniques  across two  turbine  with different capacities to normalize the wind speed and power measurements.
wtdata<-mutate_if(wtdata,is.character,as.factor)

# normalise wind speed and power
#wtdata<-mutate(wtdata,ws_norm=windspeed/max(windspeed),power_norm=power/max(power))

# create container for power curves
pc<-list()
# Status code filtering 
# Understanding what are the Wind turbine States
state.count<-wtdata %>%
  group_by(wtg_state) %>%
  summarise(n=n(),isProducing=sum(power_norm>0)/n) %>%
  arrange(desc(isProducing))

kable(state.count)
# Drawing the power curve
ggplot(wtdata,aes(ws_norm,power_norm,fill=wtg_state))+
  geom_point(color='black',pch=21,alpha=.5)+
  theme_bw()+
  scale_x_continuous(breaks=seq(0,1,.1),labels=scales::percent)+
  scale_y_continuous(breaks=seq(0,1,.1),labels=scales::percent)+
  labs(title='All data')

wtdata<-mutate(wtdata,filter.avail=wtg_state=='ok')

# STATUS CODE POWER CURVE
# Filtering the power curve for status codes

pc[['status']]<-wtdata %>% 
  filter(filter.avail & !is.na(ws_norm) & !is.na(power_norm)) %>%
  calc_binwise_pc() %>%
  mutate(method='status code')

# show plot
wtdata %>% filter(filter.avail) %>%
  ggplot(aes(ws_norm,power_norm))+
  geom_point(data=filter(wtdata,!filter.avail),aes(ws_norm,power_norm),color='gray',alpha=.5)+
  geom_point(color='black',fill='blue',pch=21,alpha=.5)+
  geom_line(data=pc[['status']],aes(ws_bin_real,power_norm),lwd=1.5,color='red')+
  theme_bw()+
  scale_x_continuous(breaks=seq(0,1,.1),labels=scales::percent)+
  scale_y_continuous(breaks=seq(0,1,.1),labels=scales::percent)+
  labs(title='Filtered for Ok turbine Stage')

# STATISTICAL FILTERING
# filter out obvious low/no ok periods

wtdata <-mutate(wtdata,filter.basic_stat=power>=10 & windspeed>3 & windspeed<25 )

pc[['basic_stat']]<-wtdata %>% filter(filter.basic_stat) %>%
  calc_binwise_pc() %>%
  mutate(method='basic statistical')

# show plot
wtdata %>% filter(filter.basic_stat) %>%
  ggplot(aes(ws_norm,power_norm))+
  geom_point(data=filter(wtdata,!filter.basic_stat),aes(ws_norm,power_norm),color='gray',alpha=.5)+
  geom_point(color='black',fill='blue',pch=21,alpha=.5)+
  geom_line(data=pc[['basic_stat']],aes(ws_bin_real,power_norm),lwd=1.5,color='red')+
  theme_bw()+
  scale_x_continuous(breaks=seq(0,1,.1),labels=scales::percent)+
  scale_y_continuous(breaks=seq(0,1,.1),labels=scales::percent)+
  labs(title='Filtered for Basic Stat Filters')
# Graph is not very good. Try to get rid of some of those outliers.

## SMOOTHING SPLINE FILTERING
# After applying the basic statistical filters, fit a smoothing spline to the data and define any point with a residual error >10% Power as an outlier.
# create smoothing spline
fit.spl<-wtdata %>%
  filter(filter.basic_stat) %>%
  dplyr::select(ws_norm,power_norm) %>%
  smooth.spline()

# predict power using the fitted spline 
wtdata$power.spline<-predict(object=fit.spl,wtdata$ws_norm)$y

#define outliers
wtdata<-mutate(wtdata,spline.resid=abs(power_norm-power.spline),
               filter.spline=spline.resid<.1)


# calculate power curve
pc[['spline']]<- wtdata %>% 
  filter(filter.basic_stat & filter.spline) %>%
  calc_binwise_pc()%>%
  mutate(method='spline')

# show plot
wtdata %>% filter(filter.basic_stat & filter.spline) %>%
  ggplot(aes(ws_norm,power_norm,color=spline.resid))+
  geom_point(data=filter(wtdata,!filter.basic_stat | !filter.spline),aes(ws_norm,power_norm),color='gray',alpha=.5)+
  geom_point(alpha=.5)+
  scale_color_viridis()+
  geom_line(data=pc[['spline']],aes(ws_bin_real,power_norm),color='red',lwd=1.5)+
  theme_bw()+
  scale_x_continuous(breaks=seq(0,1,.1),labels=scales::percent)+
  scale_y_continuous(breaks=seq(0,1,.1),labels=scales::percent)+
  labs(title='Filtered for Basic Stat Filters and Smoothing Spline')

# MAHALANOBIS FILTERING
# After applying the basic statistical filters, use a k-means approach to define centroids to the data, 
# then define any point where the mahalanobis distance from it's centroid is greater than a certain threshold. 
# As per standard  15 centroids seems to fit turbine power curves well when using all the data and outliers are defined where the mahalanobis distance is >2.5. 
#  found that a mahalanobis threshold of 4.0 is a bit more conservative.

# calculate centroids using k-means
set.seed(1)
k<- wtdata %>%
  filter(filter.basic_stat) %>%
  dplyr::select(ws_norm,power_norm) %>%
  kmeans(centers = 15,iter.max = 1000)

# format and append output
wtdata<-mutate(wtdata,cluster=NA)
wtdata$cluster[wtdata$filter.basic_stat]<-k$cluster
k_centroids<-as.data.frame(k$centers)
k_centroids$cluster<-c(1:nrow(k_centroids))
names(k_centroids)<-c('ws_centroid','kw_centroid','cluster')
wtdata<-left_join(wtdata,k_centroids,by='cluster')


# calculate the mahalanobis distance for points in each centroid
wtdata<-mutate(wtdata,mahala=NA)
for (i in 1:nrow(k_centroids)){
  x<-as.matrix(wtdata[wtdata$cluster==i & wtdata$filter.basic_stat,c('ws_norm','power_norm')])
  if(length(x)>10){
    wtdata$mahala[wtdata$cluster==i & !is.na(wtdata$cluster)]<-
      mahalanobis(x = x,center = c(k_centroids$ws_centroid[i],k_centroids$kw_centroid[i]),cov=cov(x))
  }
}

# apply mahalanobis filter
wtdata<-mutate(wtdata,filter.mahala=mahala<4)

# calculate power curve
pc[['mahala']]<-wtdata %>% 
  filter(filter.basic_stat & filter.mahala) %>%
  calc_binwise_pc()%>%
  mutate(method='mahalanobis')

# show plot
wtdata %>% filter(filter.basic_stat & filter.mahala) %>%
  ggplot(aes(ws_norm,power_norm,color=mahala))+
  geom_point(data=filter(wtdata,!filter.basic_stat),aes(ws_norm,power_norm),color='gray',alpha=.5)+
  geom_point(data=filter(wtdata,filter.basic_stat | !filter.mahala),aes(ws_norm,power_norm),color='gray',alpha=.5)+
  geom_point(alpha=.5)+
  geom_point(data=k_centroids,aes(ws_centroid,kw_centroid),size=3,color='black',fill='red',pch=21)+
  scale_color_viridis()+
  geom_line(data=pc[['mahala']],aes(ws_bin_real,power_norm),color='red',lwd=1.5)+
  theme_bw()+
  scale_x_continuous(breaks=seq(0,1,.1),labels=scales::percent)+
  scale_y_continuous(breaks=seq(0,1,.1),labels=scales::percent)+
  labs(title='Filtered for Basic Stat Filters and Mahalanobis Distance')

# Power Binning
#bin the data by power (2% bin of normalized power) and filter out points where wind speed > +/- 2 standard deviations)

#create power bins
wtdata<-mutate(wtdata,power_norm_bin=cut(power_norm,seq(0,1,.02)))

# calculate normal distribution of wind speed in each power bin
power_bins<- wtdata %>%
  group_by(power_norm_bin) %>%
  summarise(power_bin_mean=mean(ws_norm),
            power_bin_sd=sd(ws_norm))
# filter data in each bin >2sd of the mean
wtdata<-left_join(wtdata,power_bins,'power_norm_bin') %>%
  mutate(power_bin_pnorm=pnorm(ws_norm,power_bin_mean,power_bin_sd),
         filter.powerBin=between(power_bin_pnorm,1-.9545,.9545))

# show plot of bins
wtdata %>%
  filter(filter.basic_stat) %>%
  ggplot(aes(power_norm_bin,ws_norm))+
  geom_boxplot()+
  coord_flip()+
  theme_bw()+
  scale_y_continuous(breaks=seq(0,1,.1),labels=scales::percent)+
  labs(title='Power Bin Illustration')

# calculate power curve
pc[['powerBin']]<-wtdata %>% 
  filter(filter.basic_stat) %>%
  calc_binwise_pc()%>%
  mutate(method='power bin')


# show plot of filter
wtdata %>% filter(filter.basic_stat & filter.powerBin) %>%
  ggplot(aes(ws_norm,power_norm,color=power_bin_pnorm))+
  geom_point(data=filter(wtdata,!filter.basic_stat ),aes(ws_norm,power_norm),color='gray',alpha=.5)+
  geom_point(alpha=.5)+
  scale_color_viridis()+
  geom_line(data=pc[['powerBin']],aes(ws_bin_real,power_norm),color='red',lwd=1.5)+
  theme_bw()+
  scale_x_continuous(breaks=seq(0,1,.1),labels=scales::percent)+
  scale_y_continuous(breaks=seq(0,1,.1),labels=scales::percent)+
  labs(title='Filtered for Basic Stat Filters and Power Bin')

#POWER CURVES SIDE-BY-SIDE

pc<-bind_rows(pc)
pc<-data.frame(pc)
ggplot(pc,aes(ws_bin_real,power_norm,color=method))+
  geom_line(lwd=1.5,alpha=.5)+
  theme_bw()+
  scale_x_continuous(breaks=seq(0,1,.1),labels=scales::percent)+
  scale_y_continuous(breaks=seq(0,1,.1),labels=scales::percent)+
  labs(title='Power Curve Comparisons')
