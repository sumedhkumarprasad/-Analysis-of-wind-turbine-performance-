setwd("C:/Users/sumedh/Desktop/assignment/wind month wise curve")

wtdata=read.csv("wt_data.csv")
wtdata=na.omit(wtdata)
library(WindCurves)# For drawing the Power curves
#libraries
library(MASS)
library(tidyverse)
library(knitr)
library(viridis)
library(dplyr)
library(scales)
library(lubridate)
#### Converting into the data time format and storing it into the respective columns
wtdata$ttimestamplocal=dmy_hm(wtdata$ttimestamplocal)
wtdata$month=month(wtdata$ttimestamplocal)
wtdata$year=year(wtdata$ttimestamplocal)
wtdata$day=day(wtdata$ttimestamplocal)
wtdata$date=date(wtdata$ttimestamplocal)

## filter the data month wise and wind turbine state='ok'
dfjune = wtdata %>% filter(month==6,wtg_state=='ok') %>% select(windspeed,power,month,wtg_state)
dfjuly = wtdata %>% filter(month==7,wtg_state=='ok') %>% select(windspeed,power,month,wtg_state)
dfaug = wtdata %>% filter(month==8,wtg_state=='ok') %>% select(windspeed,power,month,wtg_state)
dfsep = wtdata %>% filter(month==9,wtg_state=='ok') %>% select(windspeed,power,month,wtg_state)
dfoct = wtdata %>% filter(month==10,wtg_state=='ok') %>% select(windspeed,power,month,wtg_state)
dfnov = wtdata %>% filter(month==11,wtg_state=='ok') %>% select(windspeed,power,month,wtg_state)
dfdec = wtdata %>% filter(month==12,wtg_state=='ok') %>% select(windspeed,power,month,wtg_state)

## Analysis of Wind Curve month wise
## June month

s_dfjune=dfjune$windspeed
p_dfjune=dfjune$power
da_dfjune=data.frame(s_dfjune,p_dfjune)
x_dfjune=fitcurve(da_dfjune)
validate.curve(x_dfjune)
plot(x_dfjune)
plot(dfjune$windspeed,dfjune$power, main="Power Curve for june with ok State")

# For July Month
s_dfjuly=dfjuly$windspeed
p_dfjuly=dfjuly$power
da_dfjuly=data.frame(s_dfjuly,p_dfjuly)
x_dfjuly=fitcurve(da_dfjuly)
validate.curve(x_dfjuly)
plot(x_dfjuly)
plot(dfjuly$windspeed,dfjuly$power, main="Power Curve for july month with ok State")

## For August Month
s_dfaug=dfaug$windspeed
p_dfaug=dfaug$power
da_dfaug=data.frame(s_dfaug,p_dfaug)
x_dfaug=fitcurve(da_dfaug)
validate.curve(x_dfaug)
plot(x_dfaug)
plot(dfaug$windspeed,dfaug$power, main="Power Curve for August month with ok State")

# For Sep month
s_dfsep=dfsep$windspeed
p_dfsep=dfsep$power
da_dfsep=data.frame(s_dfsep,p_dfsep)
x_dfsep=fitcurve(da_dfsep)
validate.curve(x_dfsep)
plot(x_dfsep)
plot(dfsep$windspeed,dfsep$power, main="Power Curve for September month with ok State")

# For october Month
s_dfoct=dfoct$windspeed
p_dfoct=dfoct$power
da_dfoct=data.frame(s_dfoct,p_dfoct)
x_dfoct=fitcurve(da_dfoct)
validate.curve(x_dfoct)
plot(x_dfoct)
plot(dfoct$windspeed,dfoct$power, main="Power Curve for October month with ok State")

# For November month

s_dfnov=dfnov$windspeed
p_dfnov=dfnov$power
da_dfnov=data.frame(s_dfnov,p_dfnov)
x_dfnov=fitcurve(da_dfnov)
validate.curve(x_dfnov)
plot(x_dfnov)
plot(dfnov$windspeed,dfnov$power, main="Power Curve for November month with ok State")

# For December month
s_dfdec=dfdec$windspeed
p_dfdec=dfdec$power
da_dfdec=data.frame(s_dfdec,p_dfdec)
x_dfdec=fitcurve(da_dfdec)
validate.curve(x_dfdec)
plot(x_dfdec)
plot(dfdec$windspeed,dfdec$power, main="Power Curve for December month with ok State")

#### With With Turbine 1 , Month wise and Ok State of the Turbine
dfjunet1 = wtdata %>% filter(month==6,wtg_state=='ok',unitlocation=='WTG01') %>% select(windspeed,power,month,wtg_state,unitlocation)
dfjunet2 = wtdata %>% filter(month==6,wtg_state=='ok',unitlocation=='WTG02') %>% select(windspeed,power,month,wtg_state,unitlocation)

dfjulyt1 = wtdata %>% filter(month==7,wtg_state=='ok',unitlocation=='WTG01') %>% select(windspeed,power,month,wtg_state,unitlocation)
dfjulyt2 = wtdata %>% filter(month==7,wtg_state=='ok',unitlocation=='WTG02') %>% select(windspeed,power,month,wtg_state,unitlocation)

dfaugt1 = wtdata %>% filter(month==8,wtg_state=='ok',unitlocation=='WTG01') %>% select(windspeed,power,month,wtg_state,unitlocation)
dfaugt2 = wtdata %>% filter(month==8,wtg_state=='ok',unitlocation=='WTG02') %>% select(windspeed,power,month,wtg_state,unitlocation)

dfsept1 = wtdata %>% filter(month==9,wtg_state=='ok',unitlocation=='WTG01') %>% select(windspeed,power,month,wtg_state,unitlocation)
dfsept2 = wtdata %>% filter(month==9,wtg_state=='ok',unitlocation=='WTG02') %>% select(windspeed,power,month,wtg_state,unitlocation)

dfoctt1 = wtdata %>% filter(month==10,wtg_state=='ok',unitlocation=='WTG01') %>% select(windspeed,power,month,wtg_state,unitlocation)
dfoctt2 = wtdata %>% filter(month==10,wtg_state=='ok',unitlocation=='WTG02') %>% select(windspeed,power,month,wtg_state,unitlocation)

dfnovt1 = wtdata %>% filter(month==11,wtg_state=='ok',unitlocation=='WTG01') %>% select(windspeed,power,month,wtg_state,unitlocation)
dfnovt2 = wtdata %>% filter(month==11,wtg_state=='ok',unitlocation=='WTG02') %>% select(windspeed,power,month,wtg_state,unitlocation)

dfdect1 = wtdata %>% filter(month==12,wtg_state=='ok',unitlocation=='WTG01') %>% select(windspeed,power,month,wtg_state,unitlocation)
dfdect2 = wtdata %>% filter(month==12,wtg_state=='ok',unitlocation=='WTG02') %>% select(windspeed,power,month,wtg_state,unitlocation)

# For wind turbine One , June month ,Ok state of wind turbine
s_dfjunet1=dfjunet1$windspeed
p_dfjunet1=dfjunet1$power
da_dfjunet1=data.frame(s_dfjunet1,p_dfjunet1)
x_dfjunet1=fitcurve(da_dfjunet1)
validate.curve(x_dfjunet1)
plot(x_dfjunet1)
plot(dfjunet1$windspeed,dfjunet1$power, main="Power Curve for Turbine 1 ,June month & ok State")

# For wind turbine two , June month ,Ok state of wind turbine
s_dfjunet2=dfjunet2$windspeed
p_dfjunet2=dfjunet2$power
da_dfjunet2=data.frame(s_dfjunet2,p_dfjunet2)
x_dfjunet2=fitcurve(da_dfjunet2)
validate.curve(x_dfjunet2)
plot(x_dfjunet2)
plot(dfjunet2$windspeed,dfjunet2$power, main="Power Curve for Turbine 2 ,June month & ok State")

# For wind turbine one , July month ,Ok state of wind turbine
s_dfjulyt1=dfjulyt1$windspeed
p_dfjulyt1=dfjulyt1$power
da_dfjulyt1=data.frame(s_dfjulyt1,p_dfjulyt1)
x_dfjulyt1=fitcurve(da_dfjulyt1)
validate.curve(x_dfjulyt1)
plot(x_dfjulyt1)
plot(dfjulyt1$windspeed,dfjulyt1$power, main="Power Curve for Turbine 1 ,July month & ok State")

# For wind turbine two , July month ,Ok state of wind turbine
s_dfjulyt2=dfjulyt2$windspeed
p_dfjulyt2=dfjulyt2$power
da_dfjulyt2=data.frame(s_dfjulyt2,p_dfjulyt2)
x_dfjulyt2=fitcurve(da_dfjulyt2)
validate.curve(x_dfjulyt2)
plot(x_dfjulyt2)
plot(dfjulyt2$windspeed,dfjulyt2$power, main="Power Curve for Turbine 2 ,July month & ok State")

# For wind turbine one , August month ,Ok state of wind turbine
s_dfaugt1=dfaugt1$windspeed
p_dfaugt1=dfaugt1$power
da_dfaugt1=data.frame(s_dfaugt1,p_dfaugt1)
x_dfaugt1=fitcurve(da_dfaugt1)
validate.curve(x_dfaugt1)
plot(x_dfaugt1)
plot(dfaugt1$windspeed,dfaugt1$power, main="Power Curve for Turbine 1 ,August month & ok State")

# For wind turbine two , August month ,Ok state of wind turbine
s_dfaugt2=dfaugt2$windspeed
p_dfaugt2=dfaugt2$power
da_dfaugt2=data.frame(s_dfaugt2,p_dfaugt2)
x_dfaugt2=fitcurve(da_dfaugt2)
validate.curve(x_dfaugt2)
plot(x_dfaugt2)
plot(dfaugt2$windspeed,dfaugt2$power, main="Power Curve for Turbine 2 ,August month & ok State")

# For wind turbine one , September month ,Ok state of wind turbine

s_dfsept1=dfsept1$windspeed
p_dfsept1=dfsept1$power
da_dfsept1=data.frame(s_dfsept1,p_dfsept1)
x_dfsept1=fitcurve(da_dfsept1)
validate.curve(x_dfsept1)
plot(x_dfsept1)
plot(dfsept1$windspeed,dfsept1$power, main="Power Curve for Turbine 1 ,September month & ok State")

# For wind turbine two , September month ,Ok state of wind turbine

s_dfsept2=dfsept2$windspeed
p_dfsept2=dfsept2$power
da_dfsept2=data.frame(s_dfsept2,p_dfsept2)
x_dfsept2=fitcurve(da_dfsept2)
validate.curve(x_dfsept2)
plot(x_dfsept2)
plot(dfsept2$windspeed,dfsept2$power, main="Power Curve for Turbine 2 ,September month & ok State")

# For wind turbine one , October  month ,Ok state of wind turbine

s_dfoctt1=dfoctt1$windspeed
p_dfoctt1=dfoctt1$power
da_dfoctt1=data.frame(s_dfoctt1,p_dfoctt1)
x_dfoctt1=fitcurve(da_dfoctt1)
validate.curve(x_dfoctt1)
plot(x_dfoctt1)
plot(dfoctt1$windspeed,dfoctt1$power, main="Power Curve for Turbine 1 ,October month & ok State")

# For wind turbine two , October  month ,Ok state of wind turbine
s_dfoctt2=dfoctt2$windspeed
p_dfoctt2=dfoctt2$power
da_dfoctt2=data.frame(s_dfoctt2,p_dfoctt2)
x_dfoctt2=fitcurve(da_dfoctt2)
validate.curve(x_dfoctt2)
plot(x_dfoctt2)
plot(dfoctt2$windspeed,dfoctt2$power, main="Power Curve for Turbine 2 ,October month & ok State")

# For wind turbine one, November month , ok state of wind turbine

s_dfnovt1=dfnovt1$windspeed
p_dfnovt1=dfnovt1$power
da_dfnovt1=data.frame(s_dfnovt1,p_dfnovt1)
x_dfnovt1=fitcurve(da_dfnovt1)
validate.curve(x_dfnovt1)
plot(x_dfnovt1)
plot(dfnovt1$windspeed,dfnovt1$power, main="Power Curve for Turbine 1 , November month & ok State")

# For wind turbine two, November month , ok state of wind turbine

s_dfnovt2=dfnovt2$windspeed
p_dfnovt2=dfnovt2$power
da_dfnovt2=data.frame(s_dfnovt2,p_dfnovt2)
x_dfnovt2=fitcurve(da_dfnovt2)
validate.curve(x_dfnovt2)
plot(x_dfnovt2)
plot(dfnovt2$windspeed,dfnovt2$power, main="Power Curve for Turbine 2 , November month & ok State")

# For wind turbine one , December month , ok state of wind turbine

s_dfdect1=dfdect1$windspeed
p_dfdect1=dfdect1$power
da_dfdect1=data.frame(s_dfdect1,p_dfdect1)
x_dfdect1=fitcurve(da_dfdect1)
validate.curve(x_dfdect1)
plot(x_dfdect1)
plot(dfdect1$windspeed,dfdect1$power, main="Power Curve for Turbine 1 , December month & ok State")

# For wind turbine two , December month , ok state of wind turbine

s_dfdect2=dfdect2$windspeed
p_dfdect2=dfdect2$power
da_dfdect2=data.frame(s_dfdect2,p_dfdect2)
x_dfdect2=fitcurve(da_dfdect2)
validate.curve(x_dfdect2)
plot(x_dfdect2)
plot(dfdect2$windspeed,dfdect2$power, main="Power Curve for Turbine 2 , December month & ok State")

#### Further digging out the September Month as it is giving very poor MAPE Value for both wind Turbine 

# For Wind Turbine 1
sept1=wtdata %>% filter(month==9,wtg_state=='ok',unitlocation=='WTG01') %>% select(month,day,windspeed)%>% group_by(day)%>% summarize(mean_day_wind=mean(windspeed))
ggplot(sept1,aes(day,mean_day_wind))+
  geom_line()+theme_minimal()+labs(x='day')+labs(x='September Month')+scale_x_continuous(breaks=seq(1,30,1))+scale_y_continuous(breaks=seq(1,12,1))+title(main = "In September Month Wind Turbine 1 with Ok state")

# Sep 14th,21st and 27th having the lowest windspeed in Wind Turbine 1 with Sep month and Ok State of Wind Turbine.

###################

# For wind turbine 2

sept2=wtdata %>% filter(month==9,wtg_state=='ok',unitlocation=='WTG02') %>% select(month,day,windspeed)%>% group_by(day)%>% summarize(mean_day_wind=mean(windspeed))
ggplot(sept2,aes(day,mean_day_wind))+
  geom_line()+theme_minimal()+labs(x='day')+labs(x='September Month')+scale_x_continuous(breaks=seq(1,30,1))+scale_y_continuous(breaks=seq(1,12,1))+title(main = "In September Month Wind Turbine 2 with Ok state")

# In September month 9th,14th,19th,28th having the low value and data dips sudden on these dates.
