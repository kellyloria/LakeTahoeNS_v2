library(tidyverse)
library(lubridate)
library(plotly)
library(devtools)
#install_github("nrlottig/nrlmetab")
library(nrlmetab)
library(zoo)
library(suncalc)
library(LakeMetabolizer)
library(readxl)
library(patchwork)
library(gridExtra)

do_raw <- read.csv("/Users/kellyloria/Desktop/Kelly/Alder/Rawdata/2015/2015Minidot.csv")

do.ts <- do_raw %>% select(X.1, Dissolved.Oxygen) %>% 
  rename(datetime = X.1, do.obs = Dissolved.Oxygen) %>% 
  mutate(datetime = mdy_hm(datetime))  

wtr.ts <- do_raw %>% select(X.1,Temperature) %>% 
  rename(datetime = X.1,wtr = Temperature) %>% 
  mutate(datetime = mdy_hm(datetime))

profile <- read_csv("/Users/kellyloria/Desktop/Kelly/Alder/Rawdata/2015/TestdataProfile2015.csv")

climate <- read_csv("/Users/kellyloria/Desktop/Kelly/Alder/Rawdata/2015/2015Climate.csv") %>% 
  rename(datetime='Date Time') %>% 
  rename(par="Solar Avg") %>% 
  rename(wspeed='Wind Speed Avg') %>% 
  select(datetime,par,wspeed)

dat_wtrprofile <- read_excel("/Users/kellyloria/Desktop/Kelly/Alder/Rawdata/2015/Castle2015wtrprof.xlsx") %>% 
  rename(depth = 'Depth (m)') %>% 
  gather(value="wtemp",key="datetime",-depth) %>% 
  mutate(datetime = as_date(as.numeric(datetime), origin = '1899-12-30')) %>% 
  mutate(datetime = as_datetime(paste0(datetime," 12:00:00"))) %>% 
  drop_na(wtemp) %>% 
  arrange(depth,datetime) %>% 
  pivot_wider(values_from = wtemp,names_from=depth,names_prefix="wtr_")

extcoef <- read_csv("/Users/kellyloria/Desktop/Kelly/Alder/Rawdata/2015/Castle2015extcoef.csv") 
#rename(datetime='datetime') %>% 
#rename(extcoef="extcoef") %>% 
#rename(wspeed='Wind Speed Avg') %>% 
#select(datetime,par,wspeed)

######
###clean DO data

ggplot(data = do.ts,aes(x=datetime,y=do.obs)) + geom_line()
describe.ts(do.ts)
do.ts.clean <- trim.outliers(do.ts,width = 7,sd.dev = 3)
ggplot(data = do.ts,aes(x=datetime,y=do.obs)) + geom_line() + 
  geom_line(data=do.ts.clean,aes(x=datetime,y=do.obs),col="red")
do.ts.avg <- aggregate.data(data = do.ts.clean,time.step = 60)
#Picks max values during day, and min during night.
#do.ts.avg <- extract.do.data(data = do.ts.clean,time.step = 60,lat = 41.226,lon = -122.383,tz = "US/Pacific")
ggplot(data=do.ts.clean,aes(x=datetime,y=do.obs)) + geom_line() +
  geom_line(data=do.ts.avg,aes(x=datetime,y=do.obs),col="red") +
  geom_point(data=profile,aes(x=datetime,y=doobs_3.0),col="blue")

#A way  of filtering the serie instead of doing it in excel
# do.ts.avg$value <- c(1:2833)
# do.ts.avg_series1 <- do.ts.avg %>% 
#     filter(value<1000)
# do.ts.avg_series2 <- do.ts.avg_series1 %>% 
#     filter(value<2000)


#data needs to be drift corrected

do.ts.drift <- drift.correction(dat = do.ts.avg,dat_lter = profile[,c(1:2)],var_dat = "do.obs",var_lter = "doobs_3.0")
ggplot(data = do.ts.avg,aes(x=datetime,y=do.obs)) + geom_line() +
  geom_line(data=do.ts.drift,aes(x=datetime,y=do.obs),col="blue") +
  geom_point(data=profile,aes(x=datetime,y=doobs_3.0),color="red")

#A way of merging data in case the data is split in 2 in order to run different
#drifftcorrections.

#do.ts.drift <- rbind(do.ts.drift_series1,do.ts.drift_series2)


######
###clean wtr data
ggplot(data = wtr.ts,aes(x=datetime,y=wtr)) + geom_line()
describe.ts(wtr.ts)
wtr.ts.clean <- trim.outliers(wtr.ts,width = 7,sd.dev = 5) 
ggplot(data = wtr.ts,aes(x=datetime,y=wtr)) + geom_line() + 
  geom_line(data=wtr.ts.clean,aes(x=datetime,y=wtr),col="red")
wtr.ts.avg <- aggregate.data(data = wtr.ts.clean,time.step = 60)
ggplot(data=wtr.ts.clean,aes(x=datetime,y=wtr)) + geom_line() +
  geom_line(data=wtr.ts.avg,aes(x=datetime,y=wtr),col="red") +
  geom_point(data=profile,aes(x=datetime,y=wtr_3.0),col="blue")

#In this case we apply a drift correction to the temperature.
# wtr.ts.drift <- drift.correction(dat = wtr.ts.avg,dat_lter = profile[,c(1,3)],var_dat = "wtr",var_lter = "wtr_3.0")
# ggplot(data = wtr.ts.avg,aes(x=datetime,y=wtr)) + geom_line() +
#   geom_line(data=wtr.ts.drift,aes(x=datetime,y=wtr),col="blue") +
#   geom_point(data=profile,aes(x=datetime,y=wtr_3.0),color="red")

#Join the drift dataset and Calcualte do_eq and do_sat

dat <- do.ts.avg %>% #note in this case I am not using the drift correction
  full_join(wtr.ts.avg) %>% 
  mutate(year=year(datetime), 
         yday=yday(datetime),
         hour=hour(datetime)+1) %>% 
  mutate(do_eq=o2.at.sat.base(temp = wtr,altitude = 1646)) %>% 
  mutate(o2_sat=do.obs/do_eq) %>% 
  full_join(climate)

#convert windspeed to 10m wind (required for gas exchange models per LakeMetabolizer)
dat <- dat %>% mutate(wspeed=wind.scale.base(wspeed,wnd.z=4))



####
#Calculating zmix

wtr.heat.map(as.data.frame(dat_wtrprofile))

zmix <- ts.meta.depths(dat_wtrprofile,seasonal=TRUE,na.rm=TRUE) %>% 
  mutate(year = year(datetime),yday=yday(datetime)) %>% 
  select(year,yday,top)
date_matrix <- zmix %>% expand(year, yday=full_seq(yday,1))
zmix <- date_matrix %>% left_join(zmix) %>%  #These lines are the one that do the linear extrapolation
  mutate(top = na.approx(top)) %>% 
  rename(z = top)

ggplot(data = zmix,aes(x=yday,y=z))+geom_point()+scale_y_reverse()

dat <- dat %>% full_join(zmix)

####
#Calculating par_int base on extinction coefficents, PAR and zmix (in this case I am using
# 2 in line 151 instead of z which is the variable zmix from dat)

extcoef <- extcoef %>% 
  mutate(year = year(datetime),yday=yday(datetime)) %>% 
  select(year,yday,extcoef)
date_matrix <- extcoef %>% expand(year, yday=full_seq(yday,1))
extcoef <- date_matrix %>% left_join(extcoef) %>%  #These lines are the one that do the linear extrapolation
  mutate(extcoef = na.approx(extcoef))  

ggplot(data = extcoef,aes(x=yday,y=extcoef))+geom_point()+scale_y_reverse()

dat <- dat %>% 
  full_join(extcoef) %>% 
  mutate(par_int = round((par - par*exp(-extcoef*2))/(extcoef*2),digits=0)) %>% 
  select(-extcoef)

#write.table(x = dat, file = "Castle2015Inputs.txt", row.names = TRUE)
#write.csv (x = dat, file = "D:/IADO/UNR/NoahPPR/Alder/Finalinputs/Castle2015Inputs.csv", row.names = TRUE)
