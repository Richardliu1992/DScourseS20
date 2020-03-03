library(ggplot2)
library(lubridate)
library(scales)

taxidata <- read.csv('C:/Users/Richa/Desktop/data_reports_monthly_indicators.csv',sep=',',header=TRUE)

format(taxidata$Month.Year, format="%Y/%m")

ggplot(taxidata,aes(Month.Year,as.numeric(Trips.Per.Day),group=License.Class，color=License.Class))+geom_line()+theme_classic()

ggplot(taxidata,aes(Month.Year,as.numeric(Unique.Drivers),group=License.Class，color=License.Class))+geom_point()+theme_classic()

ggplot(taxidata,aes(Month.Year,as.numeric(Unique.Vehicles),group=License.Class，color=License.Class))+geom_jitter()+theme_classic()
