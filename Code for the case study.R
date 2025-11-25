rm(list=ls())
setwd("C:/one drive/Počítač/UNI/Master studies/Timeseries Econometrics/Case study")
library(dplyr)
library(lubridate)

load("citibike.RData")
citibikedata<-as.data.frame(citibike)

citibikedata <- citibikedata %>%
  mutate(
    datetime = make_datetime(
      year  = year,
      month = month,
      day   = day,
      hour  = hour   # 0–23

    )
  )

january<- citibikedata%>%
  filter(month==5)#%>%
  filter(day==20)
plot(january$datetime,january$demand, type='l')
