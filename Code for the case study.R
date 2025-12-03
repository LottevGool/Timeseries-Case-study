rm(list=ls())
setwd("/Users/laura/Downloads/Master/Time Series/Case Study")
library(dplyr)
library(lubridate)
library(tseries)
library(urca)
library(forecast)


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

demand<- citibikedata%>%
  filter(between(month, 1, 5))

plot(demand$datetime,demand$demand, type='l', main="Demand Citi Bike Jan-May", ylab="Demand", xlab="Time")
plot(diff(demand$demand), type = "l", main = "Differenced Demand Jan-May", ylab = "Demand", xlab = "Time")

training <- citibikedata%>%
  filter(between(month, 1, 4))

y <- training$demand

# --- ADF test on raw series ---
adf_raw <- adf.test(y)
print(adf_raw)

# --- Including trend in adf ---

# The default adf.test() in R does not include trend. 
# For series with a trend, we might want ur.df() 
# from the urca package with type "trend"

adf_trend <- ur.df(y, type="trend", lags=14)
summary(adf_trend)

# --- Arima modelling ---

# correlogram to derive model

par(mfrow = c(1,1))

acf(y, main = "ACF of Demand", lag.max = 200)
pacf(y, main = "PACF of Demand", lag.max = 200)

# Fit all ARIMA models (non-seasonal)

# Model 1: ARIMA(1,0,1)
m1 <- arima(y, order = c(1,0,1), method = "ML")

# Model 2: ARIMA(2,0,1)
m2 <- arima(y, order = c(2,0,1), method = "ML")

# Model 3: ARIMA(2,0,2)
m3 <- arima(y, order = c(2,0,2), method = "ML")

# Additional models including d = 1 to compare:

# ARIMA(2,1,0)
m4 <- arima(y, order = c(2,1,0), method = "ML")

# ARIMA(0,1,2)
m5 <- arima(y, order = c(0,1,2), method = "ML")

# ARIMA(2,1,1)
m6 <- arima(y, order = c(2,1,1), method = "ML")

# Compare model fits
AIC(m1, m2, m3, m4, m5, m6)
BIC(m1, m2, m3, m4, m5, m6)

# --- auto.arima ---

# set time series object with hourly seasonality (daily cycle)
y_ts <- ts(y, frequency = 24)    # training y is hourly, frequency=24

# --- Basic automatic search (fast, default) ---
auto_default <- auto.arima(y_ts, seasonal = TRUE)   # automatic seasonal detection
summary(auto_default)
checkresiduals(auto_default)                     # residual diagnostics

# --- Data-driven but force non-seasonal search (to compare) ---
auto_nonseasonal <- auto.arima(y_ts, seasonal = FALSE, 
                               ic = "aicc", 
                               stepwise = FALSE, 
                               approximation = FALSE)
summary(auto_nonseasonal)
checkresiduals(auto_nonseasonal)

# --- More exhaustive search (slower) ---
#auto_exhaustive <- auto.arima(y_ts, seasonal = TRUE,
                              #stepwise = FALSE,        # disable fast step-wise
                              #approximation = FALSE,   # disable approximation
                              #max.p = 5, max.q = 5,
                              #max.P = 2, max.Q = 2,
                              #ic = "aicc",
                              #trace = TRUE)            # shows search steps
#summary(auto_exhaustive)
#checkresiduals(auto_exhaustive)


