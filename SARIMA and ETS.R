rm(list=ls())
setwd("C:/Users/lotte/OneDrive/Documenten/Maastricht University/Year 4/Period 2/Time Series Econometrics")

library(dplyr)
library(lubridate)
library(tseries)
library(forecast)
library(bootUR)
library(vars)

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

plot(demand$datetime,demand$demand,type = "l", main="Demand Citi Bike Jan-May", ylab="Demand", xlab="Time")
plot(diff(demand$demand), type = "l", main = "Differenced Demand Jan-May", ylab = "Demand", xlab = "Time")

training <- citibikedata%>%
  filter(between(month, 1, 4))

y <- training$demand

# --- ADF test on raw series ---
# Test H0: I(2) unit root vs. Ha: stationary
diff2_y <- diff(y, differences = 2)
plot(diff2_y)
adf_d2_y <- adf(diff2_y, deterministics = "intercept")  
print(adf_d2_y)

stat_d2_y <- adf_d2_y$statistic
p_d2_y <- adf_d2_y$p.value

cat("ADF on d2(y): test-statistic =", stat_d2_y, " p-value =", p_d2_y, "\n")
# Reject H0: so demand I(2) is stationary.

# Test H0: I(1) unit root vs. Ha: stationary
diff_y <- diff(y)
plot(diff_y)
adf_d1_y <- adf(diff_y, deterministics = "intercept")  
print(adf_d1_y)

stat_d1_y <- adf_d1_y$statistic
p_d1_y <- adf_d1_y$p.value

cat("ADF on d(y): test-statistic =", stat_d1_y, " p-value =", p_d1_y, "\n")
# Reject H0: so demand I(1) is stationary.

# Test H0: I(0) stochastic trend vs. Ha: deterministic trend
plot(y)
x <- 1:length(y)
fit <- lm(y ~ x)
abline(fit, col = "red", lwd = 2) # From the trendline we see a slight upwards trend.

adf_d0_y <- adf(y, deterministics = "trend")
print(adf_d0_y)

stat_d0_y <- adf_d0_y$statistic
p_d0_y <- adf_d0_y$p.value

cat("ADF on y: test-statistic =", stat_d0_y, " p-value =", p_d0_y, "\n")
# Reject H0: we have a deterministic trend -> trend stationary -> seasonal peaks rise and fall and strong seasonal fluctuations + short timespan can make a trend test unreliable.
# This is most likely due to the seasonanilty in the dataset, as unmodeled seasonality can mimic a trend and lead to misleading unit-root tests.


# --- SEASONALITY ---
# Seasonal frequency
y_ts <- ts(y, frequency = 24)    # daily
y_ts2 <- ts(y, frequency = 24*7)    # weekly
y_ts3 <- ts(y, frequency = 24*30)   # monthly

# Daily
seasonplot(y_ts, main = "Daily seasonality")
ggsubseriesplot(y_ts)
acf(y, main = "ACF of Demand", lag.max = 200)
abline(v = seq(24, 200, by = 24), col = "red", lty = 2)  # highlight seasonal lags

# Weekly
seasonplot(y_ts2, main = "Weekly seasonality")
ggsubseriesplot(y_ts2)
acf(y, main = "ACF of Demand", lag.max = 200*7)
abline(v = seq(24*7, 200*7, by = 24*7), col = "red", lty = 2)  # highlight seasonal lags

# Monthly
seasonplot(y_ts3, main = "Monthly seasonality")
ggsubseriesplot(y_ts3)
acf(y, main = "ACF of Demand", lag.max = 200*30)
abline(v = seq(24*30, 200*30, by = 24*30), col = "red", lty = 2)  # highlight seasonal lags

# Take seasonal differences: looks stationary so no need to take further first difference.
plot(diff(demand$demand, lag =24), type = "l", main = "Seasonal Differenced Demand Jan-May", ylab = "Demand", xlab = "Time")
plot(diff(demand$demand), type = "l", main = "Differenced Demand Jan-May", ylab = "Demand", xlab = "Time")
y_season <- diff(y, lag = 24) # daily seasonality

acf(y_season, main = "ACF of Seasonal Demand", lag.max = 75)
pacf(y_season, main = "PACF of Seasonal Demand", lag.max = 75)

#SARIMA(3,1,0)[24]
# Fit model to seasonal component
seasonal_model <- arima(y, seasonal = list(order = c(3,1,0), period = 24))
summary(seasonal_model)

# Fit model to the remaining non-seasonal component: ARIMA(2,0,0)
non_seasonal <- seasonal_model$residuals
plot(non_seasonal, main = "Residuals after Modelling Seasonality", xlab = "Time", ylab = "Demand")     
acf(non_seasonal, main = "ACF of Non-Seasonal part of Demand")
pacf(non_seasonal, main = "PACF of Non-Seasonal part of Demand") 

#SARIMA(3,1,1)[24]
# Fit model to seasonal component
seasonal_model2 <- arima(y, seasonal = list(order = c(3,1,1), period = 24))
summary(seasonal_model2)

# Fit model to the remaining non-seasonal component: ARIMA(2,0,0)
non_seasonal2 <- seasonal_model2$residuals
plot(non_seasonal2, main = "Residuals after Modelling Seasonality", xlab = "Time", ylab = "Demand")     
acf(non_seasonal2, main = "ACF of Non-Seasonal part of Demand")
pacf(non_seasonal2, main = "PACF of Non-Seasonal part of Demand") 

#SARIMA(2,1,1)[24]
# Fit model to seasonal component
seasonal_model3 <- arima(y, seasonal = list(order = c(2,1,1), period = 24))
summary(seasonal_model3)

# Fit model to the remaining non-seasonal component: ARIMA(2,0,0)
non_seasonal3 <- seasonal_model3$residuals
plot(non_seasonal3, main = "Residuals after Modelling Seasonality", xlab = "Time", ylab = "Demand")     
acf(non_seasonal3, main = "ACF of Non-Seasonal part of Demand")
pacf(non_seasonal3, main = "PACF of Non-Seasonal part of Demand") 

# Fit different SARIMA models: Note it cannot be solved by maximum likelihood
# ARIMA(2,0,0)(3,1,0)[24]
m1_season <- arima(y, order = c(2,0,0), seasonal = list(order = c(3,1,0), period = 24))

# ARIMA(2,0,0)(2,1,0)[24]
m2_season <- arima(y, order = c(2,0,0), seasonal = list(order = c(2,1,0), period = 24))

# ARIMA(2,0,0)(4,1,0)[24]
m3_season <- arima(y, order = c(2,0,0), seasonal = list(order = c(4,1,0), period = 24))

# ARIMA(2,0,0)(3,1,1)[24]
m4_season <- arima(y, order = c(2,0,0), seasonal = list(order = c(3,1,1), period = 24))

# ARIMA(2,0,0)(2,1,1)[24]
m5_season <- arima(y, order = c(2,0,0), seasonal = list(order = c(2,1,1), period = 24))

AIC(m1_season, m2_season, m3_season, m4_season, m5_season)
BIC(m1_season, m2_season, m3_season, m4_season, m5_season)

# --- Basic automatic search (fast, default) ---
auto_default <- auto.arima(y_ts, seasonal = TRUE)   # automatic seasonal detection
summary(auto_default)
checkresiduals(auto_default)                     # residual diagnostics

# --- More exhaustive search (much slower with seasonal components) ---
# auto_exhaustive <- auto.arima(y_ts, seasonal = TRUE,
# stepwise = FALSE,        # disable fast step-wise
# approximation = FALSE,   # disable approximation
# max.p = 5, max.q = 5,
# max.P = 2, max.Q = 2,
# ic = "aicc",
# trace = TRUE)            # shows search steps
# summary(auto_exhaustive)
# checkresiduals(auto_exhaustive)

# m5 has lower AIC and BIC but is not "valid" in exhaustive search, why?
summary(m5_season)

# --- Different seasonal frequencies ---
y_ts2 <- ts(y, frequency = 24*7)    # weekly
auto_default <- auto.arima(y_ts2, seasonal = TRUE)   # automatic seasonal detection
summary(auto_default)
checkresiduals(auto_default) 

#y_ts3 <- ts(y, frequency = 24*30)   # monthly
#auto_default <- auto.arima(y_ts3, seasonal = TRUE)   # automatic seasonal detection
#summary(auto_default)
#checkresiduals(auto_default) 

# --- Exponential Smoothing ---
# Automatic Model Selection
ES_model <- ets(y_ts, model = "ZZZ") # automatic selection
summary(ES_model)
plot(ES_model)

# Simple exponential smoothing 
SES <- ets(y_ts, model = "ANN")
summary(SES) 

# Multiplicative Holt-Winters
MHW <- ets(y_ts, model = "MAM")

# Holt's linear model
HLM <- ets(y_ts, model = "AAN")
summary(HLM) 

# Additive Holt-Winters: AA
AHW <- ets(y_ts, model = "AAA")
summary(AHW) 
plot(AHW)

#  --- Different seasonal frequencies ---
ES_2_model <- ets(y_ts2, model = "ZZZ")
summary(ES_2_model)

