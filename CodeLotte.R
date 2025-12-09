rm(list=ls())
setwd("C:/Users/lotte/OneDrive/Documenten/Maastricht University/Year 4/Period 2/Time Series Econometrics")

library(dplyr)
library(lubridate)
library(tseries)
library(urca)
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

plot(demand$datetime,demand$demand, type='l', main="Demand Citi Bike Jan-May", ylab="Demand", xlab="Time")
plot(diff(demand$demand), type = "l", main = "Differenced Demand Jan-May", ylab = "Demand", xlab = "Time")

training <- citibikedata%>%
  filter(between(month, 1, 4))

y <- training$demand

# --- ADF test on raw series ---
# Test H0: I(2) unit root vs. Ha: stationary
diff2_y <- diff(y, differences = 2)
plot(diff2_y)
adf_d2_y <- adf(diff2_y, deterministics = "intercept")  # Series in first difference doesn't have a trend
print(adf_d2_y)

stat_d2_y <- adf_d2_y$statistic
p_d2_y <- adf_d2_y$p.value

cat("ADF on d2(y): test-statistic =", stat_d2_y, " p-value =", p_d2_y, "\n")
# Reject H0: so demand I(2) is stationary.

# Test H0: I(1) unit root vs. Ha: stationary
diff_y <- diff(y)
plot(diff_y)
adf_d1_y <- adf(diff_y, deterministics = "intercept")  # Series in first difference doesn't have a trend
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


# --- Data-driven but force non-seasonal search (to compare) ---
auto_nonseasonal <- auto.arima(y_ts, seasonal = FALSE, 
                               ic = "aicc", 
                               stepwise = FALSE, 
                               approximation = FALSE)
summary(auto_nonseasonal)
checkresiduals(auto_nonseasonal)

# --- SEASONALITY ---
# Take seasonal differences, looks stationary so no need to take further first difference.
plot(diff(demand$demand, lag =24), type = "l", main = "Seasonal Differenced Demand Jan-May", ylab = "Demand", xlab = "Time")

y_season <- diff(y, lag = 24) # daily seasonality

# Test H0: I(1) unit root vs. Ha: stationary
# diff_ys <- diff(y_season)
#plot(diff_ys)
#adf_d1_ys <- adf(diff_ys, deterministics = "intercept")  # seasonally differenced series in first difference doesn't have a trend
#print(adf_d1_ys)

#stat_d1_ys <- adf_d1_ys$statistic
#p_d1_ys <- adf_d1_ys$p.value

#cat("ADF on ds(y): test-statistic =", stat_d1_ys, " p-value =", p_d1_ys, "\n")
# Reject H0: so seasonally differenced demand I(1) is stationary.

# Test H0: I(0) stochastic trend vs. Ha: deterministic trend
#plot(y_season)

#adf_d0_ys <- adf(y_season, deterministics = "intercept") # seasonally differenced series "in levels" doesn't have a trend
#print(adf_d0_ys)

#stat_d0_ys <- adf_d0_ys$statistic
#p_d0_ys <- adf_d0_ys$p.value

#cat("ADF on sy: test-statistic =", stat_d0_ys, " p-value =", p_d0_ys, "\n")
# Reject H0: deterministic trend (stationary) => No differencing needed

par(mfrow = c(1,1))

acf(y_season, main = "ACF of Seasonal Demand", lag.max = 75)
abline(v = seq(24, 75, by = 24), col = "red", lty = 2)  # highlight seasonal lags
# No clear significant spikes at seasonal lags, but roughly decreasing (exponential decrease)
pacf(y_season, main = "PACF of Seasonal Demand", lag.max = 75)
abline(v = seq(24, 75, by = 24), col = "red", lty = 2)  # highlight seasonal lags
# Significant spikes at seasonal lags 24, 48, 72, ..., 168 
# Take at most 3 seasonal lags to not over complicate the model + seasonal differencing => SARIMA(3,1,0)[24]
# It has 2 significant spikes at the start which suggests a non-seasonal AR(2): ARIMA(2,0,0)
# Removing seasonality by seasonal differencing has made the time series stationary.We can briefly check this by taking a look at the residuals from the SARIMA model
seasonal_model <- arima(y, seasonal = list(order = c(3,1,0), period = 24))
non_seasonal <- seasonal_model$residuals
plot(non_seasonal) # Stationary, so no further differencing is indeed necessary.

acf(non_seasonal, main = "ACF of Non-Seasonal part of Demand")
pacf(non_seasonal, main = "PACF of Non-Seasonal part of Demand") # Indeed same model fits best ARIMA(3,0,0)
par(mfrow = c(1,1))

# Fit different SARIMA models: Note it cannot be solved by maximum likelihood
#ARIMA(2,0,0)(3,1,0)[24]
m1_season <- arima(y, order = c(2,0,0), seasonal = list(order = c(3,1,0), period = 24))

m2_season <- arima(y, order = c(2,0,0), seasonal = list(order = c(2,1,0), period = 24))

m3_season <- arima(y, order = c(2,0,0), seasonal = list(order = c(4,1,0), period = 24))

AIC(m1_season, m2_season, m3_season)
BIC(m1_season, m2_season, m3_season)
# Model 3 has lowest IC score but barely outperforms m1 so not worth the extra parameter.

# --- Basic automatic search (fast, default) ---
auto_default <- auto.arima(y_ts, seasonal = TRUE)   # automatic seasonal detection
summary(auto_default)
checkresiduals(auto_default)                     # residual diagnostics
# Suggest ARIMA(2,0,0)(2,1,0)[24] so one of the models we've tried. SO approximation and fast step-wise skip over better fit models of higher order we've tried manually.

# --- More exhaustive search (much slower with seasonal components) ---
auto_exhaustive <- auto.arima(y_ts, seasonal = TRUE,
stepwise = FALSE,        # disable fast step-wise
approximation = FALSE,   # disable approximation
max.p = 4, max.q = 4,
max.P = 2, max.Q = 2,
ic = "aicc",
trace = TRUE)            # shows search steps
summary(auto_exhaustive)
checkresiduals(auto_exhaustive)

# --- Different seasonal frequencies ---
y_ts <- ts(y, frequency = 24)    # daily
y_ts2 <- ts(y, frequency = 24*7)    # weekly
y_ts3 <- ts(y, frequency = 24*7*30)   # monthly

auto_default <- auto.arima(y_ts2, seasonal = TRUE)   # automatic seasonal detection
summary(auto_default)
checkresiduals(auto_default) 
# Suggested model: ARIMA(2,0,1)(0,1,0)[168] with drift

auto_default <- auto.arima(y_ts3, seasonal = TRUE)   # automatic seasonal detection
summary(auto_default)
checkresiduals(auto_default) 
# Suggested model: ARIMA(2,1,0) => no monthly seasonality detected

# --- Exponential Smoothing ---
# Additive: Seasonality adds or subtracts a fixed amount; suitable for data with constant seasonal variance.
# Multiplicative: Seasonality multiplies the level/trend (e.g., 10% higher); suitable for data where seasonal swings grow with the series. 
# In essence, if your seasonal dips and peaks look like parallel waves on a graph, the additive model is a good fit; if the waves get wider as the data grows, try the multiplicative model


# Try exponential smoothing with daily seasonality cycle
ES_model <- ets(y_ts, model = "ZZZ") # automatic selection
summary(ES_model)
plot(ES_model)
accuracy(ES_model)
# A = additive, M = multiplicative, Z = automatically selected, N = none
# First = error type, second = trend type, third = season type
# simple exponential smoothing: ANN
SES <- ets(y_ts, model = "ANN")
summary(SES)  # worse than automatically selected model
# Multiplicative Holt-Winters: MAM
MHW <- ets(y_ts, model = "MAM")
# Warning: Innapropriate model for data with negative or zero values
# Holt's linear model: AN
HLM <- ets(y_ts, model = "AAN")
summary(HLM) #  much worse
# this is the automatically selected model
# Additive Holt-Winters: AA
AHW <- ets(y_ts, model = "AAA")
summary(AHW)  # Almost identical as ANA, just very small error of 15-ish
plot(AHW)
# allow.multiplicative trend = FALSE as we only consider holt-Winter model and simple exponential smoothing
# So, (A,N,A) model which corresponds to a additive winter's model without a trend component


# Try exponential smoothing with weekly seasonality cycle
ES_2_model <- ets(y_ts2, model = "ZZZ")
summary(ES_2_model)
# Warning message:
# In ets(y_ts2, model = "ZZZ") :
 # I can't handle data with frequency greater than 24. Seasonality will be ignored. Try stlf() if you need seasonal forecasts.
# so monthly seasonality will also not work
