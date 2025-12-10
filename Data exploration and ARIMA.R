rm(list=ls())
setwd("/Users/laura/Downloads/Master/Time Series/Case Study")
library(dplyr)
library(lubridate)
library(tseries)
library(forecast)
library(bootUR)

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
plot(diff2_y, type='l', main="Twice differenced demand", ylab="diff2_Demand", xlab="Time")
adf_d2_y <- adf(diff2_y, deterministics = "intercept")  # Series in first difference doesn't have a trend
print(adf_d2_y)

stat_d2_y <- adf_d2_y$statistic
p_d2_y <- adf_d2_y$p.value

cat("ADF on d2(y): test-statistic =", stat_d2_y, " p-value =", p_d2_y, "\n")
# Reject H0: so GDP I(2) is stationary.

# Test H0: I(1) unit root vs. Ha: stationary
diff_y <- diff(y)
plot(diff_y, type='l', main="Once differenced demand", ylab="diff1_Demand", xlab="Time")
adf_d1_y <- adf(diff_y, deterministics = "intercept")  # Series in first difference doesn't have a trend
print(adf_d1_y)

stat_d1_y <- adf_d1_y$statistic
p_d1_y <- adf_d1_y$p.value

cat("ADF on d(y): test-statistic =", stat_d1_y, " p-value =", p_d1_y, "\n")
# Reject H0: so GDP I(1) is stationary.

# Test H0: I(0) stochastic trend vs. Ha: deterministic trend
plot(y, type='l', main="Raw demand", ylab="Demand", xlab="Time")
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

# ---------------------------
#  ADF + KPSS: 2nd differences
# ---------------------------
diff2_y <- diff(y, differences = 2)
plot(diff2_y, type='l', main="Twice differenced demand")

adf_d2 <- adf(diff2_y, deterministics="intercept")
kpss_d2 <- kpss.test(diff2_y, null = "Level")  # Level-stationary test

cat("\n=== 2ND DIFFERENCE TESTS ===\n")
cat("ADF statistic:", adf_d2$statistic, "p-value:", adf_d2$p.value, "\n")
print(kpss_d2)


# ---------------------------
#  ADF + KPSS: 1st differences
# ---------------------------
diff1_y <- diff(y)
plot(diff1_y, type='l', main="Once differenced demand")

adf_d1 <- adf(diff1_y, deterministics="intercept")
kpss_d1 <- kpss.test(diff1_y, null="Level")

cat("\n=== 1ST DIFFERENCE TESTS ===\n")
cat("ADF statistic:", adf_d1$statistic, "p-value:", adf_d1$p.value, "\n")
print(kpss_d1)


# ---------------------------
#  ADF + KPSS: raw series
# ---------------------------
plot(y, type='l', main="Raw demand")
x <- 1:length(y)
fit <- lm(y ~ x)
abline(fit, col="red", lwd=2)

adf_raw <- adf(y, deterministics="trend")
kpss_raw <- kpss.test(y, null="Trend")  # Trend-stationary null

cat("\n=== RAW SERIES TESTS ===\n")
cat("ADF statistic:", adf_raw$statistic, "p-value:", adf_raw$p.value, "\n")
print(kpss_raw)

# --- Arima modelling ---

# correlogram to derive model

par(mfrow = c(1,1))

acf(y, main = "ACF of Demand", lag.max = 200)
pacf(y, main = "PACF of Demand", lag.max = 200)

acf(diff(y), main = "diff1 ACF of Demand", lag.max = 200)
pacf(diff(y), main = "diff1 PACF of Demand", lag.max = 200)

# Fit all ARIMA models (non-seasonal)

# Model 1: ARIMA(1,0,1)
m1 <- arima(y, order = c(1,0,1), method = "ML")

# Model 2: ARIMA(2,0,1)
m2 <- arima(y, order = c(2,0,1), method = "ML")

# Model 3: ARIMA(2,0,2)
m3 <- arima(y, order = c(2,0,2), method = "ML")

# Additional models including d = 1 to compare:

# ARIMA(2,1,2)
m4 <- arima(y, order = c(2,1,2), method = "ML")

# ARIMA(2,1,3)
m5 <- arima(y, order = c(2,1,3), method = "ML")

# ARIMA(3,1,3)
m6 <- arima(y, order = c(3,1,3), method = "ML")

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
auto_nonseasonal <- auto.arima(y, seasonal = FALSE, 
                               ic = "aicc", 
                               stepwise = FALSE, 
                               approximation = FALSE)
summary(auto_nonseasonal)
checkresiduals(auto_nonseasonal)

# --- More exhaustive search (slower) ---
auto_exhaustive <- auto.arima(y_ts, seasonal = TRUE,
                              stepwise = FALSE,        # disable fast step-wise
                              approximation = FALSE,   # disable approximation
                              max.p = 5, max.q = 5,
                              max.P = 2, max.Q = 2,
                              ic = "aicc",
                              trace = TRUE)            # shows search steps
summary(auto_exhaustive)
checkresiduals(auto_exhaustive)



