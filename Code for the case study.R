rm(list=ls())
setwd("C:/one drive/Počítač/UNI/Master studies/Timeseries Econometrics/Case study")
library(dplyr)
library(lubridate)
library(ggplot2)
library(timeSeries)
library(forecast)
library(uroot)
library(MCS)
library(bootUR)
library(tidyr)
library(patchwork)

load("citibike.RData")
citibikedata<-as.data.frame(citibike)

training <- citibikedata%>%
  dplyr::filter(month <5)%>%
  pull(demand)

outofsample <- citibikedata%>%
  dplyr::filter(month == 5)%>%
  pull(demand)

train_ts_daily <- ts(training, frequency = 24)
train_ts_weekly <- ts(training, frequency = 168)
outofsample_ts <- ts(outofsample, frequency = 24)

fulldata <- citibikedata%>%
  pull(demand)
fulldata_weakly <- ts(fulldata, frequency = 168)
fulldata_daily <- ts(fulldata, frequency = 24)

#######################################################################################################################################################################################################
########################################## In sample comparison #######################################################################################################################################
#######################################################################################################################################################################################################
# Here we take the specifications of ARIMA, SARIMA and ETS that we found earlier to be the best in every individual model group 
################################ ARIMA
ARIMA <- auto.arima(
  train_ts,
  seasonal = FALSE,  # <- do not consider SARIMA terms
  ic = "aicc",
  stepwise = FALSE,
  approximation = FALSE)
summary(ARIMA)
################################ SARIMA
SARIMA_daily<- Arima(train_ts_daily, order= c(1, 0, 0), seasonal = list(order = c(2, 1, 2), period = 24))
summary(SARIMA_daily)  

SARIMA_weekly <- Arima(train_ts_daily, order= c(2, 0, 1), seasonal = list(order = c(0, 1, 0), period = 168))
summary(SARIMA_weekly)  
############################### ETS
ets_fit<-ets(train_ts_daily)
summary(ets_fit)
# ets cannot handle weekly seasonality so its omitted here

#######################################################################################################################################################################################################
# Information Criteria for all the models
#AIC
ARIMA$aic
SARIMA_daily$aic
SARIMA_weekly$aic # best fit
ets_fit$aic
#AICC
ARIMA$aicc
SARIMA_daily$aicc
SARIMA_weekly$aicc # best fit
ets_fit$aicc
# BIC
ARIMA$bic
SARIMA_daily$bic
SARIMA_weekly$bic # best fit
ets_fit$bic

# In-sample accuracy - IN sample errors RMSE, MAE,MASE
acc_ARIMA         <- accuracy(ARIMA)
acc_SARIMA_daily  <- accuracy(SARIMA_daily)
acc_SARIMA_weekly <- accuracy(SARIMA_weekly)
acc_ets           <- accuracy(ets_fit)

# Extract RMSE / MAE / MASE for the training set just to have it easily readable
acc_ARIMA["Training set",  c("RMSE","MAE","MASE")]
acc_SARIMA_daily["Training set",  c("RMSE","MAE","MASE")]
acc_SARIMA_weekly["Training set", c("RMSE","MAE","MASE")] # this is the best fit
acc_ets["Training set",           c("RMSE","MAE","MASE")]


# Check the residual diagnostics
checkresiduals(ARIMA)
checkresiduals(SARIMA_daily)
checkresiduals(SARIMA_weekly) # looks the best
checkresiduals(ets_fit)
# in general no model perfectly captures the data - likely because of multiple seasonality 

lb_arima   <- checkresiduals(ARIMA,         plot = FALSE)
lb_sar24   <- checkresiduals(SARIMA_daily,  plot = FALSE)
lb_sar168  <- checkresiduals(SARIMA_weekly, plot = FALSE)
lb_ets     <- checkresiduals(ets_fit,       plot = FALSE)

# Build summary table
lb_table <- bind_rows(
  data.frame(
    Model     = "ARIMA",
    Q_stat    = unname(lb_arima$statistic),
    df        = unname(lb_arima$parameter),
    p_value   = lb_arima$p.value
  ),
  data.frame(
    Model     = "SARIMA (24)",
    Q_stat    = unname(lb_sar24$statistic),
    df        = unname(lb_sar24$parameter),
    p_value   = lb_sar24$p.value
  ),
  data.frame(
    Model     = "SARIMA (168)",
    Q_stat    = unname(lb_sar168$statistic),
    df        = unname(lb_sar168$parameter),
    p_value   = lb_sar168$p.value
  ),
  data.frame(
    Model     = "ETS",
    Q_stat    = unname(lb_ets$statistic),
    df        = unname(lb_ets$parameter),
    p_value   = lb_ets$p.value
  )
) %>%
  mutate(
    Q_stat  = round(Q_stat, 2),
    p_value = signif(p_value, 3)
  )

lb_table

# Visualize the data against the model
df_arima <- tibble(
  time   = as.numeric(time(train_ts_daily)),
  actual = as.numeric(train_ts_daily),
  fitted = as.numeric(fitted(ARIMA))
)

ggplot(df_arima, aes(x = time)) +
  geom_line(aes(y = actual, colour = "Actual data"), linewidth = 1) +
  geom_line(aes(y = fitted, colour = "ARIMA fit"), linetype = "dashed", linewidth =0.7) +
  scale_colour_manual(
    name   = NULL,
    values = c("Actual data" = "black", "ARIMA fit" = "red")
  ) +
  labs(
    title = "ARIMA: Actual vs Fitted",
    x = "Time",
    y = "Demand"
  ) +
  theme_minimal()

df_sarima_daily <- tibble(
  time   = as.numeric(time(train_ts_daily)),
  actual = as.numeric(train_ts_daily),
  fitted = as.numeric(fitted(SARIMA_daily))
)

ggplot(df_sarima_daily, aes(x = time)) +
  geom_line(aes(y = actual, colour = "Actual data"), linewidth = 1) +
  geom_line(aes(y = fitted, colour = "SARIMA (daily) fit"),linetype = "dashed", linewidth = 0.7) +
  scale_colour_manual(
    name   = NULL,
    values = c("Actual data" = "black", "SARIMA (daily) fit" = "blue")
  ) +
  labs(
    title = "SARIMA (daily): Actual vs Fitted",
    x = "Time",
    y = "Demand"
  ) +
  theme_minimal()


df_sarima_weekly <- tibble(
  time   = as.numeric(time(train_ts_weekly)),
  actual = as.numeric(train_ts_weekly),
  fitted = as.numeric(fitted(SARIMA_weekly))
)

ggplot(df_sarima_weekly, aes(x = time)) +
  geom_line(aes(y = actual, colour = "Actual data"), linewidth = 1) +
  geom_line(aes(y = fitted, colour = "SARIMA (weekly) fit"), linetype = "dashed",linewidth = 0.7) +
  scale_colour_manual(
    name   = NULL,
    values = c("Actual data" = "black", "SARIMA (weekly) fit" = "blue")
  ) +
  labs(
    title = "SARIMA (weekly): Actual vs Fitted",
    x = "Time",
    y = "Demand"
  ) +
  theme_minimal()


df_ets <- tibble(
  time   = as.numeric(time(train_ts_daily)),
  actual = as.numeric(train_ts_daily),
  fitted = as.numeric(fitted(ets_fit))
)

ggplot(df_ets, aes(x = time)) +
  geom_line(aes(y = actual, colour = "Actual data"), linewidth = 1) +
  geom_line(aes(y = fitted, colour = "ETS fit"), linetype = "dashed",linewidth = 0.7) +
  scale_colour_manual(
    name   = NULL,
    values = c("Actual data" = "black", "ETS fit" = "green")
  ) +
  labs(
    title = "ETS: Actual vs Fitted",
    x = "Time",
    y = "Demand"
  ) +
  theme_minimal()




#######################################################################################################################################################################################################
################################    Forecasting -  Rolling and Expanding window  ######################################################################################################################
#######################################################################################################################################################################################################
# Forecasting with a rolling window 
n_test <- length(outofsample_ts)
sequence <- seq(0, n_test-24, by = 24)
n_train <- length(train_ts_daily)

arima_data_daily_rolling <-c ()
sarima_data_daily_rolling <- c()
ets_data_daily_rolling <- c()
sarima_data_weekly_rolling <- c()

for(t in sequence){
  currentdata_daily <- ts(fulldata[(t+1):(n_train+t)], frequency = 24)
  currentdata_weekly <- ts(fulldata[(t+1):(n_train+t)], frequency = 168)
  
  arima <- Arima(currentdata_daily, order = c(2,1,0))
  sarima_daily <- Arima(currentdata_daily, order= c(2, 0, 1), seasonal = list(order = c(2, 1, 0), period = 24))
  sarima_weekly <- Arima(currentdata_weekly, order= c(2, 0, 1), seasonal = list(order = c(0, 1, 0), period = 168))
  ets <- ets(currentdata_daily)
  
  arima_data_daily_rolling <- c(arima_data_daily_rolling,forecast(arima, h=24)$mean)
  sarima_data_daily_rolling <- c(sarima_data_daily_rolling,forecast(sarima_daily, h=24)$mean)
  ets_data_daily_rolling <- c(ets_data_daily_rolling,forecast(ets, h=24)$mean)
  sarima_data_weekly_rolling <- c(sarima_data_weekly_rolling,forecast(sarima_weekly, h=24)$mean)
}

# Forecasting with an Expanding window
arima_data_daily_expanding <- c()
sarima_data_daily_expanding <- c()
ets_data_daily_expanding <- c()
sarima_data_weekly_expanding <- c()

for(t in sequence){
  currentdata_daily <- ts(fulldata[1:(n_train + t)], frequency = 24)
  currentdata_weekly <- ts(fulldata[1:(n_train + t)], frequency = 168)
  
  arima <- Arima(currentdata_daily, order = c(2,1,0))
  sarima_daily <- Arima(currentdata_daily, order= c(2, 0, 1), seasonal = list(order = c(2, 1, 0), period = 24))
  sarima_weekly <- Arima(currentdata_weekly, order= c(2, 0, 1), seasonal = list(order = c(0, 1, 0), period = 168))
  ets <- ets(currentdata_daily)
  
  arima_data_daily_expanding <- c(arima_data_daily_expanding,forecast(arima, h=24)$mean)
  sarima_data_daily_expanding <- c(sarima_data_daily_expanding,forecast(sarima_daily, h=24)$mean)
  ets_data_daily_expanding <- c(ets_data_daily_expanding,forecast(ets, h=24)$mean)
  sarima_data_weekly_expanding <- c(sarima_data_weekly_expanding,forecast(sarima_weekly, h=24)$mean)
}

#######################################################################################################################################################################################################
################################# Assessing the forecasts -  Out of sample comparison  ################################################################################################################
#######################################################################################################################################################################################################
#MASE - the new metric
denom <- mean(abs(diff(train_ts_daily)))
# Rolling window:
arima_daily_rolling_errors<- outofsample_ts -arima_data_daily_rolling 
sarima_daily_rolling_errors <- outofsample_ts -sarima_data_daily_rolling 
ets_daily_rolling_errors <- outofsample_ts -ets_data_daily_rolling 
sarima_weekly_rolling_errors <- outofsample_ts -sarima_data_weekly_rolling 

MASE_arima_rolling <- mean(abs(arima_daily_rolling_errors)) / denom
MASE_sarima_rolling <- mean(abs(sarima_daily_rolling_errors)) / denom
MASE_sarima_rolling_weekly <- mean(abs(sarima_weekly_rolling_errors)) / denom
MASE_ets_rolling <- mean(abs(ets_daily_rolling_errors)) / denom

accuracy(arima_data_daily_rolling,outofsample_ts)
accuracy(sarima_data_daily_rolling,outofsample_ts)
accuracy(ets_data_daily_rolling,outofsample_ts)
accuracy(sarima_data_weekly_rolling,outofsample_ts)

# Expanding window:
arima_daily_expanding_errors<-  outofsample_ts - arima_data_daily_expanding 
sarima_daily_expanding_errors <- outofsample_ts - sarima_data_daily_expanding
ets_daily_expanding_errors <- outofsample_ts - ets_data_daily_expanding 
sarima_weekly_expanding_errors <- outofsample_ts - sarima_data_weekly_expanding 

MASE_arima_expanding <- mean(abs(arima_daily_expanding_errors)) / denom
MASE_sarima_expanding <- mean(abs(sarima_daily_expanding_errors)) / denom
MASE_sarima_expanding_weekly <- mean(abs(sarima_weekly_expanding_errors)) / denom
MASE_ets_expanding <- mean(abs(ets_daily_expanding_errors)) / denom

accuracy(arima_data_daily_expanding,outofsample_ts)
accuracy(sarima_data_daily_expanding,outofsample_ts)
accuracy(ets_data_daily_expanding,outofsample_ts)
accuracy(sarima_data_weekly_expanding,outofsample_ts)

#######################################################################################################################################################################################################
################################################################# Assymetric penalty ##################################################################################################################
#######################################################################################################################################################################################################

asymmetric_loss <- function(errors, under_penalty, over_penalty = 1) {
  penalties <- ifelse(errors < 0,
                      abs(errors) * under_penalty,   # “bad” side
                      abs(errors) * over_penalty)    # “less bad” side
  mean(penalties, na.rm = TRUE)
}

err_roll <- list(
  ARIMA   = arima_daily_rolling_errors,
  SARIMAW  = sarima_weekly_rolling_errors,  # or 
  SARIMA = sarima_daily_rolling_errors,
  ETS     = ets_daily_rolling_errors
)

err_exp <- list(
  ARIMA   = arima_daily_expanding_errors,
  SARIMAW  = sarima_weekly_expanding_errors,
  SARIMA = sarima_daily_expanding_errors,
  ETS     = ets_daily_expanding_errors
)

library(dplyr)
library(knitr)

make_asym_table <- function(err_roll, err_exp,
                            under_penalties = 2:5,
                            over_penalty = 1) {
  models <- names(err_roll)
  stopifnot(identical(models, names(err_exp)))
  
  rows <- lapply(under_penalties, function(u) {
    data.frame(
      Under_Penalty          = u,
      Model                  = models,
      Rolling_Asym_Loss      = sapply(err_roll, asymmetric_loss,
                                      under_penalty = u,
                                      over_penalty  = over_penalty),
      Expanding_Asym_Loss    = sapply(err_exp, asymmetric_loss,
                                      under_penalty = u,
                                      over_penalty  = over_penalty),
      row.names = NULL
    )
  })
  
  do.call(rbind, rows)
}

asym_tab <- make_asym_table(err_roll, err_exp, under_penalties = 2:5)
asym_tab

kable(
  asym_tab,
  format   = "latex",
  booktabs = TRUE,
  digits   = 3,
  caption  = "Asymmetric loss for different under–prediction penalties.",
  col.names = c("Under-Penalty", "Model",
                "Rolling Asymmetric Loss",
                "Expanding Asymmetric Loss")
)


#######################################################################################################################################################################################################
################################# Forecast accuracy metrics ###########################################################################################################################################
#######################################################################################################################################################################################################
# DM testing 
run_dm_matrix <- function(err_list, h = 24, power = 2) {
  models <- names(err_list)
  k <- length(err_list)
  
  DM_stat  <- matrix(NA_real_, nrow = k, ncol = k,
                     dimnames = list(models, models))
  DM_pval  <- DM_stat
  
  for (i in 1:(k - 1)) {
    for (j in (i + 1):k) {
      test <- dm.test(err_list[[i]], err_list[[j]],
                      h = h, power = power)
      DM_stat[i, j] <- test$statistic
      DM_pval[i, j] <- test$p.value
    }
  }
  
  list(stat = DM_stat, pval = DM_pval)
}

# ROLLING errors
err_roll <- list(
  ARIMA_daily   = arima_daily_rolling_errors,
  SARIMA_daily  = sarima_daily_rolling_errors,
  SARIMA_weekly = sarima_weekly_rolling_errors,
  ETS_daily     = ets_daily_rolling_errors
)

dm_roll_se <- run_dm_matrix(err_roll, h = 24, power = 2)
dm_roll_se$stat   # matrix of DM statistics
dm_roll_se$pval   # matrix of p-values

# EXPANDING errors
err_exp <- list(
  ARIMA_daily   = arima_daily_expanding_errors,
  SARIMA_daily  = sarima_daily_expanding_errors,
  SARIMA_weekly = sarima_weekly_expanding_errors,
  ETS_daily     = ets_daily_expanding_errors
)
dm_exp_se <- run_dm_matrix(err_exp, h = 24, power = 2)
dm_exp_se$stat   # matrix of DM statistics
dm_exp_se$pval   # matrix of p-values


# MCS test
L_roll <- cbind(
  ARIMA_daily   = err_roll$ARIMA_daily^2,
  SARIMA_daily  = err_roll$SARIMA_daily^2,
  SARIMA_weekly = err_roll$SARIMA_weekly^2,
  ETS_daily     = err_roll$ETS_daily^2
)

set.seed(123)
mcs_roll <- MCSprocedure(
  Loss      = L_roll,
  alpha     = 0.10,      # 90% model confidence set
  B         = 1000,      # bootstrap replications
  statistic = "Tmax"     # max-t statistic
)

summary(mcs_roll)

L_exp <- cbind(
  ARIMA_daily   = err_exp$ARIMA_daily^2,
  SARIMA_daily  = err_exp$SARIMA_daily^2,
  SARIMA_weekly = err_exp$SARIMA_weekly^2,
  ETS_daily     = err_exp$ETS_daily^2
)

set.seed(123)
mcs_exp <- MCSprocedure(
  Loss      = L_exp,
  alpha     = 0.10,
  B         = 1000,
  statistic = "Tmax"
)

summary(mcs_exp)
#  Nice overleaf output of the results of forecast accuracy

dm_to_df <- function(dm_list) {
  stat <- dm_list$stat
  pval <- dm_list$pval
  # model names
  models <- rownames(stat)
  # all unique pairs 
  pairs <- t(combn(models, 2))   
  df <- as.data.frame(pairs, stringsAsFactors = FALSE)
  colnames(df) <- c("Model1", "Model2")
  
  df %>%
    mutate(
      DM_statistic = mapply(function(m1, m2) stat[m1, m2], Model1, Model2),
      p_value      = mapply(function(m1, m2) pval[m1, m2], Model1, Model2),
      Sig = case_when(
        p_value < 0.01 ~ "***",
        p_value < 0.05 ~ "**",
        p_value < 0.10 ~ "*",
        TRUE ~ ""
      )
    ) %>%
    arrange(Model1, Model2)
}

# Rolling window DM table 
dm_roll_df <- dm_to_df(dm_roll_se)

# Expanding window DM table as data frame
dm_exp_df  <- dm_to_df(dm_exp_se)

kable(
  dm_roll_df,
  format   = "latex",
  booktabs = TRUE,
  digits   = 3,
  caption  = "Diebold--Mariano test for pairwise forecast comparisons (rolling window).",
  label    = "tab:dm_rolling"
)

kable(
  dm_exp_df,
  format   = "latex",
  booktabs = TRUE,
  digits   = 3,
  caption  = "Diebold--Mariano test for pairwise forecast comparisons (expanding window).",
  label    = "tab:dm_expanding"
)

mcs_roll_df <- tibble(
  Model    = c("ARIMA_daily", "SARIMA_daily", "SARIMA_weekly", "ETS_daily"),
  Included = c(FALSE,         FALSE,          TRUE,             FALSE)
) %>%
  mutate(Status = ifelse(Included, "In superior set", "Eliminated"))

kable(
  mcs_roll_df,
  format   = "latex",
  booktabs = TRUE,
  caption  = "Model confidence set (rolling window, $\\alpha = 0.10$).",
  label    = "tab:mcs_rolling"
)

mcs_exp_df <- tibble(
  Model    = c("ARIMA_daily", "SARIMA_daily", "SARIMA_weekly", "ETS_daily"),
  Included = c(FALSE,         FALSE,          TRUE,             FALSE)  
) %>%
  mutate(Status = ifelse(Included, "In superior set", "Eliminated"))

kable(
  mcs_exp_df,
  format   = "latex",
  booktabs = TRUE,
  caption  = "Model confidence set (expanding window, $\\alpha = 0.10$).",
  label    = "tab:mcs_expanding"
)


#######################################################################################################################################################################################################
################################# Additional summary plots and statistics for the presentation ########################################################################################################
#######################################################################################################################################################################################################
time_may <- citibikedata %>%
  dplyr::filter(month == 5) %>%
  pull(datetime)
my_cols <- c(
  "ARIMA"         = "brown",  # dark grey
  "ETS"           = "#55A868",  # green
  "SARIMA_daily"  = "#4C72B0",  # blue
  "SARIMA_weekly" = "black"   # purple
)
# Build data frame for rolling forecasts
df_roll <- tibble(
  time   = time_may,
  actual = as.numeric(outofsample_ts),
  ARIMA  = as.numeric(arima_data_daily_rolling),
  SARIMA_daily  = as.numeric(sarima_data_daily_rolling),
  SARIMA_weekly = as.numeric(sarima_data_weekly_rolling),
  ETS    = as.numeric(ets_data_daily_rolling)
)

df_roll <- df_roll %>%
  pivot_longer(
    cols = c(ARIMA, SARIMA_daily, SARIMA_weekly, ETS),
    names_to  = "model",
    values_to = "forecast"
  )

ggplot(df_roll, aes(x = time, y = forecast, color = model)) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = my_cols) +
  labs(
    title = "Rolling Window Forecasts (May 2023)",
    x = "Date",
    y = "Forecasted Demand"
  ) +
  theme_minimal()

# Expanding window
df_exp <- tibble(
  time   = time_may,
  actual = as.numeric(outofsample_ts),
  ARIMA  = as.numeric(arima_data_daily_expanding),
  SARIMA_daily  = as.numeric(sarima_data_daily_expanding),
  SARIMA_weekly = as.numeric(sarima_data_weekly_expanding),
  ETS    = as.numeric(ets_data_daily_expanding)
)

df_exp <- df_exp %>%
  pivot_longer(
    cols = c(ARIMA, SARIMA_daily, SARIMA_weekly, ETS),
    names_to  = "model",
    values_to = "forecast"
  )

ggplot(df_exp, aes(x = time, y = forecast, color = model)) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = my_cols) +
  labs(
    title = "Expanding Window Forecasts (May 2023)",
    x = "Date",
    y = "Forecasted Demand"
  ) +
  theme_minimal()

# Summary statistics 
df_roll <- tibble(
  time   = time_may,
  actual = as.numeric(outofsample_ts),
  ARIMA  = as.numeric(arima_data_daily_rolling),
  SARIMA_daily  = as.numeric(sarima_data_daily_rolling),
  SARIMA_weekly = as.numeric(sarima_data_weekly_rolling),
  ETS    = as.numeric(ets_data_daily_rolling)
)
df_exp <- tibble(
  time   = time_may,
  actual = as.numeric(outofsample_ts),
  ARIMA  = as.numeric(arima_data_daily_expanding),
  SARIMA_daily  = as.numeric(sarima_data_daily_expanding),
  SARIMA_weekly = as.numeric(sarima_data_weekly_expanding),
  ETS    = as.numeric(ets_data_daily_expanding)
)

df_roll_long <- df_roll |>
  tidyr::pivot_longer(
    cols = c(ARIMA, SARIMA_daily, SARIMA_weekly, ETS),
    names_to  = "model",
    values_to = "forecast"
  )

df_expand_long <- df_exp|>
  tidyr::pivot_longer(
    cols = c(ARIMA, SARIMA_daily, SARIMA_weekly, ETS),
    names_to  = "model",
    values_to = "forecast"
  )

summarise_forecasts_long <- function(df_long, method_label) {
  df_long |>
    group_by(Method = method_label, model) |>
    summarise(
      Mean = mean(forecast, na.rm = TRUE),
      SD   = sd(forecast,   na.rm = TRUE),
      Min  = min(forecast,  na.rm = TRUE),
      Max  = max(forecast,  na.rm = TRUE),
      .groups = "drop"
    )
}


tab_roll   <- summarise_forecasts_long(df_roll_long,   "Rolling")
tab_expand <- summarise_forecasts_long(df_expand_long, "Expanding")

summary_table <- bind_rows(tab_roll, tab_expand) |>
  arrange(Method, model) |>
  mutate(
    Mean = round(Mean, 2),
    SD   = round(SD,   2),
    Min  = round(Min,  2),
    Max  = round(Max,  2)
  )

kable(
  summary_table,
  format  = "latex",
  booktabs = TRUE,
  caption = "Summary statistics of forecasts by method and model"
)


