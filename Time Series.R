

##------------------------------------------------------------------------------------------

# TUTORIAL IN PROGRESS

##------------------------------------------------------------------------------------------

# please refer to documentation of functions you haven't seen earlier
# this will help you understand the arguments better
# feel free to drop questions on linkedin or in github

##------------------------------------------------------------------------------------------

# import packages
library(fpp2)
library(tidyverse)

##------------------------------------------------------------------------------------------

# filepath
file <- "F:/Data Science/R/Time Series/daily-total-female-births-CA.csv"

# load data as dataframe
df <- read.csv(file, header = T, na.strings = c("", "NA", "NULL"), 
               strip.white = T, stringsAsFactors = T)

##------------------------------------------------------------------------------------------

## convert dataframe to time series
# we can use 'ts' or 'msts' function to convert data into time series
# data is captured daily: weekly and annual seasonal patterns are possible
# since we have just one year data, we focus on weekly seasonality
# we don't have two complete years of data to consider annual seasonality as well

# 'msts' function can be used to specify multiple seasonal patterns
ts_data <- msts(df[["births"]], seasonal.periods = c(7))

# 'ts' function can be used to only specify one type of seasonal pattern
ts_data <- ts(df[["births"]], frequency = 7)

## time series plot
# x-axis shows weeks as we have specified weekly seasonal pattern
autoplot(ts_data, series = "Births", ylab = "", xlab = "Weeks") + 
  theme(panel.background = element_rect(fill = "white", colour = "black")) + 
  scale_color_manual(values = rainbow(1))

##------------------------------------------------------------------------------------------

## time series decomposition
# time series has 3 components: trend-cycle, seasonal, white noise (remainder)
# 'mstl' function is used to decompose a time series
# 'mstl' is an extension of 'stl' to handle multiple seasonalities
mstl(ts_data, robust = T)%>%
  autoplot(col = T) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) + 
  scale_color_manual(values = rainbow(4))

##------------------------------------------------------------------------------------------

## extract time series components

# trend (t), seasonal (s), remainder(r)
# the respective function accepts a decomposed ts object
# decomposed ts object can be created usign different functions
t <- trendcycle(mstl(ts_data, robust = T))
s <- seasonal(mstl(ts_data, robust = T))
r <- remainder(mstl(ts_data, robust = T))

# plot
# choose t, s or r depending upon the plot
# change ylab depending upon the component
# change color if desired
# for plots ranging from n1 to n2 weeks, use the following:
# window(ts, start = n1, end = n2)
autoplot(window(s, start = 1, end = 16), col = "blue", ylab = "Seasonal", xlab = "Weeks") + 
  theme(panel.background = element_rect(fill = "white", colour = "black"))

##------------------------------------------------------------------------------------------

## autocorrelations
# acf = autocorrelation function
ggAcf(ts_data, lag.max = 70) + 
  labs(title = "CA Female Birth Data") +
  theme(panel.background = element_rect(fill = "white", colour = "black"))

# pacf = partial autocorrelation function
ggPacf(ts_data, lag.max = 70) +
  labs(title = "CA Female Birth Data") +
  theme(panel.background = element_rect(fill = "white", colour = "black"))

##------------------------------------------------------------------------------------------

# noise has no autocorrelation
# in real-world data, autocorrelation would be close to zero and not exactly zero
# exactly zero autocorrelation is impractical as even noise has some variation
ggAcf(r, lag.max = 70) + 
  labs(title = "Noise") +
  theme(panel.background = element_rect(fill = "white", colour = "black"))

##------------------------------------------------------------------------------------------

## train-test split

# specify fraction of training data (p)
# compute number of training set observations
p <- 0.8
train_length <- ceiling(p*length(ts_data))

# first 80% observations in training data
# last 20% observations in test data
ts_train <- head(ts_data, train_length)
ts_test <- tail(ts_data, (length(ts_data) - train_length))

##------------------------------------------------------------------------------------------

## simple forecasting methods

# set forecasting horizon (h) 
h <- length(ts_test)

# naive = naive forecast
# rwf = random walk forecast (same as naive: use one of them) 
# meanf = mean forecast
# snaive = seasonal naive forecast
# rwf with drift

# forecasts on training data
fcast_mean <- meanf(ts_train, h = h) # mean forecast
fcast_rwf <- rwf(ts_train, h = h) # random walk forecast
fcast_snaive <- snaive(ts_train, h = h) # seasonal naive forecast
fcast_rwfd <- rwf(ts_train, h = h, drift = T) # random walk forecast with drift

##------------------------------------------------------------------------------------------

## time series regression

# regression using predictors that are inherently present
# we can use either trend, season or both if we are considering only these two
fit_reg <- tslm(formula = ts_train ~ season, data = ts_train)
summary(fit_reg)

# plot of fitted values
autoplot(ts_train, series = "Train Data") + 
  autolayer(fit_reg$fitted.values, series = "Fitted Values") + 
  labs(title = "Train Data vs Fitted Values", x = "Weeks", y = "Female Birth") + 
  theme(panel.background = element_rect(fill = "white", colour = "black")) + 
  scale_color_manual(values = rainbow(2))

# forecast using ts regression
fcast_reg <- forecast(fit_reg, h = h)

##------------------------------------------------------------------------------------------

## exponential smoothing

# data has both trend and seasonal components
# holt-winter's seasonal method to be used
# we can test with both linear and damped trends
# we can test with both additive and multiplicative methods

# linear trend, additive seasonality
fcast_hws_al <- hw(ts_train, seasonal = "additive", h = h)
# linear trend, multiplicative seasonality
fcast_hws_ml <- hw(ts_train, seasonal = "multiplicative", h = h)

# damped trend, additive seasonality
fcast_hws_ad <- hw(ts_train, seasonal = "additive", damped = T, h = h)
# damped trend, multiplicative seasonality
fcast_hws_md <- hw(ts_train, seasonal = "multiplicative", damped = T, h =h)

##------------------------------------------------------------------------------------------

# forecast plots
autoplot(tail(ts_train, 10), series = "Train Data") + 
  autolayer(fcast_hws_al, series = "HW Seasonal (Linear, Additive)", PI = F) + 
  autolayer(fcast_hws_ml, series = "HW Seasonal (Linear, Multiplicative)", PI = F) + 
  autolayer(fcast_hws_ad, series = "HW Seasonal (Damped, Additive)", PI = F) + 
  autolayer(fcast_hws_md, series = "HW Seasonal (Damped, Multiplicative)", PI = F) + 
  labs(title = "Forecasts", x = "Weeks", y = "Female Birth") + 
  theme(panel.background = element_rect(fill = "white", colour = "black")) + 
  scale_color_manual(values = rainbow(5))

# comparison of forecasts with test data
autoplot(ts_test, series = "Test Data") + 
  autolayer(fcast_hws_al, series = "HW Seasonal (Linear, Additive)", PI = F) + 
  autolayer(fcast_hws_ml, series = "HW Seasonal (Linear, Multiplicative)", PI = F) + 
  autolayer(fcast_hws_ad, series = "HW Seasonal (Damped, Additive)", PI = F) + 
  autolayer(fcast_hws_md, series = "HW Seasonal (Damped, Multiplicative)", PI = F) + 
  labs(title = "Forecasts vs Test Data", x = "Weeks", y = "Female Birth") + 
  theme(panel.background = element_rect(fill = "white", colour = "black")) + 
  scale_color_manual(values = rainbow(5))

##------------------------------------------------------------------------------------------

## prediction intervals

# select forecast to be assessed
fcast <- fcast_hws_al

# for normally distributed residuals with constant variance
lower_pi <- fcast$lower; head(lower_pi)
upper_pi <- fcast$upper; head(upper_pi)

# to compute bootstrapped prediction intervals, do the following:
# while fitting the model add the argument 'bootstrap = T'
# for instance: fcast_rwf = rwf(ts_train, h = h, bootstrap = T)

##------------------------------------------------------------------------------------------

# accuracy of forecasts (RMSE, MAE, MAPE)
# select forecast to be assessed
fcast <- fcast_hws_ad
accuracy(f = fcast, x = ts_test)[, c("RMSE", "MAE", "MAPE")]

##------------------------------------------------------------------------------------------

## residual analysis

# select forecast to be assessed
fcast <- fcast_reg

# time plot, acf and histogram
checkresiduals(fcast)

# mean forecast
mean(fcast$mean)

# mean of residuals
mean(fcast$residuals, na.rm = T)

##------------------------------------------------------------------------------------------

