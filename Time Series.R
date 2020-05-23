

##------------------------------------------------------------------------------------------

# TUTORIAL (Statistical Methods for Forecasting)

##------------------------------------------------------------------------------------------

# please refer to documentation of functions you haven't seen earlier
# this will help you understand the arguments better
# feel free to drop questions on linkedin or in github

##------------------------------------------------------------------------------------------

# import packages
library(fpp2)
library(data.table)
library(tidyverse)

##------------------------------------------------------------------------------------------

# filepath
file <- "F:/Data Science/R/Time Series/daily-total-female-births-CA.csv"

# load data as dataframe
df <- fread(file)

# save data 
saveRDS(df, "ts_data.rds")

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

# forecasting horizon (h)
h <- 28 

# first 'length(ts) - h' observations in training data
# last 'h' observations in test data
ts_train <- head(ts_data, length(ts_data) - h)
ts_test <- tail(ts_data, h)

##------------------------------------------------------------------------------------------

## simple forecasting methods

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

## differencing
library(urca)

# unit root test to identify if differencing is required
# kpss test
# null-hypothesis: series is stationary
# if test-statistic > critical value, null-hypothesis is rejected
ur.kpss(ts_data, use.lag = 14)%>%
  summary()
# at 95% confidence, null-hypothesis is rejected (run the test)
# series needs differencing

# order of differencing required
ndiffs(ts_data)

# order of seasonal differencing required
nsdiffs(ts_data)
# although seasonal, why output is 0
# nsdiffs outputs > 0 if strength of seasonality > 0.64

# 1st order differencing
# 1st order = differencing is done once
# for seasonal differencing, set lag = seasonality
ts_diff <- diff(diff(ts_data, 7))

# Ljung-Box test
Box.test(ts_diff, lag = 14, type = "Ljung-Box")

# plot of differenced series
autoplot(ts_diff, series = "Differenced Series") + 
  labs(title = "", x = "Weeks", y = "") + 
  theme(panel.background = element_rect(fill = "white", colour = "black")) + 
  scale_color_manual(values = rainbow(1))

# differenced series decomposed
mstl(ts_diff, robust = T)%>%
  autoplot(col = T) +
  labs(x = "Weeks", title = "Differenced Data") +
  theme(panel.background = element_rect(fill = "white", colour = "black")) + 
  scale_color_manual(values = rainbow(4))
  

##------------------------------------------------------------------------------------------

## arima
# seasonal = T or F depending upon if you want to fit a seasonal model
# approximation = T if you want a good model, which may not be the best 
# approximation = T speeds up computation
# set approximation = F if you want the best model 
# stepwise = T for stepwise model selection to speed up the computation
# stepwise = F for extensive search 
# lambda = BoxCox.lambda() if data needs to be transformed before model fitting 
# if lambda is set, make biasadj = T to get the mean value of point forecasts
# if biasadj = F, we get median values of point forecasts
# if no constant term is required in arima, set allowdrift = F 

# model with no data transformation
fit_arima <- auto.arima(y = ts_train, 
                        seasonal = T, 
                        approximation = F,
                        stepwise = F,
                        lambda = BoxCox.lambda(ts_train),
                        biasadj = T,
                        allowdrift = F)
# model summary 
summary(fit_arima)

# forecasts with no data transformation
fcast_arima <- forecast(fit_arima, h = h)

# model with data transformation
fit_arima_tr <- auto.arima(y = ts_train,
                           seasonal = T,
                           approximation = F,
                           stepwise = F,
                           lambda = BoxCox.lambda(ts_train),
                           biasadj = T,
                           allowdrift = F)
# model summary
summary(fit_arima_tr)

# forecasts with data transformation
fcast_arima_tr <- forecast(fit_arima_tr, h = h)

##------------------------------------------------------------------------------------------

## regression with arima errors
# seasonal = T or F depending upon if you want to fit a seasonal model
# approximation = T if you want a good model, which may not be the best 
# approximation = T speeds up computation
# set approximation = F if you want the best model 
# stepwise = T for stepwise model selection to speed up the computation
# stepwise = F for extensive search 
# lambda = BoxCox.lambda() if data needs to be transformed before model fitting 
# if lambda is set, make biasadj = T to get the mean value of point forecasts
# if biasadj = F, we get median values of point forecasts
# if no constant term is required in arima, set allowdrift = F 
# set xreg = column of predictors
# ensure that names of columns in xreg are the same for both train and test data

# model with no data transformation
fit_ra <- auto.arima(y = ts_train,
                     seasonal = T,
                     approximation = F,
                     stepwise = F, 
                     allowdrift = F,
                     xreg = cbind(trend = trendcycle(mstl(ts_train)), 
                                  season = seasonal(mstl(ts_train))))
# model summary 
summary(fit_ra)

# forecasts with no data transformation
fcast_ra <- forecast(fit_ra, xreg = cbind(trend = trendcycle(mstl(ts_test)), 
                                          season = seasonal(mstl(ts_test))), h = h)

# model with data transformation
fit_ra_tr <- auto.arima(y = ts_train,
                        seasonal = T,
                        approximation = F,
                        stepwise = F,
                        lambda = BoxCox.lambda(ts_train),
                        biasadj = T,
                        allowdrift = F,
                        xreg = cbind(trend = trendcycle(mstl(ts_train)), 
                                     season = seasonal(mstl(ts_train))))
# model summary
summary(fit_ra_tr)

# forecasts with data transformation
fcast_ra_tr <- forecast(fit_ra_tr, xreg = cbind(trend = trendcycle(mstl(ts_test)), 
                                                season = seasonal(mstl(ts_test))), h = h)

##------------------------------------------------------------------------------------------

## dynamic harmonic regression (dhr)
# fourier terms are used as predictors to capture seasonality

# empty list to store dhr models and forecasts
fit_dhr <- list()
fcast_dhr <- list()

# loop to fit different dhr models
# K <= (seasonal period)/2; so, i ranges from 1 to 3
for(i in 1:3){
  fit_dhr[[i]] <- auto.arima(y = ts_train,
                        seasonal = T,
                        approximation = F,
                        stepwise = F, 
                        allowdrift = F,
                        xreg = cbind(fr = fourier(ts_train, K = i),
                                     trend = trendcycle(mstl(ts_train)))) 
  
  fcast_dhr[[i]] <- forecast(fit_dhr[[i]], 
                             xreg = cbind(fr = fourier(ts_test, K = i), 
                                          trend = trendcycle(mstl(ts_test))), h = h)
}

##------------------------------------------------------------------------------------------

# forecast plots
autoplot(ts_train, series = "Train Data") + 
  autolayer(fcast_rwf, series = "RWF", PI = F) + 
  autolayer(fcast_arima, series = "ARIMA", PI = F) +
  autolayer(fcast_arima_tr, series = "ARIMA (Box-Cox Transformed)", PI = F) +
  labs(title = "Forecasts", x = "Weeks", y = "Female Birth") + 
  theme(panel.background = element_rect(fill = "white", colour = "black")) + 
  scale_color_manual(values = rainbow(4))

# comparison of forecasts with test data
autoplot(ts_test, series = "Test Data") + 
  autolayer(fcast_rwf, series = "RWF", PI = F) + 
  autolayer(fcast_arima, series = "ARIMA", PI = F) +
  autolayer(fcast_arima_tr, series = "ARIMA (Box-Cox Transformed)", PI = F) +
  labs(title = "Forecasts", x = "Weeks", y = "Female Birth") + 
  theme(panel.background = element_rect(fill = "white", colour = "black")) + 
  scale_color_manual(values = rainbow(4))

##------------------------------------------------------------------------------------------

## prediction intervals

# select forecast to be assessed
fcast <- fcast_ra

# for normally distributed residuals with constant variance
lower_pi <- fcast$lower; head(lower_pi)
upper_pi <- fcast$upper; head(upper_pi)

# to compute bootstrapped prediction intervals, do the following:
# while fitting the model add the argument 'bootstrap = T'
# for instance: fcast_rwf = rwf(ts_train, h = h, bootstrap = T)

##------------------------------------------------------------------------------------------

# accuracy of forecasts (RMSE, MAE, MAPE)
# select forecast to be assessed
fcast <- fcast_dhr[[3]]
accuracy(f = fcast, x = ts_test)[, c("RMSE", "MAE", "MAPE")]

##------------------------------------------------------------------------------------------

## residual analysis

# select forecast to be assessed
fcast <- fcast_ra

# time plot, acf and histogram
checkresiduals(fcast)

# mean forecast
mean(fcast$mean)

# mean of residuals
mean(fcast$residuals, na.rm = T)

##------------------------------------------------------------------------------------------

## final model
fit_final <- auto.arima(y = ts_data,
                        seasonal = T,
                        approximation = F,
                        stepwise = F, 
                        allowdrift = F,
                        xreg = cbind(trend = trendcycle(mstl(ts_data)), 
                                     season = seasonal(mstl(ts_data))))
# model summary 
summary(fit_final)

# forecasts for horizon h_final
h_final <- 7
fcast_final <- forecast(fit_final, 
                        xreg = cbind(trend = tail(trendcycle(mstl(ts_data)), h_final), 
                                     season = tail(seasonal(mstl(ts_data)), h_final)), 
                        h = h_final)

##------------------------------------------------------------------------------------------



                        