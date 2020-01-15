
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


