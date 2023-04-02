## Time Series Analysis with R

## Task 1: Install the required packages(Here urca and forecast)
#install.packages("urca")
#install.packages("forecast")

## Task 2:

# Load data - good time series example EuStockMarkets
data <- read.csv("NSEI.csv", header = TRUE)
View(data)
#data = as.numeric(data[,6])
data_df <- data[!is.na(as.numeric(data[,6])),]
#data_df=as.double(data_df)


# Convert data to time series format
tsData <- ts(data_df[,6], start = c(2013, 4), end = c(2023,3), frequency = 365)
View(tsData)
plot(tsData)

# ARIMA Model

# Exploratory Analysis for time series components:
# Trend + Seasonality + Cyclicity

# components.ts = tsData
# components.ts
# plot(components.ts)

# Does it satisfy the core assumptions?

# Remove non-stationary part of the data - Unit Root Test
library("urca")
?ur.kpss

kpss_test<-ur.kpss(tsData, type = c("tau"), 
                   lags = c("short"), use.lag = NULL)
summary(kpss_test)

tsstationary = diff(tsData, differences=1)
#tsstationary = tsData
plot(tsstationary)

# Remove seasonality
timeseriesseasonallyadjusted <- tsstationary - tsData$seasonal
tsstationary <- diff(timeseriesseasonallyadjusted, differences=1)
plot(tsstationary)


# Model Fit
acf(tsstationary, lag.max=40)
pacf(tsstationary, lag.max=40)

# Decide on the type of process

# Sample test (p,d,q) = (1,1,1)

fitARIMA = arima(tsData, order=c(1,1,1),
                 seasonal = list(order = c(1,0,0), period = 12),
                 method="ML")#Maximum Likelihood
res=fitARIMA$residuals
plot(res)

# Best Fit Model

library(forecast)
model<-auto.arima(tsData, trace=TRUE)
plot(model$residuals)

# Forecast

predicted_values = forecast(model,h=200, level=c(99.5))
plot(predicted_values)