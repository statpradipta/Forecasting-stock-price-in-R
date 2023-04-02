# Load necessary libraries
library(urca)
library(forecast)

# Load data
nifty50 <- read.csv("NSEI.csv", header = TRUE, stringsAsFactors = FALSE)

# Convert date column to Date format
nifty50$Date <- as.Date(nifty50$Date, format = "%Y-%m-%d")

# Convert the Close price column to numeric format
nifty50$Close <- as.numeric(nifty50$Close)

# Remove rows with missing values
nifty50 <- nifty50[complete.cases(nifty50),]

# Create a time series object
nifty50_ts <- ts(nifty50$Close, start = c(2014, 4), end = c(2023,3), frequency = 252)

# Plot the time series
plot(nifty50_ts, main = "NIFTY50 Close Price Time Series", ylab="Price")

# Check for stationarity using Augmented Dickey-Fuller (ADF) test
adf_test <- ur.df(nifty50_ts, type = "trend", selectlags = "AIC")
summary(adf_test)

# Difference the time series to achieve stationarity
nifty50_diff <- diff(nifty50_ts, differences = 1)

# Plot the differenced time series
plot(nifty50_diff, main = "Differenced NIFTY50 Close Price Time Series")

# Check for stationarity of the differenced time series
adf_test_diff <- ur.df(nifty50_diff, type = "trend", selectlags = "AIC")
summary(adf_test_diff)

# Use auto.arima() to select the best fit model
nifty50_arima <- auto.arima(nifty50_ts, D = 1, trace = TRUE)

# View the selected model
nifty50_arima

# Forecast the next 100 days of the NIFTY50 stock price
nifty50_forecast <- forecast(nifty50_arima, h = 100)

# Plot the forecasted values
plot(nifty50_forecast, main = "Forecasted NIFTY50 Close Price", xlab="Time", ylab="Price")
