library(tidyverse)
library(tseries)
library(ggplot2)
library(zoo)
library(fpp2)
library(urca)

# Read the CSV file
data <- read.csv("C:/Users/raucc/OneDrive/Documents/MA 641/Course Project/Datasets/melbtemp.csv")

# Convert 'data.frame' to 'zoo' class
tseries <- read.zoo(data)

# Convert 'zoo' to 'ts' class
ts <- as.ts(tseries)

# Linear model to get line of best fit
lobf=lm(ts~time(ts))
summary(lobf)

# Create a sequence of dates corresponding to the first day of each year
first_day_of_year_dates <- seq(as.Date("1981-01-01"), by = "1 year", length.out = length(ts))

# Plot time series with customized x-axis labels
plot(ts, ylab = 'Temperature', xlab = 'Date', lwd = 0.1, type = 'o', col = "black", xaxt = "n")
axis(1, at = as.numeric(first_day_of_year_dates), labels = format(first_day_of_year_dates, "%Y-%m")) +
abline(lobf)

# Calculate the ACF for the time series 'ts'
acf_result <- acf(ts, lag.max = 100)

# Plot the ACF
plot(acf_result, main = "ACF for Time Series")

# Calculate PACF 
pacf_values <- pacf(ts)

# Plot the PACF
plot(pacf_values, type = "h", main = "PACF for Time Series")


# Perform Dickey-Fuller Test
adf_result <- adf.test(ts)
print(adf_result)

# Perform the KPSS test on your time series data
kpss_result <- kpss.test(ts, null = "Trend")
kpss_result1 <- kpss.test(ts, null = "Level")

# Print the results
print(kpss_result)
print(kpss_result1)

# Seasonal differencing
ts_diff <- diff(ts, lag = 12)

# Plot the differenced time series
plot(ts_diff, ylab = 'Temperature', xlab = 'Date', lwd = 0.1, type = 'o', col = "black")

# Calculate the ACF for the differenced time series
acf_result_diff <- acf(ts_diff, lag.max = 35)
plot(acf_result_diff, main = "ACF for Seasonally Differenced Time Series")

# Calculate the PACF for the differenced time series
pacf_values_diff <- pacf(ts_diff, lag.max = 150)
plot(pacf_values_diff, type = "h", main = "PACF for Seasonally Differenced Time Series")

# Fit time series to SARMA(2,0,3)x(9,1,1)_s=12
fit1 <- Arima(ts,
              order = c(2,0,3),
              seasonal = c(9,1,1))
fit1


# Fit time series to SARMA(2,0,2)x(9,1,1)_s=12
fit2 <- Arima(ts,
              order = c(2,0,2),
              seasonal = c(9,1,1))
fit2


# Fit time series to SARMA(2,0,1)x(9,1,1)_s=12
fit3 <- Arima(ts,
              order = c(2,0,1),
              seasonal = c(9,1,1))

# Display Residuals
fit3

# Fit time series to SARMA(1,0,3)x(9,1,1)_s=12
fit4 <- Arima(ts,
              order = c(1,0,3),
              seasonal = c(9,1,1))

# Display Residuals
fit4

# Fit time series to SARMA(1,0,2)x(9,1,1)_s=12
fit5 <- Arima(ts,
              order = c(1,0,2),
              seasonal = c(9,1,1))

# Display Residuals
fit5

# Fit time series to SARMA(1,0,1)x(9,1,1)_s=12
fit6 <- Arima(ts,
              order = c(1,0,1),
              seasonal = c(9,1,1))

# Display Residuals
fit6

# Fit time series to SARMA(2,0,3)x(8,1,1)_s=12
fit7 <- Arima(ts,
              order = c(2,0,3),
              seasonal = c(8,1,1))

# Display Residuals
fit7

# Fit time series to SARMA(2,0,2)x(8,1,1)_s=12
fit8 <- Arima(ts,
              order = c(2,0,2),
              seasonal = c(8,1,1))

# Display Residuals
fit8

# Fit time series to SARMA(2,0,1)x(8,1,1)_s=12
fit9 <- Arima(ts,
              order = c(2,0,1),
              seasonal = c(8,1,1))

# Display Residuals
fit9

# Fit time series to SARMA(1,0,3)x(8,1,1)_s=12
fit10 <- Arima(ts,
               order = c(1,0,3),
               seasonal = c(8,1,1))

# Display Residuals
fit10

# Fit time series to SARMA(1,0,2)x(8,1,1)_s=12
fit11 <- Arima(ts,
               order = c(1,0,2),
               seasonal = c(8,1,1))

# Display Residuals
fit11

# Fit time series to SARMA(1,0,1)x(8,1,1)_s=12
fit12 <- Arima(ts,
               order = c(1,0,1),
               seasonal = c(8,1,1))

# Display Residuals
fit12

# Fit time series to SARMA(2,0,3)x(7,1,1)_s=12
fit13 <- Arima(ts,
               order = c(2,0,3),
               seasonal = c(7,1,1))

# Display Residuals
fit13

# Fit time series to SARMA(2,0,2)x(7,1,1)_s=12
fit14 <- Arima(ts,
               order = c(2,0,2),
               seasonal = c(7,1,1))

# Display Residuals
fit14

# Fit time series to SARMA(2,0,1)x(7,1,1)_s=12
fit15 <- Arima(ts,
               order = c(2,0,1),
               seasonal = c(7,1,1))

# Display Residuals
fit15

# Fit time series to SARMA(1,0,3)x(7,1,1)_s=12
fit16 <- Arima(ts,
               order = c(1,0,3),
               seasonal = c(7,1,1))

# Display Residuals
fit16

# Fit time series to SARMA(2,0,1)x(7,1,1)_s=12
fit17 <- Arima(ts,
               order = c(2,0,1),
               seasonal = c(7,1,1))

# Display Residuals
fit17

# Fit time series to SARMA(1,0,1)x(7,1,1)_s=12
fit18 <- Arima(ts,
               order = c(1,0,1),
               seasonal = c(7,1,1))

# Display Residuals
fit18

# Fit time series to SARMA(2,0,3)x(6,1,1)_s=12
fit19 <- Arima(ts,
               order = c(2,0,3),
               seasonal = c(6,1,1))

# Display Residuals
fit19

# Fit time series to SARMA(2,0,2)x(6,1,1)_s=12
fit20 <- Arima(ts,
               order = c(2,0,2),
               seasonal = c(6,1,1))

# Display Residuals
fit20

# Fit time series to SARMA(2,0,1)x(6,1,1)_s=12
fit21 <- Arima(ts,
               order = c(2,0,1),
               seasonal = c(6,1,1))

# Display Residuals
fit21

# Fit time series to SARMA(1,0,3)x(6,1,1)_s=12
fit22 <- Arima(ts,
               order = c(1,0,3),
               seasonal = c(6,1,1))

# Display Residuals
fit22

# Fit time series to SARMA(1,0,2)x(6,1,1)_s=12
fit23 <- Arima(ts,
               order = c(1,0,2),
               seasonal = c(6,1,1))

# Display Residuals
fit23

# Fit time series to SARMA(1,0,1)x(6,1,1)_s=12
fit24 <- Arima(ts,
               order = c(1,0,1),
               seasonal = c(6,1,1))

# Display Residuals
fit24

# Fit time series to SARMA(2,0,3)x(5,1,)_s=12
fit25 <- Arima(ts,
               order = c(2,0,3),
               seasonal = c(5,1,1))

# Display Residuals
fit25

# Fit time series to SARMA(2,0,2)x(5,1,1)_s=12
fit26 <- Arima(ts,
               order = c(2,0,2),
               seasonal = c(5,1,1))

# Display Residuals
fit26

# Fit time series to SARMA(2,0,1)x(5,1,1)_s=12
fit27 <- Arima(ts,
               order = c(2,0,1),
               seasonal = c(5,1,1))

# Display Residuals
fit27

# Fit time series to SARMA(1,0,3)x(5,1,1)_s=12
fit28 <- Arima(ts,
               order = c(1,0,3),
               seasonal = c(5,1,1))

# Display Residuals
fit28

# Fit time series to SARMA(1,0,2)x(5,1,1)_s=12
fit29 <- Arima(ts,
               order = c(1,0,2),
               seasonal = c(5,1,1))

# Display Residuals
fit29

# Fit time series to SARMA(1,0,1)x(5,1,1)_s=12
fit30 <- Arima(ts,
               order = c(1,0,1),
               seasonal = c(5,1,1))

# Display Residuals
fit30

# Fit time series to SARMA(2,0,3)x(4,1,1)_s=12
fit31 <- Arima(ts,
               order = c(2,0,3),
               seasonal = c(4,1,1))

# Display Residuals
fit31

# Fit time series to SARMA(2,0,2)x(4,1,1)_s=12
fit32 <- Arima(ts,
               order = c(2,0,2),
               seasonal = c(4,1,1))

# Display Residuals
fit32

# Fit time series to SARMA(2,0,1)x(4,1,1)_s=12
fit33 <- Arima(ts,
               order = c(2,0,1),
               seasonal = c(4,1,1))

# Display Residuals
fit33

# Fit time series to SARMA(1,0,3)x(4,1,1)_s=12
fit34 <- Arima(ts,
               order = c(1,0,3),
               seasonal = c(4,1,1))

# Display Residuals
fit34

# Fit time series to SARMA(1,0,2)x(4,1,1)_s=12
fit35 <- Arima(ts,
               order = c(1,0,2),
               seasonal = c(4,1,1))

# Display Residuals
fit35

# Fit time series to SARMA(1,0,1)x(4,1,1)_s=12
fit36 <- Arima(ts,
               order = c(1,0,1),
               seasonal = c(4,1,1))

# Display Residuals
fit36

# Fit time series to SARMA(2,0,3)x(3,1,1)_s=12
fit37 <- Arima(ts,
               order = c(2,0,3),
               seasonal = c(3,1,1))

# Display Residuals
fit37

# Fit time series to SARMA(2,0,2)x(3,1,1)_s=12
fit38 <- Arima(ts,
               order = c(2,0,2),
               seasonal = c(3,1,1))

# Display Residuals
fit38

# Fit time series to SARMA(2,0,1)x(3,1,1)_s=12
fit39 <- Arima(ts,
               order = c(2,0,1),
               seasonal = c(3,1,1))

# Display Residuals
fit39

# Fit time series to SARMA(1,0,3)x(3,1,1)_s=12
fit40 <- Arima(ts,
               order = c(1,0,3),
               seasonal = c(3,1,1))

# Display Residuals
fit40

# Fit time series to SARMA(1,0,2)x(3,1,1)_s=12
fit41 <- Arima(ts,
               order = c(1,0,2),
               seasonal = c(3,1,1))

# Display Residuals
fit41

# Fit time series to SARMA(1,0,1)x(3,1,1)_s=12
fit42 <- Arima(ts,
               order = c(1,0,1),
               seasonal = c(3,1,1))

# Display Residuals
fit42

# Fit time series to SARMA(2,0,3)x(2,1,1)_s=12
fit43 <- Arima(ts,
               order = c(2,0,3),
               seasonal = c(2,1,1))

# Display Residuals
fit43

# Fit time series to SARMA(2,0,2)x(2,1,1)_s=12
fit44 <- Arima(ts,
               order = c(2,0,2),
               seasonal = c(2,1,1))

# Display Residuals
fit44

# Fit time series to SARMA(2,0,1)x(2,1,1)_s=12
fit45 <- Arima(ts,
               order = c(2,0,1),
               seasonal = c(2,1,1))

# Display Residuals
fit45

# Fit time series to SARMA(1,0,3)x(2,1,1)_s=12
fit46 <- Arima(ts,
               order = c(1,0,3),
               seasonal = c(2,1,1))

# Display Residuals
fit46

# Fit time series to SARMA(1,0,2)x(2,1,1)_s=12
fit47 <- Arima(ts,
               order = c(1,0,2),
               seasonal = c(2,1,1))

# Display Residuals
fit47

# Fit time series to SARMA(1,0,1)x(2,1,1)_s=12
fit48 <- Arima(ts,
               order = c(1,0,1),
               seasonal = c(2,1,1))

# Display Residuals
fit48

# Fit time series to SARMA(2,0,3)x(1,1,1)_s=12
fit49 <- Arima(ts,
               order = c(2,0,3),
               seasonal = c(1,1,1))

# Display Residuals
fit49

# Fit time series to SARMA(2,0,2)x(1,1,1)_s=12
fit50 <- Arima(ts,
               order = c(2,0,2),
               seasonal = c(1,1,1))

# Display Residuals
fit50

# Fit time series to SARMA(2,0,1)x(1,1,1)_s=12
fit51 <- Arima(ts,
               order = c(2,0,1),
               seasonal = c(1,1,1))

# Display Residuals
fit51

# Fit time series to SARMA(1,0,3)x(1,1,1)_s=12
fit52 <- Arima(ts,
               order = c(1,0,3),
               seasonal = c(1,1,1))

# Display Residuals
fit52

# Fit time series to SARMA(1,0,2)x(1,1,1)_s=12
fit53 <- Arima(ts,
               order = c(1,0,2),
               seasonal = c(1,1,1))

# Display Residuals
fit53

# Fit time series to SARMA(1,0,1)x(1,1,1)_s=12
fit54 <- Arima(ts,
               order = c(1,0,1),
               seasonal = c(1,1,1))

# Display Residuals
fit54


## Following principle of parsimony, the three best working models are: 
## 1. (1,0,3)x(1,1,1)
## 2. (2,0,3)x(1,1,1)
## 3. (2,0,2)x(1,1,1)

## Plot residuals and Box-Ljung results for 3 best working models
tsdiag(fit52)
tsdiag(fit49)
tsdiag(fit50)

# Forecast the next 100 points
fit52 %>%
  forecast(h=500) %>%
  autoplot()
