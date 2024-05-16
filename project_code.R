
# Load necessary libraries
library(astsa)  # For time series analysis
library(stats)  # For statistical functions

# Load your time series data
# Replace "your_data.csv" with the actual filename
data <- read.csv("your_data.csv", header = TRUE)

# Check the structure of your data
str(data)

# Convert your data to a time series object if it's not already
# Replace "your_column_name" with the actual column name containing the time series data
ts_data <- ts(data$your_column_name, start = c(start_year, start_month), frequency = 12)
#Exploratory Data Analysis (EDA):

# Plot the time series data
plot(ts_data, main = "Time Series Data", xlab = "Time", ylab = "Value")

# Check for seasonality and trend
seasonplot(ts_data)
#Stationarity Test:

# Augmented Dickey-Fuller test for stationarity
adf_test <- adf.test(ts_data)
print(adf_test)


#Characterization of Roots:


# Compute the roots of the autoregressive (AR) model
ar_model <- ar(ts_data)
roots <- polyroot(c(1, -ar_model$ar))
print(roots)

#Check for Complex Roots:

# Check if any roots are complex
complex_roots <- any(Im(roots) != 0)
if (complex_roots) {
  print("Complex roots detected.")
} else {
  print("No complex roots detected.")
}

#Further Analysis (Optional):

# Perform additional analysis such as forecasting


# Fit an ARIMA model to the time series data
arima_model <- auto.arima(ts_data)

# Forecast future values
forecast_result <- forecast(arima_model, h = 12)  # Forecasting for the next 12 time points

# Plot the forecast
plot(forecast_result, main = "Forecasted Values", xlab = "Time", ylab = "Value")

# Compute the periodogram
periodogram_result <- spectrum(ts_data)

# Plot the periodogram
plot(periodogram_result, main = "Periodogram", xlab = "Frequency", ylab = "Power")
# Summarize your findings