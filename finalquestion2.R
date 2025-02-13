# Install and load necessary packages
install.packages("fpp2", dependencies = TRUE)

# Load required libraries
library(fpp2)

# Load the dataset "elecequip" into the workspace
data(elecequip)

# (a) Plot the time series data
plot(elecequip, 
     main = "Electrical Equipment Time Series", 
     ylab = "Production Index", 
     xlab = "Time")

# (b) Check if the time series is stationary by observing the plot for trends/seasonality
print("The time series shows trends or seasonality, indicating it is non-stationary.")

# (c) Plot the ACF to check for seasonal effects
acf(elecequip, 
    main = "ACF of Electrical Equipment Time Series")

# Based on ACF, check for significant spikes at lag 12 (suggesting annual seasonality).

# (d) Perform twice-differencing to make the series stationary
# First differencing
diff1 <- diff(elecequip, differences = 1)

# Second differencing
diff2 <- diff(diff1, differences = 1)

# Plot the twice-differenced series
plot(diff2, 
     main = "Twice-Differenced Time Series", 
     ylab = "Differenced Production Index", 
     xlab = "Time")

# Plot the ACF of the differenced series
acf(diff2, 
    main = "ACF of Twice-Differenced Time Series")
print("The twice-differenced series is stationary based on ACF plot.")

# (e) Fit a seasonal ARIMA model (3,2,2)(1,0,0)[12]
model1 <- Arima(elecequip, order = c(3, 2, 2), seasonal = c(1, 0, 0))

# Display the fitted coefficients for ARIMA(3,2,2)(1,0,0)[12]
print("Coefficients of ARIMA(3,2,2)(1,0,0)[12]:")
print(model1)

# Check residual diagnostics for the model
checkresiduals(model1)

# (f) Fit another seasonal ARIMA model (2,2,2)(1,0,0)[12]
model2 <- Arima(elecequip, order = c(2, 2, 2), seasonal = c(1, 0, 0))

# Display the fitted coefficients for ARIMA(2,2,2)(1,0,0)[12]
print("Coefficients of ARIMA(2,2,2)(1,0,0)[12]:")
print(model2)

# Compare the models based on AIC
print("AIC of ARIMA(3,2,2)(1,0,0)[12]:")
print(AIC(model1))
print("AIC of ARIMA(2,2,2)(1,0,0)[12]:")
print(AIC(model2))

# Select the better model based on AIC
if (AIC(model1) < AIC(model2)) {
  selected_model <- model1
} else {
  selected_model <- model2
}

# Predict the next 3 months using the selected model
future_forecast <- forecast(selected_model, h = 3)

# Print the predicted 3-month values
print("3-Month Forecasted Values:")
print(future_forecast$mean)

# Plot the forecast
autoplot(future_forecast, 
         main = "3-Month Forecast for Electrical Equipment Time Series")

# (h) Calculate the 95% confidence intervals for the predicted values
print("95% Confidence Intervals for Forecast:")
print(future_forecast$lower)
print(future_forecast$upper)

# The confidence intervals are also shown in the forecast plot from (g).

