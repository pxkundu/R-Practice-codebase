# Load necessary libraries
library(ggplot2)
library(stats)

# Load the AirPassengers dataset
data("AirPassengers")

# 1. Identify the number of variables
num_vars <- ncol(AirPassengers)
cat("Number of variables:", num_vars, "\n")

# 2. Perform correlation analysis (since it's a time series, we'll use autocorrelation)
acf(AirPassengers)

# 3. Plot a correlation matrix (not directly applicable for time series)

# 4. Perform regression analysis (trend analysis)
model <- lm(AirPassengers ~ time(AirPassengers))
summary(model)

# Plot regression results
ggplot(data.frame(Time = time(AirPassengers), Passengers = AirPassengers), aes(x = Time, y = Passengers)) +
  geom_point() +
  geom_line(aes(y = fitted(model)), color = "red") +
  ggtitle("Regression Analysis")

# 5. Perform T-test and ANOVA (not directly applicable for time series)

# 6. Visualize results
# Time series plot
ggplot(data.frame(Time = time(AirPassengers), Passengers = AirPassengers), aes(x = Time, y = Passengers)) +
  geom_line() +
  ggtitle("Time Series Plot of Air Passengers")


# Histogram
ggplot(data.frame(Passengers = AirPassengers), aes(x = Passengers)) +
  geom_histogram(binwidth = 10) +
  ggtitle("Histogram of Passengers")
