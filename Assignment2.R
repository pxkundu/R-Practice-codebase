library(dplyr)  # For data manipulation
library(corrplot)  # For creating correlation plots
library(GGally)  # For creating scatterplot matrices
library(caret)  # For regression analysis and performance metrics
library(glmnet)  # For Ridge and Lasso regression
library(caTools)  # For data partitioning

# Assuming the data is in a CSV file named "housing_data.csv"
housing_data1 <- read.csv("HousingData.csv")

# Remove rows with missing values in the ZN column
housing_data <- housing_data1 %>% na.omit()

# a. Identifying the Number of Variables
num_vars <- ncol(housing_data)
cat("Number of variables in the Boston housing dataset:", num_vars, "\n")

# b. Plotting a Heat Map to Study Correlation Analysis
corr_matrix <- cor(housing_data)
corrplot(corr_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

# c. Identifying Variables Crucial for Pricing
# (Examine the heatmap for correlations and regression coefficients)

# d. Constructing a Scatter Matrix
ggpairs(housing_data, lower = list(continuous = "points"), upper = list(continuous = "cor"))

# e. Constructing a Regression Analysis Model

# Check for missing values in the medv column
sum(is.na(housing_data$medv))
# Check for unique values in the medv column
length(unique(housing_data$medv))
# Summary statistics
summary(housing_data)

set.seed(123)
# Split the data into training and testing sets using caTools

# split <- sample.split(housing_data$medv, SplitRatio = 0.75)
# train_data <- subset(housing_data, split == TRUE)
# test_data <- subset(housing_data, split == FALSE)

model <- lm(MEDV ~ CRIM + RM + AGE + LSTAT, data = housing_data)

# Display the summary of the linear regression model
summary(model)

model

r_squared <- summary(model)$r.squared
adj_r_squared <- summary(model)$adj.r.squared
mse <- mean((housing_data$MEDV - predict(model, newdata = housing_data))^2)

cat("R-squared:", r_squared, "\n")
cat("Adjusted R-squared:", adj_r_squared, "\n")
cat("Mean squared error:", mse, "\n")

# Create Predicted Values from the model and plot them
predicted_values <- predict(model)

plot(housing_data$MEDV, predicted_values, xlab = "Actual Price", ylab = "Predicted Price", main = "Actual vs. Predicted Price")
abline(0, 1, col = "red")  # Add a red line with slope 1 to show perfect predictions
