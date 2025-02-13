install.packages("ggplot2")
library(ggplot2)

setwd("/Users/parthasarathikundu/Downloads")
myDataset <- read.csv("customer_shopping_data.csv")

summary(myDataset)

# Load the required library
library(dplyr)

# Assuming your dataset is loaded into a data frame named 'df'
# Check for missing values
missing_values <- sum(is.na(myDataset))

# Impute missing 'age' values with the mean age
mean_age <- mean(myDataset$age, na.rm = TRUE)
myDataset$age[is.na(myDataset$age)] <- mean_age

df <- myDataset

# Load the required libraries
library(dplyr)
library(caret)

# 2. Encoding Categorical Variables
# Use one-hot encoding for 'gender' and 'payment_method'
df <- df %>%
  mutate(gender_Male = as.numeric(gender == "Male"),
         gender_Female = as.numeric(gender == "Female"),
         payment_Credit_Card = as.numeric(payment_method == "Credit Card"),
         payment_Debit_Card = as.numeric(payment_method == "Debit Card"),
         payment_Cash = as.numeric(payment_method == "Cash")) %>%
  select(-c(gender, payment_method))

# 3. Data Splitting (Training and Testing Sets)
set.seed(123)  # Set a seed for reproducibility
trainIndex <- createDataPartition(df$churn, p = 0.8, list = FALSE)
train_data <- df[trainIndex, ]
test_data <- df[-trainIndex, ]


# Load the required libraries
install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)  # For tree visualization

# Assuming your preprocessed dataset is loaded into 'train_data'
# The target variable 'churn' is binary (0 or 1)

# Develop the Decision Tree model
cart_model <- rpart(churn ~ ., data = df, method = "class", control = rpart.control(minsplit = 10, minbucket = 5))

cart_model

# Visualize the Decision Tree
rpart.plot(cart_model, extra = 101, type = 4, fallen.leaves = TRUE)

# Visualize the Decision Tree with improved settings
rpart.plot(cart_model, extra = 102, type = 4, fallen.leaves = TRUE, under = TRUE, nn = TRUE)


# Assuming your test dataset is loaded into 'test_data'
# Make predictions on the test data
predictions <- predict(cart_model, test_data, type = "class")

# Confusion Matrix
conf_matrix <- table(Actual = test_data$churn, Predicted = predictions)
print(conf_matrix)

# Calculate Accuracy, Precision, Recall, F1-Score
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the evaluation metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-Score:", f1_score, "\n")

