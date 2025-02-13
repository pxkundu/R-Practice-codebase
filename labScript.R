install.packages("ggplot2")

library(ggplot2)
install.packages("arules")
library(arules)

install.packages("arulesViz")
library(arulesViz)

install.packages("openxlsx")
library(openxlsx)
setwd("/Users/parthasarathikundu/Downloads")

myDataset <- read.csv("customer_shopping_data.csv")

# One-hot encoding for Payment Method
data <- cbind(myDataset, model.matrix(~ payment_method - 1, data = myDataset))

# Standardize numeric variables
data$price <- scale(data$price)
data$age <- scale(data$age)

# Perform PCA
pca_result <- prcomp(data[, c("price", "age", "payment_methodCredit Card", "payment_methodCash", "payment_methodDebit Card")], center = TRUE, scale = TRUE)

# Explained variance
explained_variance <- pca_result$sdev^2
proportion_explained_variance <- explained_variance / sum(explained_variance)

# Scree plot
plot(1:length(proportion_explained_variance), proportion_explained_variance, type = "b", xlab = "Principal Component", ylab = "Proportion of Variance Explained")

# Rename the column
colnames(data)[colnames(data) == "payment_methodCredit Card"] <- "payment_methodCreditCard"
# Rename the column
colnames(data)[colnames(data) == "payment_methodDebit Card"] <- "payment_methodDebitCard"

# Summary statistics
summary(data)

# Box plot for Price
ggplot(data, aes(y = price)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Box Plot for Price", y = "Price")

# Box plot for Age
ggplot(data, aes(y = age)) +
  geom_boxplot(fill = "green") +
  labs(title = "Box Plot for Age", y = "Age")

# Create a scatterplot matrix for Price, Age, and quantity
pairs(data[, c("price", "age", "quantity")], pch = 20, col = "blue")



# Bar plot for Payment Method
payment_method_data <- data %>%
  select(payment_methodCreditCard, payment_methodCash, payment_methodDebitCard)
payment_method_counts <- colSums(payment_method_data)
payment_method_counts <- data.frame(Method = names(payment_method_counts), Frequency = payment_method_counts)
ggplot(payment_method_counts, aes(x = Method, y = Frequency)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Distribution of Payment Methods", x = "Payment Method", y = "Frequency")

# Scatter plot for Price vs. Quantity
ggplot(data, aes(x = price, y = quantity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Price vs. Quantity", x = "Price", y = "Quantity")

# Scatter plot for Age vs. Quantity
ggplot(data, aes(x = age, y = quantity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(title = "Age vs. Quantity", x = "Age", y = "Quantity")


# Create the scatter plot
lmplot <- ggplot(data, aes(x = price, y = quantity, color = factor(payment_method))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Price vs. Quantity by Payment Method", x = "Price", y = "Quantity", color = "Payment Method") +
  scale_color_manual(values = c("blue", "red", "green"))  # Customize colors


lmplot
summary(lmplot)

model<-lm(data$price~data$quantity + data$age)
model
summary(model)
#new line
plot(data$age,data$price)

# Create a scatterplot matrix for Price, Age, and quantity
pairs(data[, c("price", "age", "quantity")], pch = 20, col = "blue")


# Subset the data for the "Clothing" category
clothing_data <- subset(data, category == "Clothing")

# Perform a simple linear regression
lm_model <- lm(price ~ payment_method, data = clothing_data)

# Summary of the regression model
summary(lm_model)
lm_model

# Create a scatter plot with regression line
ggplot(data, aes(x = quantity, y = payment_method)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Price vs. Quantity for Payment Method", x = "Price", y = "Quantity")


# Perform a simple linear regression
lm_model <- lm(quantity ~ price, data = data)

# Summary of the regression model
summary(lm_model)
lm_model

# Create a scatter plot with regression line
ggplot(data, aes(x = price, y = quantity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Price vs. Quantity for Clothing", x = "Price", y = "Quantity")


# Create a scatter plot
ggplot(data, aes(x = price, y = quantity)) +
  geom_point() +                # Add data points
  geom_abline(intercept = coef(lm_model)[1], slope = coef(lm_model)[2], color = "blue") +  # Add the regression line
  labs(title = "Scatter Plot with Linear Regression Line", x = "Price", y = "Quantity")


# Create a scatter plot with different colors for payment methods
ggplot(data, aes(x = price, y = quantity, color = payment_method)) +
  geom_point() +                # Add data points with color differentiation
  geom_abline(intercept = coef(lm_model)[1], slope = coef(lm_model)[2], color = "blue") +  # Add the regression line
  labs(title = "Scatter Plot with Linear Regression Line by Payment Method", x = "Price", y = "Quantity") +
  scale_color_manual(values = c("Credit Card" = "blue", "Debit Card" = "green", "Cash" = "red"))  # Customize colors

# Create a linear regression model with payment_method
lm_model <- lm(quantity ~ price + payment_method, data = data)

# Display the summary of the linear regression model
summary(lm_model)

lm_model


# Create a linear regression model with multiple independent variables
lm_model <- lm(quantity ~ price + gender + age + category, data = data)

# Display the summary of the linear regression model
summary(lm_model)
lm_model

# Get the predicted values from the linear regression model
predicted_values <- predict(lm_model)

# Create a scatter plot of observed vs. predicted values
plot(data$quantity, predicted_values, 
     xlab = "Observed Quantity", ylab = "Predicted Quantity",
     main = "Observed vs. Predicted Quantity")
abline(0, 1, col = "red")  # Add a diagonal line for reference



#my most efficient model so far
lm_model <- lm(price ~ quantity + gender + age + category + payment_method + churn, data = data)

# Display the summary of the linear regression model
summary(lm_model)

lm_model

predicted_values <- predict(lm_model)

plot(data$price, predicted_values, xlab = "Actual Price", ylab = "Predicted Price", main = "Actual vs. Predicted Price")
abline(0, 1, col = "red")  # Add a red line with slope 1 to show perfect predictions









# Convert price to low or high based on a threshold (e.g., 1000)
myDataset$price_low <- ifelse(myDataset$price <= 1000, 1, 0)
myDataset$price_high <- ifelse(myDataset$price > 1000, 1, 0)

# Convert payment_method to binary columns
myDataset$payment_method_Cash <- ifelse(myDataset$payment_method == "Cash", 1, 0)
myDataset$payment_method_Credit_Card <- ifelse(myDataset$payment_method == "Credit Card", 1, 0)
myDataset$payment_method_Debit_Card <- ifelse(myDataset$payment_method == "Debit Card", 1, 0)

# Select only the relevant columns for association analysis
myTransactionData <- myDataset[, c(
  "age", "price_low", "price_high",
  "payment_method_Cash", "payment_method_Credit_Card", "payment_method_Debit_Card"
)]


# Implementing the Apriori algorithm with specified parameters
ar1 <- apriori(myTransactionData, parameter = list(support=0.01, confidence = 0.5, minlen=2))

# Display rules and quality measures
inspect(ar1) # Show rules and quality measures
inspect(sort(ar1, by="lift")[1:4]) # Check the rules with the top 4 lifts
inspect(sort(ar1, by="support")[1:3]) # Check the rules with the top 3 supports

# Assuming your transformed data is stored in 'myTransactionData' as a data frame
write.xlsx(myTransactionData, "transformed_data.xlsx", rowNames = FALSE)

# Inspect the top 10 association rules based on lift
top_10_rules <- head(sort(ar1, by = "lift"), 10)

# Display the top 10 association rules
inspect(top_10_rules)

# Interpretation of the best rule (example)
best_rule <- top_10_rules[1]

inspect(best_rule)

# Convert the dataset to transaction format
transactions <- as(myTransactionData, "transactions")

summary(transactions)

write.xlsx(transactions, "transaction_data.xlsx", rowNames = FALSE)


# Visualize the frequency of the rules
itemFrequencyPlot(transactions, topN = 10)


# Find rules with support equal to 0.1
rules_support_0.1 <- apriori(transactions, parameter = list(support = 0.1, confidence = 0.5, minlen = 2))


# Assuming you have a variable 'rules_support_0.1' with the rules
# Replace 'rules_support_0.1' with your actual rules variable
rules <- rules_support_0.1

# Generate a summary of the rules
summary(rules)

# Plot the frequency of rules

plot(rules, method = "graph")

plot(rules, method = "paracoord")

plot(rules, method = "matrix3D")















# Calculate item support
item_support <- itemFrequency(myTransactionData)

# Subset items with support >= 10%
high_support_items <- subset(item_support, support >= 0.10)

# Plot items with support >= 10%
itemFrequencyPlot(high_support_items, main = "Items with Support >= 10%", ylab = "Support")

# Calculate the frequency of the association rules
rule_frequency <- frequency(ar1, support=0.1)

# Plot the frequency of the rules
barplot(rule_frequency, 
        main = "Frequency of Top 10 Association Rules",
        xlab = "Association Rules",
        ylab = "Frequency",
        col = "skyblue",
        names.arg = 1:10)









# Create Bar Chart for Category Vs Quantity
ggplot(data, aes(x = category, y = quantity)) +
     geom_bar(stat = "identity", fill = "blue") +
    labs(title = "Bar Chart of shopping items", x = "Category", y = "Quantity")

# this did not work
ggplot(data, aes(x = "", y = Value, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart Example", fill = "Categories")


install.packages("plotly")
library(plotly)

# Create Pie Chart for shoping category as per their quantity
pie_chart <- plot_ly(data, labels = ~category, values = ~quantity, type = 'pie') %>%
  layout(title = "Pie Chart for Shopping data")
#show it
pie_chart


# Create Pie Chart for shoping category as per their quantity
pie_chart2 <- plot_ly(data, labels = ~price, values = ~quantity, type = 'pie') %>%
  layout(title = "Pie Chart for Shopping data")
#show it
pie_chart2


# Create Bar chart for Shopping List Items by Gender
ggplot(data, aes(x = gender, y = quantity, fill = category)) +
 geom_bar(stat = "identity", position = "dodge") +
 labs(title = "Shopping List Items by Gender", x = "Gender", y = "Quantity") +
 scale_fill_brewer(palette = "Set1")


library(dplyr)

#Find total of Category Quantity data
total_quantity_data <- data %>%
  group_by(gender) %>%
  summarise(Total_Quantity = sum(quantity))

# Create bar chart for Total Quantity by Gender
ggplot(total_quantity_data, aes(x = gender, y = Total_Quantity, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Quantity by Gender", x = "Gender", y = "Total Quantity") +
  scale_fill_brewer(palette = "Set1")

# Create pie chart for payment method distribution
ggplot(data, aes(x = gender, fill = category)) +
  geom_bar() +
  labs(title = "Category Distribution by Gender")

# Create histogram for age distribution
ggplot(data, aes(x = age, fill = gender)) +
  geom_histogram(binwidth = 5, color = "black") +
  labs(title = "Age Histogram by Gender", x = "Age", y = "Frequency") +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "red")) +
  theme_minimal()

# Create pie chart for payment method distribution
ggplot(data, aes(x = "", fill = payment_method)) +
  geom_bar() +
  coord_polar("y") +
  labs(title = "Payment Method Distribution")


# Create a stacked bar chart for payment method distribution by gender
ggplot(data, aes(x = gender, fill = payment_method)) +
  geom_bar() +
  labs(title = "Payment Method Distribution by Gender", x = "Gender", y = "Count") +
  scale_fill_brewer(palette = "Set1") +  # You can change the color palette
  theme_minimal()


# Create scatter plot for age vs. price
ggplot(data, aes(x = age, y = price)) +
  geom_point() +
  labs(title = "Scatter Plot of Age vs. Price")


summary(data[c("age", "quantity", "price")])

correlation_matrix <- cor(data[c("age", "quantity", "price")])
correlation_matrix

# Create density plots for numeric variables
ggplot(data, aes(x = price)) +
  geom_density() +
  labs(title = "Density Plot of Total Purchase Price")

model <- lm(price ~ age + quantity + category, data = data)
model

summary(model)


# Load required libraries
library(ggplot2)
library(caret)

# Load the dataset
data <- read.csv("your_dataset.csv", header = TRUE)

# Data Preprocessing
# Handle missing data using imputation
data$age[is.na(data$age)] <- mean(data$age, na.rm = TRUE)

# Encode categorical variable 'category'
data$category <- as.numeric(factor(data$category))

# Split data into training and testing sets
set.seed(123)
train_index <- createDataPartition(data$total_purchase_price, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Linear Regression Modeling
model <- lm(total_purchase_price ~ age + quantity + category, data = train_data)

# Model Evaluation
summary(model)
predicted_values <- predict(model, newdata = test_data)
residuals <- residuals(model)
correlation_matrix <- cor(train_data)

# Visualization (Optional)
# Example: Create a scatter plot of age vs. total purchase price
ggplot(data, aes(x = age, y = total_purchase_price)) +
  geom_point() +
  labs(title = "Scatter Plot of Age vs. Total Purchase Price")


ggplot(data, aes(x = quantity, y = price)) +
 geom_point() +
 labs(title = "Scatter Plot of quantity vs. Total Purchase Price")


# Fit your linear regression model (replace with your model)
model <- lm(price ~ age + quantity + category, data = train_data)

# Load the 'lmtest' package for Link test
install.packages("lmtest")  # Install if not already installed
library(lmtest)

# Run the Link test (Breusch-Pagan test for heteroscedasticity)
link_test <- bptest(model)
print(link_test)


# Load necessary libraries
library(lmtest)

# Fit your linear regression model (replace with your model)
model <- lm(price ~ age + quantity + category, data = train_data)
# Perform the Rainbow test (RESET test)
rainbow_test <- resettest(model, power = 3)  # You can specify a different power for the test

# Print the results
print(rainbow_test)


reset_test_result <- list(
  test = "RESET test",
  RESET = 0.42265,
  df1 = 1,
  df2 = 69616,
  p_value = 0.5156
)



# Load required libraries
library(dplyr)

# Create a logistic regression model
model <- glm(shopping_trend ~ gender + age + category + quantity + price + payment_method + shopping_mall, data = data, family = binomial)

# Print model summary
summary(model)





# Load necessary libraries
library(readr)
library(caret)
library(dplyr)
library(ROCR)
library(e1071)

# Load the dataset
data <- read.csv("your_dataset.csv")  # Replace with your dataset file path

# Split data into training and testing sets
set.seed(42)  # For reproducibility
train_indices <- createDataPartition(data$Churn, p = 0.7, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Logistic regression model
model <- glm(Churn ~ ., family = binomial(link = "logit"), data = train_data)

# Model summary
summary(model)

# Model evaluation
predicted_probabilities <- predict(model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predicted_probabilities >= 0.5, 1, 0)

# Accuracy
accuracy <- sum(predicted_classes == test_data$Churn) / length(test_data$Churn)

# Classification report
conf_matrix <- confusionMatrix(predicted_classes, test_data$Churn)
classification_report <- confusionMatrix(predicted_classes, test_data$Churn)

# Model recommendation
if (accuracy > 0.7) {
  cat("The logistic regression model is recommended for predicting customer churn.\n")
} else {
  cat("The logistic regression model may need further improvement for accurate predictions.\n")
}




install.packages("cluster")
install.packages("dplyr")

# Load necessary libraries
library(cluster)
library(dplyr)

# Load the dataset (assuming it's stored in a dataframe df)
# Select the relevant columns
# data <- df %>% select(age, price, payment_method)
df <- data.frame(data)

data <- df[, c("age", "price", "payment_method")]

# Handling missing data (if any)
data <- na.omit(data)

# Encode categorical data ('payment_method') using one-hot encoding
data <- data %>% 
  mutate(payment_method = factor(payment_method)) %>%
  dummyVars(" ~ .", data = .) %>%
  predict(., data)

# Standardize or normalize the 'age' and 'price' columns (feature scaling)
data[, c("age", "price")] <- scale(data[, c("age", "price")])

# Choosing the number of clusters (you can determine the optimal k using the Elbow Method or Silhouette Score)
k <- 3

# Initialize and fit the K-Means model
kmeans_model <- kmeans(data[, c("age", "price")], centers = k, nstart = 25)

# Add cluster labels to the original dataframe
df$cluster <- kmeans_model$cluster

# Check cluster centers and sizes

cluster_centers <- kmeans_model$centers

# cluster_centers <- scale(kmeans_model$centers, center = FALSE, scale = attr(kmeans_model$centers, "scaled:center"))
cluster_counts <- table(df$cluster)

# Print cluster centers and counts
cat("Cluster Centers (age, price):\n")
print(cluster_centers)

cat("\nCluster Counts:\n")
print(cluster_counts)


cluster_sorted <- data[order(cluster_centers),]
print(cluster_sorted)
DistMatrixSorted <- as.matrix(dist(cluster_sorted, method = "euclidean"))
library(lattice)
levelplot(DistMatrixSorted)

ggplot(data, aes(x = age, y = price, color = factor(cluster_sorted))) +
  geom_point(size = 3) +
  labs(x = "Age", y = "Shopping Amount") +
  scale_color_discrete(name = "Cluster") +
  ggtitle("Clusters Based on Age and Shopping Amount")


library(lattice)

# Create a lattice plot of the clusters
xyplot(price ~ age, data = data, groups = kmeans_model$cluster,
       type = c("p", "g"), auto.key = TRUE,
       main = "Clusters Based on Age and Shopping Amount")

# Initialize variables to store WSS and silhouette scores
wss_scores <- vector("double", length = 10)
silhouette_scores <- vector("double", length = 10)

# Calculate WSS and silhouette scores for different numbers of clusters (k)
for (k in 2:10) {
  kmeans_model <- kmeans(data[, c("age", "price")], centers = k, nstart = 25)
  wss_scores[k] <- sum(kmeans_model$withinss)
  silhouette_scores[k] <- silhouette(kmeans_model$cluster, dist(data[, c("age", "price")]))
}

# Plot WSS and silhouette scores to determine optimal k
par(mfrow = c(1, 2))
plot(2:10, wss_scores[2:10], type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters (k)", ylab = "WSS",
     main = "Within-Cluster Sum of Squares (WSS)")
plot(2:10, silhouette_scores[2:10], type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters (k)", ylab = "Silhouette Score",
     main = "Silhouette Score")

# Find the optimal k based on the elbow method for WSS
optimal_k_wss <- which.min(wss_scores)

# Find the optimal k based on the highest silhouette score
optimal_k_silhouette <- which.max(silhouette_scores)

# Print the optimal k values
cat("Optimal k based on WSS:", optimal_k_wss, "\n")
cat("Optimal k based on Silhouette Score:", optimal_k_silhouette, "\n")


