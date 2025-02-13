library(dplyr)
library(caret)

# Update this line of code with the dataset location
setwd("/Users/parthasarathikundu/Downloads")

income_data <- read.csv("income_data.csv")

# Select the first 1200 rows
income_data <- head(income_data, 1200)

# Perform linear regression
model <- lm(Income ~ Age + Education + Gender, data = income_data)

# Print the regression summary before optimizing the model
summary(model)

# Check for non-significant variables and update the model if necessary
if (any(summary(model)$coefficients[, 4] > 0.05)) {
  # Identifying non-significant variables
  non_significant_vars <- names(summary(model)$coefficients)[summary(model)$coefficients[, 4] > 0.05]
  
  # Removed non-significant variables from the model
  formula <- paste("Income ~", paste(setdiff(colnames(income_data), non_significant_vars), collapse = " + "))
  model <- lm(as.formula(formula), data = income_data)
  
  formula
  # Print the updated regression summary after optimization
  summary(model)
}


set.seed(1234)  # Set a seed for reproducibility

# Simulate two datasets
x <- rnorm(400, mean = 10, sd = 1)
y <- rnorm(200, mean = 9.5, sd = 1.5)

# Output mean values
cat("Mean of x:", mean(x), "\n")
cat("Mean of y:", mean(y), "\n")

# Student's t-test
t_test_student <- t.test(x, y, var.equal = TRUE)

# Welch's t-test
t_test_welch <- t.test(x, y, var.equal = FALSE)

t_test_student
t_test_welch


library(arules)

# Load the Groceries dataset
data(Groceries)

# Select the first 3000 transactions
groceries_3000 <- Groceries[1:3000]

# (a) Identify frequent 1-itemset and 2-itemset with minimum support 0.02
rules1 <- apriori(groceries_3000, parameter = list(support = 0.02, minlen = 1, maxlen = 1))
rules2 <- apriori(groceries_3000, parameter = list(support = 0.02, minlen = 2, maxlen = 2))

inspect(rules1)
inspect(rules2)

# Count the number of itemsets for each case
num_itemsets1 <- length(rules1)
num_itemsets2 <- length(rules2)

length(num_itemsets1)
length(num_itemsets2)

# (b) Obtain association rules with minimum support 0.01 and minimum confidence 0.5
rules <- apriori(groceries_3000, parameter = list(support = 0.01, confidence = 0.5))
length(rules)
inspect(rules)
# (c) Plot the rules
plot(rules, method = "graph")
