# Install necessary libraries (if not already installed)
install.packages(c("rpart", "rpart.plot", "e1071"))

# Load required libraries
library(rpart)       # For building the decision tree
library(rpart.plot)  # For visualizing the decision tree
library(e1071)       # For Naive Bayes model

# Update this line of code with the dataset location
setwd("/Users/parthasarathikundu/Downloads")

# Load the dataset (replace with the actual path to your file)
data <- read.csv("Playdecision.csv") # Ensure "Playdecision.csv" is in your working directory

# View the structure of the data
print(head(data)) # Check the first few rows of the dataset

# (a) Build a decision tree model
decision_tree <- rpart(
  Play ~ Outlook + Temperature + Humidity + Wind, 
  data = data, 
  method = "class"
)

# Print decision tree summary
print(summary(decision_tree))

# (b) Find the importance ranking of input variables
importance <- decision_tree$variable.importance
importance <- sort(importance, decreasing = TRUE) # Sort by importance
print("Importance Ranking of Input Variables:")
print(importance)

# (c) Plot the decision tree
rpart.plot(
  decision_tree, 
  type = 4, 
  extra = 104, 
  fallen.leaves = TRUE, 
  main = "Decision Tree"
)

# (d) Predict the play decision based on given inputs
# Input conditions: Outlook = rainy, Temperature = mild, Humidity = low, Wind = TRUE
new_data <- data.frame(
  Outlook = "rainy", 
  Temperature = "mild", 
  Humidity = "low", 
  Wind = TRUE
)
decision_tree_prediction <- predict(decision_tree, new_data, type = "class")
print("Decision Tree Prediction for Given Inputs:")
print(decision_tree_prediction)

# (e) Use Naive Bayes method to solve the problem
# Train the Naive Bayes model
naive_bayes_model <- naiveBayes(
  Play ~ Outlook + Temperature + Humidity + Wind, 
  data = data
)

# Predict the play decision using Naive Bayes
nb_prediction <- predict(naive_bayes_model, new_data)
print("Naive Bayes Prediction for Given Inputs:")
print(nb_prediction)

# (f) Compare the predictions
if (decision_tree_prediction == nb_prediction) {
  print("The Decision Tree and Naive Bayes predictions are CONSISTENT.")
} else {
  print("The Decision Tree and Naive Bayes predictions are INCONSISTENT.")
}

