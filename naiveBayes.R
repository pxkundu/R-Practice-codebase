# Load necessary libraries
install.packages("e1071", dep= TRUE)
library(e1071)
library(RCOR)
library(caret)
library(pROC)

# Update this line of code with the dataset location
setwd("/Users/parthasarathikundu/Downloads")

# Load the dataset
sample <- read.table("sample1.csv", header=TRUE, sep=",")

traindata<-as.data.frame(sample[1:14,])
testdata<-as.data.frame(sample[15,])

traindata
testdata

# Genarate model output based on the target value of Enroll
model<-naiveBayes(Enrolls~Age+Income+JobSatisfaction+Desire, traindata)

# Genarate model output based on the target value of Age
model<-naiveBayes(Age~Enrolls+Income+JobSatisfaction+Desire, traindata)

# Genarate model output based on the target value of Income
model<-naiveBayes(Income~Enrolls+Age+JobSatisfaction+Desire, traindata)

model

# Provides test prediction
result<-predict(model, testdata)

result




# Select only the required columns
data <- sample[, c("JobSatisfaction", "Desire", "Enrolls")]

# Convert categorical variables to factors
data$JobSatisfaction <- factor(data$JobSatisfaction)
data$Desire <- factor(data$Desire)
data$Enrolls <- factor(data$Enrolls)

# Split the data into training and testing sets (80% training, 20% testing)
set.seed(123)  # Set seed for reproducibility
index <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[index, ]
test_data <- data[-index, ]

# Build the Naive Bayes model
model <- naiveBayes(Enrolls ~ ., data = train_data)

# Make predictions on the test set
predictions <- predict(model, newdata = test_data, type = "raw")

# Build the Naive Bayes model
model <- naiveBayes(Enrolls ~ ., data = train_data)

# Make predictions on the test set
predictions <- predict(model, newdata = test_data, type = "class")  # Use type = "class" for direct class predictions

# Check if predictions and test data have the same length
if (length(predictions) != nrow(test_data)) {
  stop("Prediction length doesn't match test data length. Check model fitting and prediction.")
}

# Calculate the confusion matrix and accuracy
confusion_matrix <- table(test_data$Enrolls, predictions)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(confusion_matrix)
print(paste("Accuracy:", accuracy))

# Get the predicted probabilities
predicted_probs <- predict(model, newdata = test_data, type = "prob")

# Calculate multi-class ROC curve and AUC
multiclass_roc <- multiclass.roc(test_data$Enrolls, predictions)
plot(multiclass_roc)

auc <- auc(roc_obj)
print(paste("AUC:", auc))

# Plot the ROC curve
plot(roc_obj, main = "ROC Curve", col = "blue")









