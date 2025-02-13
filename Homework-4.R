# Load required libraries
library(caret)
library(pROC)

# Load the mtcars dataset
data(mtcars)

ncol(mtcars)
head(mtcars, 10)

# Convert the response variable (am) to a factor for logistic regression
mtcars$am <- factor(mtcars$am, levels = c(0, 1))

# Split the data into training and testing sets
set.seed(123)
index <- createDataPartition(mtcars$am, p = 0.7, list = FALSE)
training <- mtcars[index, ]
testing <- mtcars[-index, ]

# Build the logistic regression model
model <- glm(am ~ mpg + hp + wt, data = training, family = binomial)

# Predict probabilities on the testing set
probs <- predict(model, newdata = testing, type = "response")


# Check data dimensions and unique values
if (length(probs) != length(testing$am)) {
  stop("Length of probs and testing$am do not match.")
}

unique_probs <- unique(probs)
if (length(unique_probs) < 2) {
  warning("Insufficient unique probabilities for ROC curve.")
}

# Create ROC curve object
roc_obj <- roc(testing$am, probs)

# Plot the ROC curve
plot(roc_obj, main = "ROC Curve", col = "blue", lwd = 2)

# Add diagonal line
abline(a = 0, b = 1, lty = 2)

# Add AUC text
text(x = 0.8, y = 0.2, labels = paste("AUC =", round(auc(roc_obj), 3)))

