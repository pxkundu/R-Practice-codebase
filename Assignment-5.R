# Load necessary libraries
library(rpart)
library(rpart.plot)

# Update this line of code with the dataset location
setwd("/Users/parthasarathikundu/Downloads")

# Load the dataset
data <- read.csv("DTdata.csv")

# 1. Identify the number of variables
num_vars <- ncol(data)
cat("Number of variables:", num_vars, "\n")

summary(data)

# table(data)
# Count the values in each column
column_counts <- apply(data, 2, table)

# Print the results
print(column_counts)

# 2. Build the Decision Tree Model
fit <-rpart(Play ~	Outlook	+ Temperature +	Humidity +	Wind,
            method="class", data=data,control=rpart.control(minsplit = 1),
            parms=list(split='information'))

summary(fit)
# 3. Plot the Decision Tree Model
rpart.plot(fit, type=4, extra=, clip.right.labs=FALSE, varlen=0, faclen=5)
