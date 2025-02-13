install.packages("arules")
install.packages("arulesViz")
install.packages("dplyr")
# install.packages("assocrules")

library(arules)
library(arulesViz)
library(dplyr)
# library(assocrules)

setwd("/Users/parthasarathikundu/Downloads")

# Load the dataset (replace "Online_retail.csv" with the actual file path)
data <- read.csv("Online_retail.csv")

# Clean and prepare the data
data <- data %>%
  mutate(InvoiceNo = as.factor(InvoiceNo),
         CustomerID = as.factor(CustomerID)) %>%
  na.omit()

# Create a transaction list
transactions <- as(data, "transactions")

# Create a data frame for the first 20 items
first_20 <- transactions[1:20]

# Apply Apriori with different support values to the first 20 items
# Apply Apriori with different support values
rules_0.02_first_20 <- apriori(transactions, parameter = list(support = 0.02, confidence = 0.02))
rules_0.05_first_20 <- apriori(transactions, parameter = list(support = 0.05, confidence = 0.05))
rules_0.5_first_20 <- apriori(transactions, parameter = list(support = 0.5, confidence = 0.5))

max_items_first_20 <- inspect(rules_0.02_first_20)[1]
lift_first_20 <- max_items_first_20[10]$lift
confidence_first_20 <- max_items_first_20[10]$confidence

# Plot histogram and graph network for the first 20 items
item_freq_first_20 <- itemFrequency(first_20)
hist(item_freq_first_20, main = "Item Frequency Histogram (First 20 Items)")

# plot(rules_0.02_first_20, method = "graph", layout = "circle")
plot(rules_0.02_first_20, method = "graph", layout = "circle")

