# Install necessary libraries (if not already installed)
install.packages(c("ggplot2", "dplyr", "readxl", "corrplot"))
install.packages("tidyverse")
install.packages("GGally")
install.packages("arules")
install.packages("rpart")
install.packages("tm")
install.packages("wordcloud")

# Load required libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(corrplot)
library(tidyverse)
library(GGally)
library(arules)
library(rpart)

# Update this line of code with the dataset location
setwd("/Users/parthasarathikundu/Downloads")
setwd("C:/Users/t3student7/Downloads/walmart/refineddataset/mergeddataset")

# Load the dataset (replace with the actual path to your file)
final_dataset <- read.csv("finalDataset.csv") # Ensure "finalDataset.csv" is in your working directory

# 1. View the structure and summary of the dataset
str(final_dataset)
summary(final_dataset)

# Check for missing values
missing_values <- colSums(is.na(final_dataset))
print(missing_values)

# Check distribution of numerical variables
final_dataset %>%
  select_if(is.numeric) %>%
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(x = Value)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  facet_wrap(~Variable, scales = "free") +
  theme_minimal()

# Correlation matrix for numeric variables
correlation_matrix <- cor(data %>% select_if(is.numeric), use = "complete.obs")
print(correlation_matrix)

# 2. Visualize numerical variables: histograms and density plots
numerical_columns <- sapply(final_dataset, is.numeric)
num_data <- final_dataset[, numerical_columns]

# 3. Visualize categorical variables: bar plots
categorical_columns <- sapply(final_dataset, is.factor)
cat_data <- final_dataset[, categorical_columns]


# 4. Correlation heatmap for numerical variables
cor_matrix <- cor(num_data, use = "complete.obs")
corrplot(cor_matrix, method = "color", tl.cex = 0.8, number.cex = 0.8)

# 5. Pairplot using GGally library (optional)
# Install if not available: install.packages("GGally")
library(GGally)
ggpairs(num_data, title = "Pairplot of Numerical Variables")

# Association Rules
# Convert relevant columns to factors (e.g., "CATEGORY" and "SUBCATEGORY")
final_dataset$CATEGORY <- as.factor(final_dataset$CATEGORY)
final_dataset$SUBCATEGORY <- as.factor(final_dataset$SUBCATEGORY)

# Select relevant columns for transactions (e.g., "CATEGORY" and "SUBCATEGORY")
transaction_data <- final_dataset %>%
  select(CATEGORY, SUBCATEGORY)

# Convert the data to a transaction format
transactions <- as(split(transaction_data$SUBCATEGORY, transaction_data$CATEGORY), "transactions")

# Generate association rules using the Apriori algorithm
rules <- apriori(transactions, parameter = list(supp = 0.01, conf = 0.8))

# Inspect the top 10 rules
inspect(rules[1:10])

# Optional: Visualize the rules (requires arulesViz package)
install.packages("arulesViz") # Uncomment if not installed
library(arulesViz)
plot(rules, method = "graph", engine = "htmlwidget")

# Regression
# Linear Regression
# Linear regression model
model_linear <- lm(Purchase ~ Gender + Age + Occupation, data = final_dataset)

# Summary of the model
summary(model_linear)
# Logistic Regression
# Convert a target column to binary (e.g., Promotion Yes/No)
final_dataset$PromotionBinary <- ifelse(final_dataset$PROMOTION == "Yes", 1, 0)

# Logistic regression model
model_logistic <- glm(PromotionBinary ~ Age + Occupation + Purchase, data = final_dataset, family = "binomial")

# Summary of the model
summary(model_logistic)


# Build a decision tree
tree_model <- rpart(PROMOTION ~ Age + Gender + Purchase, data = final_dataset, method = "class")

# Plot the tree
library(rpart.plot)
rpart.plot(tree_model)


# Load library
library(e1071)

# Build a Naive Bayes model
naive_bayes_model <- naiveBayes(PROMOTION ~ Age + Gender + Purchase, data = final_dataset)

# Predictions
predictions <- predict(naive_bayes_model, final_dataset)
table(predictions, final_dataset$PROMOTION)


# Load the necessary library
library(forecast)

# Filter data for time series analysis
ts_data <- ts(final_dataset$Purchase, frequency = 12)

# Fit an ARIMA model
fit <- auto.arima(ts_data)

# Summary of the model
summary(fit)

# Forecast the next 12 months
forecasted <- forecast(fit, h = 12)
plot(forecasted)

# Load necessary libraries
library(tm)
library(wordcloud)

# Create a text corpus
corpus <- Corpus(VectorSource(final_dataset$CustomerReviewContent))

# Preprocess the text
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))

# Term-document matrix
tdm <- TermDocumentMatrix(corpus)
tdm_matrix <- as.matrix(tdm)

# Word cloud
wordcloud(words = rownames(tdm_matrix), freq = rowSums(tdm_matrix), max.words = 100, colors = brewer.pal(8, "Dark2"))

# Install required libraries (if not already installed)
install.packages("tidytext")
install.packages("textdata")
install.packages("dplyr")
install.packages("ggplot2")

library(tidytext)
library(textdata)
library(dplyr)
library(ggplot2)


# Preprocess the CustomerReviewContent column
# Assuming the column name is "CustomerReviewContent"
reviews <- final_dataset %>%
  select(CustomerReviewContent) %>%
  filter(!is.na(CustomerReviewContent)) %>%
  mutate(CustomerReviewContent = tolower(CustomerReviewContent)) %>%
  mutate(CustomerReviewContent = gsub("[[:punct:]]", " ", CustomerReviewContent)) %>%
  mutate(CustomerReviewContent = gsub("[[:digit:]]", " ", CustomerReviewContent))

# Tokenize the text into individual words
reviews_tokenized <- reviews %>%
  unnest_tokens(word, CustomerReviewContent)

# Use the Bing sentiment lexicon
bing_sentiments <- get_sentiments("bing")

# Join with the tokenized dataset to get sentiment scores
sentiment_scores <- reviews_tokenized %>%
  inner_join(bing_sentiments, by = "word") %>%
  count(sentiment, sort = TRUE)

# Plot the sentiment scores
ggplot(sentiment_scores, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Sentiment Distribution", x = "Sentiment", y = "Count") +
  theme_minimal()

# Calculate sentiment score for each review
review_sentiments <- reviews_tokenized %>%
  inner_join(bing_sentiments, by = "word") %>%
  group_by(CustomerReviewContent) %>%
  summarise(sentiment_score = sum(ifelse(sentiment == "positive", 1, -1))) %>%
  arrange(desc(sentiment_score))

# Join the sentiment scores back to the original dataset
final_dataset <- final_dataset %>%
  left_join(review_sentiments, by = "CustomerReviewContent")


# Plot the distribution of sentiment scores
ggplot(review_sentiments, aes(x = sentiment_score)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  labs(title = "Sentiment Score Distribution", x = "Sentiment Score", y = "Frequency") +
  theme_minimal()


