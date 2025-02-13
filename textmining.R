install.packages("tm")
install.packages("SnowballC")
install.packages("RColorBrewer")
install.packages("syuzhet")
install.packages("ggplot2")
install.packages("wordcloud")

library(tm)
library(SnowballC)
library(RColorBrewer)
library(syuzhet)
library(ggplot2)

# Load necessary libraries
library(readr)
library(wordcloud)
library(dplyr)  # Load dplyr for %>%

# Update this line of code with the dataset location
setwd("/Users/parthasarathikundu/Downloads")

# Load the dataset
data <- read.csv("all_kindle_review .csv")
data

# Step 2: Data Cleaning and Preprocessing
reviews <- data$reviewText

# Create a Corpus
corpus <- Corpus(VectorSource(reviews))

# Clean the text data
corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace)

# Step 3: Tokenization and Term-Document Matrix
tdm <- TermDocumentMatrix(corpus)
tdm_matrix <- as.matrix(tdm)

# Step 4: Frequent Terms and Word Cloud
frequent_terms <- findFreqTerms(tdm, lowfreq = 50)  # Adjust 'lowfreq' as needed

# Get the frequencies of terms in the term-document matrix
term_freq <- rowSums(tdm_matrix)

# Filter terms based on minimum frequency and remove any NA values
frequent_terms <- names(term_freq[term_freq >= 50])
frequent_term_freqs <- term_freq[frequent_terms]

# Plot Word Cloud with filtered terms
wordcloud(words = frequent_terms, freq = frequent_term_freqs,
          min.freq = 50, max.words = 100, random.order = FALSE)
# Step 5: Sentiment Analysis
sentiments <- get_nrc_sentiment(reviews)
sentiment_summary <- colSums(sentiments)
sentiment_summary
barplot(sentiment_summary, main = "Sentiment Analysis", col = rainbow(10))
sentiments$positive
# Step 6: Visualization of Top Terms (optional)
top_terms <- sort(rowSums(tdm_matrix), decreasing = TRUE)
barplot(top_terms[1:10], las = 2, col = "lightblue", main = "Top 10 Most Frequent Words")


# Calculate total positive and negative words
pos_neg_summary <- data.frame(
  Positive = rowSums(sentiment_summary[, c("positive")]),
  Trust = rowSums(sentiment_summary[, c("trust")])
)

# Plot comparison
barplot(colSums(pos_neg_summary), main = "Positive vs Negative Words", col = c("green", "red"))
