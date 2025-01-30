library(quanteda)
library(ggplot2)
library(dplyr)
library(quanteda.textstats)

# Set data directory (replace with your actual path)
data_dir <- "C:/Users/User/Documents/GitRepos/Data_Science_Capstone_NLP/final/en_US" # **Replace with your data path**

# Let's start by loading a sample of the data from each file.
# Function to read a sample of lines from a file
read_sample <- function(filepath, sample_size = 0.01) {
  con <- file(filepath, "r")
  lines <- readLines(con)
  close(con)
  
  n_lines <- length(lines)
  sample_lines <- sample(n_lines, size = round(sample_size * n_lines))
  
  return(lines[sample_lines])
}


# Read a 1% sample from each file
blogs_sample <- read_sample(file.path(data_dir, "en_US.blogs.txt"))
news_sample <- read_sample(file.path(data_dir, "en_US.news.txt"))
twitter_sample <- read_sample(file.path(data_dir, "en_US.twitter.txt"))

# Combine the samples into a single data frame (optional for now)
all_samples <- c(blogs_sample, news_sample, twitter_sample)

cleaned_sample <- clean_text(all_samples)
# Tokenize the cleaned sample
tokens <- tokens(cleaned_sample, what = "word")

# Sample profanity list (replace with a comprehensive one)
read_and_index_first_column <- function(file_path) {
  # Read the CSV file
  data <- read.csv(file_path)
  
  # Index the first column (assuming it's named 'X' or 'V1', or use column index 1)
  first_column <- data[, 1]
  
  return(first_column)
}

# Example usage:
file_path <- 'profanity_en.csv'
profanity_list <- read_and_index_first_column(file_path)

# Remove profanity
tokens_no_profanity <- tokens_remove(tokens, pattern = profanity_list)
tokens_no_stopwords <- tokens_select(tokens_no_profanity, pattern = stopwords("en"), selection = "remove")
# 1. Word Frequencies

# Create a document-feature matrix (DFM)
dfm <- dfm(tokens_no_stopwords)

# Get word frequencies
word_freq <- textstat_frequency(dfm)

# (a) Plot the top 20 most frequent words
top_words <- word_freq[1:20, ]
ggplot(top_words, aes(x = reorder(feature, frequency), y = frequency)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Word") +
  ylab("Frequency") +
  ggtitle("Top 20 Most Frequent Words")

# (b) Calculate cumulative frequencies
word_freq$cumulative_freq <- cumsum(word_freq$frequency)
word_freq$cumulative_prop <- word_freq$cumulative_freq / sum(word_freq$frequency)

# (c) Plot cumulative frequencies
ggplot(word_freq, aes(x = rank, y = cumulative_prop)) +
  geom_line() +
  xlab("Rank") +
  ylab("Cumulative Proportion") +
  ggtitle("Cumulative Word Frequencies")

# 2. N-gram Frequencies (2-grams and 3-grams)

# (a) Create 2-grams
bigrams <- tokens_ngrams(tokens_no_stopwords, n = 2)
dfm_bigrams <- dfm(bigrams)
bigram_freq <- textstat_frequency(dfm_bigrams)

# (b) Create 3-grams
trigrams <- tokens_ngrams(tokens_no_stopwords, n = 3)
dfm_trigrams <- dfm(trigrams)
trigram_freq <- textstat_frequency(dfm_trigrams)

# (c) Plot top 20 bigrams
top_bigrams <- bigram_freq[1:20, ]
ggplot(top_bigrams, aes(x = reorder(feature, frequency), y = frequency)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Bigram") +
  ylab("Frequency") +
  ggtitle("Top 20 Most Frequent Bigrams")

# (d) Plot top 20 trigrams
top_trigrams <- trigram_freq[1:20, ]
ggplot(top_trigrams, aes(x = reorder(feature, frequency), y = frequency)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Trigram") +
  ylab("Frequency") +
  ggtitle("Top 20 Most Frequent Trigrams")

# 3. Dictionary Coverage

# (a) Find the number of unique words needed to cover 50% and 90% of word instances
total_words <- sum(word_freq$frequency)
word_freq <- word_freq[order(-word_freq$frequency), ]  # Sort by frequency (descending)
word_freq$cum_freq <- cumsum(word_freq$frequency)
word_freq$cum_prop <- word_freq$cum_freq / total_words

coverage_50 <- min(which(word_freq$cum_prop >= 0.5))
coverage_90 <- min(which(word_freq$cum_prop >= 0.9))

cat("Words needed to cover 50% of instances:", coverage_50, "\n")
cat("Words needed to cover 90% of instances:", coverage_90, "\n")

# 4. Foreign Language Words (Heuristic Approach)

# (a) Load a list of English words (you can find these online or use the 'words' package)
install.packages("words")
library(words)
data(words)

# (b) Identify potential foreign words (not in the English dictionary)
potential_foreign <- word_freq$feature[!word_freq$feature %in% words$word]

# (c) Calculate the proportion of potential foreign words
prop_foreign <- length(potential_foreign) / nrow(word_freq)
cat("Proportion of potential foreign words:", prop_foreign, "\n")

# 5. Increasing Coverage (Ideas)

# (a) Stemming/Lemmatization: Reduce words to their root form to combine variations of the same word.
# (b) Use a larger dictionary: Incorporate more words into your dictionary, potentially from other corpora.
# (c) Character n-grams: Tokenize into sequences of characters instead of words to handle misspellings and out-of-vocabulary words.