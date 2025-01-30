# Task 1: Getting and Cleaning the Data

# Step 1: Load the Data (Sample)

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

# Set the directory where the data is located
data_dir <- "path/to/your/data/final/en_US" # Replace with your data path

# Read a 1% sample from each file
blogs_sample <- read_sample(file.path(data_dir, "en_US.blogs.txt"))
news_sample <- read_sample(file.path(data_dir, "en_US.news.txt"))
twitter_sample <- read_sample(file.path(data_dir, "en_US.twitter.txt"))

# Combine the samples into a single data frame (optional for now)
all_samples <- c(blogs_sample, news_sample, twitter_sample)

#Save the samples on your pc
 writeLines(blogs_sample, "blogs_sample.txt")
 writeLines(news_sample, "news_sample.txt")
 writeLines(twitter_sample, "twitter_sample.txt")
 writeLines(all_samples, "all_samples.txt")

# STEP 2: Basic Cleaning

# Now, let's define a function for some basic cleaning and apply it to the samples.
library(stringr)
library(tm)
library(dplyr)

clean_text <- function(text) {
  # 1. Remove URLs
  text <- str_replace_all(text, "http\\S+|www\\.\\S+", "")
  
  # 2. Remove Twitter handles and hashtags (optional - keep for now)
  text <- str_replace_all(text, "@\\S+", "")
   text <- str_replace_all(text, "#\\S+", "")
  
  # 3. Convert to lowercase
  text <- tolower(text)
  
  # 4. Remove punctuation (except apostrophes)
  text <- str_replace_all(text, "[^[:alnum:]'\\s]", "")
  
  # 5. Remove numbers
  text <- removeNumbers(text)
  
  # 6. Remove extra whitespace
  text <- str_replace_all(text, "\\s+", " ")
  text <- str_trim(text)
  
  return(text)
}

# Apply cleaning to the combined sample
cleaned_sample <- clean_text(all_samples)

# Step 3: Tokenization

# We'll use the quanteda package for tokenization.

library(quanteda)

# Tokenize the cleaned sample
tokens <- tokens(cleaned_sample, what = "word")

# (Optional) View the first few tokens
head(tokens)

# Step 4: Profanity Filtering

# You'll need a list of profane words. You can create your own or find one online.
# I'll use a sample list for demonstration.

# Sample profanity list (replace with a comprehensive one)
profanity_list <- c("badword1", "badword2", "badword3") # Add more words

# Remove profanity
tokens_no_profanity <- tokens_remove(tokens, pattern = profanity_list)

