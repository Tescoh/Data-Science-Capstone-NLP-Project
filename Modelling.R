library(quanteda)
library(data.table)
library(dplyr)

# Set data directory (replace with your actual path)
data_dir <- "C:/Users/User/Documents/GitRepos/Data_Science_Capstone_NLP/final/en_US" # **Replace with your data path**

# Function to clean a single file (modified to process in chunks)
clean_file <- function(filepath, chunk_size = 100000) {
  con <- file(filepath, "r")
  on.exit(close(con)) # Ensure connection is closed even if an error occurs
  
  cleaned_chunks <- list()
  i <- 1
  
  while (TRUE) {
    lines <- readLines(con, n = chunk_size)
    if (length(lines) == 0) {
      break # End of file
    }
    
    cleaned_chunk <- clean_text(lines)
    cleaned_chunks[[i]] <- cleaned_chunk
    i <- i + 1
  }
  
  return(unlist(cleaned_chunks))
}

# Clean all three files in chunks
cleaned_blogs <- clean_file(file.path(data_dir, "en_US.blogs.txt"))
cleaned_news <- clean_file(file.path(data_dir, "en_US.news.txt"))
cleaned_twitter <- clean_file(file.path(data_dir, "en_US.twitter.txt"))

# Combine the cleaned data
all_cleaned <- c(cleaned_blogs, cleaned_news, cleaned_twitter)

# Tokenize the combined cleaned data
tokens <- tokens(all_cleaned, what = "word")

# Sample profanity list (replace with a comprehensive one)
file_path <- 'profanity_en.csv'
profanity_list <- read_and_index_first_column(file_path)

# Remove profanity and stopwords
tokens_no_profanity <- tokens_remove(tokens, pattern = profanity_list)
tokens_no_stopwords <- tokens_select(tokens_no_profanity, pattern = stopwords("en"), selection = "remove")

# Save the cleaned tokens for later use
saveRDS(tokens_no_stopwords, "cleaned_tokens.rds")

# Function to create n-grams and their frequencies (optimized)
create_ngram_freq <- function(tokens, n, top_k = NULL) {
  ngrams <- tokens_ngrams(tokens, n = n)
  dfm_ngrams <- dfm(ngrams)
  
  # Keep only the top_k most frequent ngrams if specified
  if (!is.null(top_k)) {
    dfm_ngrams <- dfm_trim(dfm_ngrams, min_termfreq = top_k, termfreq_type = "rank")
  }
  
  ngram_freq <- textstat_frequency(dfm_ngrams)
  ngram_freq_dt <- as.data.table(ngram_freq)
  ngram_freq_dt[, paste0("word", 1:n) := tstrsplit(feature, "_", fixed = TRUE)]
  
  if (n > 1) {
    ngram_freq_dt[, context := do.call(paste, c(.SD, sep = "_")), .SDcols = paste0("word", 1:(n - 1))]
    ngram_freq_dt[, target := get(paste0("word", n))]
    ngram_freq_dt[, frequency := as.numeric(frequency)]
    ngram_freq_dt[, total_count := sum(frequency), by = context]
    ngram_freq_dt[, prob := frequency / total_count]
    ngram_freq_dt <- ngram_freq_dt[, .(context, target, prob)] # Select only relevant columns
  } else {
    ngram_freq_dt[, prob := frequency / sum(frequency)]
    ngram_freq_dt <- ngram_freq_dt[, .(target = feature, prob)] # Select and rename for consistency
  }
  
  return(ngram_freq_dt)
}

# Load or create and save the n-gram frequency tables, keeping only the top 10,000 most frequent ngrams
if (!file.exists("unigram_freq.rds")) {
  unigram_freq <- create_ngram_freq(tokens_no_stopwords, 1, top_k = 10000)
  saveRDS(unigram_freq, "unigram_freq.rds")
} else {
  unigram_freq <- readRDS("unigram_freq.rds")
}

if (!file.exists("bigram_freq.rds")) {
  bigram_freq <- create_ngram_freq(tokens_no_stopwords, 2, top_k = 10000)
  saveRDS(bigram_freq, "bigram_freq.rds")
} else {
  bigram_freq <- readRDS("bigram_freq.rds")
}

if (!file.exists("trigram_freq.rds")) {
  trigram_freq <- create_ngram_freq(tokens_no_stopwords, 3, top_k = 10000)
  saveRDS(trigram_freq, "trigram_freq.rds")
} else {
  trigram_freq <- readRDS("trigram_freq.rds")
}
if (!file.exists("quadgram_freq.rds")) {
  quadgram_freq <- create_ngram_freq(tokens_no_stopwords, 4, top_k = 5000)
  saveRDS(quadgram_freq, "quadgram_freq.rds")
} else {
  quadgram_freq <- readRDS("quadgram_freq.rds")
}

# Updated Prediction Function (with quadgrams and backoff)
predict_next_word <- function(input_phrase, quadgram_freq_dt, trigram_freq_dt, bigram_freq_dt, unigram_freq_dt, lambda = 0.4) {
  input_phrase_cleaned <- clean_text(input_phrase)
  input_tokens <- unlist(strsplit(input_phrase_cleaned, " "))
  
  # Quadgram prediction
  context_quadgram <- paste(tail(input_tokens, 3), collapse = "_")
  predictions <- quadgram_freq_dt[context == context_quadgram]
  
  # Backoff to trigram if no quadgram is found
  if (nrow(predictions) == 0) {
    context_trigram <- paste(tail(input_tokens, 2), collapse = "_")
    predictions <- trigram_freq_dt[context == context_trigram][, prob := prob * lambda]
    
    # Backoff to bigram if no trigram is found
    if (nrow(predictions) == 0) {
      context_bigram <- tail(input_tokens, 1)
      predictions <- bigram_freq_dt[context == context_bigram][, prob := prob * lambda^2]
      
      # Backoff to unigram if no bigram is found
      if (nrow(predictions) == 0) {
        predictions <- unigram_freq_dt[, prob := prob * lambda^3]
      }
    }
  }
  
  predictions <- predictions[order(-prob)]
  return(head(predictions, 3))
}


# Example usage
input_phrase <- "I am going"
predictions <- predict_next_word(input_phrase, quadgram_freq, trigram_freq, bigram_freq, unigram_freq)
print(predictions)