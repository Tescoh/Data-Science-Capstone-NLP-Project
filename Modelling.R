# Required Libraries
library(quanteda)
library(data.table)
library(stringi)
library(textclean)
library(pryr)
library(quanteda.textstats)


# --------------------------
# Parameters (Adjustable)
# --------------------------
SAMPLE_SIZE <- 0.3  # Balance between performance and model size
NGRAM_MAX <- 4      # Maximum n-gram length
MIN_COUNT <- 2      # Minimum n-gram frequency
TOP_NGRAMS <- 200000 # Max n-grams per order to keep
PROFANITY_FILE <- "profanity.txt"

# --------------------------
# Data Loading & Sampling
# --------------------------

load_corpus <- function(files) {
  corpus <- lapply(files, function(f) {
    con <- file(f, "rb")
    text <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
    close(con)
    sample(text, length(text) * SAMPLE_SIZE)
  })
  unlist(corpus)
}

# --------------------------
# Advanced Text Cleaning
# --------------------------

clean_text <- function(text) {
  text %>%
    str_to_lower() %>%
    replace_non_ascii() %>%
    replace_url() %>%
    replace_hash() %>%
    replace_contraction() %>%
    replace_word_elongation() %>%
    str_remove_all("[^[:alpha:][:space:]]") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

# --------------------------
# Efficient N-gram Modeling
# --------------------------

build_ngram_model <- function(corpus) {
  # Create tokens object with quanteda
  toks <- tokens(corpus,
                 remove_punct = TRUE,
                 remove_numbers = TRUE,
                 remove_symbols = TRUE)
  
  # Build n-gram frequency tables
  ngrams_list <- lapply(1:NGRAM_MAX, function(n) {
    ngrams <- tokens_ngrams(toks, n = n) %>%
      unlist(use.names = FALSE)
    
    data.table(ngram = ngrams)[, .(count = .N), by = ngram] %>%
      .[count >= MIN_COUNT] %>%
      .[order(-count)] %>%
      head(TOP_NGRAMS)
  })
  
  # Split n-grams into components
  for(i in seq_along(ngrams_list)) {
    cols <- paste0("word", 1:i)
    ngrams_list[[i]] <- ngrams_list[[i]][, 
                                         (cols) := tstrsplit(ngram, "_", fixed = TRUE)
    ][, ngram := NULL]
  }
  
  names(ngrams_list) <- paste0("ngram", 1:NGRAM_MAX)
  return(ngrams_list)
}

# --------------------------
# Model Optimization
# --------------------------

add_probabilities <- function(model) {
  # Calculate discounted probabilities with Kneser-Ney smoothing
  lapply(model, function(ngram) {
    n <- ncol(ngram) - 1  # Number of words in n-gram
    
    if(n == 1) {
      # For unigrams, use Kneser-Ney continuation probability
      total_bigrams <- sum(model$ngram2$count)
      ngram[, prob := sapply(count, function(c) sum(model$ngram2$word1 == word1)) / total_bigrams]
    } else {
      # For higher n-grams
      context_cols <- head(names(ngram), n-1)
      context_count <- ngram[, .(context_count = sum(count)), by = context_cols]
      
      ngram <- merge(ngram, context_count, by = context_cols)
      ngram[, prob := (count - 0.5) / context_count]
    }
    
    ngram[, c("count", "context_count") := NULL]
    ngram
  })
}

# --------------------------
# Prediction Engine
# --------------------------

# Corrected Prediction Function
predict_next_word <- function(input, model, max_pred = 3) {
  input <- clean_text(input) %>% str_split(" ") %>% unlist()
  
  # Start from highest order n-gram and backoff
  for(i in min(length(input), NGRAM_MAX-1):1) {
    context <- tail(input, i)
    context_str <- paste(context, collapse = " ")
    
    # Look for matching n-grams
    if(i+1 == 2) {
      predictions <- model$ngram2[word1 == context_str]
    } else if(i+1 == 3) {
      predictions <- model$ngram3[word1 == context[1] & word2 == context[2]]
    } else if(i+1 == 4) {
      predictions <- model$ngram4[word1 == context[1] & word2 == context[2] & word3 == context[3]]
    }
    
    if(exists("predictions") && nrow(predictions) > 0) {
      return(head(
        if(i+1 == 2) predictions$word2
        else if(i+1 == 3) predictions$word3
        else if(i+1 == 4) predictions$word4,
        max_pred
      ))
    }
  }
  
  # Fallback to top unigrams
  head(model$ngram1$word1, max_pred)
}
# --------------------------
# Execution Pipeline
# --------------------------

# 1. Load and preprocess data
files <- c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt")
corpus <- load_corpus(files) %>% clean_text()

# 2. Build and optimize model
model <- build_ngram_model(corpus)
model <- add_probabilities(model)

# 3. Size optimization
cat("Model size:", object_size(model) %>% format(units = "MB"), "\n")

# 4. Save optimized model
saveRDS(model, "optimized_model.rds", compress = "xz")

# Example usage
test_phrases <- c(
  "I want to go to the",
  "How are you",
  "The quick brown fox"
)

lapply(test_phrases, function(p) {
  list(input = p, prediction = predict_next_word(p, model))
})



# Updated Example Output
test_phrases <- c(
  "The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
  "You're the reason why I smile everyday. Can you follow me please? It would mean the",
  "Hey sunshine, can you follow me and make me the"
)

lapply(test_phrases, function(p) {
  list(input = p, prediction = predict_next_word(p, model))
})

