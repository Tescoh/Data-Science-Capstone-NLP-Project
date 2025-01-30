#----------- server.R -----------#
library(shiny)
library(data.table)
library(stringr)
library(textclean)
library(pryr)
library(shinycssloaders)
library(dplyr)

NGRAM_MAX <- 4      # Maximum n-gram length
MIN_COUNT <- 2      # Minimum n-gram frequency

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

server <- function(input, output, session) {
  # Load pre-trained model
  model <- readRDS("optimized_model.rds")
  
  # Reactive predictions with debounce
  predictions <- reactive({
    input_text <- input$user_input
    req(nchar(input_text) > 0)
  
    
    # Use the prediction function from previous steps
    words <- predict_next_word(input_text, model)
    
    # Return formatted predictions
    if(length(words) == 0) return(NULL)
    words[!is.na(words)]
  }) %>% debounce(300)  # 300ms delay
  
  # Render prediction buttons
  output$predictions_ui <- renderUI({
    preds <- predictions()
    if(is.null(preds) || length(preds) == 0) return(NULL)
    
    tagList(
      div(class = "prediction-box",
          h4("Top Predictions:"),
          lapply(preds, function(word) {
            actionButton(paste0("pred_", word), word, 
                        class = "prediction-item",
                        onclick = paste0('$("#user_input").val($("#user_input").val() + " ', word, '")'))
          })
      )
    )
  })
}
