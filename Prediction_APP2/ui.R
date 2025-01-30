#----------- ui.R -----------#
library(shiny)
library(bslib)
library(shinycssloaders)

ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "minty",
    primary = "#2c3e50",
    success = "#18bc9c",
    "border-radius" = "0.5rem",
    "progress-height" = "0.4rem",
    "enable-gradients" = TRUE
  ),
  
  tags$head(
    tags$style(HTML("
      .gradient-header {
        background: linear-gradient(45deg, #2c3e50, #3498db);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        padding-bottom: 0.5rem;
        border-bottom: 3px solid #f8f9fa;
      }
      
      .prediction-card {
        border: none;
        box-shadow: 0 0.5rem 1rem rgba(0, 0, 0, 0.15);
        transition: transform 0.2s;
      }
      
      .prediction-item {
        transition: all 0.2s ease;
        border: 1px solid #dee2e6;
      }
      
      .prediction-item:hover {
        transform: translateY(-2px);
        box-shadow: 0 0.5rem 1rem rgba(0, 0, 0, 0.15);
      }
      
      .input-group {
        box-shadow: 0 0.5rem 1rem rgba(0, 0, 0, 0.1);
        border-radius: 2rem;
      }
      
      #user_input {
        border-radius: 2rem !important;
        padding: 1.5rem;
        font-size: 1.1rem;
      }
    "))
  ),
  
  div(class = "container py-5",
      style = "max-width: 800px;",
      
      h1(class = "text-center gradient-header mb-5 display-4 fw-bold",
         "Next Word Predictor"),
      
      div(class = "input-group mb-4",
          textInput("user_input", 
                    label = NULL,
                    placeholder = "Start typing your text here...",
                    width = "100%") %>%
            tagAppendAttributes(class = "form-control-lg")
      ),
      
      uiOutput("predictions_ui") %>%
        withSpinner(type = 6, color = "#18bc9c"),
      
      div(class = "text-muted mt-4 text-center",
          tags$small(
            "Built using n-gram language model with Kneser-Ney smoothing",
            br(),
            "Predictions update as you type (300ms delay)",
            br(), br(),
            HTML("&copy; 2025 Teslim Mohammed"),
            br(),
            tags$span(style = "font-size:0.8em;", 
                      "All rights reserved")
          )
      )
  )
)