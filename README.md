# Next Word Prediction App

[![R](https://img.shields.io/badge/R-%2764.svg?logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-1.7.0-blue?logo=rstudio)](https://shiny.rstudio.com/)

An intelligent text prediction application powered by n-gram language models and Natural Language Processing (NLP) techniques.

## Features

- Real-time next word prediction as you type
- Professional-grade text cleaning and preprocessing
- Optimized n-gram model with Kneser-Ney smoothing
- Interactive Shiny web interface
- Comprehensive EDA and technical documentation

## File Structure

```
.
├── App_Presentation_Pitch/               # Presentation materials
│   ├── Presentation_pitch.Rmd            # Pitch R Markdown source
│   └── Presentation_pitch.html           # Compiled presentation
│
├── EDA_Report_files/                     # EDA visualizations
│   └── figure-html/                      # Generated plots
│       ├── cumulative_word_coverage-1.png
│       └── word_frequency-1.png
│   ├── EDA_Report.html                   # Compiled EDA report
│   └── EDA_Report.Rmd                    # EDA report source
│
│
├── Prediction_APP/                       # Shiny application
│   ├── rsconnect/                        # Deployment configuration
│   ├── optimized_model.rds               # Trained prediction model
│   ├── server.R                          # App backend logic
│   └── ui.R                              # App frontend interface
│
├── .gitignore                            # Version control exclusions
├── Clean_and_Get_Data.R                  # Data preprocessing script
├── Data_Science_Capstone_NLP.Rproj       # RStudio project file
├── EDA.R                                 # Exploratory analysis script
├── Modelling.R                           # Model training code
└── profanity.txt                         # Profanity filter list
```

## Installation

1. Clone repository:
```bash
git clone https://github.com/yourusername/text-prediction-app.git
```

2. Install required R packages:
```r
install.packages(c("shiny", "rmarkdown", "data.table", "stringr", "textclean"))
```

## Usage

### Run Shiny App Locally
```r
shiny::runApp("Prediction_APP")
```

### Reproduce EDA
```r
rmarkdown::render("EDA_Report.Rmd")
```

### Rebuild Presentation
```r
rmarkdown::render("App_Presentation_Pitch/Presentation_pitch.Rmd")
```

## Links

- 🔗 [Live Application](<https://ml73o6-mohammed-teslim.shinyapps.io/Prediction_App/>)
- 📊 [EDA Report](https://yourusername.github.io/repo/EDA_Report.html)
- 🎥 [Presentation Pitch](https://yourusername.github.io/repo/App_Presentation_Pitch/Presentation_pitch.html)

## License
open

*Created by Teslim Mohammed - 2025*
