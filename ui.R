library(shiny)
library("shinyWidgets")
library(text2vec)
library(glmnet)
library(shiny)
library(tidyverse)
library("shinycssloaders")
library(DT)
fluidPage(
  titlePanel("Mammal-parasite paper recommender"),
  #title = "Search Bar",
  title = "Welcome to the “Mammal-parasite paper recommender",
  helpText(" Type in any  string (e.g., 'hantavirus', 'rodent virus', 'bat')."),
  helpText("The results will be PubMed papers sorted according to their ‘classification_score’, which is the probability that a given paper contains mammal-parasite interactions."),

  
  textInput(inputId = "searchme",label =  "Search bar", placeholder = "Search term"),
  actionButton("go", "Go"),
  sliderInput("obs", "Number of items searched:",
              min = 10, max = 300, value = 20
  ),
  fluidRow( textOutput("selected_var") ),
  
  

  fluidRow(
    column(8,
           verbatimTextOutput("text"),
           br(),
           br(),
           p("This prototype service uses a machine learning model trained
             on titles and abstract from the Global Mammal Parasite Database 
             (Stephens et al. 2017) to assess the similarity of given papers 
             and thus the probability that they contain novel interaction data.
             Here is a description of our (methods)[insert link].", 
             
             "This work was funded by NIH ", 
             a("(1R21AI164268-01)", 
               href = "https://reporter.nih.gov/search/jHonNExiyEulTWBDs1zc-Q/project-details/10289637"),
             "'Intelligently predicting viral spillover risks from bats and other wild mammals' awarded to Arizona State University.
")
    )
  ),
  
  fluidRow( 
    shinycssloaders::withSpinner(
      DT::DTOutput(
        "tablepubmedsearch")
    ))
  
)

