library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Text Predictor"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("sentence", "Sentence")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      strong(textOutput("prediciton"))
    )
  )
))