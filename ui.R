# 
# 
#
#

require(shiny)

# Define UI for Ngram word prediction algorithm
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Ngram Next Word Prediction by Ethan Bench"),
  
  # Sidebar with a text input for the source sentence. 
  sidebarLayout(
    sidebarPanel(
       textInput("userText",
                   "Enter word/phrase/sentence:",
                   value = "Text",
                   width = 200,
                   placeholder = "Insert Text Here")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       tableOutput("predTable")
    )
  )
))
