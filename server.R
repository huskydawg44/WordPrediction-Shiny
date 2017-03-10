

require(shiny)
require(dplyr)

# Load in prediction data frames
con<-gzfile("CompressedPreds6.Rdf")
load(con, envir = .GlobalEnv)
close(con)

source("Text_Cleaner.R")
source("NgramPredForShiny.R")




# Define server logic required for Ngram prediction
shinyServer(function(input, output) {

   
  output$predTable <- renderTable({
  userPred<-NgramPred(input$userText)
    #   Provide table from prediction algorithm for output.
       userPred<-as.data.frame(userPred)
       userPred<-mutate(userPred,Rank=1:8)
       names(userPred)[1]<-"Predictions"
       userPred
    
  })
  
})
