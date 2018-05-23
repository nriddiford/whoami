#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("sentiments.R")

server <- function(input, output, session) {
  output$contents <- renderTable({

    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL)
    
    d <- readEmails(inFile$datapath)
    
    
    # output$contents <- DT::renderDataTable(
    #   d,
    #   options = list(scrollX = TRUE)
    # )
    # Main page
    output$contents <- renderDataTable({
      d
    })
    

    observe({
      updateSelectInput(session, inputId = 'wordlength', label = 'Minimum word length',
                        choices = c(1:5), selected = 3)
      updateSelectInput(session, inputId = 'top', label = 'Top n words to show',
                        choices = c(1:5), selected = 15)
    })
    
    # tabPanel 2 - Top words
    output$wordCount <-renderPlot({
      wordFreq(df =d, wordlength=input$wordlength, top=input$top)
      
    })
    
    # tabPanel 3 - Top words
    observe({
      updateSelectInput(session, inputId = 'method', label = 'Method',
                        choices = c("nrc", 'bing', 'loughran'), selected = 'loughran')
      updateSelectInput(session, inputId = 'recipients', label = 'Top n recipients',
                        choices = c(1:10), selected = 5)
    })
    
   
    # tabPanel 1 - Number of messages
    output$sentiments <-renderPlot({
      emailSentiments(df = d, recipients = input$recipients, method = input$method)
    })
  })

}