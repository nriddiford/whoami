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
    
    # Main page
    output$contents <- renderTable({
      head(d, 25)
    })
    
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