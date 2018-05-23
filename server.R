library(shiny)
source("sentiments.R")

server <- function(input, output, session) {
  output$contents <- renderTable({

    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL)
    
    d <- readEmails(inFile$datapath)
    
    output$contents <- renderDataTable({
      d
    })
    
    
    output$contents<-DT::renderDataTable({
      DT::datatable(d,
                    filter = list(position = "top"),
                    options = list(searchHighlight=TRUE,
                                   server = TRUE,
                                   processing = FALSE))
      })
    
    
    # tabPanel 2 - Top words
    observe({
      updateSelectInput(session, inputId = 'wordlength', label = 'Minimum word length',
                        choices = c(1:5), selected = 3)
      updateSelectInput(session, inputId = 'top', label = 'Top n words to show',
                        choices = c(1:5), selected = 15)
    })
    
    output$wordCount <-renderPlot({
      wordFreq(df =d, wordlength=input$wordlength, top=input$top)
      
    })
    
    # tabPanel 3 - Sentiments
    observe({
      updateSelectInput(session, inputId = 'method1', label = 'Method',
                        choices = c("nrc", 'bing', 'loughran'), selected = 'loughran')
      updateSelectInput(session, inputId = 'top_recipients', label = 'Top n recipients',
                        choices = c(1:10), selected = 5)
    })
    
    output$sentiments <-renderPlot({
      emailSentiments(df = d, top_recipients = input$top_recipients, method = input$method1)
    })
    
    # tabPanel 4 - Contributions
    observe({
      updateSelectInput(session, inputId = 'method2', label = 'Method',
                        choices = c("nrc", 'bing', 'loughran'), selected = 'loughran')
      updateSelectInput(session, inputId = 'top_words', label = 'Top n words in each category',
                        choices = c(1:10), selected = 5)
    })
    
    # ctable <- contributions(df = d, method = input$method2, top_words = input$top_words )
    # output$contributionstbl<-DT::renderDataTable({
    #   DT::datatable(ctable,
    #                 filter = list(position = "top"),
    #                 options = list(searchHighlight=TRUE,
    #                                server = TRUE,
    #                                processing = FALSE))
    # })
    # 
    output$contributions <-renderPlot({
      contributions(df = d, method = input$method2, top_words = input$top_words )
    })
    
  })

}