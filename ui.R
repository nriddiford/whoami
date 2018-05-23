#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
  tabsetPanel(
    # Tab 1
    tabPanel("Upload File",
             titlePanel("Upload your Emails log"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file', 'Select your Email chat log',
                           accept=c(".tsv", ".txt")
                 ),
                 
                 tags$div(class="header", checked=NA,
                          tags$p("To see instructions on how to generate this file please: "),
                          tags$a(href="https://github.com/nriddiford/whoami/blob/master/README.md", "Click Here!")
                 ),
                 tags$hr()
               ),
               mainPanel(
                 h3("Whoami? - an online tool to perform Sentiment Analysis on your emails"),
                 p("To start, you need to extract your emails (follow link in 'Click Here!' to see further details)"),
                 p("Once the file is uploaded, click on the tabs above to see different analyses of your emails"),
                 dataTableOutput('contents')
               )
             )
    ),
    
    # Tab 3
    tabPanel("Word Frequency",
             pageWithSidebar(
               headerPanel('Most common words'),
               sidebarPanel(
                 
                 sliderInput("wordlength", "Minimum word length",
                             min = 2, max = 10, ""),
                 sliderInput("top", "Top n words",
                             min = 5, max = 50, "")
                ),
               mainPanel(
                 plotOutput('wordCount')
               )
             )
    ),
    
    # Tab 2
    tabPanel("Sentiments",
             pageWithSidebar(
               headerPanel('Sentiment Analysis of emails'),
               sidebarPanel(
                 selectInput('method', 'Method', ""),
                 sliderInput("recipients", "Top n recipients",
                             min = 1, max = 10, "")
               ),
               mainPanel(
                 plotOutput('sentiments')
               )
             )
    )
  )
)
