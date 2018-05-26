#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme=shinytheme('flatly'),
  navbarPage("whoami"),
  tabsetPanel(
    # Tab 1
    tabPanel("Upload File",
             titlePanel("Upload your Emails log"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file', 'Select your Email chat log'),
                 tags$div(class="header", checked=NA,
                          tags$p("To see instructions on how to generate this file please: "),
                          tags$a(href="https://github.com/nriddiford/whoami/blob/master/README.md", "Click Here!")
                 )
               ),
               mainPanel(
                 h3("Whoami? - an online tool to perform Sentiment Analysis on your emails"),
                 p("To start, you need to extract your emails (follow link in 'Click Here!' to see further details)"),
                 p("Once the file is uploaded, click on the tabs above to see different analyses of your emails"),
                 DT::DTOutput('contents')
              )
            )
    ),
    
    # Tab 2
    tabPanel("Word Frequency",
             pageWithSidebar(
               headerPanel('Most common words'),
               sidebarPanel(
                 sliderInput("wordlength", "Minimum word length",
                             min = 2, max = 10, ""),
                 tags$hr(),
                 sliderInput("top", "Top n words",
                             min = 5, max = 50, "")
                ),
               
               mainPanel(
                 plotOutput('wordCount')
               )
              
            )
    ),
    
    # Tab 3
    tabPanel("Sentiments",
             plotOutput('sentiments'),
             fluidRow(
               column(3,
             # pageWithSidebar(
             #   headerPanel('Sentiment Analysis of emails'),
             #   sidebarPanel(
                 selectInput('method1', 'Method', "")
               ),
                column(3,
                 sliderInput("top_recipients", "Top n recipients",
                             min = 1, max = 10, "")
                )
             )
               # ,
               # mainPanel(
               #   plotOutput('sentiments')
               # )
             # )
    ),
    
    # Tab 4
    tabPanel("Contributions",
             pageWithSidebar(
               headerPanel('Major contributing words'),
               sidebarPanel(
                 selectInput('method2', 'Method', ""),
                 sliderInput("top_words", "Top n words in each category",
                             min = 1, max = 10, "")
               ),
               fluidPage(
                 plotOutput('contributions')
                 # DT::dataTableOutput('contributionstbl')
               )
             )
          )

  )
 )
)
