# install.packages('shiny')
library(shiny)

shinyUI(bootstrapPage(
  
  headerPanel("US Census 2010"),
  
  mainPanel(
    # multiple tabs
    tabsetPanel(
      tabPanel('Plot',
               selectInput(inputId = "n_breaks",
                           label = "Number of bins in histogram (approximate):",
                           choices = c(10, 20, 35, 50),
                           selected = 20),
               plotOutput('hist')
      )
    )
  )
  
))