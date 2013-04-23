# install.packages('shiny')
library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("US Census 2010"),
  sidebarPanel(
    selectInput(inputId = "n_breaks",
                label = "Number of bins in histogram (approximate):",
                choices = c(10, 20, 35, 50),
                selected = 20)
  ),
  mainPanel(
    plotOutput('hist')
  )
))