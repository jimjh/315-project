# install.packages('shiny')
library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("US Census 2010"),
  sidebarPanel(),
  mainPanel(
    plotOutput('choropleth')
  )
))