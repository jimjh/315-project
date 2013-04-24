# install.packages('shiny')
library(shiny)
library(UScensus2010blkgrp)
library(UScensus2010)

# load data
load('louisiana-census2010-plus-acs.RData')

# some global constants
LONGITUDE <- c(-90.29, -89.84)
LATITUDE  <- c(29.81, 30.10)

shinyServer(function(input, output) {
  output$hist <- renderPlot({
    hist(louisiana.blkgrp10$income.male, breaks=as.numeric(input$n_breaks))
  })
})