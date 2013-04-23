# install.packages('shiny')
library(shiny)
library(UScensus2010blkgrp)
library(UScensus2010)

# load data
data(louisiana.blkgrp10)
louisiana.10 <- louisiana.blkgrp10

# some global constants
LONGITUDE <- c(-90.29, -89.84)
LATITUDE  <- c(29.81, 30.10)

shinyServer(function(input, output) {
  output$choropleth <- renderPlot({
    choropleth(louisiana.10, xlim=LONGITUDE, ylim=LATITUDE)
  })
})