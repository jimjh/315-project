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
  source('partials/histogram.R', local=T)
})