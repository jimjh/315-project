# install.packages('shiny')
library(shiny)
library(UScensus2010blkgrp)
library(UScensus2010)
library(MASS)

# load data
load('louisiana-census2010-plus-acs.RData')

# some global constants
LONGITUDE <- c(-90.29, -89.84)
LATITUDE  <- c(29.81, 30.10)

shinyServer(function(input, output) {
  source('partials/histogram.R', local=T)
  source('partials/age-race.R', local=T)
  source('partials/income-race.R', local=T)
  source('partials/age_pop.R', local=T)
  source('partials/income_pop.R', local=T)
  source('partials/pop.R', local=T)
  source('partials/agepopscatter.R', local=T)

})