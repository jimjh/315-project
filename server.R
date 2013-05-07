# install.packages('shiny')
# install.packages('SDMTools')
library(shiny)
library(UScensus2010blkgrp)
library(UScensus2010)
library(UScensus2000blkgrp)
library(MASS)
library(SDMTools)

# load data
data(louisiana.blkgrp)
load('louisiana-census2010-plus-acs.RData')

# some global constants
LONGITUDE <- c(-90.29, -89.84)
LATITUDE  <- c(29.81, 30.10)


shinyServer(function(input, output) {
  source('partials/plot2.R', local=T)
  source('partials/race-proportion.R', local=T)
  source('partials/pop-violin-lat.R', local=T)
  source('partials/age-race.R', local=T)
  source('partials/income-race.R', local=T)
  source('partials/age_map_pop_contour.R', local=T)
  source('partials/income_map_pop_contour.R', local=T)
  source('partials/barplot-household.R', local=T)
  source('partials/barplot-raceprop.R', local=T)
})