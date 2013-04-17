library(UScensus2010blkgrp)
data(louisiana.blkgrp10)
louisiana<-louisiana.blkgrp10
plot(louisiana, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10))
# Chloropleth of population
choropleth(louisiana, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10))
white<-louisiana$P0030002
asian<-louisiana$P0030005
black<-louisiana$P0030003
native<-louisiana$P0030004
hispanic<-louisiana$P0040001
histogram(white)
plot(louisiana, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10),col=white.col)
