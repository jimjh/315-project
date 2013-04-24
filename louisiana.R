# Question 1
# Load data
library(UScensus2010blkgrp)
library(UScensus2010)
data(louisiana.blkgrp10)
data(louisiana.blkgrp)

# Question 2
louisiana<-louisiana.blkgrp10
louisiana2000<-louisiana.blkgrp
dim(louisiana)
dim(louisiana2000)
dim(louisiana2000)[1]-dim(louisiana)[1]
dim(louisiana@data)[1]-dim(louisiana2000@data)[1]

The 2000 data has 40 more rows.

length(unique(louisiana$tract))
length(unique(louisiana2000$tract))
There are 35 more tracts in the 2010 data than in the 2000 data.

length(unique(louisiana$blkgrp))
length(unique(louisiana2000$blkgrp))
There is 1 more tract in the 2010 data than in the 2000 data.


# Question 3
names(louisiana)
names(louisiana2000)

pop2000<-louisiana2000$pop2000
pop2010<-louisiana$P0010001
white2000<-louisiana2000$white
white2010<-louisiana$P0050003
owneroccupied2000<-louisiana$hh.owner
owneroccupied2010<-louisiana2010$H0120002


# Question 4
summary(pop2000)
summary(pop2010)

col.vector<-function(variable){
  summary.vec<-summary(variable)
  col.vec<-vector(length=length(variable))
  first<-summary.vec[[1]]
  second<-summary.vec[[2]]
  third<-summary.vec[[4]]
  col.vec[which(variable<first)]<-"gold"
  col.vec[which(variable<second & variable>=first)]<-"darkgoldenrod2"
  col.vec[which(variable<third & variable>=second)]<-"darkorange"
  col.vec[which(variable>=third)]<-"firebrick2"
  return (col.vec)
}

choropleth(louisiana2000, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10),col=col.vector(pop2000))

# Chloropleth of population
choropleth(louisiana, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10))
white<-louisiana$P0030002
asian<-louisiana$P0030005
black<-louisiana$P0030003
native<-louisiana$P0030004
hispanic<-louisiana$P0040001

# White
histogram(white)
col.vector.white<-function(white){
  col.vec<-vector(length=length(white))
  col.vec[which(white<400)]<-"gold"
  col.vec[which(white<700 & white>=400)]<-"darkgoldenrod2"
  col.vec[which(white<1000 & white>=700)]<-"darkorange"
  col.vec[which(white>=1000)]<-"firebrick2"
  return (col.vec)
}
plot(louisiana, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10),col=col.vector.white(white))


# Asian
summary(asian)
col.vector.asian<-function(asian){
  col.vec<-vector(length=length(asian))
  col.vec[which(asian<1)]<-"gold"
  col.vec[which(asian<6 & asian>=1)]<-"darkgoldenrod2"
  col.vec[which(asian<18 & asian>=6)]<-"darkorange"
  col.vec[which(asian>=18)]<-"firebrick2"
  return (col.vec)
}
plot(louisiana, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10),col=col.vector.asian(asian))


# Black
summary(black)
col.vector.black<-function(black){
  col.vec<-vector(length=length(black))
  col.vec[which(black<87)]<-"gold"
  col.vec[which(black<286 & black>=87)]<-"darkgoldenrod2"
  col.vec[which(black<622 & black>=286)]<-"darkorange"
  col.vec[which(black>=622)]<-"firebrick2"
  return (col.vec)
}
plot(louisiana, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10),col=col.vector.black(black))

# Native
summary(native)
col.vector.native<-function(native){
  col.vec<-vector(length=length(native))
  col.vec[which(native<1)]<-"gold"
  col.vec[which(native<4 & native>=1)]<-"darkgoldenrod2"
  col.vec[which(native<8 & native>=4)]<-"darkorange"
  col.vec[which(native>=8)]<-"firebrick2"
  return (col.vec)
}
plot(louisiana, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10),col=col.vector.native(native))

# Hispanic
summary(hispanic)
col.vector.hispanic<-function(hispanic){
  col.vec<-vector(length=length(hispanic))
  col.vec[which(hispanic<810)]<-"gold"
  col.vec[which(hispanic<1160 & hispanic>=810)]<-"darkgoldenrod2"
  col.vec[which(hispanic<1660 & hispanic>=1160)]<-"darkorange"
  col.vec[which(hispanic>=1660)]<-"firebrick2"
  return (col.vec)
}
plot(louisiana, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10),col=col.vector.hispanic(hispanic))


# Population Shares
plot(louisiana, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10),col=white.col)

