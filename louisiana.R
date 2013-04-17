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

histogram(native)
histogram(hispanic)

