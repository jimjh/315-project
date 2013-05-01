
col.vector<-function(variable){
  summary.vec<-summary(variable)
  col.vec<-vector(length=length(variable))
  first<-summary.vec[[1]]
  second<-summary.vec[[2]]
  third<-summary.vec[[4]]
  col.vec[which(variable<first)]<-"yellow"
  col.vec[which(variable<second & variable>=first)]<-"gold"
  col.vec[which(variable<third & variable>=second)]<-"darkgoldenrod2"
  col.vec[which(variable>=third)]<-"darkorange"
  col.vec[which(variable == 0)] <- "white"
  col.vec[which(is.na(variable))] <- "white"
  return (col.vec)
}



# get the coordinates for the center of every blkgrp


coords <- coordinates(louisiana.blkgrp10)

# get the population for only the block groups in New Orleans
population <- louisiana.blkgrp10$P0010001[coords[,1] > LONGITUDE[1] & coords[,1] < LONGITUDE[2] & coords[,2] > LATITUDE[1] & coords[,2] < LATITUDE[2]]

# create x and y coordinate vectors the length of the pop vector
len <- length(population)
xcoords <- rep(NA, len)
ycoords <- rep(NA, len)

# separate the x and y coordinates
xcoords <- coords[,1][coords[,1] > LONGITUDE[1] & coords[,1] < LONGITUDE[2] & coords[,2] > LATITUDE[1] & coords[,2] < LATITUDE[2]]
ycoords <- coords[,2][coords[,1] > LONGITUDE[1] & coords[,1] < LONGITUDE[2] & coords[,2] > LATITUDE[1] & coords[,2] < LATITUDE[2]]

# weight the x and y coords based on the pop
num <- 10
weightedx <- rep(0, 0)
weightedy <- rep(0, 0)

for (i in 1:length(population)) {
  x <- rep(0, population[i]/num)
  y <- rep(0, population[i]/num)
  for (j in 1:(population[i]/num)) {
    x[j] <- xcoords[i]
    y[j] <- ycoords[i]
  }
  weightedx <- c(weightedx, x)
  weightedy <- c(weightedy, y)
}

# get the density estimate
dens <- kde2d(weightedx, weightedy)

agepop.data <- list('male' = louisiana.blkgrp10$age.male,
                    'female' = louisiana.blkgrp10$age.female)

output$age_vs_pop <- renderPlot({
  par(mfrow=c(1,2))
  plot(louisiana.blkgrp10, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10), col=col.vector(agepop.data[['male']]), border=NA)
  title("Map of Male Age Distribution vs. Population Density in New Orleans (2010)")
  
  legend("top",
         legend=c("First Quartile","Second Quartile","Third Quartile","Fourth Quartile"),
         col=c("yellow","gold","darkgoldenrod2","darkorange"),lwd=3)
  
  if (input$age_contour == TRUE) {
    # plot the contour overlay on the map showing pop density
    contour(dens, col=rgb(0,0,0,.5), lwd=2, add=T)
  }
  
  plot(louisiana.blkgrp10, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10), col=col.vector(agepop.data[['female']]), border=NA)
  title("Map of Female Age Distribution vs. Population Density in New Orleans (2010)")
  
  legend("top",
         legend=c("First Quartile","Second Quartile","Third Quartile","Fourth Quartile"),
         col=c("yellow","gold","darkgoldenrod2","darkorange"),lwd=3)
  
  if (input$age_contour == TRUE) {
    # plot the contour overlay on the map showing pop density
    contour(dens, col=rgb(0,0,0,.5), lwd=2, add=T)
  }
})
