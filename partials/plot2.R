output$plot2 <- renderPlot({
  col.vector<-function(variable){
    col.vec<-vector(length=length(variable))
    col.vec[which(variable<10)]<-"yellow"
    col.vec[which(variable<100 & variable>=10)]<-"gold"
    col.vec[which(variable<700 & variable>=100)]<-"darkgoldenrod2"
    col.vec[which(variable<1500 & variable>=700)]<-"darkorange"
    col.vec[which(variable>=1500)]<-"darkorange3"
    col.vec[which(is.na(variable))]<-"white"
    col.vec[which(variable==0)]<-"white"
    return (col.vec)
  }
  
  louisiana.2000<-louisiana.blkgrp
  louisiana.pop.2000<-louisiana.2000$pop2000/areaPoly(louisiana.2000)
  louisiana.2010<-louisiana.blkgrp10
  louisiana.pop.2010<-louisiana.2010$P0010001/areaPoly(louisiana.2010)
  
  LONGITUDE <- c(-90.29, -89.84)
  LATITUDE  <- c(29.81, 30.10)
  coords.2000 <- coordinates(louisiana.blkgrp)
  coords.2010 <- coordinates(louisiana.blkgrp10)
  
  if (input$race.year=="White"){
    race.2000<-louisiana.2000$nh.white[coords.2000[,1] > LONGITUDE[1] & coords.2000[,1] < LONGITUDE[2] & coords.2000[,2] > LATITUDE[1] & coords.2000[,2] < LATITUDE[2]]
    race.2010<-louisiana.2010$P0050003[coords.2010[,1] > LONGITUDE[1] & coords.2010[,1] < LONGITUDE[2] & coords.2010[,2] > LATITUDE[1] & coords.2010[,2] < LATITUDE[2]]
    race.2000.all<-louisiana.2000$nh.white
    race.2010.all<-louisiana.2010$P0050003
  } 
  if (input$race.year=="Black"){
    race.2000<-louisiana.2000$nh.black[coords.2000[,1] > LONGITUDE[1] & coords.2000[,1] < LONGITUDE[2] & coords.2000[,2] > LATITUDE[1] & coords.2000[,2] < LATITUDE[2]]
    race.2010<-louisiana.2010$P0050004[coords.2010[,1] > LONGITUDE[1] & coords.2010[,1] < LONGITUDE[2] & coords.2010[,2] > LATITUDE[1] & coords.2010[,2] < LATITUDE[2]]
    race.2000.all<-louisiana.2000$nh.black
    race.2010.all<-louisiana.2010$P0050004
  }
  if (input$race.year=="American Indian/Native"){
    race.2000<-louisiana.2000$nh.ameri.es[coords.2000[,1] > LONGITUDE[1] & coords.2000[,1] < LONGITUDE[2] & coords.2000[,2] > LATITUDE[1] & coords.2000[,2] < LATITUDE[2]]
    race.2010<-louisiana.2010$P0050005[coords.2010[,1] > LONGITUDE[1] & coords.2010[,1] < LONGITUDE[2] & coords.2010[,2] > LATITUDE[1] & coords.2010[,2] < LATITUDE[2]]
    race.2000.all<-louisiana.2000$nh.ameri.es
    race.2010.all<-louisiana.2010$P0050005
  }
  if (input$race.year=="Asian"){
    race.2000<-louisiana.2000$nh.asian[coords.2000[,1] > LONGITUDE[1] & coords.2000[,1] < LONGITUDE[2] & coords.2000[,2] > LATITUDE[1] & coords.2000[,2] < LATITUDE[2]]
    race.2010<-louisiana.2010$P0050006[coords.2010[,1] > LONGITUDE[1] & coords.2010[,1] < LONGITUDE[2] & coords.2010[,2] > LATITUDE[1] & coords.2010[,2] < LATITUDE[2]]
    race.2000.all<-louisiana.2000$nh.asian
    race.2010.all<-louisiana.2010$P0050006
  }
  if (input$race.year=="Hispanic"){
    race.2000<-louisiana.2000$hispanic.t[coords.2000[,1] > LONGITUDE[1] & coords.2000[,1] < LONGITUDE[2] & coords.2000[,2] > LATITUDE[1] & coords.2000[,2] < LATITUDE[2]]
    race.2010<-louisiana.2010$P0040003[coords.2010[,1] > LONGITUDE[1] & coords.2010[,1] < LONGITUDE[2] & coords.2010[,2] > LATITUDE[1] & coords.2010[,2] < LATITUDE[2]]
    race.2000.all<-louisiana.2000$hispanic.t
    race.2010.all<-louisiana.2010$P0040003
  }
  
  
  # 2000 Data
  # create x and y coordinate vectors the length of the pop vector
  len.2000 <- length(race.2000)
  xcoords.2000 <- rep(NA, len.2000)
  ycoords.2000 <- rep(NA, len.2000)
  
  # separate the x and y coord.2000inates
  xcoords.2000 <- coords.2000[,1][coords.2000[,1] > LONGITUDE[1] & coords.2000[,1] < LONGITUDE[2] & coords.2000[,2] > LATITUDE[1] & coords.2000[,2] < LATITUDE[2]]
  ycoords.2000 <- coords.2000[,2][coords.2000[,1] > LONGITUDE[1] & coords.2000[,1] < LONGITUDE[2] & coords.2000[,2] > LATITUDE[1] & coords.2000[,2] < LATITUDE[2]]
  
  # weight the x and y coords.2000 based on the pop
  num <- 10
  weighted.2000x <- rep(0, 0)
  weighted.2000y <- rep(0, 0)
  
  for (i in 1:length(race.2000)) {
    x <- rep(0, race.2000[i]/num)
    y <- rep(0, race.2000[i]/num)
    for (j in 1:(race.2000[i]/num)) {
      x[j] <- xcoords.2000[i]
      y[j] <- ycoords.2000[i]
    }
    weighted.2000x <- c(weighted.2000x, x)
    weighted.2000y <- c(weighted.2000y, y)
  }
  
  na.weighted.2000x<-which(is.na(weighted.2000x))
  na.weighted.2000y<-which(is.na(weighted.2000y))
  if (length(na.weighted.2000x)!=0 & length(na.weighted.2000y)!=0){
    weighted.2000x.no.na<-weighted.2000x[-na.weighted.2000x]
    weighted.2000x.no.na<-weighted.2000x[-na.weighted.2000y]
    
    weighted.2000y.no.na<-weighted.2000y[-na.weighted.2000x]
    weighted.2000y.no.na<-weighted.2000y[-na.weighted.2000y]
    weighted.2000x.no.na.finite<-weighted.2000x.no.na[which(is.finite(weighted.2000x.no.na) & is.finite(weighted.2000y.no.na))]
    weighted.2000y.no.na.finite<-weighted.2000y.no.na[which(is.finite(weighted.2000y.no.na) & is.finite(weighted.2000y.no.na))]
    
    dens.2000 <- kde2d(weighted.2000x.no.na.finite, weighted.2000y.no.na.finite)
  } else {
    dens.2000 <- kde2d(weighted.2000x, weighted.2000y)
  }
  # 2010 Data
  # create x and y coordinate vectors the length of the pop vector
  len.2010 <- length(race.2010)
  xcoords.2010 <- rep(NA, len.2010)
  ycoords.2010 <- rep(NA, len.2010)
  
  # separate the x and y coord.2010inates
  xcoords.2010 <- coords.2010[,1][coords.2010[,1] > LONGITUDE[1] & coords.2010[,1] < LONGITUDE[2] & coords.2010[,2] > LATITUDE[1] & coords.2010[,2] < LATITUDE[2]]
  ycoords.2010 <- coords.2010[,2][coords.2010[,1] > LONGITUDE[1] & coords.2010[,1] < LONGITUDE[2] & coords.2010[,2] > LATITUDE[1] & coords.2010[,2] < LATITUDE[2]]
  
  # weight the x and y coords.2010 based on the pop
  num <- 10
  weighted.2010x <- rep(0, 0)
  weighted.2010y <- rep(0, 0)
  
  for (i in 1:length(race.2010)) {
    x <- rep(0, race.2010[i]/num)
    y <- rep(0, race.2010[i]/num)
    for (j in 1:(race.2010[i]/num)) {
      x[j] <- xcoords.2010[i]
      y[j] <- ycoords.2010[i]
    }
    weighted.2010x <- c(weighted.2010x, x)
    weighted.2010y <- c(weighted.2010y, y)
  }
  
  # get the density estimate
  na.weighted.2010x<-which(is.na(weighted.2010x))
  na.weighted.2010y<-which(is.na(weighted.2010y))
  if (length(na.weighted.2010x)!=0 & length(na.weighted.2010y)!=0){
    weighted.2010x.no.na<-weighted.2010x[-na.weighted.2010x]
    weighted.2010x.no.na<-weighted.2010x[-na.weighted.2010y]
    
    weighted.2010y.no.na<-weighted.2010y[-na.weighted.2010x]
    weighted.2010y.no.na<-weighted.2010y[-na.weighted.2010y]
    weighted.2010x.no.na.finite<-weighted.2010x.no.na[which(is.finite(weighted.2010x.no.na) & is.finite(weighted.2010y.no.na))]
    weighted.2010y.no.na.finite<-weighted.2010y.no.na[which(is.finite(weighted.2010y.no.na) & is.finite(weighted.2010y.no.na))]
    
    dens.2010 <- kde2d(weighted.2010x.no.na.finite, weighted.2010y.no.na.finite)
  } else {
    dens.2010 <- kde2d(weighted.2010x, weighted.2010y)
  }
  
  
  par(mfrow=c(1,2),mar=c(5, 4, 4, 2) + 0.1)
  race.name<-paste(input$race.year,"s",sep="")
  title.2000<-paste("Map of Louisiana 2000 with Showing Population Distribution of",race.name,sep=" ")
  title.2010<-paste("Map of Louisiana 2010 with Showing Population Distribution of",race.name,sep=" ")
  
  if (input$pop_contour == TRUE) {
    # plot the contour overlay on the map showing pop density
    plot(louisiana.2000, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10),
         col=col.vector(race.2000.all),border=NA)
    contour(dens.2000, col=rgb(0,0,0,.5), lwd=2, add=T)
  } else {
    plot(louisiana.2000, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10),
         col=col.vector(race.2000.all),border="gray74")
  }
  title(title.2000)
  if (input$pop_contour == TRUE) {
    # plot the contour overlay on the map showing pop density
    plot(louisiana.2010, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10),
         col=col.vector(race.2010.all),border=NA)
    contour(dens.2010, col=rgb(0,0,0,.5), lwd=2, add=T)
  } else {
    plot(louisiana.2010, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10),
         col=col.vector(race.2010.all),border="gray74")
  }
  title(title.2010)
  legend("bottomright",
         legend=c("< 10","10-100","100-700","700-1500", "> 1500"),
         col=c("yellow","gold","darkgoldenrod2","darkorange","darkorange3"),lwd=2)
})