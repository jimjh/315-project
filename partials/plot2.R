output$plot2 <- renderPlot({
  col.vector<-function(variable.2000, variable.2010){
    summary.vec<-summary(variable.2000)
    col.vec<-vector(length=length(variable.2010))
    first<-summary.vec[[1]]
    second<-summary.vec[[2]]
    third<-summary.vec[[4]]
    col.vec[which(variable.2010<first)]<-"yellow"
    col.vec[which(variable.2010<second & variable.2010>=first)]<-"gold"
    col.vec[which(variable.2010<third & variable.2010>=second)]<-"darkgoldenrod2"
    col.vec[which(variable.2010>=third)]<-"darkorange"
    return (col.vec)
  }
  
  louisiana.2000<-louisiana.blkgrp
  louisiana.pop.2000<-louisiana.2000$pop2000/areaPoly(louisiana.2000)
  louisiana.2010<-louisiana.blkgrp10
  louisiana.pop.2010<-louisiana.2010$P0010001/areaPoly(louisiana.2010)
  
  LONGITUDE <- c(-90.29, -89.84)
  LATITUDE  <- c(29.81, 30.10)
  coord.2000s <- coordinates(louisiana.blkgrp)
  coord.2010s <- coordinates(louisiana.blkgrp10)
  
  if (input$race.year=="White"){
    race.2000<-louisiana.2000$nh.white[coords[,1] > LONGITUDE[1] & coords[,1] < LONGITUDE[2] & coords[,2] > LATITUDE[1] & coords[,2] < LATITUDE[2]]
    race.2010<-louisiana.2010$P0050003[coords[,1] > LONGITUDE[1] & coords[,1] < LONGITUDE[2] & coords[,2] > LATITUDE[1] & coords[,2] < LATITUDE[2]]
  } 
  if (input$race.year=="Black"){
    race.2000<-louisiana.2000$nh.black[coords[,1] > LONGITUDE[1] & coords[,1] < LONGITUDE[2] & coords[,2] > LATITUDE[1] & coords[,2] < LATITUDE[2]]
    race.2010<-louisiana.2010$P0050004[coords[,1] > LONGITUDE[1] & coords[,1] < LONGITUDE[2] & coords[,2] > LATITUDE[1] & coords[,2] < LATITUDE[2]]
  }
  if (input$race.year=="American Indian/Native"){
    race.2000<-louisiana.2000$nh.ameri.es[coords[,1] > LONGITUDE[1] & coords[,1] < LONGITUDE[2] & coords[,2] > LATITUDE[1] & coords[,2] < LATITUDE[2]]
    race.2010<-louisiana.2010$P0050005[coords[,1] > LONGITUDE[1] & coords[,1] < LONGITUDE[2] & coords[,2] > LATITUDE[1] & coords[,2] < LATITUDE[2]]
  }
  if (input$race.year=="Asian"){
    race.2000<-louisiana.2000$nh.asian[coords[,1] > LONGITUDE[1] & coords[,1] < LONGITUDE[2] & coords[,2] > LATITUDE[1] & coords[,2] < LATITUDE[2]]
    race.2010<-louisiana.2010$P0050006[coords[,1] > LONGITUDE[1] & coords[,1] < LONGITUDE[2] & coords[,2] > LATITUDE[1] & coords[,2] < LATITUDE[2]]
  }
  if (input$race.year=="Hispanic"){
    race.2000<-louisiana.2000$hispanic.t[coords[,1] > LONGITUDE[1] & coords[,1] < LONGITUDE[2] & coords[,2] > LATITUDE[1] & coords[,2] < LATITUDE[2]]
    race.2010<-louisiana.2010$P0040003[coords[,1] > LONGITUDE[1] & coords[,1] < LONGITUDE[2] & coords[,2] > LATITUDE[1] & coords[,2] < LATITUDE[2]]
  }
  
  # 2000 Data
  # create x and y coordinate vectors the length of the pop vector
  len.2000 <- length(race.2000)
  xcoord.2000s <- rep(NA, len.2000)
  ycoord.2000s <- rep(NA, len.2000)
  
  # separate the x and y coord.2000inates
  xcoord.2000s <- coord.2000s[,1][coord.2000s[,1] > LONGITUDE[1] & coord.2000s[,1] < LONGITUDE[2] & coord.2000s[,2] > LATITUDE[1] & coord.2000s[,2] < LATITUDE[2]]
  ycoord.2000s <- coord.2000s[,2][coord.2000s[,1] > LONGITUDE[1] & coord.2000s[,1] < LONGITUDE[2] & coord.2000s[,2] > LATITUDE[1] & coord.2000s[,2] < LATITUDE[2]]
  
  # weight the x and y coord.2000s based on the pop
  num <- 10
  weighted.2000x <- rep(0, 0)
  weighted.2000y <- rep(0, 0)
  
  for (i in 1:length(race.2000)) {
    x <- rep(0, race.2000[i]/num)
    y <- rep(0, race.2000[i]/num)
    for (j in 1:(race.2000[i]/num)) {
      x[j] <- xcoord.2000s[i]
      y[j] <- ycoord.2000s[i]
    }
    weighted.2000x <- c(weighted.2000x, x)
    weighted.2000y <- c(weighted.2000y, y)
  }
  
  na.weighted.2000x<-which(is.na(weighted.2000x))
  na.weighted.2000y<-which(is.na(weighted.2000y))
  weighted.2000x.no.na<-weighted.2000x[-na.weighted.2000x]
  weighted.2000x.no.na<-weighted.2000x[-na.weighted.2000y]
  
  weighted.2000y.no.na<-weighted.2000y[-na.weighted.2000x]
  weighted.2000y.no.na<-weighted.2000y[-na.weighted.2000y]
  
  weighted.2000x.no.na.finite<-weighted.2000x.no.na[which(is.finite(weighted.2000x.no.na) & is.finite(weighted.2000y.no.na))]
  weighted.2000y.no.na.finite<-weighted.2000y.no.na[which(is.finite(weighted.2000y.no.na) & is.finite(weighted.2000y.no.na))]
  # get the density estimate
  dens.2000 <- kde2d(weighted.2000x.no.na.finite, weighted.2000y.no.na.finite)
  
  # 2010 Data
  # create x and y coordinate vectors the length of the pop vector
  len.2010 <- length(race.2010)
  xcoord.2010s <- rep(NA, len.2010)
  ycoord.2010s <- rep(NA, len.2010)
  
  # separate the x and y coord.2010inates
  xcoord.2010s <- coord.2010s[,1][coord.2010s[,1] > LONGITUDE[1] & coord.2010s[,1] < LONGITUDE[2] & coord.2010s[,2] > LATITUDE[1] & coord.2010s[,2] < LATITUDE[2]]
  ycoord.2010s <- coord.2010s[,2][coord.2010s[,1] > LONGITUDE[1] & coord.2010s[,1] < LONGITUDE[2] & coord.2010s[,2] > LATITUDE[1] & coord.2010s[,2] < LATITUDE[2]]
  
  # weight the x and y coord.2010s based on the pop
  num <- 10
  weighted.2010x <- rep(0, 0)
  weighted.2010y <- rep(0, 0)
  
  for (i in 1:length(race.2010)) {
    x <- rep(0, race.2010[i]/num)
    y <- rep(0, race.2010[i]/num)
    for (j in 1:(race.2010[i]/num)) {
      x[j] <- xcoord.2010s[i]
      y[j] <- ycoord.2010s[i]
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
  
  
  par(mfrow=c(2,1),mar=c(0, 4, 4, 2) + 0.1)
  race.name<-paste(input$race.year,"s",sep="")
  title.2000<-paste("Map of Louisiana 2000 with Showing Population Distribution of",race.name,sep=" ")
  title.2010<-paste("Map of Louisiana 2010 with Showing Population Distribution of",race.name,sep=" ")
  plot(louisiana.2000, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10),
       col=col.vector(race.2000,race.2000),border="grey74")
  if (input$pop_contour == TRUE) {
    # plot the contour overlay on the map showing pop density
    contour(dens.2000, col=rgb(0,0,0,.5), lwd=2, add=T)
  }
  title(title.2000)
  plot(louisiana.2010, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10),
       col=col.vector(race.2000,race.2010),border="grey74")
  if (input$pop_contour == TRUE) {
    # plot the contour overlay on the map showing pop density
    contour(dens.2010, col=rgb(0,0,0,.5), lwd=2, add=T)
  }
  title(title.2010)
  legend("bottomright",
         legend=c("First Quartile","Second Quartile","Third Quartile","Fourth Quartile"),
         text.col=c("yellow","gold","darkgoldenrod2","darkorange"),
         col=c("yellow","gold","darkgoldenrod2","darkorange"),lwd=2)
})