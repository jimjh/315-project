output$race.lat <- renderPlot({
  # Getting race population
  louisiana.2000<-louisiana.blkgrp
  louisiana.2010<-louisiana.blkgrp10
  if (input$race.lat=="White"){
    race.2000<-louisiana.2000$nh.white
    race.2010<-louisiana.2010$P0050003
    prop.2000<-race.2000/louisiana.2000$pop2000*100
    prop.2010<-race.2010/louisiana.2010$P0010001*100
  }
  if (input$race.lat=="Black"){
    race.2000<-louisiana.2000$nh.black
    race.2010<-louisiana.2010$P0050004
    prop.2000<-race.2000/louisiana.2000$pop2000*100
    prop.2010<-race.2010/louisiana.2010$P0010001*100
  }
  if (input$race.lat=="American Indian/Native"){
    race.2000<-louisiana.2000$nh.ameri.es
    race.2010<-louisiana.2010$P0050005
    prop.2000<-race.2000/louisiana.2000$pop2000*100
    prop.2010<-race.2010/louisiana.2010$P0010001*100
  }
  if (input$race.lat=="Asian"){
    race.2000<-louisiana.2000$nh.asian
    race.2010<-louisiana.2010$P0050006
    prop.2000<-race.2000/louisiana.2000$pop2000*100
    prop.2010<-race.2010/louisiana.2010$P0010001*100
  }
  if (input$race.lat=="Hispanic"){
    race.2000<-louisiana.2000$hispanic.t
    race.2010<-louisiana.2010$P0040003
    prop.2000<-race.2000/louisiana.2000$pop2000*100
    prop.2010<-race.2010/louisiana.2010$P0010001*100
  }
  # Getting xy coords for 2000
  len.2000 <- length(louisiana.2000@polygons)
  xcoords.2000 <- rep(NA, len.2000)
  ycoords.2000 <- rep(NA, len.2000)
  
  for (i in 1:len.2000)   {
    xcoords.2000[i] <- mean(louisiana.2000@polygons[[i]]@Polygons[[1]]@coords[,1])
    ycoords.2000[i] <- mean(louisiana.2000@polygons[[i]]@Polygons[[1]]@coords[,2])
  }
  # Getting xy coords for 2010
  len.2010 <- length(louisiana.2010@polygons)
  xcoords.2010 <- rep(NA, len.2010)
  ycoords.2010 <- rep(NA, len.2010)
  
  for (i in 1:len.2010)   {
    xcoords.2010[i] <- mean(louisiana.2010@polygons[[i]]@Polygons[[1]]@coords[,1])
    ycoords.2010[i] <- mean(louisiana.2010@polygons[[i]]@Polygons[[1]]@coords[,2])
  }

  
  # 2000
  race.2000.cat<-vector(length=length(race.2000))
  race.2000.cat[which(race.2000<(5.0))]<-1
  race.2000.cat[which(race.2000<(10.0)& race.2000>=(5.0))]<-2
  race.2000.cat[which(race.2000<(20.0)& race.2000>=(10.0))]<-3
  race.2000.cat[which(race.2000<(35.0)& race.2000>=(20.0))]<-4
  race.2000.cat[which(race.2000<(50.0)& race.2000>=(35.0))]<-5
  race.2000.cat[which(race.2000>=(50.0))]<-6

  # 2010
  race.2010.cat<-vector(length=length(race.2010))
  race.2010.cat[which(race.2010<(5.0))]<-1
  race.2010.cat[which(race.2010<(10.0)& race.2010>=(5.0))]<-2
  race.2010.cat[which(race.2010<(20.0)& race.2010>=(10.0))]<-3
  race.2010.cat[which(race.2010<(35.0)& race.2010>=(20.0))]<-4
  race.2010.cat[which(race.2010<(50.0)& race.2010>=(35.0))]<-5
  race.2010.cat[which(race.2010>=(50.0))]<-6
  
  library(wvioplot)
  if (input$lat.lon=="lon") {
    # xcoords 2000
    x.coords.2000.1<-xcoords.2000[which(race.2000.cat==1)]
    x.coords.2000.2<-xcoords.2000[which(race.2000.cat==2)]
    x.coords.2000.3<-xcoords.2000[which(race.2000.cat==3)]
    x.coords.2000.4<-xcoords.2000[which(race.2000.cat==4)]
    x.coords.2000.5<-xcoords.2000[which(race.2000.cat==5)]
    x.coords.2000.6<-xcoords.2000[which(race.2000.cat==6)]
    # xcoords 2010
    x.coords.2010.1<-xcoords.2010[which(race.2010.cat==1)]
    x.coords.2010.2<-xcoords.2010[which(race.2010.cat==2)]
    x.coords.2010.3<-xcoords.2010[which(race.2010.cat==3)]
    x.coords.2010.4<-xcoords.2010[which(race.2010.cat==4)]
    x.coords.2010.5<-xcoords.2010[which(race.2010.cat==5)]
    x.coords.2010.6<-xcoords.2010[which(race.2010.cat==6)]
    
    race.name<-paste(input$race.lat,"s",sep="")
    race.name.2000<-paste(race.name,"in 2000 (Longitude)",sep=" ")
    race.name.2010<-paste(race.name,"in 2010 (Longitude)",sep=" ")
    title.2000<-paste("Violin Plot of Showing Population Distribution of ",race.name.2000,sep=" ")
    title.2010<-paste("Violin Plot of Showing Population Distribution of ",race.name.2010,sep=" ")
    
    par(mfrow=c(1,2),mar=c(0, 4, 4, 2) + 0.1)
    violin.2000<-wvioplot(x.coords.2000.1,x.coords.2000.2,
                          x.coords.2000.3,x.coords.2000.4,
                          x.coords.2000.5,x.coords.2000.6,
                          col="dodgerblue",
                          names=c("<5%","5%-10%","10%-20%","20%-35%","35%-50%",">50%"),
                          clip=F,adjust=input$violin.adjust)
    title(title.2000,ylab="Longitude")
    violin.2000<-wvioplot(x.coords.2010.1,x.coords.2010.2,
                          x.coords.2010.3,x.coords.2010.4,
                          x.coords.2010.5,x.coords.2010.6,
                          col="dodgerblue",
                          names=c("<5%","5%-10%","10%-20%","20%-35%","35%-50%",">50%"),
                          clip=F,adjust=input$violin.adjust)
    title(title.2010,ylab="Longitude")
  }
  if (input$lat.lon=="lat") {
    # ycoords 2000
    y.coords.2000.1<-ycoords.2000[which(race.2000.cat==1)]
    y.coords.2000.2<-ycoords.2000[which(race.2000.cat==2)]
    y.coords.2000.3<-ycoords.2000[which(race.2000.cat==3)]
    y.coords.2000.4<-ycoords.2000[which(race.2000.cat==4)]
    y.coords.2000.5<-ycoords.2000[which(race.2000.cat==5)]
    y.coords.2000.6<-ycoords.2000[which(race.2000.cat==6)]
    # y coords 2010
    y.coords.2010.1<-ycoords.2010[which(race.2010.cat==1)]
    y.coords.2010.2<-ycoords.2010[which(race.2010.cat==2)]
    y.coords.2010.3<-ycoords.2010[which(race.2010.cat==3)]
    y.coords.2010.4<-ycoords.2010[which(race.2010.cat==4)]
    y.coords.2010.5<-ycoords.2010[which(race.2010.cat==5)]
    y.coords.2010.6<-ycoords.2010[which(race.2010.cat==6)]
    
    race.name<-paste(input$race.lat,"s",sep="")
    race.name.2000<-paste(race.name,"in 2000 (Latitude)",sep=" ")
    race.name.2010<-paste(race.name,"in 2010 (Latitude)",sep=" ")
    title.2000<-paste("Violin Plot of Showing Population Distribution of ",race.name.2000,sep=" ")
    title.2010<-paste("Violin Plot of Showing Population Distribution of ",race.name.2010,sep=" ")
    
    par(mfrow=c(1,2),mar=c(5, 4, 4, 2) + 0.1)
    violin.2000<-wvioplot(y.coords.2000.1,y.coords.2000.2,
                          y.coords.2000.3,y.coords.2000.4,
                          y.coords.2000.5,y.coords.2000.6,
                          col="dodgerblue",
                          names=c("<5%","5%-10%","10%-20%","20%-35%","35%-50%",">50%"),
                          clip=F,adjust=input$violin.adjust)
    title(title.2000,ylab="Latitude")
    
    violin.2000<-wvioplot(y.coords.2010.1,y.coords.2010.2,
                          y.coords.2010.3,y.coords.2010.4,
                          y.coords.2010.5,y.coords.2010.6,
                          col="dodgerblue",
                          names=c("<5%","5%-10%","10%-20%","20%-35%","35%-50%",">50%"),
                          clip=F,adjust=input$violin.adjust)
    title(title.2010,ylab="Latitude")
    
  }
 
})