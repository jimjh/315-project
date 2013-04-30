output$plot2 <- renderPlot({
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
  
  louisiana.2000<-louisiana.blkgrp
  louisiana.pop.2000<-louisiana.2000$pop2000/areaPoly(louisiana.2000)
  louisiana.2010<-louisiana.blkgrp10
  louisiana.pop.2010<-louisiana.2010$P0010001/areaPoly(louisiana.2010)
  
  if (input$race.year=="White"){
    race.2000<-louisiana.2000$nh.white
    race.2010<-louisiana.2010$P0050003
  } 
  if (input$race.year=="Black"){
    race.2000<-louisiana.2000$nh.black
    race.2010<-louisiana.2010$P0050004
  }
  if (input$race.year=="American Indian/Native"){
    race.2000<-louisiana.2000$nh.ameri.es
    race.2010<-louisiana.2010$P0050005
  }
  if (input$race.year=="Asian"){
    race.2000<-louisiana.2000$nh.asian
    race.2010<-louisiana.2010$P0050006
  }
  if (input$race.year=="Hispanic"){
    race.2000<-louisiana.2000$hispanic.t
    race.2010<-louisiana.2010$P0040003
  }
  par(mfrow=c(2,1),mar=c(0, 4, 4, 2) + 0.1)
  race.name<-paste(input$race.year,"s",sep="")
  title.2000<-paste("Map of Louisiana 2000 with Showing Population Distribution of",race.name,sep=" ")
  title.2010<-paste("Map of Louisiana 2010 with Showing Population Distribution of",race.name,sep=" ")
  plot(louisiana.2000, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10),
       col=col.vector(race.2000))
  title(title.2000)
  plot(louisiana.2010, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10),
       col=col.vector(race.2010))
  title(title.2010)
})