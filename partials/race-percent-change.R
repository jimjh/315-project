output$race.percent.change <- renderPlot({
  
  louisiana.2000<-louisiana.blkgrp
  louisiana.pop.2000<-louisiana.2000$pop2000/areaPoly(louisiana.2000)
  louisiana.2010<-louisiana.blkgrp10
  louisiana.pop.2010<-louisiana.2010$P0010001/areaPoly(louisiana.2010)
  
  if (input$race.percent=="White"){
    race.2000<-louisiana.2000$nh.white
    race.2010<-louisiana.2010$P0050003
  } 
  if (input$race.percent=="Black"){
    race.2000<-louisiana.2000$nh.black
    race.2010<-louisiana.2010$P0050004
  }
  if (input$race.percent=="American Indian/Native"){
    race.2000<-louisiana.2000$nh.ameri.es
    race.2010<-louisiana.2010$P0050005
  }
  if (input$race.percent=="Asian"){
    race.2000<-louisiana.2000$nh.asian
    race.2010<-louisiana.2010$P0050006
  }
  if (input$race.percent=="Hispanic"){
    race.2000<-louisiana.2000$hispanic.t
    race.2010<-louisiana.2010$P0040003
  }
  
  col.vector<-function(variable){
    col.vec<-vector(length=length(variable))
    col.vec[which(variable<(0.0))]<-"yellow"
    col.vec[which(variable<(15.0)& variable>=(0.0))]<-"gold"
    col.vec[which(variable<(30.0)& variable>=(15.0))]<-"darkgoldenrod2"
    col.vec[which(variable<(60.0) & variable>=(30.0))]<-"darkorange"
    col.vec[which(variable<(100.0) & variable>=(60.0))]<-"firebrick2"
    col.vec[which(variable>=(100.0))]<-"firebrick4"
    col.vec[which(variable==(0.0))]<-"white"
    return (col.vec)
  }
  percent.change<-race.2010
  for (i in length(louisiana.2010$tract)) {
    tracts.2000<-louisiana.2000$tract
    indexes<-which(tracts.2000==i)
    for (j in indexes){
      percent.change[j]<-(race.2000[j]-race.2010[j])/race.2000[j]
    }
  }
  race.name<-paste(input$race.percent,"s",sep="")
  title<-paste("Map of Louisiana Showing Percentage Change in Population Distribution of",race.name,sep=" ")
  plot(louisiana.2010, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10),
       col=col.vector(percent.change),border=NA)
  title(title)
  legend("bottomright",
         legend=c("<0%","0%-15%","15%-30%","30%-60%","60%-100%",">100%"),
         text.col=c("yellow","gold","darkgoldenrod2","darkorange","firebrick2","firebrick4"),
         col=c("yellow","gold","darkgoldenrod2","darkorange","firebrick2","firebrick4"),lwd=2)
})