output$race.proportion <- renderPlot({
  
  louisiana.2000<-louisiana.blkgrp
  louisiana.2010<-louisiana.blkgrp10
  
  if (input$race.prop=="White"){
    race.2000<-louisiana.2000$nh.white
    race.2010<-louisiana.2010$P0050003
  } 
  if (input$race.prop=="Black"){
    race.2000<-louisiana.2000$nh.black
    race.2010<-louisiana.2010$P0050004
  }
  if (input$race.prop=="American Indian/Native"){
    race.2000<-louisiana.2000$nh.ameri.es
    race.2010<-louisiana.2010$P0050005
  }
  if (input$race.prop=="Asian"){
    race.2000<-louisiana.2000$nh.asian
    race.2010<-louisiana.2010$P0050006
  }
  if (input$race.prop=="Hispanic"){
    race.2000<-louisiana.2000$hispanic.t
    race.2010<-louisiana.2010$P0040003
  }
  
  col.vector<-function(variable){
    col.vec<-vector(length=length(variable))
    col.vec[which(variable<(5.0))]<-"yellow"
    col.vec[which(is.na(variable))]<-"white"
    col.vec[which(variable<(10.0)& variable>=(5.0))]<-"gold"
    col.vec[which(variable<(20.0)& variable>=(10.0))]<-"darkgoldenrod2"
    col.vec[which(variable<(35.0) & variable>=(20.0))]<-"darkorange"
    col.vec[which(variable<(50.0) & variable>=(35.0))]<-"firebrick2"
    col.vec[which(variable>=(50.0))]<-"firebrick4"
    return (col.vec)
  }
  prop.race.2000<-race.2000/louisiana.2000$pop2000*100
  prop.race.2010<-race.2010/louisiana.2010$P0010001*100
  
  race.name<-paste(input$race.prop,"s",sep="")
  race.name.2000<-paste(race.name,"in 2000",sep=" ")
  race.name.2010<-paste(race.name,"in 2010",sep=" ")
  title.2000<-paste("Map of Louisiana Showing Proportion of ",race.name.2000,sep=" ")
  title.2010<-paste("Map of Louisiana Showing Proportion of ",race.name.2010,sep=" ")
  par(mfrow=c(1,2),mar=c(5, 4, 4, 2) + 0.1)
  plot(louisiana.2000, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10),
       col=col.vector(prop.race.2000),border=NA)
  title(title.2000)
  plot(louisiana.2010, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10),
       col=col.vector(prop.race.2010),border=NA)
  title(title.2010)
  legend("bottomright",
         legend=c("<5%","5%-10%","10%-20%","20%-35%","35%-50%",">50%"),
         text.col=c("yellow","gold","darkgoldenrod2","darkorange","firebrick2","firebrick4"),
         col=c("yellow","gold","darkgoldenrod2","darkorange","firebrick2","firebrick4"),lwd=2)
})