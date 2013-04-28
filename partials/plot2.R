output$plot2 <- renderPlot({
  col.vector<-function(variable){
    summary.vec<-summary(variable)
    col.vec<-vector(length=length(variable))
    first<-summary.vec[[1]]
    second<-summary.vec[[2]]
    third<-summary.vec[[4]]
    col.vec[which(variable<first)]<-"gray90"
    col.vec[which(variable<second & variable>=first)]<-"gray83"
    col.vec[which(variable<third & variable>=second)]<-"gray63"
    col.vec[which(variable>=third)]<-"gray43"
    return (col.vec)
  }
  louisiana.data<-list(year2000=louisiana.blkgrp, year2010=louisiana.blkgrp10)
  if (input$year==2000){
    louisiana.year<-louisiana.data$year2000
    louisiana.pop<-louisiana.year$pop2000/areaPoly(louisiana.year)
  } else {
    louisiana.year<-louisiana.data$year2010
    louisiana.pop<-louisiana.year$P0010001/areaPoly(louisiana.year)
  }
  par(mfrow=c(1,2))
  plot(louisiana.year, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10),col=col.vector(louisiana.pop))
  plot(louisiana.year, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10),col=col.vector(louisiana.pop))
})