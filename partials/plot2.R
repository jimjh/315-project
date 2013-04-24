output$map2 <- renderPlot({
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
  lousiana.data<-list(year2000=louisiana.blkgrp, year2010=louisiana.blkgrp10)
  if (input$year==2000){
    louisiana.year<-lousiana.data$year2000
  } else {
    louisiana.year<-lousiana.data$year2010
  }
  choropleth(louisiana.year, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10),col=col.vector(louisiana.year$pop2000))
})