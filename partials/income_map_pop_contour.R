incomepop.data <- list('male' = louisiana.blkgrp10$income.male,
                    'female' = louisiana.blkgrp10$income.female)

output$income_vs_pop <- renderPlot({
  par(mfrow=c(1,2))
  plot(louisiana.blkgrp10, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10), col=col.vector(incomepop.data[['male']]), border=NA)
  title("Map of Male Income Distribution vs. Population Density in New Orleans (2010)")
  
  legend("top",
         legend=c("First Quartile","Second Quartile","Third Quartile","Fourth Quartile"),
         col=c("yellow","gold","darkgoldenrod2","darkorange"),lwd=2)
  
  if (input$income_contour == TRUE) {
    # plot the contour overlay on the map showing pop density
    contour(dens, col=rgb(0,0,0,.5), lwd=2, add=T)
  }
  
  
  plot(louisiana.blkgrp10, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10), col=col.vector(incomepop.data[['female']]), border=NA)
  title("Map of Female Income Distribution vs. Population Density in New Orleans (2010)")
  
  legend("top",
         leend=c("First Quartile","Second Quartile","Third Quartile","Fourth Quartile"),
         col=c("yellow","gold","darkgoldenrod2","darkorange"),lwd=2)
  
  if (input$income_contour == TRUE) {
    # plot the contour overlay on the map showing pop density
    contour(dens, col=rgb(0,0,0,.5), lwd=2, add=T)
  }
})