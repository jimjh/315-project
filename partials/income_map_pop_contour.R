incomepop.data <- list('male' = louisiana.blkgrp10$income.male,
                       'female' = louisiana.blkgrp10$income.female)

output$income_vs_pop <- renderPlot({
  par(mfrow=c(1,2))

  if (input$income_contour == TRUE) {
    # plot the contour overlay on the map showing pop density
    plot(louisiana.blkgrp10, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10), col=col.vector(incomepop.data[['male']]), border=NA)
    contour(dens, col=rgb(0,0,0,.5), lwd=2, add=T)
  }
  else {
    plot(louisiana.blkgrp10, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10), col=col.vector(incomepop.data[['male']]), border=1, lwd=.5)
    rect(-90.13884, 29.98311, -90.06836, 29.92592, border=2, lwd=4)
  }
  
  title("Map of Male Income Distribution vs. Population Density in New Orleans (2010)")
  legend("top",
         legend=c("First Quartile","Second Quartile","Third Quartile","Fourth Quartile"),
         col=c("yellow","gold","darkgoldenrod2","darkorange"),lwd=3)
  
  
  if (input$income_contour == TRUE) {
    # plot the contour overlay on the map showing pop density
    plot(louisiana.blkgrp10, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10), col=col.vector(incomepop.data[['female']]), border=NA)
    contour(dens, col=rgb(0,0,0,.5), lwd=2, add=T)
  }
  else {
    plot(louisiana.blkgrp10, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10), col=col.vector(incomepop.data[['female']]), , border=1, lwd=.5)
    rect(-90.13884, 29.98311, -90.06836, 29.92592, border=2, lwd=4)
  }
  
  title("Map of Female Income Distribution vs. Population Density in New Orleans (2010)")
  legend("top",
         legend=c("First Quartile","Second Quartile","Third Quartile","Fourth Quartile"),
         col=c("yellow","gold","darkgoldenrod2","darkorange"),lwd=3)
})

output$zoomed_in_income <- renderPlot({
  par(mfrow=c(1,2))
  plot(louisiana.blkgrp10, xlim=c(-90.13884, -90.06836), ylim=c(29.98311, 29.92592), col=col.vector(incomepop.data[['male']]), border=NA)
  title("Zoomed: Male Income Distribution in City Center")
  plot(louisiana.blkgrp10, xlim=c(-90.13884, -90.06836), ylim=c(29.98311, 29.92592), col=col.vector(incomepop.data[['female']]), border=NA)
  title("Zoomed: Female Income Distribution in City Center")
})