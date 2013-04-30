incomepop.data <- list('male' = louisiana.blkgrp10$income.male,
                    'female' = louisiana.blkgrp10$income.female)

output$income_vs_pop <- renderPlot({
  plot(louisiana.blkgrp10, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10), col=col.vector(incomepop.data[[input$income_gender_select]]), border=NA)
  if (input$income_contour == TRUE) {
    # plot the contour overlay on the map showing pop density
    contour(dens, col=rgb(0,0,0,.5), lwd=2, add=T)
  }
})