# *********** Population ***********

poppop <- louisiana$P0010001

output$pop_map <- renderPlot({
  plot(louisiana.blkgrp10, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10), col=col.vector(poppop))
})