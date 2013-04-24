  output$hist <- renderPlot({
    hist(louisiana.blkgrp10$income.male, breaks=as.numeric(input$n_breaks))
  })