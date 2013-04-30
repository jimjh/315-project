# *********** Income ***********
incomepop.data <- list('male' = louisiana.blkgrp10$income.male,
                 'female' = louisiana.blkgrp10$income.female)

# income distribution by gender
#incomepop <- incomepop.data[[input$income_gender_select]]

# ==== Income of Males ====
output$income_map <- renderPlot({
  plot(louisiana.blkgrp10, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10), col=col.vector(incomepop.data[[input$income_gender_select]]))
})