

#popdens <- density(pop7)
#xcoord <- # x coordinate
#ycoord <- # y coordinate

#contour(xcoord, ycoord, pop7)


#louisiana.blkgrp10@polygons[[1]]@Polygons[[1]]@coords[,1] # mean x coord
#louisiana.blkgrp10@polygons[[1]]@Polygons[[1]]@coords[,2] # mean y coord




# PLOTS ===============================================================================

louisiana <- louisiana.blkgrp10


# ********** Age **********

agepop.data <- list('male' = louisiana.blkgrp10$age.male,
                    'female' = louisiana.blkgrp10$age.female)

# age distribution by gender
#agepop <- agepop.data[[input$age_gender_select]]


# ==== Age of Males =====
output$age_map <- renderPlot({
  plot(louisiana.blkgrp10, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10), col=col.vector(agepop.data[[input$age_gender_select]]))
})

