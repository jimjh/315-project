output$age.race <- renderPlot({
  
  capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s,1,1)),
                             {s <- substring(s,2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  }
  
  # poor man's map
  age.data <- list('male' = louisiana.blkgrp10$age.male, 'female' = louisiana.blkgrp10$age.female)
  race.data <- list(
    'white' = louisiana.blkgrp10$P0030002,
    'black' = louisiana.blkgrp10$P0030003,
    'american.indian' = louisiana.blkgrp10$P0030004,
    'asian' = louisiana.blkgrp10$P0030005
    )
  
  # age
  age <- age.data[[input$age.gender]]
  # data imputation
  age[is.na(age)] <- mean(age, na.rm=T)
  
  # race percentage
  race <- race.data[[input$age.race]]
  pct.race <- race/louisiana.blkgrp10$P0030001
  # data imputation
  pct.race[is.na(pct.race)] <- sum(race, na.rm=T)/sum(louisiana.blkgrp10$P0030001, na.rm=T)
  age.density <- kde2d(x=age, y=pct.race, h=c(input$age.adjust.x, input$age.adjust.y), n=50)
  
  image(age.density, col=rev(heat.colors(12)),
        xlab=sprintf('Median Age for %ss (Years)', capwords(input$age.gender)),
        ylab=sprintf('Pop. Percentage of %ss (%%)', capwords(input$age.race)))
  title(sprintf('Distribution of Age and Racial Proportion',
                capwords(input$age.race), capwords(input$age.gender)))
  points(x=age, y=pct.race, pch=4, col=rgb(.2, .2 , .2, .3))
  contour(age.density, add=T)
  
})