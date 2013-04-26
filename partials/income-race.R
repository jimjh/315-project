output$income.race <- renderPlot({
  
  capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s,1,1)),
                             {s <- substring(s,2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  }
  
  # poor man's map
  income.data <- list('male' = louisiana.blkgrp10$income.male,
                      'female' = louisiana.blkgrp10$income.female)
  race.data <- list(
    'white' = louisiana.blkgrp10$P0030002,
    'black' = louisiana.blkgrp10$P0030003,
    'american.indian' = louisiana.blkgrp10$P0030004,
    'asian' = louisiana.blkgrp10$P0030005
  )
  
  # income
  income <- income.data[[input$income.gender]]
  # data imputation
  income[is.na(income)] <- mean(income, na.rm=T)
  
  # race percentage
  race <- race.data[[input$income.race]]
  pct.race <- race/louisiana.blkgrp10$P0030001
  # data imputation
  pct.race[is.na(pct.race)] <- sum(race, na.rm=T)/sum(louisiana.blkgrp10$P0030001, na.rm=T)
  income.density <- kde2d(x=income, y=pct.race, h=c(input$income.adjust.x, input$income.adjust.y),
                          n=50)
  
  image(income.density, col=rev(heat.colors(12)),
        xlab=sprintf('Median Income for %ss (USD)', capwords(input$income.gender)),
        ylab=sprintf('Pop. Percentage of %ss (%%)', capwords(input$income.race)))
  points(x=income, y=pct.race, pch=4, col=rgb(.2, .2 , .2, .3))
  contour(income.density, add=T)
  
})