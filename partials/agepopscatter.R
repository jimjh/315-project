age.male_pop <- louisiana.blkgrp10$age.male

no_nas <- age.male_pop[!is.na(age.male_pop)]
mean(no_nas)

age.male_pop[is.na(age.male_pop)] <- mean(no_nas)

output$scatteragepop <- renderPlot({
  plot(age.male_pop, pop7, col=2, xlim=c(0,80))
  
  s <- summary(age.male_pop)
  fq <- s[2]
  mean <- s[4]
  tq <- s[5]
  
  if (input$age1 == TRUE) {
    abline(v=fq, col=5, lwd = 2)
  }
  
  if (input$agem == TRUE) {
    abline(v=mean, col=5, lwd=2)
  }
  
  if (input$age3 == TRUE) {
    abline(v=tq, col=5, lwd = 2)
  }
  
  s2 <- summary(pop7)
  fq2 <- s2[2]
  mean2 <- s2[4]
  tq2 <- s2[5]
  
  if (input$pop1 == TRUE) {
    abline(h=fq2, col=5, lwd = 2)
  }
  
  if (input$popm == TRUE) {
    abline(h=mean2, col=5, lwd=2)
  }
  
  if (input$pop3 == TRUE) {
    abline(h=tq2, col=5, lwd = 2)
  }
  
  if (input$cont == TRUE) {
    dens <- kde2d(age.male_pop, pop7)
    contour(dens, col="yellow", add=TRUE, lwd = 2)
  }
  
  if (input$showLine == TRUE) {
    lines(lowess(age.male_pop, pop7, f=input$age.adjust.f), col=4, lwd=3)
  }
})

age.female_pop <- louisiana.blkgrp10$age.female

no_nas_female <- age.female_pop[!is.na(age.female_pop)]
mean(no_nas_female)

age.female_pop[is.na(age.female_pop)] <- mean(no_nas_female)

output$scatteragepop_female <- renderPlot({
  plot(age.female_pop, pop7, col=2, xlim=c(0,80))
  
  fes <- summary(age.female_pop)
  fefq <- fes[2]
  femean <- fes[4]
  fetq <- fes[5]
  
  if (input$feage1 == TRUE) {
    abline(v=fefq, col=5, lwd = 2)
  }
  
  if (input$feagem == TRUE) {
    abline(v=femean, col=5, lwd=2)
  }
  
  if (input$feage3 == TRUE) {
    abline(v=fetq, col=5, lwd = 2)
  }
  
  fes2 <- summary(pop7)
  fefq2 <- fes2[2]
  femean2 <- fes2[4]
  fetq2 <- fes2[5]
  
  if (input$fepop1 == TRUE) {
    abline(h=fefq2, col=5, lwd = 2)
  }
  
  if (input$fepopm == TRUE) {
    abline(h=femean2, col=5, lwd=2)
  }
  
  if (input$fepop3 == TRUE) {
    abline(h=fetq2, col=5, lwd = 2)
  }
  
  if (input$fecont == TRUE) {
    fedens <- kde2d(age.female, pop7)
    contour(fedens, col="yellow", add=TRUE, lwd = 2)
  }
  
  if (input$feshowLine == TRUE) {
    lines(lowess(age.female_pop, pop7, f=input$age.adjust.f_female), col=4, lwd=3)
  }
})


