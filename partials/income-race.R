capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s,1,1)), {s <- substring(s,2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

coords <- coordinates(louisiana.blkgrp10)
new.orleans.indices <- which(coords[,1] > LONGITUDE[1] & coords[,1] < LONGITUDE[2] & coords[,2] > LATITUDE[1] & coords[,2] < LATITUDE[2])
new.orleans <- louisiana.blkgrp10[new.orleans.indices,]

# poor man's map
income.data <- list('male' = new.orleans$income.male,
                    'female' = new.orleans$income.female)
race.data <- list(
  'white' = new.orleans$P0030002,
  'black' = new.orleans$P0030003,
  'american.indian' = new.orleans$P0030004,
  'asian' = new.orleans$P0030005
)

output$income.race <- renderPlot({

  # income
  income <- income.data[[input$income.gender]]
  # data imputation
  income[is.na(income)] <- mean(income, na.rm=T)

  # race percentage
  race <- race.data[[input$income.race]]
  pct.race <- race/new.orleans$P0030001
  # data imputation
  pct.race[is.na(pct.race)] <- sum(race, na.rm=T)/sum(new.orleans$P0030001, na.rm=T)
  income.density <- kde2d(x=income, y=pct.race, h=c(input$income.adjust.x, input$income.adjust.y),
                          n=input$income.adjust.n)

  prev.par <- par(mar=(c(5, 4, 4, 7.3) + 0.1), xpd=T)
  max.income <- 150000
  min.income <- min(income)
  image(income.density, col=rev(heat.colors(12)), xlim=c(min.income, max.income),
        xlab=sprintf('Median Income for %ss (USD)', capwords(input$income.gender)),
        ylab=sprintf('Pop. Percentage of %ss (%%)', capwords(input$income.race)))
  par(xpd=F)
  title('Distribution of Income and Race Proportion')
  points(x=income, y=pct.race, pch=4, col=rgb(.2, .2 , .2, .3))
  contour(income.density, add=T)
  par(prev.par)
  abline(v=mean(income), lty=2)

  # legend
  par(xpd=T)
  dx <- floor(0.05 * (max.income - min.income))
  xl <- max.income + dx/2; xr <- xl + dx
  dy <- 0.6 * diff(range(pct.race))
  yb <- max(pct.race) - dy; yt <- yb + dy
  coords <- cbind(x=c(xl, xr, xr, xl), y=c(yb, yb, yt, yt))
  legend.gradient(coords, rev(heat.colors(12)), c('Low', 'High'), xpd=T, title='')

})
