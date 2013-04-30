install.packages("shiny")
install.packages('SDMTools')
shiny::runApp('.')

install.packages('UScensus2010blkgrp')

library(UScensus2010blkgrp)

library(UScensus2010)
install.blkgrp("windows")


# Trying to plot population density over map color coded by age

LONGITUDE <- c(-90.29, -89.84)
LATITUDE  <- c(29.81, 30.10)

population <- louisiana.blkgrp10$P0010001[coords[,1] > LONGITUDE[1] & coords[,1] < LONGITUDE[2] & coords[,2] > LATITUDE[1] & coords[,2] < LATITUDE[2]]

#len <- length(louisiana.blkgrp10@polygons)
xcoords <- rep(NA, len(population))
ycoords <- rep(NA, len(population))


coords <- coordinates(louisiana.blkgrp10)

xcoords <- coords[,1][coords[,1] > LONGITUDE[1] & coords[,1] < LONGITUDE[2] & coords[,2] > LATITUDE[1] & coords[,2] < LATITUDE[2]]
ycoords <- coords[,2][coords[,1] > LONGITUDE[1] & coords[,1] < LONGITUDE[2] & coords[,2] > LATITUDE[1] & coords[,2] < LATITUDE[2]]

pop.dens <- louisiana.blkgrp10$P0010001/areaPoly(louisiana.blkgrp10)



# plotting points by pop density over map

contourplot(louisiana.blkgrp10$P0010001 ~ xcoords * ycoords)
pop.vec <- cbind(xcoords, ycoords, louisiana.blkgrp10$P0010001)

plot(louisiana.blkgrp10, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10), col=col.vector(louisiana.blkgrp10$age.male))

points(xcoords, ycoords, col=rgb(0, 0, 0, .5), pch=16, cex=(pop.dens/50000000))


# plotting contour based on population density on map

pop.fit <- loess(pop.dens ~ cbind(xcoords, ycoords), span=0.001)
x <- seq(from=LONGITUDE[1], to=LONGITUDE[2], by=0.005)
y <- seq(from=LATITUDE[1], to=LATITUDE[2], by=0.005)
pop.grid <- expand.grid(SURFLON=x, SURFLAT=y)
pop.interp <- predict(pop.fit, pop.grid)
contour(x, y, pop.interp, nlevels=5)

# Jim got it here
help(kde2d)
pop.dens <- louisiana.blkgrp10$P0010001/areaPoly(louisiana.blkgrp10)
pop.df   <- data.frame(dens = pop.dens, x = xcoords, y = ycoords)

pop.fit <- loess(dens ~ x * y, pop.df, span=0.1, degree=2, normalize=F)
pop.mar <- list(x = seq(from=LONGITUDE[1], to=LONGITUDE[2], by=0.005),
                y = seq(from=LATITUDE[1], to=LATITUDE[2], by=0.005))
pop.grid <- expand.grid(pop.mar)
#contour(pop.mar, z=predict(pop.fit, pop.grid))
#points(pop.df$x, pop.df$y, pch=4, col=rgb(0, 0, 0, 0.5))

plot(louisiana.blkgrp10, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10), col=col.vector(louisiana.blkgrp10$age.male))
contour(pop.mar, z=predict(pop.fit, pop.grid), add=T, col=1, lwd=2)

# trying to manually weight the points
num <- 10
weightedx <- rep(0, 0)
weightedy <- rep(0, 0)

for (i in 1:length(population)) {
  x <- rep(0, population[i]/num)
  y <- rep(0, population[i]/num)
  for (j in 1:(population[i]/num)) {
    x[j] <- xcoords[i]
    y[j] <- ycoords[i]
  }
  weightedx <- c(weightedx, x)
  weightedy <- c(weightedy, y)
}

dens <- kde2d(weightedx, weightedy)
help(plot)

plot(louisiana.blkgrp10, xlim=c(-90.29, -89.84), ylim=c(29.81, 30.10), col=col.vector(louisiana.blkgrp10$age.male), border=NA)
contour(dens, col=rgb(0,0,0,.5), lwd=2, add=T)
