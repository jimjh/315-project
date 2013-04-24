load('louisiana.Rdata')
library(UScensus2000blkgrp)
data(louisiana.blkgrp10)

ls()


dim(louisiana.blkgrp)
dim(louisiana.blkgrp10)

plot(louisiana.blkgrp)
plot(louisiana.blkgrp$tract)

louisiana.blkgrp$tract
louisiana.blkgrp10$tract

# x(-90.3414, -89.82)
# y(29.77, 30.095)


color.total <- function(input) sapply(input, function(kk) rgb(1-kk/max(input), 1-kk/max(input), 1))

hispanic <- louisiana.blkgrp10$P0040003

black <- louisiana.blkgrp10$P0030003

plot(louisiana.blkgrp10, col=color.total(black), xlim=c(-90.3414, -89.82), ylim=c(29.77, 30.094))


blkgrp <- louisiana.blkgrp$blkgrp
blkgrp10 <- louisiana.blkgrp10$blkgrp

tract <- louisiana.blkgrp$tract
tract10 <- louisiana.blkgrp10$tract

unique(blkgrp)
unique(blkgrp10)

length(unique(tract))
length(unique(tract10))

par(mfrow=c(2,1))
plot(louisiana.blkgrp, col=blkgrp, xlim=c(-90.3414, -89.82), ylim=c(29.77, 30.094))
title("Louisiana Block Group 2000")
plot(louisiana.blkgrp10, col=blkgrp10, xlim=c(-90.3414, -89.82), ylim=c(29.77, 30.094))
title("Louisiana Block Group 2010")

par(mfrow=c(2,1))
plot(louisiana.blkgrp, col=tract, xlim=c(-90.3414, -89.82), ylim=c(29.77, 30.094))
title("Louisiana Tract 2000")
plot(louisiana.blkgrp10, col=tract10, xlim=c(-90.3414, -89.82), ylim=c(29.77, 30.094))
title("Louisiana Tract 2010")

asian <- louisiana.blkgrp$asian
asian10 <- louisiana.blkgrp10$P0030005

owner_occ <- louisiana.blkgrp$hh.owner
owner_occ10 <- louisiana.blkgrp10$H0120002

pop <- louisiana.blkgrp$pop2000
pop10 <- louisiana.blkgrp10$P0010001


par(mfrow=c(2,1))
plot(louisiana.blkgrp, col=color.total(asian), xlim=c(-90.3414, -89.82), ylim=c(29.77, 30.094))
title("Louisiana Asian Race 2000")
plot(louisiana.blkgrp10, col=color.total(asian10), xlim=c(-90.3414, -89.82), ylim=c(29.77, 30.094))
title("Louisiana Asian Race 2010")

par(mfrow=c(2,1))
plot(louisiana.blkgrp, col=color.total(owner_occ), xlim=c(-90.3414, -89.82), ylim=c(29.77, 30.094))
title("Louisiana Owner Occupied 2000")
plot(louisiana.blkgrp10, col=color.total(owner_occ10), xlim=c(-90.3414, -89.82), ylim=c(29.77, 30.094))
title("Louisiana Owner Occupied 2010")

par(mfrow=c(2,1))
plot(louisiana.blkgrp, col=color.total(pop), xlim=c(-90.3414, -89.82), ylim=c(29.77, 30.094))
title("Louisiana Population 2000")
plot(louisiana.blkgrp10, col=color.total(pop10), xlim=c(-90.3414, -89.82), ylim=c(29.77, 30.094))
title("Louisiana Population 2010")