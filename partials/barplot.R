#Proportional stacked barplot for 36-315 Spring project
#Luke Davis

library(UScensus2010blkgrp)
library(UScensus2000blkgrp)
data(louisiana.blkgrp10)
data(louisiana.blkgrp)

#2000 statewide racial proportions
whProp00 <- sum(louisiana.blkgrp$nh.white)/sum(louisiana.blkgrp$pop2000) #white
hiProp00 <- sum(louisiana.blkgrp$hispanic.t)/sum(louisiana.blkgrp$pop2000) #hispanic
blProp00 <- sum(louisiana.blkgrp$nh.black)/sum(louisiana.blkgrp$pop2000) #black
inProp00 <- sum(louisiana.blkgrp$nh.ameri.es)/sum(louisiana.blkgrp$pop2000) #Am indian
asProp00 <- sum(louisiana.blkgrp$nh.asian)/sum(louisiana.blkgrp$pop2000) #Asian
hwProp00 <- sum(louisiana.blkgrp$nh.hawn.pi)/sum(louisiana.blkgrp$pop2000) #Hawaiian
otProp00 <- sum(louisiana.blkgrp$nh.other)/sum(louisiana.blkgrp$pop2000) #Other
muProp00 <- sum(louisiana.blkgrp$mult.race)/sum(louisiana.blkgrp$pop2000) #Multi

#2010 statewide racial proportions
whProp10 <- sum(louisiana.blkgrp10$P0050003)/sum(louisiana.blkgrp10$P0010001) #white
hiProp10 <- sum(louisiana.blkgrp10$P0040003)/sum(louisiana.blkgrp10$P0010001) #hispanic
blProp10 <- sum(louisiana.blkgrp10$P0050004)/sum(louisiana.blkgrp10$P0010001) #black
inProp10 <- sum(louisiana.blkgrp10$P0050005)/sum(louisiana.blkgrp10$P0010001) #Am indian
asProp10 <- sum(louisiana.blkgrp10$P0050006)/sum(louisiana.blkgrp10$P0010001) #Asian
hwProp10 <- sum(louisiana.blkgrp10$P0050007)/sum(louisiana.blkgrp10$P0010001) #Hawaiian
otProp10 <- sum(louisiana.blkgrp10$P0050008)/sum(louisiana.blkgrp10$P0010001) #Other
muProp10 <- sum(louisiana.blkgrp10$P0050009)/sum(louisiana.blkgrp10$P0010001) #Multi

#Barplot of racial proportions
prop.col <- rainbow(8)
prop.col[4] <- "yellow"

par(mfrow=c(2,1), mar=c(0,4,4,11), xpd=TRUE)
barplot(as.matrix(c(whProp00,blProp00,hiProp00,otProp00,inProp00,asProp00,hwProp00,muProp00)),
        col=prop.col, ylab="2000", main="Racial Breakdown of Louisiana", beside=TRUE,
        horiz=TRUE, axes=FALSE)
par(mar=c(4,4,0,11))
barplot(as.matrix(c(whProp10,blProp10,hiProp10,otProp10,inProp10,asProp10,hwProp10,muProp10)),
        col=prop.col, beside=TRUE, horiz=TRUE,
        xlab="Proportion of Total Population", ylab="2010")
legend(0.7, 7, legend=c("White", "Black", "Hispanic", "Am. Indian", "Asian",
                         "Pacific Islander", "Other", "Multiple"), fill=prop.col)

#Barplot of racial proportions without white and black
par(mfrow=c(2,1), mar=c(0,4,4,11))
barplot(as.matrix(c(hiProp00,otProp00,inProp00,asProp00,hwProp00,muProp00)),
        col=prop.col[3:9], ylab="2000", main="Racial Breakdown of Louisiana", beside=TRUE,
        horiz=TRUE, axes=FALSE, xlim=c(0,0.05))
par(mar=c(4,4,0,11))
barplot(as.matrix(c(hiProp10,otProp10,inProp10,asProp10,hwProp10,muProp10)),
        col=prop.col[3:9], beside=TRUE, horiz=TRUE,
        xlab="Proportion of Total Population", ylab="2010", xlim=c(0,0.05))
legend(0.055, 6, legend=c("Hispanic", "American Indian", "Asian",
                            "Pacific Islander", "Other", "Multiple"), fill=prop.col[3:9])


#Household size by race
#2000
#White
hh.ylimit=c(0,0.35)
hh.col="SlateBlue"
#par(mfrow=c(2,2))

par(mfrow=c(1,1), mar=c(4,4,4,4))
output$hh.race.plot <- renderPlot({
  if(input$hh.race=="white"){
    Total <- sum(louisiana.blkgrp$hh.nh.white.1p,louisiana.blkgrp$hh.nh.white.2p,
                 louisiana.blkgrp$hh.nh.white.3p,louisiana.blkgrp$hh.nh.white.4p,
                 louisiana.blkgrp$hh.nh.white.5p,louisiana.blkgrp$hh.nh.white.6p,
                 louisiana.blkgrp$hh.nh.white.7p)
    Prop1p <- sum(louisiana.blkgrp$hh.nh.white.1p)/Total
    Prop2p <- sum(louisiana.blkgrp$hh.nh.white.2p)/Total
    Prop3p <- sum(louisiana.blkgrp$hh.nh.white.3p)/Total
    Prop4p <- sum(louisiana.blkgrp$hh.nh.white.4p)/Total
    Prop5p <- sum(louisiana.blkgrp$hh.nh.white.5p)/Total
    Prop6p <- sum(louisiana.blkgrp$hh.nh.white.6p)/Total
    Prop7p <- sum(louisiana.blkgrp$hh.nh.white.7p)/Total
  } 
  
  if(input$hh.race=="black"){
    Total <- sum(louisiana.blkgrp$hh.black.1p,louisiana.blkgrp$hh.black.2p,
                 louisiana.blkgrp$hh.black.3p,louisiana.blkgrp$hh.black.4p,
                 louisiana.blkgrp$hh.black.5p,louisiana.blkgrp$hh.black.6p,
                 louisiana.blkgrp$hh.black.7p)
    Prop1p <- sum(louisiana.blkgrp$hh.black.1p)/Total
    Prop2p <- sum(louisiana.blkgrp$hh.black.2p)/Total
    Prop3p <- sum(louisiana.blkgrp$hh.black.3p)/Total
    Prop4p <- sum(louisiana.blkgrp$hh.black.4p)/Total
    Prop5p <- sum(louisiana.blkgrp$hh.black.5p)/Total
    Prop6p <- sum(louisiana.blkgrp$hh.black.6p)/Total
    Prop7p <- sum(louisiana.blkgrp$hh.black.7p)/Total
  }
  
  barplot(as.matrix(c(Prop1p,Prop2p,Prop3p,Prop4p,Prop5p,
                      Prop6p,Prop7p)), col=hh.col, beside=TRUE,
          ylab="Proportion of Households", xlab="Household Size", 
          main=paste("Household Size: ", input$hh.race,  " Householder"), ylim=hh.ylimit)
  axis(side=1, at=seq(from=1.5, by=1, to=7.5), labels=1:7)
)}

#Black
blackTotal <- sum(louisiana.blkgrp$hh.black.1p,louisiana.blkgrp$hh.black.2p,
                  louisiana.blkgrp$hh.black.3p,louisiana.blkgrp$hh.black.4p,
                  louisiana.blkgrp$hh.black.5p,louisiana.blkgrp$hh.black.6p,
                  louisiana.blkgrp$hh.black.7p)
blackProp1p <- sum(louisiana.blkgrp$hh.black.1p)/blackTotal
blackProp2p <- sum(louisiana.blkgrp$hh.black.2p)/blackTotal
blackProp3p <- sum(louisiana.blkgrp$hh.black.3p)/blackTotal
blackProp4p <- sum(louisiana.blkgrp$hh.black.4p)/blackTotal
blackProp5p <- sum(louisiana.blkgrp$hh.black.5p)/blackTotal
blackProp6p <- sum(louisiana.blkgrp$hh.black.6p)/blackTotal
blackProp7p <- sum(louisiana.blkgrp$hh.black.7p)/blackTotal

par(mfrow=c(1,1), mar=c(4,4,4,4))
barplot(as.matrix(c(blackProp1p,blackProp2p,blackProp3p,blackProp4p,blackProp5p,
                    blackProp6p,blackProp7p)), col=hh.col, beside=TRUE,
        ylab="Proportion of Households", xlab="Household Size", 
        main="Household Size: Black Householder", ylim=hh.ylimit)
axis(side=1, at=seq(from=1.5, by=1, to=7.5), labels=1:7)

#Hispanic
hispTotal <- sum(louisiana.blkgrp$hh.hisp.1p,louisiana.blkgrp$hh.hisp.2p,
                  louisiana.blkgrp$hh.hisp.3p,louisiana.blkgrp$hh.hisp.4p,
                  louisiana.blkgrp$hh.hisp.5p,louisiana.blkgrp$hh.hisp.6p,
                  louisiana.blkgrp$hh.hisp.7p)
hispProp1p <- sum(louisiana.blkgrp$hh.hisp.1p)/hispTotal
hispProp2p <- sum(louisiana.blkgrp$hh.hisp.2p)/hispTotal
hispProp3p <- sum(louisiana.blkgrp$hh.hisp.3p)/hispTotal
hispProp4p <- sum(louisiana.blkgrp$hh.hisp.4p)/hispTotal
hispProp5p <- sum(louisiana.blkgrp$hh.hisp.5p)/hispTotal
hispProp6p <- sum(louisiana.blkgrp$hh.hisp.6p)/hispTotal
hispProp7p <- sum(louisiana.blkgrp$hh.hisp.7p)/hispTotal

par(mfrow=c(1,1), mar=c(4,4,4,4))
barplot(as.matrix(c(hispProp1p,hispProp2p,hispProp3p,hispProp4p,hispProp5p,
                    hispProp6p,hispProp7p)), col=hh.col, beside=TRUE,
        ylab="Proportion of Households", xlab="Household Size", 
        main="Household Size: Hispanic Householder", ylim=hh.ylimit)
axis(side=1, at=seq(from=1.5, by=1, to=7.5), labels=1:7)

#Asian
asianTotal <- sum(louisiana.blkgrp$hh.asian.1p,louisiana.blkgrp$hh.asian.2p,
                  louisiana.blkgrp$hh.asian.3p,louisiana.blkgrp$hh.asian.4p,
                  louisiana.blkgrp$hh.asian.5p,louisiana.blkgrp$hh.asian.6p,
                  louisiana.blkgrp$hh.asian.7p)
asianProp1p <- sum(louisiana.blkgrp$hh.asian.1p)/asianTotal
asianProp2p <- sum(louisiana.blkgrp$hh.asian.2p)/asianTotal
asianProp3p <- sum(louisiana.blkgrp$hh.asian.3p)/asianTotal
asianProp4p <- sum(louisiana.blkgrp$hh.asian.4p)/asianTotal
asianProp5p <- sum(louisiana.blkgrp$hh.asian.5p)/asianTotal
asianProp6p <- sum(louisiana.blkgrp$hh.asian.6p)/asianTotal
asianProp7p <- sum(louisiana.blkgrp$hh.asian.7p)/asianTotal

par(mfrow=c(1,1), mar=c(4,4,4,4))
barplot(as.matrix(c(asianProp1p,asianProp2p,asianProp3p,asianProp4p,asianProp5p,
                    asianProp6p,asianProp7p)), col=hh.col, beside=TRUE,
        ylab="Proportion of Households", xlab="Household Size", 
        main="Household Size: Asian Householder", ylim=hh.ylimit)
axis(side=1, at=seq(from=1.5, by=1, to=7.5), labels=1:7)