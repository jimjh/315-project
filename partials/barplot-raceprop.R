#Proportional stacked barplot for 36-315 Spring project
#Luke Davis

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
prevPar <- par()
prop.lab <- c("White", "Black", "Hispanic", "Am. Indian", "Asian",
              "Pacific Islander", "Other", "Multiracial")

output$prop.race.plot <- renderPlot({
  par(mfrow=c(2,1), mar=c(0,10,4,4))
  if(input$prop.include){
  barplot(as.matrix(c(whProp00,blProp00,hiProp00,otProp00,inProp00,asProp00,hwProp00,muProp00)),
          col=prop.col, main="Racial Breakdown of Louisiana", beside=TRUE,
          horiz=TRUE, axes=FALSE, xlim=c(0,0.6))
  title(ylab="2000", line=8, font.lab=2)
  axis(2, at=seq(from=1.5, by=1, to=8.5), labels=prop.lab, tick=FALSE, las=1)
  par(mar=c(4,10,0,4))
  barplot(as.matrix(c(whProp10,blProp10,hiProp10,otProp10,inProp10,asProp10,hwProp10,muProp10)),
          col=prop.col, beside=TRUE, horiz=TRUE,
          xlab="Proportion of Total Population", xlim=c(0,0.6))
  title(ylab="2010", line=8, font.lab=2)
  axis(2, at=seq(from=1.5, by=1, to=8.5), labels=prop.lab, tick=FALSE, las=1)
  }
  else{
  #Barplot of racial proportions without white and black
  par(mfrow=c(2,1), mar=c(0,10,4,4))
  barplot(as.matrix(c(hiProp00,otProp00,inProp00,asProp00,hwProp00,muProp00)),
          col=prop.col[3:9], main="Racial Breakdown of Louisiana", beside=TRUE,
          horiz=TRUE, axes=FALSE, xlim=c(0,0.05))
  axis(2, at=seq(from=1.5, by=1, to=7.5), labels=prop.lab[3:9], tick=FALSE, las=1)
  title(ylab="2000", line=8, font.lab=2)
  par(mar=c(4,10,0,4))
  barplot(as.matrix(c(hiProp10,otProp10,inProp10,asProp10,hwProp10,muProp10)),
          col=prop.col[3:9], beside=TRUE, horiz=TRUE,
          xlab="Proportion of Total Population", xlim=c(0,0.05))
  axis(2, at=seq(from=1.5, by=1, to=7.5), labels=prop.lab[3:9], tick=FALSE, las=1)
  title(ylab="2010", line=8, font.lab=2)
  } 
})
par(prevPar)