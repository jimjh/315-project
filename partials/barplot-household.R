#Barplots of Louisiana Household Size for 36-315 Spring project
#Luke Davis

#Household size by race
#2000
hh.ylimit=c(0,0.35)
hh.col="SlateBlue"

par(mfrow=c(1,1), mar=c(4,4,4,4))
output$hh.race.plot <- renderPlot({
  if(input$hh.race=="White"){
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
  
  if(input$hh.race=="Black"){
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
  if(input$hh.race=="Hispanic"){
    Total <- sum(louisiana.blkgrp$hh.hisp.1p,louisiana.blkgrp$hh.hisp.2p,
                     louisiana.blkgrp$hh.hisp.3p,louisiana.blkgrp$hh.hisp.4p,
                     louisiana.blkgrp$hh.hisp.5p,louisiana.blkgrp$hh.hisp.6p,
                     louisiana.blkgrp$hh.hisp.7p)
    Prop1p <- sum(louisiana.blkgrp$hh.hisp.1p)/Total
    Prop2p <- sum(louisiana.blkgrp$hh.hisp.2p)/Total
    Prop3p <- sum(louisiana.blkgrp$hh.hisp.3p)/Total
    Prop4p <- sum(louisiana.blkgrp$hh.hisp.4p)/Total
    Prop5p <- sum(louisiana.blkgrp$hh.hisp.5p)/Total
    Prop6p <- sum(louisiana.blkgrp$hh.hisp.6p)/Total
    Prop7p <- sum(louisiana.blkgrp$hh.hisp.7p)/Total
  }
  if(input$hh.race=="Asian"){
    Total <- sum(louisiana.blkgrp$hh.asian.1p,louisiana.blkgrp$hh.asian.2p,
                      louisiana.blkgrp$hh.asian.3p,louisiana.blkgrp$hh.asian.4p,
                      louisiana.blkgrp$hh.asian.5p,louisiana.blkgrp$hh.asian.6p,
                      louisiana.blkgrp$hh.asian.7p)
    Prop1p <- sum(louisiana.blkgrp$hh.asian.1p)/Total
    Prop2p <- sum(louisiana.blkgrp$hh.asian.2p)/Total
    Prop3p <- sum(louisiana.blkgrp$hh.asian.3p)/Total
    Prop4p <- sum(louisiana.blkgrp$hh.asian.4p)/Total
    Prop5p <- sum(louisiana.blkgrp$hh.asian.5p)/Total
    Prop6p <- sum(louisiana.blkgrp$hh.asian.6p)/Total
    Prop7p <- sum(louisiana.blkgrp$hh.asian.7p)/Total
  }
  
  barplot(as.matrix(c(Prop1p,Prop2p,Prop3p,Prop4p,Prop5p,
                      Prop6p,Prop7p)), col=hh.col, beside=TRUE,
          ylab=paste("Proportion of", input$hh.race, "Households"), xlab="Household Size", 
          main=paste("2000 Household Size:", input$hh.race, "Householder"), ylim=hh.ylimit)
  axis(side=1, at=seq(from=1.5, by=1, to=7.5), labels=1:7)
})