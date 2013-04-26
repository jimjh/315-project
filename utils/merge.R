
###############################################################################
#
# Integrating American Community Survey summary data into UScensus objects
# ACT, 4-22-13
#
###############################################################################

# For example: Kansas

library(UScensus2010blkgrp)
data(louisiana.blkgrp10)

# Geometric identity files from ACS.
la.acs <- read.csv("acs-geographies/g20115la.csv", header=FALSE)

# In data frames, convert from factors to numbers, carefully.
ac <- function(x) as.numeric(as.character(x))

# Both files have block-group names, organized by tract. Make sure there's a match.
name.usc <- paste(ac(louisiana.blkgrp10$tract), ac(louisiana.blkgrp10$blkgrp), sep="")
name.acs <- paste(ac(la.acs[,14]), ac(la.acs[,15]), sep="")

# Get the rows in the ACS corresponding to our identifiers. 
trials <- la.acs[match(name.usc, name.acs), 5]


#median income:
state <- "la"
income.file <- read.csv(paste("acs-files-quick/e20115",state,"0064000.txt",sep=""), header=FALSE)

#Get the rows in the new file that corresponds to our block groups.
rows <- match(trials, income.file[,6])

louisiana.blkgrp10$income.male <- ac(income.file[rows, 51])
louisiana.blkgrp10$income.female <- ac(income.file[rows, 52])

age.file <- read.csv(paste("acs-files-quick/e20115",state,"0003000.txt",sep=""), header=FALSE)
rows <- match(trials, age.file[,6])
louisiana.blkgrp10$age.male <- ac(age.file[rows, 101])
louisiana.blkgrp10$age.female <- ac(age.file[rows, 102])

# Check everything.
par(mfrow=c(2,2))
hist(louisiana.blkgrp10$income.male)
hist(louisiana.blkgrp10$income.female)
hist(louisiana.blkgrp10$age.male)
hist(louisiana.blkgrp10$age.female)


# Save the new data frame.

save(louisiana.blkgrp10, file="louisiana-census2010-plus-acs.RData")
