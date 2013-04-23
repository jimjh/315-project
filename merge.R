
###############################################################################
#
# Integrating American Community Survey summary data into UScensus objects
# ACT, 4-22-13
#
###############################################################################

# For example: Kansas

library(UScensus2010blkgrp)
data(kansas.blkgrp10)

# Geometric identity files from ACS.
ks.acs <- read.csv("g20115ks.csv", header=FALSE)

# In data frames, convert from factors to numbers, carefully.
ac <- function(x) as.numeric(as.character(x))

# Both files have block-group names, organized by tract. Make sure there's a match.
name.usc <- paste(ac(kansas.blkgrp10$tract), ac(kansas.blkgrp10$blkgrp), sep="")
name.acs <- paste(ac(ks.acs[,14]), ac(ks.acs[,15]), sep="")

# Get the rows in the ACS corresponding to our identifiers. 
trials <- ks.acs[match(name.usc, name.acs), 5]


#median income:
state <- "ks"
income.file <- read.csv(paste("e20115",state,"0064000.txt",sep=""), header=FALSE)

#Get the rows in the new file that corresponds to our block groups.
rows <- match(trials, income.file[,6])

kansas.blkgrp10$income.male <- ac(income.file[rows, 51])
kansas.blkgrp10$income.female <- ac(income.file[rows, 52])

age.file <- read.csv(paste("e20115",state,"0003000.txt",sep=""), header=FALSE)
rows <- match(trials, age.file[,6])
kansas.blkgrp10$age.male <- ac(age.file[rows, 101])
kansas.blkgrp10$age.female <- ac(age.file[rows, 102])

# Check everything.
par(mfrow=c(2,2))
hist(kansas.blkgrp10$income.male)
hist(kansas.blkgrp10$income.female)
hist(kansas.blkgrp10$age.male)
hist(kansas.blkgrp10$age.female)


# Save the new data frame.

save(kansas.blkgrp10, file="kansas-census2010-plus-acs.RData")
