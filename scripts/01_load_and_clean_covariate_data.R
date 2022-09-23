#-------------------------------------------------------------------------------
# Project title: Predicting out-of-sample stream attractiveness to stray
# hatchery-origin chum salmon in Southeast Alaska (MS thesis chapter 2)

# Molly K Payne

# Purpose of this script: Load and clean model covariate data for 640 SEAK chum
# salmon streams (building upon the original 57 streams from chapter 1 used to
# fit the model)

# Last updated: May 26, 2022
#-------------------------------------------------------------------------------



#1. Read in data ===============================================================
Chp2_MasterDat <- read.csv("data/Chp2_Master_dataset.csv")
str(Chp2_MasterDat)

### Tailoring misc.
#Convert 'Year' to factor
Chp2_MasterDat$Year <- as.factor(Chp2_MasterDat$Year) 

#Change NAs in WMA_Releases_by_Yr variables to 0s (bc these aren't really NAs)
Chp2_MasterDat$WMA_Releases_in_millions[is.na(
  Chp2_MasterDat$WMA_Releases_in_millions)] <- 0

#Correct name of WMA_Releases_in_millions to WMA_Releases_by_Yr to match Chp1
colnames(Chp2_MasterDat)[10] <- "WMA_Releases_by_Yr"

#Remove streams that are missing flow data 
Chp2_MasterDat <- Chp2_MasterDat[!is.na(Chp2_MasterDat$CV_flow),]
sapply(Chp2_MasterDat, function(x) sum(is.na(x)))

### Stick 2020 and 2021 streams into a separate dataset for part 2 of chapter 2
dat_20_21 <- Chp2_MasterDat[Chp2_MasterDat$Year %in% c("2020", "2021"),]
dat_08_19 <- Chp2_MasterDat[!Chp2_MasterDat$Year %in% c("2020", "2021"),]

#Updated GitHub personal access token, this is a test line to see if it worked



