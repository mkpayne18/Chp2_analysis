

#Read in data
getwd()
Chp2_MasterDat <- read.csv("Chp2_Master_dataset.csv")
#As before, make sure that 'year' is a factor variable and the NAs in WMA_Releases_
#_by_Yr are converted to 0s, because 0 fish were released within 40km of those 
#streams, not NA fish
Chp2_MasterDat$Year <- as.factor(Chp2_MasterDat$Year)
Chp2_MasterDat$WMA_Releases_in_millions[is.na(Chp2_MasterDat$WMA_Releases_in_millions)] <- 0
str(Chp2_MasterDat)



#1. Recreate final models from chapter 1, use to make new predictions ##########
Chp1_Master <- read.csv("Chp1_Master_dataset copy.csv")
#The following lines of code in section 1 here are copied over from Model_fitting3.R
#in my chapter 1 analysis

#1.1. Tailor dataset ===========================================================
Chp1_Master$Year <- as.factor(Chp1_Master$Year)
Chp1_Master$WMA_Releases_by_Yr[is.na(Chp1_Master$WMA_Releases_by_Yr)] <- 0
str(Chp1_Master)
#remove Chilkat River, Disappearance Creek, Black River, Saook Bay West Head in
#2010 only, and Herman Creek from analysis. These creeks were sampled either 
#really late bc they are fall-run (Chilkat + Dis.Creek), or early (before 7/20).
library(tidyverse)
library(dplyr)
delete <- c("Chilkat River", "Disappearance Creek", "Black River", "Herman Creek")
f <- Chp1_Master[!(Chp1_Master$StreamName %in% delete),]
#only delete 2010 for Saook Bay West Head, not 2011:
f <- f[!(f$Year == "2010" & f$StreamName == "Saook Bay West Head"), ]
#also remove 1) rows containing NA values, and 
f <- f[complete.cases(f), ]
#2) the Ketchikan Creek outlier, which was not ultimately included in the final 
#model
f <- f[!(f$StreamName == "Ketchikan Creek"),]
rownames(f) <- 1:nrow(f)

### Round response variable to nearest integer
f$Avg_number_strays <- round(f$Avg_number_strays)


#1.2. Scale covariates =========================================================
m <- apply(f[ , c(10:19)], 2, scale.default)
f_scaled <- cbind.data.frame(f[ , c(1:9)], m)


#1.3. Fit top models from chapter 1 analysis ===================================
library(lme4)
bm1u <- glmer.nb(Avg_number_strays ~ (1|Year) + Cons_Abundance + WMA_Releases_by_Yr
                 + CV_flow + I(CV_flow^2), data = f_scaled) #'bm' for "best model"
bm2u <- glmer.nb(Avg_number_strays ~ (1|Year) + WMA_Releases_by_Yr + CV_flow +
                   I(CV_flow^2), data = f_scaled)


#2. Make predictions for streams without straying data #########################
### For now, remove rows containing NA from new dataframe. This is most of your 
#rows since there isn't any Cons_Abundance data for most SEAK streams
Chp2_MasterDat2 <- Chp2_MasterDat[complete.cases(Chp2_MasterDat),]
rownames(Chp2_MasterDat2) <- 1:nrow(Chp2_MasterDat2)

#2.1. Scale covariates for modeling ============================================
h <- apply(Chp2_MasterDat2[ , c(7,8,10)], 2, scale.default)
Chp2_scaled <- cbind.data.frame(Chp2_MasterDat2[ , c(1:5)], h)

#2.2. Predict for new streams (some old ones as well)! =========================
#Column names need to match
colnames(Chp2_scaled)[7] <- "WMA_Releases_by_Yr"

?predict #using the predict() function from lme4 specifically for merMod objects
#like lmer, glmer, etc
predict(bm1u, newdata = Chp2_scaled, type = "response")
