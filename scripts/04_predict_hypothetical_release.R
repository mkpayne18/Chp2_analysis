#-------------------------------------------------------------------------------
# Project title: Predicting out-of-sample stream attractiveness to stray
# hatchery-origin chum salmon in Southeast Alaska (MS thesis chapter 2)

# Molly K Payne

# Purpose of this script: Predict out-of-sample stream attractiveness for streams
# given hypothetical new hatchery release site scenarios

# Last updated: May 30, 2022
#-------------------------------------------------------------------------------
#The three new hypothetical release sites scenarios are completely arbitrary and 
#made-up for the sole purpose of evaluating this straying model as a predictive
#tool. The three hypothetical scenarios are:

# 1. Increase in releases from CRAWFISH INLET release site
# 2. Increase in releases from PORT ASUMCION release site
# 3. Establishment of a release site in FRESHWATER BAY on eastern Chicagof
# Island (57.933113, -135.1800125)

library(dplyr)
library(lme4)
library(MASS)

source("scripts/01_load_and_clean_covariate_data.R")
#Remove objects not needed for this script
rm(Chp2_MasterDat, dat_08_19)

#Other needed objects
stray_dat <- readRDS("output/stray_dat.rds")
bm2 <- readRDS("output/best_model2.rds")
chp1_mod <- bm2

head(dat_20_21) #contains covariate data for all 640 streams in 2020-2021


#1. Create df for modeling =====================================================
### I have verified that all of the streams within 40km of the above release 
#sites are only within 40km of those sites (i.e., none of the streams are also 
#within 40km of an additional release site). Hence, I can simply add the increased
#number of released hatchery fish to the 2020 release value for each of the creeks
#within 40km of one of the 3 release sites above (not 2021 because that AK fish-
#eries enhancement report has not been released yet)
hypothetical_release <- read.csv("data/Hypothetical_releas.csv") #contains all 
#names of streams within 40km of the release site increases/proposed sites
colnames(hypothetical_release)[2] <- "Release_site_LAT"
colnames(hypothetical_release)[3] <- "Release_site_LONG"
colnames(hypothetical_release)[4] <- "StreamName"



### Attach df containing releases by site and year and append the 2020 (most
#recent year in dataset) releases
Releases_2020 <- read.csv("data/Releases_2020.csv") #this always outputs a warning
#but the data look fine
hypRel <- left_join(hypothetical_release, Releases_2020, by = "ReleaseSite")

#Increase WMA_Releases_by_Yr by 1 SD for the hypothetical release change streams
mean(dat_20_21$WMA_Releases_by_Yr) #10.304
sd(dat_20_21$WMA_Releases_by_Yr) #across all 640 Chp2 streams, the mean num-
#ber of fish released within 40km of all streams is 10.304, and the SD is 20.836.
#I will increase the number of fish released by 20.836 (million) fish for each
#stream within a hypothetical release change (Crawfish Inlet, Port Asumcion,
#and Freshwater Bay) because that seems like a sensible number to increase by
hypRel$SUM_Releases_in_millions <- hypRel$SUM_Releases_in_millions + 20.836


#Now attach data from other covariates (CV_flow)
hypRel2 <- left_join(hypRel, dat_20_21, by = "StreamName") #to get CV_flow
hypRel2 <- hypRel2[,c(1:4,8,12:13,19)]
hypRel2 <- hypRel2[!duplicated(hypRel2),]
colnames(hypRel2)[5] <- "WMA_Releases_by_Yr"


#Note that the 2020 releases for the 3 hypothetical sites are not WMA values, so
#you are making inferences for 4 years later (5 yo fish) from 2020; 2024 onwards
#assuming that releases have remained constant at 20.836 million fish since 2020
#within 40km of the given stream 

rm(hypRel, hypothetical_release, Releases_2020)






#2. Scale covariates for out-of-sample predictions =============================
### Curry says to scale the data with the mean and sd of the data (from chp1)
#that was used to fit the model, NOT with the mean and sd of the new data:
zscore_fxn <- function(x, vec){
  (x - mean(vec))/sd(vec)
}

#Z-score standardize (scale) the chp2 covariates that were included in chapter 1
#best model (currently WMA_Releases and CV_flow only):
zWMA <- zscore_fxn(hypRel2$WMA_Releases_by_Yr, stray_dat$WMA_Releases_by_Yr)
zCVf <- zscore_fxn(hypRel2$CV_flow, stray_dat$CV_flow)

hypRel_scaled <- cbind.data.frame(hypRel2[ , c(1:4,6,7)], zWMA, zCVf)
sapply(hypRel_scaled, function(x) sum(is.na(x)))
colnames(hypRel_scaled)[7] <- "WMA_Releases_by_Yr"
colnames(hypRel_scaled)[8] <- "CV_flow"

#Remove unneeded objects:
rm(zWMA, zCVf)




#3. Predict hypothetical release stream attractiveness =========================
#Use exact same bootstrapping approach as that used in 03_predict_2020_2021.R,
#which allows you to estimate a random effect for 2020-2021 predictions (these
#fall outside of 2008-2019 and therefore do not have a random effect). See script
#03 for more info

#3.1. Bootstrap step 1: generate distributions =================================
summary(chp1_mod) #create random normal distributions using the mean and sd for
#each model covariate

### Covariate distributions
rWMA_releas <- rnorm(1000, 0.364, 0.093)
rCV_f <- rnorm(1000, 0.463, 0.088)
rCV_f2 <- rnorm(1000, 0.800, 0.081)


### Random effect distribution
#Create df containing random intercepts
re <- as.data.frame(ranef(chp1_mod)) #specifies random EFFECTS, not intercepts
re2 <- re[,c(3:4)]
names(re2) <- c("Year", "R.Intercept")
summary(chp1_mod) #mean intercept = 0.48075
re2$R.Intercept <- re2$R.Intercept + 0.48075
sd(re2$R.Intercept) #sd of the random intercepts is 0.71179
#Random intercept distribution:
rRE <- rnorm(1000, 0.48075, 0.71179)




#3.2. Make predictions using distributions =====================================
#Read in function to run model during bootstrapping process
source("scripts/run_mod2_function.R")
run_mod2

### Create empty df to store predictions from each model iteration
length(hypRel_scaled$ReleaseSite) #57
mod_preds <- data.frame(matrix(ncol = 1000, nrow = 57))
#For loop to run model!
for (i in 1:1000) {
  R.Intercept <- sample(rRE, 1, replace = T)
  WMA <- sample(rWMA_releas, 1, replace = T)
  CVf <- sample(rCV_f, 1, replace = T)
  CVf2 <- sample(rCV_f2, 1, replace = T)
  mod_preds[,i] <- run_mod2(hypRel_scaled, R.Intercept, WMA, CVf, CVf2)
}


### Summarize predictions
mod_preds
mod_preds$Mean <- rowMeans(mod_preds)
preds_hypRel <- cbind.data.frame(hypRel_scaled[,4], mod_preds$Mean)




#3.3. Calculate prediction uncertainty =========================================
### Create empty df to store predictions from each model iteration
mod_preds2 <- data.frame(matrix(ncol = 1000, nrow = 57))
#For loop to run model!
for (i in 1:1000) {
  R.Intercept <- sample(rRE, 1, replace = T)
  WMA <- sample(rWMA_releas, 1, replace = T)
  CVf <- sample(rCV_f, 1, replace = T)
  CVf2 <- sample(rCV_f2, 1, replace = T)
  mod_preds2[,i] <- run_mod2(hypRel_scaled, R.Intercept, WMA, CVf, CVf2) +
    rnegbin(1, theta = 0.9932) #add random NB deviate to each prediction so you
  #can generate a prediction interval
}


### Summarize (calculate CV for each row, link to stream data)
mod_preds2
mod_preds2$Mean <- rowMeans(mod_preds2)
mod_preds2$SD <- apply(mod_preds2, 1, sd)
mod_preds2$CV <- mod_preds2$SD/mod_preds2$Mean
pred_CV <- cbind.data.frame(hypRel_scaled[,4], mod_preds2$CV)




#3.4. Calculate mean prediction uncertainty ====================================
#Since there was only one prediction for each stream just link stream and year
#information to bootstrapped predictions and CVs (no need to summarize by year):
mean_preds_hypRel <- cbind.data.frame(preds_hypRel, pred_CV$`mod_preds2$CV`)
colnames(mean_preds_hypRel)[1] <- "StreamName"
colnames(mean_preds_hypRel)[2] <- "Mean"
colnames(mean_preds_hypRel)[3] <- "CV"

#Save this item^^ to output
saveRDS(mean_preds_hypRel, "output/mean_preds_hypRel.rds")

#Remove unneeded items from script
rm(re, re2, CVf, CVf2, i, R.Intercept, WMA, mod_preds, mod_preds2)


