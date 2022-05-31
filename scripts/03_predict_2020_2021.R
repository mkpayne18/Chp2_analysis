#-------------------------------------------------------------------------------
# Project title: Predicting out-of-sample stream attractiveness to stray
# hatchery-origin chum salmon in Southeast Alaska (MS thesis chapter 2)

# Molly K Payne

# Purpose of this script: Predict out-of-sample stream attractiveness for streams
# beyond (and including) the original 57 streams in chapter 1 in 2020 and 2021

# Last updated: May 30, 2022
#-------------------------------------------------------------------------------
#I've updated model covariate data to 2020-2021 values so that in this script I 
#can predict stream attractiveness to streams in 2020-2021
library(dplyr)
library(lme4)
library(MASS)

source("scripts/01_load_and_clean_covariate_data.R")
#Remove objects not needed for this script
rm(Chp2_MasterDat, dat_08_19)

head(dat_20_21) #contains covariate data for all 640 streams in 2020-2021




#1. Read in necessary objects ==================================================
### Best model object from chapter 1
#In 03_fit_NB_model.R script in Chp1_analysis, I outputted the best model object,
#titled "best_model2.rds" to the output folder here in chapter 2, that way any
#changes I make to the model in chapter 1 will be reflected here:
bm2 <- readRDS("output/best_model2.rds")
summary(bm2)
#Define this as a new object in this script in case the "best model" object from
#chapter 1 ever changes, that way you don't have to go through and change the 
#name of the model object through the whole script:
chp1_mod <- bm2

#You will also need chapter 1 data (covariate data for original 57 streams):
stray_dat <- readRDS("output/stray_dat.rds")





#2. Scale covariates for out-of-sample predictions =============================
### Curry says to scale the data with the mean and sd of the data (from chp1)
#that was used to fit the model, NOT with the mean and sd of the new data:
zscore_fxn <- function(x, vec){
  (x - mean(vec))/sd(vec)
}

#Z-score standardize (scale) the chp2 covariates that were included in chapter 1
#best model (currently WMA_Releases and CV_flow only):
zWMA <- zscore_fxn(dat_20_21$WMA_Releases_by_Yr, stray_dat$WMA_Releases_by_Yr)
zCVf <- zscore_fxn(dat_20_21$CV_flow, stray_dat$CV_flow)

dat_20_21_scaled <- cbind.data.frame(dat_20_21[ , c(1:7)], zWMA, zCVf)
sapply(dat_20_21_scaled, function(x) sum(is.na(x)))
colnames(dat_20_21_scaled)[8] <- "WMA_Releases_by_Yr"
colnames(dat_20_21_scaled)[9] <- "CV_flow"

#Remove unneeded objects:
rm(zWMA, zCVf)





#3. Predict 2020-2021 stream attractiveness ====================================
### By predicting outside of the time range of the observed data used to fit the 
#model (2008-2019), I no longer have year random effects. Thus, I will use a
#bootstrapping approach to estimate a random effect for 2020/2021 predictions in-
#stead of making predictions directly from the model as I did for 2008-2019 data

#This means that I will generate my model predictions AND my model uncertainty
#estimates using almost the exact same bootstrapping approach. The model pred-
#ictions will be the mean of the 1000 bootstrapped values without adding an
#rnegbin() deviate to each. The uncertainty will be the coefficient of variation
#(CV) around each mean prediction, with an rnegbin() deviate added on (to gener-
#ate prediction intervals instead of confidence intervals). This approach is diff-
#erent from script 02 (2008-2019 predictions) where I used predict() the generate
#predictions straight from the model, and then used bootstrapping only to estimate
#the CV (uncertainty around predictions)

#As in 02_predict_2008_2019.R, my general bootstrapping approach is:

#1) Run rnorm( ) for each covariate to specify a distribution for that covariate.
#E.g., if my coefficient estimate is 0.364 and its SE is 0.093, then my 
#rnorm(1000, mean = 0.364, sd = 0.093). ## THIS TIME include a distribution for
#the random effect as well

#2) Randomly sample 1 value from rnorm distribution for each covariate (allowing
#replacement each time). This is your coefficient estimate to use for each model
#run

#3) Run the model with coefficient estimate, make predictions, store predictions

#4) Repeat steps 2-3 1000 times

#5) Calculate a CV or 95% prediction interval from each set of 1000 (1000 for
#each individual prediction -> get one CV or 95% CI for each prediction)


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
length(dat_20_21_scaled$Stream_Number) #1280
mod_preds <- data.frame(matrix(ncol = 1000, nrow = 1280))
#For loop to run model!
for (i in 1:1000) {
  R.Intercept <- sample(rRE, 1, replace = T)
  WMA <- sample(rWMA_releas, 1, replace = T)
  CVf <- sample(rCV_f, 1, replace = T)
  CVf2 <- sample(rCV_f2, 1, replace = T)
  mod_preds[,i] <- run_mod2(dat_20_21_scaled, R.Intercept, WMA, CVf, CVf2)
}


### Summarize predictions
mod_preds
mod_preds$Mean <- rowMeans(mod_preds)
preds_20_21 <- cbind.data.frame(dat_20_21_scaled[,c(1:3)], mod_preds$Mean)





#3.3. Calculate prediction uncertainty =========================================
### Create empty df to store predictions from each model iteration
mod_preds2 <- data.frame(matrix(ncol = 1000, nrow = 1280))
#For loop to run model!
for (i in 1:1000) {
  R.Intercept <- sample(rRE, 1, replace = T)
  WMA <- sample(rWMA_releas, 1, replace = T)
  CVf <- sample(rCV_f, 1, replace = T)
  CVf2 <- sample(rCV_f2, 1, replace = T)
  mod_preds2[,i] <- run_mod2(dat_20_21_scaled, R.Intercept, WMA, CVf, CVf2) +
    rnegbin(1, theta = 0.9932) #add random NB deviate to each prediction so you
  #can generate a prediction interval
}


### Summarize (calculate CV for each row, link to stream data)
mod_preds2
mod_preds2$Mean <- rowMeans(mod_preds2)
mod_preds2$SD <- apply(mod_preds2, 1, sd)
mod_preds2$CV <- mod_preds2$SD/mod_preds2$Mean
pred_CV <- cbind.data.frame(dat_20_21_scaled[,c(1:3)], mod_preds2$CV)



#3.4. Calculate mean predictions and uncertainty ===============================
#In the sections above, you determined the mean predicted value and CV for each
#individual stream-year row of data. Now determine the mean prediction and CV for
#each stream (across years)

#First link stream and year information to bootstrapped predictions
bootstrap_res <- cbind.data.frame(preds_20_21, pred_CV$`mod_preds2$CV`)
colnames(bootstrap_res)[4] <- "Mean"
colnames(bootstrap_res)[5] <- "CV"

### Calculate mean predictions and combine those predictions with estimated
#uncertainty
mean_preds_20_21 <- bootstrap_res %>%
  group_by(StreamName) %>%
  summarise(Mean_prediction = mean(Mean),
            Mean_CV = mean(CV))

#Save this item^^ to output
saveRDS(mean_preds_20_21, "output/mean_preds_20_21.rds")

#Remove unneeded items from script
rm(re, re2, CVf, CVf2, i, R.Intercept, WMA, mod_preds, mod_preds2)


