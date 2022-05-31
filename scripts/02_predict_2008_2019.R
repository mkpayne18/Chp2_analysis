#-------------------------------------------------------------------------------
# Project title: Predicting out-of-sample stream attractiveness to stray
# hatchery-origin chum salmon in Southeast Alaska (MS thesis chapter 2)

# Molly K Payne

# Purpose of this script: Predict out-of-sample stream attractiveness for streams
# beyond and (including) the original 57 streams in chapter 1 from 2008 to 2019

# Last updated: May 26, 2022
#-------------------------------------------------------------------------------
library(lme4)
library(dplyr)
library(MASS)


source("scripts/01_load_and_clean_covariate_data.R")
#Remove objects not needed for this script
rm(Chp2_MasterDat, dat_20_21)


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
zWMA <- zscore_fxn(dat_08_19$WMA_Releases_by_Yr, stray_dat$WMA_Releases_by_Yr)
zCVf <- zscore_fxn(dat_08_19$CV_flow, stray_dat$CV_flow)

dat_08_19_scaled <- cbind.data.frame(dat_08_19[ , c(1:7)], zWMA, zCVf)
sapply(dat_08_19_scaled, function(x) sum(is.na(x)))
colnames(dat_08_19_scaled)[8] <- "WMA_Releases_by_Yr"
colnames(dat_08_19_scaled)[9] <- "CV_flow"

#Remove unneeded objects:
rm(zWMA, zCVf)





#3. Predict stream attractiveness for chp 2 streams ============================
?predict #using the predict() function from lme4 specifically for merMod objects
#like lmer, glmer, etc

### Using best model object from chapter 1
chp1_mod

#Make predictions:
preds_helper <- as.data.frame(predict(chp1_mod, newdata = dat_08_19_scaled,
                                     type = "response"))

#Create tidy dataframe with new predictions:
preds_08_19 <- cbind.data.frame(dat_08_19_scaled[, c(1:7)], preds_helper)
colnames(preds_08_19)[8] <- "Predicted_strays"
preds_08_19$Predicted_strays <- round(preds_08_19$Predicted_strays, 1)
sum(is.na(preds_08_19$Predicted_strays))


#Remove unneeded items
rm(preds_helper)





#4. Bootstrap prediction uncertainty ===========================================
### Bootstrapping steps
#1) Run rnorm( ) for each covariate to specify a distribution for that covariate.
#E.g., if my coefficient estimate is 0.364 and its SE is 0.093, then my 
#rnorm(1000, mean = 0.364, sd = 0.093). 

#2) Randomly sample 1 value from rnorm distribution for each covariate (allowing
#replacement each time). This is your coefficient estimate to use for each model
#run

#3) Run the model with coefficient estimate, make predictions, store predictions

#4) Repeat steps 2-3 1000 times

#5) Calculate a CV or 95% prediction interval from each set of 1000 (1000 for
#each individual prediction -> get one CV or 95% CI for each prediction)



#4.1. Bootstrap step 1: create distributions ===================================
summary(chp1_mod) #create random normal distributions using the mean and sd for
#each model covariate, i.e., the coefficient estimate and standard error
rWMA_releas <- rnorm(1000, 0.364, 0.093)
rCV_f <- rnorm(1000, 0.463, 0.088)
rCV_f2 <- rnorm(1000, 0.800, 0.081)




#4.2. Generate bootstrapped predictions ========================================
### Recall that you have a random effect of year, so the intercept will vary for 
#your predictions depending on the year. You can incorporate this into the model
#most easily by creating a column of the appropriate random intercepts:
#Create column of random intercepts for each year
re <- as.data.frame(ranef(chp1_mod)) #specifies random EFFECTS, not intercepts
re2 <- re[,c(3:4)]
names(re2) <- c("Year", "R.Intercept")
summary(chp1_mod)
re2$R.Intercept <- re2$R.Intercept + 0.48075 #0.48075 is the overall (mean)
#intercept from the chp1_mod output. re2$RE should match the (Intercept) column
#specified by:
coef(chp1_mod) #but I wasn't able to make this a df by itself. Hence the above
#steps
dat_08_19_scaled <- left_join(dat_08_19_scaled, re2, by = "Year")


#Read in function to run model during bootstrapping process
source("scripts/run_mod_function.R")
run_mod


#Create empty df to store predictions from each model iteration
mod_preds <- data.frame(matrix(ncol = 1000,
                               nrow = length(dat_08_19_scaled$Subregion)))
#For loop to run model!
for (i in 1:1000) {
  WMA <- (sample(rWMA_releas, 1, replace = T))
  CVf <- (sample(rCV_f, 1, replace = T))
  CVf2 <- (sample(rCV_f2, 1, replace = T))
  mod_preds[,i] <- run_mod(dat_08_19_scaled, WMA, CVf, CVf2) +
    rnegbin(1, theta = 0.9932)
  #add random normal deviate to each prediction so you can generate a prediction 
  #interval instead of just a confidence interval
}



#4.3. Calculate prediction uncertainty =========================================
#I.e., calculate the CV (coefficient of variation), i.e., the measure of un-
#certainty around each mean (mean of the 1000) model prediction
mod_preds
mod_preds$Mean <- rowMeans(mod_preds)
mod_preds$SD <- apply(mod_preds, 1, sd)
mod_preds$CV <- mod_preds$SD/mod_preds$Mean




#4.4. Calculate mean prediction uncertainty ====================================
#In the section above, you determined the CV for each individual stream-year 
#prediction. Now determine the mean CV for each stream (across years)

#First link stream and year information to bootstrapped predictions
bootstrap_res <- cbind.data.frame(preds_08_19, mod_preds[,c(1001:1003)])

### Calculate mean predictions and combine those predictions with estimated
#uncertainty
mean_preds_08_19 <- bootstrap_res %>%
  group_by(StreamName) %>%
  summarise(Mean_prediction = mean(Predicted_strays),
  Mean_CV = mean(CV))

#Send this item^^ to output
saveRDS(mean_preds_08_19, "output/means_preds_08_19.rds")




#Remove unneeded items from script
rm(re, re2, CVf, CVf2, i, WMA)

