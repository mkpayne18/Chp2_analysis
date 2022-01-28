

#Read in data
getwd()
Chp2_MasterDat <- read.csv("Chp2_Master_dataset.csv")
library(tidyverse)
library(dplyr)
#As before, make sure that 'year' is a factor variable and the NAs in WMA_Releases_
#_by_Yr are converted to 0s, because 0 fish were released within 40km of those 
#streams, not NA fish
Chp2_MasterDat$Year <- as.factor(Chp2_MasterDat$Year)
Chp2_MasterDat$WMA_Releases_in_millions[is.na(Chp2_MasterDat$WMA_Releases_in_millions)] <- 0
str(Chp2_MasterDat)

### Initial tailoring misc.
#Pink_Abundance and mean_flow are no longer a part of the model
Chp2_MasterDat <- Chp2_MasterDat %>%  select(-c(8,11))
#Correct name of WMA_Releases_in_millions to WMA_Releases_by_Yr to match Chp1
colnames(Chp2_MasterDat)[9] <- "WMA_Releases_by_Yr"
#Remove streams that are missing flow data (should leave you with 640 streams with
#10 years of data each = 640 total rows)
Chp2_MasterDat <- Chp2_MasterDat[!is.na(Chp2_MasterDat$CV_flow),]



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
sapply(Chp2_MasterDat, function(x) sum(is.na(x))) #only Cons_A has NAs, good
Chp2_MasterDat2 <- Chp2_MasterDat[complete.cases(Chp2_MasterDat),]
rownames(Chp2_MasterDat2) <- 1:nrow(Chp2_MasterDat2) #820 total. There are 87
#streams that have Cons_A data, but only 82 of them have flow data
sapply(Chp2_MasterDat2, function(x) sum(is.na(x))) #No NAs


#2.1. Scale covariates for modeling ============================================
h <- apply(Chp2_MasterDat2[ , c(8:10)], 2, scale.default)
Chp2_scaled <- cbind.data.frame(Chp2_MasterDat2[ , c(1:7)], h)


#2.2. Predict for new streams (some old ones as well)! =========================
?predict #using the predict() function from lme4 specifically for merMod objects
#like lmer, glmer, etc
### Best model (bm1u):
preds1 <- as.data.frame(predict(bm1u, newdata = Chp2_scaled, type = "response"))
Mod1_Chp2_predictions <- cbind.data.frame(Chp2_scaled[, c(1:7)], preds1)
colnames(Mod1_Chp2_predictions)[8] <- "Predictions"

### Second best model (bm2u):
preds2 <- as.data.frame(predict(bm2u, newdata = Chp2_scaled, type = "response"))
Mod2_Chp2_predictions <- cbind.data.frame(Chp2_scaled[, c(1:7)], preds2)
colnames(Mod2_Chp2_predictions)[8] <- "Predictions"

#Take the weighted average of the two models' predictions (63-37 for mod 1 vs 2,
#see chp 1 model fitting script or manuscript for this result)
vec2 <- (Mod1_Chp2_predictions$Predictions*0.63) +
  (Mod2_Chp2_predictions$Predictions*0.37)
Chp2_predictions <- cbind.data.frame(Mod1_Chp2_predictions[,c(1:7)], vec2)
colnames(Chp2_predictions)[8] <- "Chp2_predictions"


#2.3 Compare chp1 and chp2 predictions =========================================
#That is, for the streams that appear in both datasets (the original 56 streams),
#compare the predictions between the chp1 and chp2 models
### Best model (bm1u):
preds11 <- as.data.frame(fitted(bm1u))
Mod1_Chp1_predictions <- cbind.data.frame(f_scaled[,c(1:3)], preds11)
colnames(Mod1_Chp1_predictions)[4] <- "Chp1_predictions"

### Second best model (bm2u):
preds22 <- as.data.frame(fitted(bm2u))
Mod2_Chp1_predictions <- cbind.data.frame(f_scaled[,c(1:3)], preds22)
colnames(Mod2_Chp1_predictions)[4] <- "Chp1_predictions"

#Take the weighted average
vec1 <- (Mod1_Chp1_predictions$Chp1_predictions*0.63) +
  (Mod2_Chp1_predictions$Chp1_predictions*0.37)
Chp1_predictions <- cbind.data.frame(Mod1_Chp1_predictions[,c(1:3)], vec1)
colnames(Chp1_predictions)[4] <- "Chp1_predictions"




df <- inner_join(Chp2_predictions, Chp1_predictions, by = c("StreamName", "Year"))
Xdf <- anti_join(Chp1_predictions, df, by = c("StreamName", "Year")) #Ushk, Camp
#Coogan, and Staney Creek did not 'join' the new df because they were not included
#in the model predictions in the Chp2 dataset. The reason they were not included
#was because they had no Cons_Abundance data. The Cons_Abundance data they did 
#have in Chp1 was interpolated (see Cons_Abundance methods detail in chp 1 Data_
#sources folder for more info). Since these creeks only had 1-3 years of data
#(not 10), which was interpolated anyway, I did not include them in the Chp2_scaled
#dataset for modeling. You'll still have their predictions from chp1 you just won't
#be comparing them to any chp2 predictions

colnames(df)[1] <- "Subregion"
Combined_preds <- df[,c(1:7,10,8)]
plot(Combined_preds$Chp2_predictions ~ Combined_preds$Chp1_predictions)
lm_pred <- lm(Chp2_predictions ~ Chp1_predictions, data = Combined_preds)
summary(lm_pred)
abline(lm_pred)
abline(0,1, col = "red")
#Conclusions: Chapter 2 predictions are larger than Chp 1 predictions (1.4x on
#average), but the relationships is very linear, which is to say, the same streams
#are predicted to be attractive/unattractive as before



#3. Visualization ##############################################################
### Map of chp2 predictions
#First find averages by site
mean_predsChp2 <- Chp2_predictions %>% group_by(StreamName) %>%
  mutate(mean(Chp2_predictions)) #%>% ungroup
mean_predsChp2 <- mean_predsChp2 %>% select(-8)
colnames(mean_predsChp2)[8] <- "Mean_pred_strays"
mean_predsChp2 <- mean_predsChp2[!duplicated(mean_predsChp2$Mean_pred_strays),]

#Get map
library(ggmap)
myMap <- get_stamenmap(location <- c(-137, 54.5, -130, 59.5), zoom = 6,
                       maptype = "terrain-background", color = "bw", crop = TRUE)
ggmap(myMap)
strays_mapChp2 <- ggmap(myMap) + geom_point(aes(x = LONGITUDE, y = LATITUDE, size = 10,
                                                        fill = Mean_pred_strays),
                                                    colour = "black", pch = 21,
                                                    data = mean_predsChp2)
strays_mapChp2
