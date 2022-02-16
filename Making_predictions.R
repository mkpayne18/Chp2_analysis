
#Note that in this script I often reference "high" and "low" confidence predictions.
#The high confidence predictions are predicted attractiveness indices for streams
#which have Cons_Abundance data (n = 82), and I therefore have more confidence
#in their predictions due to their having all required covariates. The low conf-
#idence streams lack Cons_Abundance data (section 4 and beyond of this script)
#but I still predicted their attractiveness indices using WMA_Releases_by_Yr and
#CV_flow


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

#Stick 2020 and 2021 streams into a separate dataset for part 2 of chapter 2
X20_21_Chp2 <- Chp2_MasterDat[Chp2_MasterDat$Year %in% c("2020", "2021"),]
Chp2_MasterDat <- Chp2_MasterDat[!Chp2_MasterDat$Year %in% c("2020", "2021"),]


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
#2/15/22: No longer using second best candidate model for making high-confidence
#predictions, but you will still use it below to make lower-confidence predictions
#in section 4.
#Only use 1st model to make predictions for high-confidence streams so that I can
#more easily estimate uncertainty around the predictions



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

### Second best model (bm2u): NO LONGER USING
#preds2 <- as.data.frame(predict(bm2u, newdata = Chp2_scaled, type = "response"))
#Mod2_Chp2_predictions <- cbind.data.frame(Chp2_scaled[, c(1:7)], preds2)
#colnames(Mod2_Chp2_predictions)[8] <- "Predictions"

#Take the weighted average of the two models' predictions (63-37 for mod 1 vs 2,
#see chp 1 model fitting script or manuscript for this result)
#vec2 <- (Mod1_Chp2_predictions$Predictions*0.63) +
 # (Mod2_Chp2_predictions$Predictions*0.37)
#Chp2_predictions <- cbind.data.frame(Mod1_Chp2_predictions[,c(1:7)], vec2)
#colnames(Chp2_predictions)[8] <- "Chp2_predictions"



#2.3. Calculate prediction uncertainty using bootstrap approach ================

### Bootstrapping steps
#1) Run rnorm( ) for each covariate to specify a distribution for that covariate.
#E.g., if my coefficient estimate is 0.215 and its SE is 0.12, then my 
#rnorm(1000, mean = 0.215, sd = 0.12). 

#2) Randomly sample 1000 values, with replacement, from the rnorm distribution
#for each covariate, calculate a mean coefficient estimate each time you sample 

#3) Run the model with mean coefficient estimate, make predictions, store
#predictions

#4) Repeat steps 2-3 1000 times

#5) Calculate a CV or 95% confidence interval from each set of 1000 (1000 for
#each individual prediction -> get one CV or 95% CI for each prediction)


######   Bootstrap step 1)   ######
summary(bm1u) #create random normal distributions using the mean and sd for each
#model covariate, i.e., the coefficient estimate and standard error
rWMA_releas <- rnorm(1000, 0.412, 0.086)
rCons_A <- rnorm(1000, 0.215, 0.120)
rCV_f <- rnorm(1000, 0.503, 0.091)
rCV_f2 <- rnorm(1000, 0.623, 0.078)


######   Bootstrap steps 2), 3), 4)   ######
#Recall that you have a random effect of year, so the intercept will vary for 
#your predictions depending on the year. You can incorporate this into the model
#most easily by creating a column of the appropriate random intercepts:
#Create column of random intercepts for each year
re <- as.data.frame(ranef(bm1u)) #specifies random EFFECTS, not intercepts
re2 <- re[,c(3:4)]
names(re2) <- c("Year", "R.Intercept")
summary(bm1u)
re2$R.Intercept <- re2$R.Intercept + 0.41315 #0.41315 is the overall (mean)
#intercept from the bm1u model output. re2$RE should match the (Intercept) column
#specified by:
coef(bm1u) #but I wasn't able to make this a df by itself. Hence the above steps
eval_HC_df <- left_join(Chp2_scaled, re2, by = "Year")


run_mod <- function(WMA, CA, CVf, CVf2){
  y <- exp(eval_HC_df$R.Intercept +  (WMA * eval_HC_df$WMA_Releases_by_Yr) -
             (CA * eval_HC_df$Cons_Abundance) + (CVf * eval_HC_df$CV_flow) +
             (CVf2 * (eval_HC_df$CV_flow)^2))
} #function I will use to run model with each sampled rnorm() coefficient estimate


#Create empty df to store predictions from each model iteration
mod_preds <- data.frame(matrix(ncol = 1000, nrow = 820))
#For loop to run model!
for (i in 1:1000) {
  WMA <- mean(sample(rWMA_releas, 1000, replace = T))
  CA <- mean(sample(rCons_A, 1000, replace = T))
  CVf <- mean(sample(rCV_f, 1000, replace = T))
  CVf2 <- mean(sample(rCV_f2, 1000, replace = T))
  mod_preds[,i] <- run_mod(WMA, CA, CVf, CVf2)
}
mod_preds
mod_preds$Mean <- rowMeans(mod_preds)
mod_preds$SD <- apply(mod_preds, 1, sd)


######   Bootstrap step 5)   ######
#Link stream and year information to mean and SD of bootstrapped predictions
bs_preds_HC <- cbind.data.frame(Chp2_scaled[,c(1:7)], mod_preds[,c(1001:1002)])
#this^^ gives the mean and sd of the 1000 bootstrapped ("bs") model predictions
#for each individual stream-year prediction. You want to know the mean and sd for
#each stream overall:
CV_HC <- bs_preds_HC %>% group_by(StreamName) %>%
  summarise(Mean = mean(Mean), SD = max(SD)) #find the max of the SD, not the 
#mean so that I am showing the maximum possible amount of uncertainty around a 
#mean predicted attractiveness index across time
CV_HC$CV <- CV_HC$SD/CV_HC$Mean
CV_HC$CV_percent <- CV_HC$CV*100




#2.4. Compare chp1 and chp2 predictions ========================================
#That is, for the streams that appear in both datasets (the original 56 streams),
#compare the predictions between the chp1 and chp2 models
### Best model (bm1u):
preds11 <- as.data.frame(fitted(bm1u))
Mod1_Chp1_predictions <- cbind.data.frame(f_scaled[,c(1:3)], preds11)
colnames(Mod1_Chp1_predictions)[4] <- "Chp1_predictions"

### Second best model (bm2u):
#preds22 <- as.data.frame(fitted(bm2u))
#Mod2_Chp1_predictions <- cbind.data.frame(f_scaled[,c(1:3)], preds22)
#colnames(Mod2_Chp1_predictions)[4] <- "Chp1_predictions"

#Take the weighted average
#vec1 <- (Mod1_Chp1_predictions$Chp1_predictions*0.63) +
 # (Mod2_Chp1_predictions$Chp1_predictions*0.37)
#Chp1_predictions <- cbind.data.frame(Mod1_Chp1_predictions[,c(1:3)], vec1)
#colnames(Chp1_predictions)[4] <- "Chp1_predictions"




df <- inner_join(Mod1_Chp2_predictions, Mod1_Chp1_predictions,
                 by = c("StreamName", "Year"))
Xdf <- anti_join(Mod1_Chp1_predictions, df, by = c("StreamName", "Year")) #Ushk,
#Camp Coogan, and Staney Creek did not 'join' the new df because they were not
#included in the model predictions in the Chp2 dataset. The reason they were not
#included was because they had no Cons_Abundance data. The Cons_Abundance data
#they did have in Chp1 was interpolated (see Cons_Abundance methods detail in
#chp 1 Data_sources folder for more info). Since these creeks only had 1-3 years
#of data (not 10), which was interpolated anyway, I did not include them in the
#Chp2_scaled dataset for modeling so that I would only include streams with actual
#data (not interpolated) for each covariate. 

colnames(df)[1] <- "Subregion"
Combined_preds <- df[,c(1:7,10,8)]
plot(Combined_preds$Predictions ~ Combined_preds$Chp1_predictions)
lm_pred <- lm(Predictions ~ Chp1_predictions, data = Combined_preds)
summary(lm_pred)
abline(lm_pred)
abline(0,1, col = "red")
#Conclusions: Chapter 2 predictions are larger than Chp 1 predictions (1.4x on
#average), but the relationship is very linear, which is to say, the same streams
#are predicted to be attractive/unattractive as before



### Comparison of predictions from before and after 2/15/22 update: The update
#was to only use the single best candidate model (bm1u) to predict stream attract-
#iveness for chp 2 streams, instead of model averaging predictions from candidate
#mods #1 and #2. Here I compare the differences between predictions for only the
#single best candidate model (Mod1_Chp2_predictions object) and the predictions
#that were averaged based on mods #1 & #2 AICc weights. Uncomment-out the code
#chunks below where I create Mod1_Chp2_predictions to recreate this object (called
#Chp2_predictions) in the future if need be (approximately line 106 above)
plot(Mod1_Chp2_predictions$Predictions ~ Chp2_predictions$Chp2_predictions)
#Create a ggplot to use in supplementary material
for_supplem <- data.frame(Mod1_Chp2_predictions$Predictions,
                          Chp2_predictions$Chp2_predictions)
sup_plot <- ggplot(for_supplem, aes(Mod1_Chp2_predictions.Predictions,
                                    Chp2_predictions.Chp2_predictions)) +
  geom_point() +
  labs(x = "Single model predictions", y = "Model-averaged predictions") +
  theme_bw() + theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman"))
sup_plot
getwd()
#Export as high-res figure
tiff("Model_compare_supple.tiff", width = 7, height = 4, pointsize = 12,
     units = 'in', res = 300)
sup_plot #graph that you want to export
dev.off( )



#3. Visualization of predicted attractiveness for high confidence streams ######
### Map of chp2 predictions
#First find averages by site
mean_predsChp2 <- Mod1_Chp2_predictions %>% group_by(StreamName) %>%
  mutate(mean(Predictions)) #%>% ungroup
mean_predsChp2 <- mean_predsChp2 %>% select(-c(7,8))
colnames(mean_predsChp2)[7] <- "Mean_pred_strays"
mean_predsChp2 <- mean_predsChp2[!duplicated(mean_predsChp2$Mean_pred_strays),]
#Calculate percentiles of # of strays
p <- scale(mean_predsChp2$Mean_pred_strays,
           center=min(mean_predsChp2$Mean_pred_strays),
           scale=diff(range(mean_predsChp2$Mean_pred_strays)))
mean_predsChp2$Percentile <- p

#Get map
library(ggmap)
library(reshape2)
library(scales)
myMap <- get_stamenmap(location <- c(-137, 54.5, -130, 59.5), zoom = 6,
                       maptype = "terrain-background", crop = TRUE)
ggmap(myMap)
strays_map1 <- ggmap(myMap) + geom_point(aes(x = LONGITUDE, y = LATITUDE,
                                                fill = Percentile), size = 4,
                                            colour = "black", pch = 21,
                                            data = mean_predsChp2) +
  labs(x = "Latitude", y = "Longitude", fill = "Predicted Index
Percentile") +
  guides(size = "none") +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman")) +
  scale_fill_gradientn(colours = c("#CFE8F3", "#A2D4EC", "#73BFE2",
                                   "#46ABDB", "#1696D2", "#12719E", "#0A4C6A",
                                   "#062635"), values = rescale(c(0,10,25,50,75,200)))
strays_map1

#create inset map:
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
world <- ne_countries(scale='medium',returnclass = 'sf')
usa_can <- subset(world, admin == "United States of America" | admin == "Canada")
alaska <- ggplot(data = usa_can) +
  geom_sf(fill = "grey") +
  coord_sf(crs = st_crs(3467), xlim = c(-1000000, 1800000), ylim = c(250000, 
                                                                     2500000),
           expand = FALSE, datum = NA) + geom_rect(aes(xmin = 900000,
                                                       xmax = 1550000,
                                                       ymin = 700000,
                                                       ymax = 1270000),
                                                   fill = "transparent",
                                                   color = "black", size = 1.5) +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

#add to original figure
library(grid) #to be able to use the "grob" commands
strays_map2 <- strays_map1 + inset(grob = ggplotGrob(alaska), xmin = -133, xmax = -130,
                       ymin = 58.2, ymax = 59.55) 
strays_map2

#add scale bar and north arrow
library(ggsn)
strays_map3 <- strays_map2 + scalebar(x.min = -137.2, x.max = -135.2, y.min = 54.8,
                                      y.max = 55, dist = 50, dist_unit = "km",
                                      transform = T, height = 0.5, st.dist = 0.6,
                                      st.size = 5)
north2(strays_map3, x = 0.18, y = 0.20, symbol = 3)




#4. Make predictions for streams without straying data #########################
### This is a follow-up to section 2 above, where I removed all the streams that
#did not have Cons_Abundance data. This time I am including the streams that lack
#Cons_A data and making slightly lower confidence predictions
sapply(Chp2_MasterDat, function(x) sum(is.na(x))) #only Cons_A has NAs, good
#Remove Cons_Abundance from this dataset since it will not be used for modeling
Chp2_MasterDat3 <- Chp2_MasterDat %>% select(-8)


#4.1. Scale covariates for modeling ============================================
j <- apply(Chp2_MasterDat3[ , c(8:9)], 2, scale.default)
Chp2_scaled_all <- cbind.data.frame(Chp2_MasterDat3[ , c(1:7)], j)

#4.2 Make the predictions! =====================================================
#Use bm2u model (see section 1.3 above) because it does not contain Cons_Abundance
#as a covariate
all_preds <- as.data.frame(predict(bm2u, newdata = Chp2_scaled_all, type = "response"))
All_strms_predictions <- cbind.data.frame(Chp2_scaled_all[ , c(1:7)], all_preds)
colnames(All_strms_predictions)[8] <- "Predicted_strays"


#How do these compare to the higher confidence predictions?
a <- left_join(Mod1_Chp2_predictions, All_strms_predictions, by = c("StreamName", "Year"))
plot(a$Predictions ~ a$Predicted_strays, xlab = "Lower confidence predictions",
     ylab = "High confidence predictions") #relationship looks very linear
cor.test(a$Predictions, a$Predicted_strays) #linear indeed, however it is
#not a 1:1. The lower confidence stream predictions are way huger than the high
#confidence streams. You should use percentiles of predicted attractiveness for
#low and high confidence stream predictions if you want to be able to include them
#in the same tool or show them in the same figures



#4.2. Calculate prediction uncertainty using bootstrap approach ================
#Adapted from section 2.3 above

### Bootstrapping steps
#1) Run rnorm( ) for each covariate to specify a distribution for that covariate.
#E.g., if my coefficient estimate is 0.215 and its SE is 0.12, then my 
#rnorm(1000, mean = 0.215, sd = 0.12). 

#2) Randomly sample 1000 values, with replacement, from the rnorm distribution
#for each covariate, calculate a mean coefficient estimate each time you sample 

#3) Run the model with mean coefficient estimate, make predictions, store
#predictions

#4) Repeat steps 2-3 1000 times

#5) Calculate a CV or 95% confidence interval from each set of 1000 (1000 for
#each individual prediction -> get one CV or 95% CI for each prediction)


######   Bootstrap step 1)   ######
summary(bm2u) #create random normal distributions using the mean and sd for each
#model covariate, i.e., the coefficient estimate and standard error
rWMA_releasLC <- rnorm(1000, 0.436, 0.086)
rCV_fLC <- rnorm(1000, 0.564, 0.084)
rCV_f2LC <- rnorm(1000, 0.616, 0.079)


######   Bootstrap steps 2), 3), 4)   ######
#Recall that you have a random effect of year, so the intercept will vary for 
#your predictions depending on the year. You can incorporate this into the model
#most easily by creating a column of the appropriate random intercepts:
#Create column of random intercepts for each year
re_LC <- as.data.frame(ranef(bm2u)) #specifies random EFFECTS, not intercepts
re_LC2 <- re_LC[,c(3:4)]
names(re_LC2) <- c("Year", "R.Intercept")
summary(bm2u)
re_LC2$R.Intercept <- re_LC2$R.Intercept + 0.43488 #0.43488 is the overall (mean)
#intercept from the bm2u model output. re2$RE should match the (Intercept) column
#specified by:
coef(bm2u) #but I wasn't able to make this a df by itself. Hence the above steps
eval_LC_df <- left_join(Chp2_scaled_all, re_LC2, by = "Year")


run_modLC <- function(WMA, CVf, CVf2){ #no Cons_Abundance this time
  y <- exp(eval_LC_df$R.Intercept +  (WMA * eval_LC_df$WMA_Releases_by_Yr) +
             (CVf * eval_LC_df$CV_flow) + (CVf2 * (eval_LC_df$CV_flow)^2))
} #function I will use to run model with each sampled rnorm() coefficient estimate


#Create empty df to store predictions from each model iteration
mod_predsLC <- data.frame(matrix(ncol = 1000, nrow = 6400))
#For loop to run model!
for (i in 1:1000) {
  WMA <- mean(sample(rWMA_releasLC, 1000, replace = T))
  CVf <- mean(sample(rCV_fLC, 1000, replace = T))
  CVf2 <- mean(sample(rCV_f2LC, 1000, replace = T))
  mod_predsLC[,i] <- run_modLC(WMA, CVf, CVf2)
}
mod_predsLC
mod_predsLC$Mean <- rowMeans(mod_predsLC)
mod_predsLC$SD <- apply(mod_predsLC, 1, sd)


######   Bootstrap step 5)   ######
#Link stream and year information to mean and SD of bootstrapped predictions
bs_preds_LC <- cbind.data.frame(Chp2_scaled_all[,c(1:7)], mod_predsLC[,c(1001:1002)])
#this^^ gives the mean and sd of the 1000 bootstrapped ("bs") model predictions
#for each individual stream-year prediction. You want to know the mean and sd for
#each stream overall:
CV_LC <- bs_preds_LC %>% group_by(StreamName) %>%
  summarise(Mean = mean(Mean), SD = max(SD)) #find the max of the SD, not the 
#mean so that I am showing the maximum possible amount of uncertainty around a 
#mean predicted attractiveness index across time
CV_LC$CV <- CV_LC$SD/CV_LC$Mean
CV_LC$CV_percent <- CV_LC$CV*100
#remove duplicated streams between high and low confidence sets:
CV_LC <- anti_join(CV_LC, CV_HC, by = "StreamName")





############  UPDATED UP TIL THIS POINT 2/15/22 ################################




#5. Visualization of predicted attractiveness for low confidence streams #######
#First find averages by site
ALLmean_predsChp2 <- All_strms_predictions %>% group_by(StreamName) %>%
  mutate(mean(Predicted_strays)) #%>% ungroup
ALLmean_predsChp2 <- ALLmean_predsChp2 %>% select(-c(7:8))
colnames(ALLmean_predsChp2)[7] <- "Mean_pred_strays"
#some streams which share watersheds have the same CV_flow data, so their pred-
#icted number of strays are the same. Hence, using a !duplicated function to 
#remove the excess rows for each stream based on the Mean_pred_strays column
#will not work. Use the following line of code instead:
library(plyr)
ALLmean_predsChp2 <- ddply(ALLmean_predsChp2, "StreamName", function(x) head(x,1))
#remove the 82 higher confidence streams
lowr_conf_preds <- anti_join(ALLmean_predsChp2, mean_predsChp2, by = "StreamName")
#Sullivan Creek, Barlow Cove W Shore, and Beardslee River all have mean predicted
#indices in the thousands, compared to <200 for all other sites. As a result, you
#are unable to see really any variation in the data beyond those 3 streams. I would
#suggest changing their values to be closer to the range of the data, but still
#accurately reflect how they are the most predicted attractive streams
lowr_conf_preds[lowr_conf_preds$StreamName %in%
                  c("Sullivan Creek", "Barlow Cove W Shore"), 7] <- 250
lowr_conf_preds[lowr_conf_preds$StreamName == "Beardslee River", 7] <- 200


#calculate and append a percentile column for the predicted attractiveness indices
#so that you can use alongsisde the higher confidence predicted attractinveness
#indices which have a vastly different range
p2 <- scale(lowr_conf_preds$Mean_pred_strays,
           center=min(lowr_conf_preds$Mean_pred_strays),
           scale=diff(range(lowr_conf_preds$Mean_pred_strays)))
lowr_conf_preds$Percentile <- p2



strays_map1a <- ggmap(myMap) + geom_point(aes(x = LONGITUDE, y = LATITUDE,
                                             fill = Percentile), size = 2,
                                         colour = "black", pch = 21,
                                         data = lowr_conf_preds) +
  labs(x = "Latitude", y = "Longitude", fill = "Predicted Index
Percentile") +
  guides(size = "none") +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman")) +
  scale_fill_gradientn(colours = c("#CFE8F3", "#A2D4EC", "#73BFE2",
                                   "#46ABDB", "#1696D2", "#12719E", "#0A4C6A",
                                   "#062635"), values = rescale(c(0,0.1,0.25,1)))
strays_map1a

#add to original figure
strays_map2a <- strays_map1a + inset(grob = ggplotGrob(alaska), xmin = -133, xmax = -130,
                                   ymin = 58.2, ymax = 59.55) 
strays_map2a

#add scale bar and north arrow
strays_map3a <- strays_map2a + scalebar(x.min = -137.2, x.max = -135.2, y.min = 54.8,
                                      y.max = 55, dist = 50, dist_unit = "km",
                                      transform = T, height = 0.5, st.dist = 0.6,
                                      st.size = 5)
north2(strays_map3a, x = 0.18, y = 0.20, symbol = 3)



#6. Create multi-panel map #####################################################
#Read in hatchery release site location data
H_Release_Locations <-
  read_csv("~/Documents/CHUM_THESIS/Data Sources/Release_Sites_Age/H_Release_Locations.csv")

### Here is the code from the overall SE AK map above if you wish to add anything
#It is paraphrased slightly, i.e., I cut out the part where I wrote all the code
#to get the inset map, and instead just use the inset map
library(ggspatial) #for more modern version of north arrow with easier code
myMap <- get_stamenmap(location <- c(-137, 54.5, -130, 59.5), zoom = 6,
                       maptype = "terrain-background", crop = TRUE)
ggmap(myMap)
strays_map1 <- ggmap(myMap) + geom_point(aes(x = LONGITUDE, y = LATITUDE,
                                             fill = Percentile), size = 2,
                                         colour = "black", pch = 24,
                                         data = lowr_conf_preds) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, fill = Percentile), size = 4,
                                         colour = "black", pch = 21,
                                         data = mean_predsChp2) +
  labs(x = "Latitude", y = "Longitude", fill = "Predicted Index
Percentile") +
  guides(fill = "none") +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman")) +
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "Times New Roman")) +
  scale_fill_gradientn(colours = c("#CFE8F3", "#A2D4EC", "#73BFE2",
                                   "#46ABDB", "#1696D2", "#12719E", "#0A4C6A",
                                   "#062635"),
                       values = rescale(c(0,10,25,50,75,200))) + #additional
  #code here to add rectangles to map to show areas you are zooming into
  geom_rect(aes(xmin = -135.55,
                xmax = -134.4,
                ymin = 58.15,
                ymax = 58.75),
            fill = "transparent",
            color = "black", size = 1) +
  annotate("text", x = -135.68, y = 58.74, label = "1", fontface = 2, size = 5) +
  geom_rect(aes(xmin = -135.5,
                xmax = -134.75,
                ymin = 56.59,
                ymax = 57),
            fill = "transparent",
            color = "black", size = 1) +
  annotate("text", x = -135.65, y = 56.95, label = "2", fontface = 2, size = 5) +
  theme(plot.margin = unit(c(0,-0.5,0,1), "cm"))
  
strays_map1 #warning message about true north not being meaningful. This is bc
#I included the north arrow as an annotation on the plot, rather than using data
#to locate it. You can ignore


### Code to create inset map was here. See section 3 above for that code if you 
#want it

#add to original figure
strays_map2 <- strays_map1 + inset(grob = ggplotGrob(alaska), xmin = -133, xmax = -130,
                                   ymin = 58.2, ymax = 59.55) 
strays_map2

#add scale bar
strays_map3 <- strays_map2 + scalebar(x.min = -137, x.max = -135, y.min = 54.75,
                                      y.max = 54.95, dist = 50, dist_unit = "km",
                                      transform = T, height = 0.4, st.dist = 0.6,
                                      st.size = 4)
strays_map3


#6.1. "Zoom-in" region #1: Lynn Canal, Amalga Harbor area ======================
zoom1_map <- get_stamenmap(location <- c(-135.55, 58.15, -134.41, 58.74), zoom = 9,
                       maptype = "terrain-background", crop = TRUE)
ggmap(zoom1_map)

Amalga_map <- ggmap(zoom1_map) + geom_point(aes(x = LONGITUDE, y = LATITUDE,
                                                fill = Percentile), size = 5,
                                            colour = "black", pch = 21,
                                            data = mean_predsChp2) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE,
                                         fill = Percentile), size = 3,
                                     colour = "black", shape = 24,
                                     data = lowr_conf_preds) +
  geom_point(aes(x = Longitude, y = Latitude), shape = 22, size = 4,
             fill = "darkred", data = H_Release_Locations) +
  labs(x = "", y = "", fill = "Predicted
Index
Percentile") +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman")) +
  annotate("text", x = -135.49, y = 58.7, label = "1", fontface = 2, size = 8) +
  theme(plot.margin = unit(c(1,0,0,-0.5), "cm")) + #the plot.margin positioning
  #specified here makes the plot have no margin space at the bottom or on its left
  #side, and I did that so that it would be closer to the Crawfish_map below it
  #and the SEAK strays map to its left when I put them together in ggarrange()
  scalebar(x.min = -135.45, x.max = -134.8, y.min = 58.2, y.max = 58.24,
           transform = T, dist_unit = "km", dist = 20, height = 0.6,
           st.dist = 0.6, st.size = 4.5) +
  scale_fill_gradientn(colours = c("#CFE8F3", "#A2D4EC", "#73BFE2",
                                   "#46ABDB", "#1696D2", "#12719E", "#0A4C6A",
                                   "#062635"), values = rescale(c(0,0.1,0.25,1)))
Amalga_map



#6.2. "Zoom-in" region #2: Crawfish Inlet ======================================
#Previously I had zoomed in on the Neets Bay area. Here is the map for that if
#you wish to return to using that area again:
#zoom2_map <- get_stamenmap(location <- c(-132.29, 55.5, -131.43, 55.97), zoom = 10,
                           #maptype = "terrain-background", crop = TRUE)
#ggmap(zoom2_map)

zoom3_map <- get_stamenmap(location <- c(-135.53, 56.6, -134.73, 57.03), zoom = 10,
                           maptype = "terrain-background", crop = TRUE)
ggmap(zoom3_map)

Crawfish_map <- ggmap(zoom3_map) + geom_point(aes(x = LONGITUDE, y = LATITUDE,
                                  fill = Percentile), size = 5,
                              colour = "black", pch = 21,
                              data = mean_predsChp2) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE,
                 fill = Percentile), size = 3,
             colour = "black", shape = 24,
             data = lowr_conf_preds) +
  geom_point(aes(x = Longitude, y = Latitude), shape = 22, size = 4,
             fill = "darkred", data = H_Release_Locations) +
  labs(x = "", y = "", fill = "Predicted
Index
Percentile") +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman")) +
  annotate("text", x = -135.48, y = 57, label = "2", fontface = 2, size = 8) +
  theme(plot.margin = unit(c(0,0,1,-0.5), "cm")) + #the plot.margin positioning
  #specified here makes the plot have no margin space at the top or on its left
  #side, and I did that so that it would be closer to the Amalga_map above it
  #and the SEAK strays map to its left when I put them together in ggarrange()
  scalebar(x.min = -135.5, x.max = -134.83, y.min = 56.64, y.max = 56.67,
           transform = T, dist_unit = "km", dist = 20, height = 0.6,
           st.dist = 0.7, st.size = 4.5) +
  scale_fill_gradientn(colours = c("#CFE8F3", "#A2D4EC", "#73BFE2",
                                   "#46ABDB", "#1696D2", "#12719E", "#0A4C6A",
                                   "#062635"), values = rescale(c(0,0.1,0.25,1)))
Crawfish_map


#6.3. Put whole map together ===================================================
library(gridExtra)
library(ggpubr)

right_side <- ggarrange(Amalga_map, Crawfish_map, ncol = 1,
                        common.legend = T, legend = "right")

whole_map <- ggarrange(strays_map3, right_side, align = "v",
                       common.legend = T, legend = "right")
whole_map #hot damn

#Export as high-res figure
tiff("fig1.tiff", width = 9, height = 6, pointsize = 12, units = 'in', res = 300)
whole_map #graph that you want to export
dev.off( )




#7. Data for range of attractiveness table #####################################
head(mean_predsChp2) #high confidence mean predictions by stream are in this df
head(lowr_conf_preds) #lower confidence mean predictions by stream are in this df

### Recall that percentile means that X% of the values are equal to or lower than
#that score. So the 90% percentile should contain all values in the top 91-100%,
#not 90-100%

#7.1. High confidence streams range of attractiveness table ====================
hc91100 <- mean_predsChp2 %>% filter(Percentile > quantile(Percentile, 0.9)) #cannot
#get this to work for some reason, keeps returning empty tibble

#Base R solution:
hc_91_100 <- as.data.frame(mean_predsChp2[mean_predsChp2$Percentile >
                                                quantile(mean_predsChp2$Percentile,
                                                         0.9),])
hc_0_20 <- as.data.frame(mean_predsChp2[mean_predsChp2$Percentile <=
                                            quantile(mean_predsChp2$Percentile,
                                                     0.2),])
Xhc_21_90 <- anti_join(mean_predsChp2, hc_91_100, by = "Percentile")
hc_21_90 <- anti_join(Xhc_21_90, hc_0_20, by = "Percentile")

Percentile_range <- c("91-100", "21-90", "0-20")
hc_n <- c(length(hc_91_100$Percentile), length(hc_21_90$Percentile),
          length(hc_0_20$Percentile))
hc_df <- cbind.data.frame(Percentile_range, hc_n)


### Append mean and range of covariate data to each percentile grouping
head(Chp2_MasterDat2)
length(Chp2_MasterDat2$WMA_Releases_by_Yr) #only contains the 82 streams with Cons_
#Abundance data (AKA the high confidence streams we are summarizing here in 7.1)
hc_covariate_dat <- Chp2_MasterDat2 %>% group_by(StreamName) %>%
  summarize(across(c(WMA_Releases_by_Yr, Cons_Abundance, CV_flow), mean))
  
hc_91_100a <- left_join(hc_91_100, hc_covariate_dat, by = "StreamName")
hc_21_90a <- left_join(hc_21_90, hc_covariate_dat, by = "StreamName")
hc_0_20a <- left_join(hc_0_20, hc_covariate_dat, by = "StreamName")


hc_list <- list(hc_91_100a, hc_21_90a, hc_0_20a)

cov_fun <- function(x){
  out <- x %>% summarise(across(c(WMA_Releases_by_Yr, Cons_Abundance, CV_flow),
                                c(mean, min, max)))
  return(out)
}

hc_df2 <- purrr::map_df(hc_list, cov_fun)
HC_table <- cbind.data.frame(hc_df, hc_df2)
getwd()
write.csv(HC_table, "High_confidence_streams.csv")


#7.2. Lower confidence streams range of attractiveness table ===================
lc_91_100 <- as.data.frame(lowr_conf_preds[lowr_conf_preds$Percentile >
                                            quantile(lowr_conf_preds$Percentile,
                                                     0.9),])
lc_0_20 <- as.data.frame(lowr_conf_preds[lowr_conf_preds$Percentile <=
                                          quantile(mean_predsChp2$Percentile,
                                                   0.2),])
Xlc_21_90 <- anti_join(lowr_conf_preds, lc_91_100, by = "Percentile")
lc_21_90 <- anti_join(Xlc_21_90, lc_0_20, by = "Percentile")

lc_n <- c(length(lc_91_100$Percentile), length(lc_21_90$Percentile),
          length(lc_0_20$Percentile))
lc_df <- cbind.data.frame(Percentile_range, lc_n)


### Append mean and range of covariate data to each percentile grouping
head(Chp2_MasterDat3)
length(Chp2_MasterDat3$WMA_Releases_by_Yr) #contains 640 streams (6400 rows), 
#including the 82 high-confidence streams. These need to be removed
use_for_lc_covariate <- anti_join(Chp2_MasterDat3, Chp2_MasterDat2,
                                  by = c("StreamName", "Year"))
lc_covariate_dat <- use_for_lc_covariate %>% group_by(StreamName) %>%
  summarize(across(c(WMA_Releases_by_Yr, CV_flow), mean))

lc_91_100a <- left_join(lc_91_100, lc_covariate_dat, by = "StreamName")
lc_21_90a <- left_join(lc_21_90, lc_covariate_dat, by = "StreamName")
lc_0_20a <- left_join(lc_0_20, lc_covariate_dat, by = "StreamName")


lc_list <- list(lc_91_100a, lc_21_90a, lc_0_20a)

cov_fun2 <- function(x){ #doesn't include Cons_Abundance this time
  out <- x %>% summarise(across(c(WMA_Releases_by_Yr, CV_flow),
                                c(mean, min, max)))
  return(out)
}

lc_df2 <- purrr::map_df(lc_list, cov_fun2)
LC_table <- cbind.data.frame(lc_df, lc_df2)
write.csv(LC_table, "Lower_confidence_streams.csv")




save.image("Making_predictions_objects.RData")
load("Making_predictions_objects.RData")




#1. Run rnorm( ) for each covariate to specify a distribution for that covariate
#2. Randomly sample from your rnorm distribution for each covariate 
#3. Run your model, make predictions, store predictions
#4. Repeat steps 2 & 3 1000(?) times: resample that same rnorm() distribution
#for each covariate with replacement
#5. Calculate CV for each site, e.g., you have 10 predictions for Fish Creek for
#each of 10 years






