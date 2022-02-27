
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

#2) Randomly sample 1 value from rnorm distribution for each covariate (allowing
#replacement each time). This is your coefficient estimate to use for each model
#run

#3) Run the model with coefficient estimate, make predictions, store predictions

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
  y <- exp(eval_HC_df$R.Intercept + (WMA * eval_HC_df$WMA_Releases_by_Yr) -
             (CA * eval_HC_df$Cons_Abundance) + (CVf * eval_HC_df$CV_flow) +
             (CVf2 * (eval_HC_df$CV_flow^2)))
} #function I will use to run model with each sampled rnorm() coefficient estimate


#Create empty df to store predictions from each model iteration
mod_preds <- data.frame(matrix(ncol = 1000, nrow = 820))
#For loop to run model!
for (i in 1:1000) {
  WMA <- (sample(rWMA_releas, 1, replace = T))
  CA <- (sample(rCons_A, 1, replace = T))
  CVf <- (sample(rCV_f, 1, replace = T))
  CVf2 <- (sample(rCV_f2, 1, replace = T))
  mod_preds[,i] <- run_mod(WMA, CA, CVf, CVf2)
}
mod_preds
mod_preds$Mean <- rowMeans(mod_preds)
mod_preds$SD <- apply(mod_preds, 1, sd)


######   Bootstrap step 5)   ######
#Link stream and year information to bootstrapped predictions
bs_preds_HC <- cbind.data.frame(Mod1_Chp2_predictions, mod_preds[,c(1001:1002)])

#summarize mean and sd by stream across time to calculate a CV
#bsHC_groups <- bs_preds_HC %>% group_split(StreamName)
#bs2 <- purrr::map(bsHC_groups, ~.x %>% pivot_longer(c(9:1008)))
#bs3 <- t(as.data.frame(purrr::map(bs2, ~mean(.$value))))
#bs4 <- t(as.data.frame(purrr::map(bs2, ~sd(.$value))))
#High_conf_preds <- cbind.data.frame(mean_predsChp2, bs3, bs4)
#rownames(High_conf_preds) <- 1:nrow(High_conf_preds)
#head(High_conf_preds) #mean_pred_strays from the model are not even close to
#the mean I calculated in the 6 steps immediately above this. Not sure how to fix
#error. I will resume what I did before to calculate the CV:


#bs_preds_HC gives the mean and sd of the 1000 bootstrapped ("bs") model predi-
#ctions for each individual stream-year prediction. You want to know the mean
#and sd for each stream overall:
CV_HC <- bs_preds_HC %>% group_by(StreamName) %>%
  summarise(Mean_bs = mean(Mean), SD_bs = mean(SD)) 
CV_HC$CV <- CV_HC$SD_bs/CV_HC$Mean_bs
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

#ggplot2 version:
comp_chp1_chp2 <- ggplot(Combined_preds) + geom_point(aes(Predictions,
                                                          Chp1_predictions)) +
  labs(x = "In-sample Predicted Indices", y = "Out-of-sample Predicted Indices") +
  theme_bw() +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(text=element_text(family="Times New Roman")) 
comp_chp1_chp2

#Conclusions: Chapter 2 predictions are larger than Chp 1 predictions (1.4x on
#average), but the relationship is very linear, which is to say, the same streams
#are predicted to be attractive/unattractive as before
tiff("Outvsin_sample_supple.tiff", width = 7, height = 4, pointsize = 12,
     units = 'in', res = 300)
comp_chp1_chp2 #graph that you want to export
dev.off( )



### Comparison of predictions from before and after 2/15/22 update: The update
#was to only use the single best candidate model (bm1u) to predict stream attract-
#iveness for chp 2 streams, instead of model averaging predictions from candidate
#mods #1 and #2. Here I compare the differences between predictions for only the
#single best candidate model (Mod1_Chp2_predictions object) and the predictions
#that were averaged based on mods #1 & #2 AICc weights. Uncomment-out the code
#chunks below where I create Mod1_Chp2_predictions to recreate the object (called
#Chp2_predictions) that has model-averaged predictions for chapter 2 streams in
#the future if need be (approximately line 106 above)
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
redsChp2$Mean_pred_strays)))

#Add on the predicted uncertainty column from bootstrapping (section 2.3)
mean_predsChp2 <- left_join(mean_predsChp2, CV_HC, by = "StreamName")

#Get map
library(ggmap)
library(reshape2)
library(scales)
myMap <- get_stamenmap(location <- c(-137, 54.5, -130, 59.5), zoom = 6,
                       maptype = "terrain-background", crop = TRUE)
ggmap(myMap)
strays_map1 <- ggmap(myMap) + geom_point(aes(x = LONGITUDE, y = LATITUDE,
                                                size = Mean_pred_strays,
                                             fill = CV_percent),
                                            colour = "black", pch = 21,
                                            data = mean_predsChp2) +
  labs(x = "Latitude", y = "Longitude", size = "Predicted Index",
       fill = "Prediction CV") +
  #guides(size = "none") +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman")) +
  scale_fill_gradientn(colours = c("#062635", "#0A4C6A", "#12719E", "#1696D2",
                                   "#46ABDB", "#73BFE2", "#A2D4EC", "#CFE8F3"),
                       values = rescale(c(0,10,25,50,75,200)))
strays_map1

#color scale reversed for scale_fill_gradientn():
("#CFE8F3", "#A2D4EC", "#73BFE2",
  "#46ABDB", "#1696D2", "#12719E", "#0A4C6A",
  "#062635")

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
#not a 1:1. The lower confidence stream predictions are bigger than the high
#confidence streams by a bit




#4.2. Calculate prediction uncertainty using bootstrap approach ================
#Adapted from section 2.3 above

### Bootstrapping steps
#1) Run rnorm( ) for each covariate to specify a distribution for that covariate.
#E.g., if my coefficient estimate is 0.215 and its SE is 0.12, then my 
#rnorm(1000, mean = 0.215, sd = 0.12)

#2) Randomly sample 1 value from rnorm distribution for each covariate (allowing
#replacement each time). This is your coefficient estimate to use for each model
#run

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
             (CVf * eval_LC_df$CV_flow) + (CVf2 * (eval_LC_df$CV_flow^2)))
} #function I will use to run model with each sampled rnorm() coefficient estimate


#Create empty df to store predictions from each model iteration
mod_predsLC <- data.frame(matrix(ncol = 1000, nrow = 6400))
#For loop to run model!
for (i in 1:1000) {
  WMA <- sample(rWMA_releasLC, 1, replace = T)
  CVf <- sample(rCV_fLC, 1, replace = T)
  CVf2 <- sample(rCV_f2LC, 1, replace = T)
  mod_predsLC[,i] <- run_modLC(WMA, CVf, CVf2)
}
mod_predsLC
mod_predsLC$Mean <- rowMeans(mod_predsLC)
mod_predsLC$SD <- apply(mod_predsLC, 1, sd)


######   Bootstrap step 5)   ######
#Link stream and year information to mean and SD of bootstrapped predictions
bs_preds_LC <- cbind.data.frame(All_strms_predictions, mod_predsLC[,c(1001:1002)])
#this^^ gives the mean and sd of the 1000 bootstrapped ("bs") model predictions
#for each individual stream-year prediction. You want to know the mean and sd for
#each stream overall:
CV_LC <- bs_preds_LC %>% group_by(StreamName) %>%
  summarise(Mean_bs = mean(Mean), SD_bs = max(SD)) #find the max of the SD, not
#the mean so that I am showing the maximum possible amount of uncertainty around
#a mean predicted attractiveness index across time
CV_LC$CV <- CV_LC$SD_bs/CV_LC$Mean_bs
CV_LC$CV_percent <- CV_LC$CV*100
#remove duplicated streams between high and low confidence sets:
CV_LC <- anti_join(CV_LC, CV_HC, by = "StreamName")





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
#remove the 82 higher confidence streams so that you aren't making double pred-
#ictions:
lowr_conf_preds <- anti_join(ALLmean_predsChp2, mean_predsChp2, by = "StreamName")
#Sullivan Creek, Barlow Cove W Shore, and Beardslee River all have mean predicted
#indices in the thousands, compared to <200 for all other sites. As a result, you
#are unable to see really any variation in the data beyond those 3 streams. I would
#suggest changing their values to be closer to the range of the data, but still
#accurately reflect how they are the most predicted attractive streams
lowr_conf_preds[lowr_conf_preds$StreamName %in%
                  c("Sullivan Creek", "Barlow Cove W Shore"), 7] <- 250
lowr_conf_preds[lowr_conf_preds$StreamName == "Beardslee River", 7] <- 200

#Add on the predicted uncertainty column from bootstrapping (section 4.2)
lowr_conf_preds <- left_join(lowr_conf_preds, CV_LC, by = "StreamName")

### Sidebar: CV is very high (>2) for many of these streams. Explore why:
plot(lowr_conf_preds$CV_percent ~ lowr_conf_preds$Mean_pred_strays) #while it is
#not a linear increase, it appears that more (predicted to be) attractive streams
#have higher CVs. This makes sense bc those streams will have a larger variability
#across time compared to a stream where only 0-2 strays ever show up




strays_map1a <- ggmap(myMap) + geom_point(aes(x = LONGITUDE, y = LATITUDE,
                                              size = Mean_pred_strays,
                                              fill = CV_percent),
                                          colour = "black", pch = 21,
                                          data = lowr_conf_preds) +
  labs(x = "Latitude", y = "Longitude", size = "Predicted Index",
       fill = "Prediction CV") +
  #guides(size = "none") +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman")) +
  scale_fill_gradientn(colours = c("#062635", "#0A4C6A", "#12719E", "#1696D2",
                                   "#46ABDB", "#73BFE2", "#A2D4EC", "#CFE8F3"),
                       values = rescale(c(0,0.1,0.25,1)))
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
                                             size = Mean_pred_strays,
                                             fill = CV_percent),
                                         colour = "black", pch = 24,
                                         data = lowr_conf_preds) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = Mean_pred_strays,
                 fill = CV_percent), colour = "black", pch = 21,
             data = mean_predsChp2) +
  labs(x = "Latitude", y = "Longitude", size = "Predicted Index",
       fill = "Prediction CV") +
  guides(fill = "none", size = "none") + #removes the legend for this plot. I will
#specify the legend in the zoomed in plots below instead so that I can have the 
#legen positioned properly
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
  scale_fill_gradientn(colours = c("#062635", "#0A4C6A", "#12719E", "#1696D2",
                                   "#46ABDB", "#73BFE2", "#A2D4EC", "#CFE8F3"))+
                       #values = rescale(c(0,10,25,50,75,200))) + #additional
  #code here to add rectangles to map to show areas you are zooming into:
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
                                                size = Mean_pred_strays,
                                                fill = CV_percent),
                                            colour = "black", pch = 24,
                                            data = lowr_conf_preds) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = Mean_pred_strays,
                 fill = CV_percent), colour = "black", pch = 21,
             data = mean_predsChp2) +
  geom_point(aes(x = Longitude, y = Latitude), shape = 22, size = 4,
             fill = "darkred", data = H_Release_Locations) +
  labs(x = "", y = "", size = "Predicted
Index", fill = "Prediction CV") +
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
  scale_fill_gradientn(colours = c("#062635", "#0A4C6A", "#12719E", "#1696D2",
                                   "#46ABDB", "#73BFE2", "#A2D4EC", "#CFE8F3"))
                       #values = rescale(c(0,0.1,0.25,1)))
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
                                                  size = Mean_pred_strays,
                                                  fill = CV_percent),
                                              colour = "black", pch = 24,
                                              data = lowr_conf_preds) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = Mean_pred_strays,
                 fill = CV_percent), colour = "black", pch = 21,
             data = mean_predsChp2) +
  geom_point(aes(x = Longitude, y = Latitude), shape = 22, size = 4,
             fill = "darkred", data = H_Release_Locations) +
  labs(x = "", y = "", size = "Predicted
Index", fill = "Prediction CV")  +
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
  scale_fill_gradientn(colours = c("#062635", "#0A4C6A", "#12719E", "#1696D2",
                                   "#46ABDB", "#73BFE2", "#A2D4EC", "#CFE8F3"))
                       #values = rescale(c(0,0.1,0.25,1)))
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
tiff("fig1_v3.tiff", width = 9, height = 6, pointsize = 12, units = 'in',
     res = 300)
whole_map #graph that you want to export
dev.off( )





#7. Data for range of attractiveness table #####################################
head(mean_predsChp2) #high confidence mean predictions by stream are in this df
head(lowr_conf_preds) #lower confidence mean predictions by stream are in this df
length(mean_predsChp2$StreamName) #82 
length(lowr_conf_preds$StreamName) #558
0.10*82 #10% of 82 is 8.2, so take the top 8 streams as most attractive and 
#the bottom 41 (50%) as least attractive
0.10*558 #10% of 558 is 55.8, so take the top 56 streams as most attractive
#and the bottom 279 as least attractive




#7.1. High confidence streams range of attractiveness table ====================
hc_top10 <- mean_predsChp2 %>% slice_max(order_by = Mean_pred_strays, n = 8)
#can't use prop = 10 (for top 10%) because 10% of 82 is not an integer, must use
#n = 8 here instead

hc_bot50 <- mean_predsChp2 %>% slice_min(order_by = Mean_pred_strays, n = 41)

Xhc_mid40 <- anti_join(mean_predsChp2, hc_top10, by = "Mean_pred_strays")
hc_mid40 <- anti_join(Xhc_mid40, hc_bot50, by = "Mean_pred_strays")



PercentsHC <- c("90-100", "50-90", "0-50")
hc_n <- c(length(hc_top10$Mean_pred_strays), length(hc_mid40$Mean_pred_strays),
          length(hc_bot50$Mean_pred_strays))
hc_df <- cbind.data.frame(PercentsHC, hc_n)


### Append mean and range of covariate data to each percentile grouping
head(Chp2_MasterDat2)
length(Chp2_MasterDat2$WMA_Releases_by_Yr) #only contains the 82 streams with Cons_
#Abundance data (AKA the high confidence streams we are summarizing here in 7.1)
hc_covariate_dat <- Chp2_MasterDat2 %>% group_by(StreamName) %>%
  summarize(across(c(WMA_Releases_by_Yr, Cons_Abundance, CV_flow), mean))
  
hc_top10a <- left_join(hc_top10, hc_covariate_dat, by = "StreamName")
hc_mid40a <- left_join(hc_mid40, hc_covariate_dat, by = "StreamName")
hc_bot50a <- left_join(hc_bot50, hc_covariate_dat, by = "StreamName")


hc_list <- list(hc_top10a, hc_mid40a, hc_bot50a)

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
head(lowr_conf_preds) #lower confidence mean predictions by stream are in this df
length(lowr_conf_preds$StreamName) #558
0.10*558 #10% of 558 is 55.8, so take the top 56 streams as most attractive
#and the bottom 279 (50%) as least attractive

lc_top10 <- lowr_conf_preds %>% slice_max(order_by = Mean_pred_strays, n = 56)

#lc_bot50 <- lowr_conf_preds %>% slice_min(order_by = Mean_pred_strays, n = 279)
#does not work because there are matching values in Mean_pred_strays for several
#streams (due to streams having the same CV_flow vals). Instead use:
ord_lowr_conf <- lowr_conf_preds[order(lowr_conf_preds$Mean_pred_strays,
                                       decreasing = T),]
rownames(ord_lowr_conf) <- 1:nrow(ord_lowr_conf)

lc_bot50 <- ord_lowr_conf %>% slice_tail(n = 279)

lc_mid40 <- ord_lowr_conf %>% slice(c(57:279))


lc_n <- c(length(lc_top10$Mean_pred_strays), length(lc_mid40$Mean_pred_strays),
          length(lc_bot50$Mean_pred_strays))
lc_df <- cbind.data.frame(PercentsHC, lc_n) #use same "PercentsHC" object as above


### Append mean and range of covariate data to each percentile grouping
head(Chp2_MasterDat3)
length(Chp2_MasterDat3$WMA_Releases_by_Yr) #contains 640 streams (6400 rows), 
#including the 82 high-confidence streams. These need to be removed
use_for_lc_covariate <- anti_join(Chp2_MasterDat3, Chp2_MasterDat2,
                                  by = c("StreamName", "Year"))
lc_covariate_dat <- use_for_lc_covariate %>% group_by(StreamName) %>%
  summarize(across(c(WMA_Releases_by_Yr, CV_flow), mean))

lc_top10a <- left_join(lc_top10, lc_covariate_dat, by = "StreamName")
lc_mid40a <- left_join(lc_mid40, lc_covariate_dat, by = "StreamName")
lc_bot50a <- left_join(lc_bot50, lc_covariate_dat, by = "StreamName")


lc_list <- list(lc_top10a, lc_mid40a, lc_bot50a)

cov_fun2 <- function(x){ #doesn't include Cons_Abundance this time
  out <- x %>% summarise(across(c(WMA_Releases_by_Yr, CV_flow),
                                c(mean, min, max)))
  return(out)
}

lc_df2 <- purrr::map_df(lc_list, cov_fun2)
LC_table <- cbind.data.frame(lc_df, lc_df2)
write.csv(LC_table, "Lower_confidence_streams.csv")






#8. Predict future stream attractiveness in 2020 and 2021 ######################
#In the previous 7 sections, I predicted out-of-sample stream attractiveness for
#(high and lower confidence) streams using data from 2008-2019, which is the 
#same set of years used to initially create the model in chapter 1. Now I am
#going to predict out-of-sample stream attractiveness for years outside of my
#modeled time range as well

#I will do this^^ in 2 parts: 1) Predict stream attractiveness for 2020 and 2021
#using actual updated hatchery release site and conspecific abundance data, and
#2) predict stream attractiveness in any given hypothetical future year with
#addition of release sites (section 9)


#Dataframe that contains 2020 and 2021 (from before section 1 at top of script):
X20_21_Chp2
length(X20_21_Chp2$Cons_Abundance[!is.na(X20_21_Chp2$Cons_Abundance)]) #164 of
#these streams (out of 1280) will be high-confidence predictions (i.e., have
#Cons_Abundance data)



#8.1. High-confidence 2020-2021 stream attractiveness predictions ==============
sapply(X20_21_Chp2, function(x) sum(is.na(x))) #only Cons_A has NAs, good
X2020_2021_HC <- X20_21_Chp2[complete.cases(X20_21_Chp2),]
rownames(X2020_2021_HC) <- 1:nrow(X2020_2021_HC) #164 total
sapply(X2020_2021_HC, function(x) sum(is.na(x))) #No NAs



#8.2. Scale covariates for modeling ============================================
hc20_21 <- apply(X2020_2021_HC[ , c(8:10)], 2, scale.default)
scaled_20_21hc <- cbind.data.frame(X2020_2021_HC[ , c(1:7)], hc20_21)



#8.3. Predict for new HIGH CONFIDENCE 2020 and 2021 streams ====================
#Note that I have a random effect of year in the model. Unfortunately, by pre-
#dicting into the future, I don't have any way to estimate random intercepts for
#new years. I will deal with this by bootstrapping predictions around the mean
#intercept. Use SD of the random intercepts to create my bootstrapping 
#distribution
summary(bm1u) #mean intercept is 0.41315
re2 #from section 2.3 above; contains the random intercepts for bm1u model
sd(re2$R.Intercept) #sd of the random intercepts is 0.60845
rRE_HC <- rnorm(1000, 0.41315, 0.60845)

summary(bm1u) #create random normal distributions using the mean and sd for each
#model covariate, i.e., the coefficient estimate and standard error
rWMA_releas <- rnorm(1000, 0.412, 0.086)
rCons_A <- rnorm(1000, 0.215, 0.120)
rCV_f <- rnorm(1000, 0.503, 0.091)
rCV_f2 <- rnorm(1000, 0.623, 0.078)

run_mod20_21HC <- function(R.Intercept, WMA, CA, CVf, CVf2){ 
  y <- exp(R.Intercept +  (WMA * scaled_20_21hc$WMA_Releases_by_Yr) -
             (CA * scaled_20_21hc$Cons_Abundance) +
             (CVf * scaled_20_21hc$CV_flow) + (CVf2 * (scaled_20_21hc$CV_flow^2)))
} 


#Create empty df to store predictions from each model iteration
length(scaled_20_21hc$Stream_Number) #164
mod_preds20_21HC <- data.frame(matrix(ncol = 1000, nrow = 164))
#For loop to run model!
for (i in 1:1000) {
  R.Intercept <- sample(rRE_HC, 1, replace = T)
  WMA <- sample(rWMA_releas, 1, replace = T)
  CA <- sample(rCons_A, 1, replace = T)
  CVf <- sample(rCV_f, 1, replace = T)
  CVf2 <- sample(rCV_f2, 1, replace = T)
  mod_preds20_21HC[,i] <- run_mod20_21HC(R.Intercept, WMA, CA, CVf, CVf2)
}
mod_preds20_21HC
mod_preds20_21HC$Mean <- rowMeans(mod_preds20_21HC)
mod_preds20_21HC$SD <- apply(mod_preds20_21HC, 1, sd)


#Link stream and year information to mean and SD of bootstrapped predictions
bs_preds_2021HC <-
  cbind.data.frame(scaled_20_21hc[,c(1:7)], mod_preds20_21HC[,c(1001:1002)])
#this^^ gives the mean and sd of the 1000 bootstrapped ("bs") model predictions
#for each individual stream-year prediction. You want to know the mean and sd for
#each stream overall:
CV_HC20_21 <- bs_preds_2021HC %>% group_by(StreamName) %>%
  summarise(Mean_bs = mean(Mean), SD_bs = mean(SD)) 
CV_HC20_21$CV <- CV_HC20_21$SD_bs/CV_HC20_21$Mean_bs
CV_HC20_21$CV_percent <- CV_HC20_21$CV*100


### Are these^^ predictions total garbage? I.e., how do they compare to chp1
#high confidence predictions?
junk_Chp1 <- Chp1_predictions %>% group_by(StreamName) %>%
  summarise(Mean_prediction = mean(Chp1_predictions))
junk <- right_join(junk_Chp1, CV_HC20_21, by = "StreamName")
plot(junk$Mean_bs ~ junk$Mean_prediction)
lm_junk <- lm(junk$Mean_bs ~ junk$Mean_prediction)
summary(lm_junk)
abline(lm_junk)
abline(0,1, col = "red")
cor.test(junk$Mean_bs, junk$Mean_prediction) #0.97. Predictions aren't that far
#off from what one would expect them to be




#8.4. LOWER CONFIDENCE 2020-2021 stream attractiveness predictions =============
X20_21_Chp2 #df with all 2020 and 2021 streams (high and lower confidence together)
X2020_2021_LC <- X20_21_Chp2[is.na(X20_21_Chp2$Cons_Abundance),]
X2020_2021_LC <- X2020_2021_LC %>% select(-Cons_Abundance)
rownames(X2020_2021_LC) <- 1:nrow(X2020_2021_LC) #1116 total
sapply(X2020_2021_LC, function(x) sum(is.na(x))) #No NAs



#8.5. Scale covariates for modeling ============================================
lc20_21 <- apply(X2020_2021_LC[ , c(8:9)], 2, scale.default)
scaled_20_21lc <- cbind.data.frame(X2020_2021_LC[ , c(1:7)], lc20_21)


#8.6. Predict for new LOWER CONFIDENCE 2020 and 2021 streams ===================
#Create random intercept distribution
summary(bm2u) #mean intercept is 0.43488
re_LC2 #from section 4.2 above; contains the random intercepts for bm2u model
sd(re_LC2$R.Intercept) #sd of the random intercepts is 0.62495
rRE_LC <- rnorm(1000, 0.43488, 0.62495)

summary(bm2u) #create random normal distributions using the mean and sd for each
#model covariate, i.e., the coefficient estimate and standard error
rWMA_releasLC <- rnorm(1000, 0.436, 0.086)
rCV_fLC <- rnorm(1000, 0.564, 0.084)
rCV_f2LC <- rnorm(1000, 0.616, 0.079)


run_mod20_21LC <- function(R.Intercept, WMA, CVf, CVf2){ #No Cons_A this time
  y <- exp(R.Intercept +  (WMA * scaled_20_21lc$WMA_Releases_by_Yr) +
             (CVf * scaled_20_21lc$CV_flow) + (CVf2 * (scaled_20_21lc$CV_flow^2)))
} 


#Create empty df to store predictions from each model iteration
length(scaled_20_21lc$Stream_Number) #1116
mod_preds20_21LC <- data.frame(matrix(ncol = 1000, nrow = 1116))
#For loop to run model!
for (i in 1:1000) {
  R.Intercept <- sample(rRE_LC, 1, replace = T)
  WMA <- sample(rWMA_releasLC, 1, replace = T)
  CVf <- sample(rCV_fLC, 1, replace = T)
  CVf2 <- sample(rCV_f2LC, 1, replace = T)
  mod_preds20_21LC[,i] <- run_mod20_21LC(R.Intercept, WMA, CVf, CVf2)
}
mod_preds20_21LC
mod_preds20_21LC$Mean <- rowMeans(mod_preds20_21LC)
mod_preds20_21LC$SD <- apply(mod_preds20_21LC, 1, sd)


#Link stream and year information to mean and SD of bootstrapped predictions
bs_preds_2021LC <-
  cbind.data.frame(scaled_20_21lc[,c(1:7)], mod_preds20_21LC[,c(1001:1002)])
#this^^ gives the mean and sd of the 1000 bootstrapped ("bs") model predictions
#for each individual stream-year prediction. You want to know the mean and sd for
#each stream overall:
CV_LC20_21 <- bs_preds_2021LC %>% group_by(StreamName) %>%
  summarise(Mean_bs = mean(Mean), SD_bs = mean(SD)) 
CV_LC20_21$CV <- CV_LC20_21$SD_bs/CV_LC20_21$Mean_bs
CV_LC20_21$CV_percent <- CV_LC20_21$CV*100


range(CV_HC20_21$Mean_bs)
range(CV_LC20_21$Mean_bs) #Note that there are 3 exceptionally high predicted
#indices in the lower confidence set, which will make it hard to see any variation
#in the data beyond those 3 streams. Change these to values that are closer to 
#the range of the rest of the data (but still higher than the rest):

CV_LC20_21[CV_LC20_21$StreamName %in%
                  c("Sullivan Creek", "Barlow Cove W Shore"), 2] <- 350
CV_LC20_21[CV_LC20_21$StreamName == "Beardslee River", 2] <- 300





#8.7. Top 10, middle 40, bottom 50% of attractiveness table ====================
### High confidence streams ###
hc_top10_2021 <- CV_HC20_21 %>% slice_max(order_by = Mean_bs, n = 8) #8 = ~10%
#of 82

hc_bot50_2021 <- CV_HC20_21 %>% slice_min(order_by = Mean_bs, n = 41)

Xhc_mid40_2021 <- anti_join(CV_HC20_21, hc_top10_2021, by = "Mean_bs")
hc_mid40_2021 <- anti_join(Xhc_mid40_2021, hc_bot50_2021, by = "Mean_bs")


### Append mean and range of covariate data to each percentile grouping
head(X2020_2021_HC)
length(X2020_2021_HC$WMA_Releases_by_Yr) #only contains the 82 streams (82*2=164)
#with Cons_Abundance data (AKA the high confidence streams we are summarizing here)
hc_covariate_dat2021 <- X2020_2021_HC %>% group_by(StreamName) %>%
  summarize(across(c(WMA_Releases_by_Yr, Cons_Abundance, CV_flow), mean))

hc_top10_2021a <- left_join(hc_top10_2021, hc_covariate_dat2021, by = "StreamName")
hc_mid40_2021a <- left_join(hc_mid40_2021, hc_covariate_dat2021, by = "StreamName")
hc_bot50_2021a <- left_join(hc_bot50_2021, hc_covariate_dat2021, by = "StreamName")


hc_list2021 <- list(hc_top10_2021a, hc_mid40_2021a, hc_bot50_2021a)

cov_fun <- function(x){
  out <- x %>% summarise(across(c(WMA_Releases_by_Yr, Cons_Abundance, CV_flow),
                                c(mean, min, max)))
  return(out)
}

hc_df2021 <- purrr::map_df(hc_list2021, cov_fun)
HC_table2021 <- cbind.data.frame(hc_df, hc_df2021) #you can reuse hc_df from
#section 7.1
getwd()
write.csv(HC_table2021, "High_confidence_streams2021.csv")



### Low confidence streams ###
head(CV_LC20_21) #lower confidence mean predictions by stream are in this df
length(CV_LC20_21$StreamName) #558
0.10*558 #10% of 558 is 55.8, so take the top 56 streams as most attractive
#and the bottom 279 (50%) as least attractive

lc_top10_2021 <- CV_LC20_21 %>% slice_max(order_by = Mean_bs, n = 56)

#lc_bot50 <- CV_LC20_21 %>% slice_min(order_by = Mean_bs, n = 279)
#does not work because there are matching values in Mean_pred_strays for several
#streams (due to streams having the same CV_flow vals). Instead use:
ord_lowr_conf2 <- as.data.frame(CV_LC20_21[order(CV_LC20_21$Mean_bs,
                                                 decreasing = T),])
rownames(ord_lowr_conf2) <- 1:nrow(ord_lowr_conf2)

lc_bot50_2021 <- ord_lowr_conf2 %>% slice_tail(n = 279)

lc_mid40_2021 <- ord_lowr_conf2 %>% slice(c(57:279))


lc_n <- c(length(lc_top10_2021$Mean_bs),
          length(lc_mid40_2021$Mean_bs),
          length(lc_bot50_2021$Mean_bs))
lc_df <- cbind.data.frame(PercentsHC, lc_n) #use same "PercentsHC" object as above


### Append mean and range of covariate data to each percentile grouping
head(Chp2_MasterDat3)
length(Chp2_MasterDat3$WMA_Releases_by_Yr) #contains 640 streams (6400 rows), 
#including the 82 high-confidence streams. These need to be removed
use_for_lc_covariate <- anti_join(Chp2_MasterDat3, Chp2_MasterDat2,
                                  by = c("StreamName", "Year"))
lc_covariate_dat <- use_for_lc_covariate %>% group_by(StreamName) %>%
  summarize(across(c(WMA_Releases_by_Yr, CV_flow), mean))

lc_top10_2021a <- left_join(lc_top10_2021, lc_covariate_dat, by = "StreamName")
lc_mid40_2021a <- left_join(lc_mid40_2021, lc_covariate_dat, by = "StreamName")
lc_bot50_2021a <- left_join(lc_bot50_2021, lc_covariate_dat, by = "StreamName")


lc_list2021 <- list(lc_top10_2021a, lc_mid40_2021a, lc_bot50_2021a)

cov_fun2 <- function(x){ #doesn't include Cons_Abundance this time
  out <- x %>% summarise(across(c(WMA_Releases_by_Yr, CV_flow),
                                c(mean, min, max)))
  return(out)
}

lc_df2021 <- purrr::map_df(lc_list2021, cov_fun2)
LC_table2021 <- cbind.data.frame(lc_df, lc_df2021) #you can reuse hc_df from
#section 7.1
write.csv(LC_table, "Lower_confidence_streams2021.csv")





#8.8. W Crawfish NE Arm Hd and W Crawfish N Arm NE table =======================
### Data to manually type into W Crawfish table in word doc:
lowr_conf_junk <-
  lowr_conf_preds[order(lowr_conf_preds$Mean_pred_strays, decreasing = T),]
rownames(lowr_conf_junk) <- 1:nrow(lowr_conf_junk)
which(lowr_conf_junk$StreamName == "W Crawfish N Arm NE") #ranked 134th in rela-
#tive attractiveness

LC20_21_junk <-
  CV_LC20_21[order(CV_LC20_21$Mean_bs, decreasing = T),]
#rownames(LC20_21_junk) <- 1:nrow(LC20_21_junk)
which(LC20_21_junk$StreamName == "W Crawfish N Arm NE") #ranked 134th in rela-
#tive attractiveness

#From section 7.1 (contains mean of each covariate for each stream 2008-2019):
head(hc_covariate_dat)
#this^^ only gives the mean values, find the ranges for the two streams as well:
zx <- Chp2_MasterDat %>% filter(StreamName %in% c("W Crawfish NE Arm Hd",
                                                  "W Crawfish N Arm NE")) %>%
  group_by(StreamName) %>%
  summarise(across(c(WMA_Releases_by_Yr, Cons_Abundance, CV_flow),
                   c(mean, min, max)))
  

#Covariate data for 2020-2021:
X20_21_Chp2
xx <- X20_21_Chp2 %>% filter(StreamName %in% c("W Crawfish NE Arm Hd",
                                               "W Crawfish N Arm NE"))





#8.9. W Crawfish indices over time figure ======================================
#Include obs and predicted attractiveness indices for W Crawfish NE Arm Hd from
#chapter 1 (barchart) with a line showing the increase in WMA_Releases_by_Yr as
#well. Include 2020 and 2021 predicted data 
head(Chp1_Master) #contains obs. attractiveness index for W Crawfish 
head(Mod1_Chp2_predictions) #contains pred. index for W Crawfish

WCraw_obs <- Chp1_Master %>% filter(StreamName == "W Crawfish NE Arm Hd")
WCraw_obs <- WCraw_obs[,-c(1,4:8,10:14,16:19)]
colnames(WCraw_obs)[3] <- "Index"
surveydat_WCraw18_19 <- read.csv("Data/Chum salmon otolith data in D113.csv") 
#sent to me by Lorna Wilson (2/10/22 email). Contains # H strays data for 2018
#and 2019 in W Crawfish. See rows 12-13 (2 surveys in 2018) and 37-38 (2 surveys
#in 2019) in the "Marked" column
print(surveydat_WCraw18_19[c(12:13),]) #In 2018, survey #1 found 57 strays and 
#survey #2 found 86 strays, so the observed attractiveness index in W Crawfish
#NE Arm Hd in 2018 is 
(57+86)/2 #71.5
#And in 2019:
print(surveydat_WCraw18_19[c(37:38),])
(5+89)/2 #47
#Add these attractiveness indices to the observed data for W Crawfish NE Arm Hd:
newrow2018 <- c("2018", "W Crawfish NE Arm Hd", "71.5", "9.250540365") #9.25 is
#the WMA_Releases_by_Yr val for W Crawfish NE Arm Hd in 2018. See Chp2_MasterDat
newrow2019 <- c("2019", "W Crawfish NE Arm Hd", "47", "22.57044722")
WCraw_obs <- rbind.data.frame(WCraw_obs, newrow2018, newrow2019)
WCraw_obs$Index_Type <- rep("Observed", length(WCraw_obs$Year))



WCraw_pred <- Mod1_Chp2_predictions %>%
  filter(StreamName == "W Crawfish NE Arm Hd")
WCraw_pred <- WCraw_pred[,c(7,3,8)] #the only years that have observed indices 
#are 2008-2009, 2013-2015, and now 2018-2019 (added above)
colnames(WCraw_pred)[3] <- "Index"
WCraw_pred <- WCraw_pred %>% filter(Year %in% c("2008", "2009", "2013", "2014",
                                                "2015", "2018", "2019"))
#Add 2020 and 2021 predictions as well
head(bs_preds_2021HC)
add_20_21_to_WCraw <- bs_preds_2021HC %>%
  filter(StreamName == "W Crawfish NE Arm Hd")
add_20_21_to_WCraw <- add_20_21_to_WCraw[,c(7,3,8)]
colnames(add_20_21_to_WCraw)[3] <- "Index"
WCraw_pred2 <- rbind.data.frame(WCraw_pred, add_20_21_to_WCraw)
WCraw_pred2$Index_Type <- rep("Predicted", length(WCraw_pred2$Year))
#add WMA_Releases_by_Yr to W Crawfish predicted values (obs. already has)
head(Chp2_MasterDat)
head(X20_21_Chp2)
basura <- rbind.data.frame(Chp2_MasterDat, X20_21_Chp2)
WCraw_pred3 <- left_join(WCraw_pred2, basura, by = c("StreamName", "Year"))
WCraw_pred3 <- WCraw_pred3[,c(1:3,11,4)]


WCraw_all <- rbind.data.frame(WCraw_obs, WCraw_pred3)
WCraw_all <- rbind(WCraw_all, rep(WCraw_all[c(15:16),], 1))
WCraw_all[15,3] <- 0 #change "Index" of the duplicated 2020 and 2021 rows to 0
#so that there will be a space on the barplot for the observed column
WCraw_all[16,3] <- 0
WCraw_all[15,5] <- "Observed"
WCraw_all[16,5] <- "Observed"
#reduce number of decimal places for index (there are >10 right now)
WCraw_all$Index <- round(as.numeric(WCraw_all$Index), 1)


#WMA_Releases_by_Yr data:
Craw_WMA <- WCraw_all[,c(1,4)]
Craw_WMA <- Craw_WMA[!duplicated(Craw_WMA),]
str(Craw_WMA)
Craw_WMA$WMA_Releases_by_Yr <- as.numeric(Craw_WMA$WMA_Releases_by_Yr)


#Make barplot:
WCraw_plot <- ggplot() +
  geom_bar(aes(x = Year, y = Index, fill = Index_Type), stat = "identity",
           position = "dodge", data = WCraw_all) +
  scale_fill_grey() +
  geom_line(aes(x = Year, y = WMA_Releases_by_Yr, group = 1,
                linetype = "Releases within
40 km"), size = 1, col = "red", data = Craw_WMA) +
  scale_y_continuous(name =
                       "Index",
                     sec.axis =
                       sec_axis(trans = ~.*0.5,
                                name =
                                  "Number of fish released within 40 km (in millions)")) +
  labs(fill = "Index Type", linetype = "") + theme_bw() +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman"))
WCraw_plot

#Export as high-res figure:
tiff("WCrawfishNE_barplot.tiff", width = 8, height = 5, pointsize = 12,
     units = 'in', res = 300)
WCraw_plot #graph that you want to export
dev.off( )







#9. Predict attractiveness near 3 hypothetical release site scenarios ##########

 # 1. Increase in releases from CRAWFISH INLET release site
 # 2. Increase in releases from PORT ASUMCION release site
 # 3. Establishment of a release site in FRESHWATER BAY on eastern Chicagof
 # Island (57.933113, -135.1800125)


#9.1. Create df for modeling ===================================================
### I have verified that all of the streams within 40km of the above release 
#sites are only within 40km of those sites (i.e., none of the streams are also 
#within 40km of an additional release site). Hence, I can simply add the increased
#number of released hatchery fish to the 2020 release value for each of the creeks
#within 40km of one of the 3 release sites above (not 2021 because that AK fish-
#eries enhancement report has not been released yet)
hypothetical_release <- read.csv("Data/Hypothetical_releas.csv") #contains all 
#names of streams within 40km of the release site increases/proposed sites
colnames(hypothetical_release)[2] <- "Release_site_LAT"
colnames(hypothetical_release)[3] <- "Release_site_LONG"
colnames(hypothetical_release)[4] <- "StreamName"


### Attach df containing releases by site and year and append the 2019 (most
#recent year in dataset) releases
Releases_2020 <- read_csv("Data/Releases_2020.csv")
hypRel <- left_join(hypothetical_release, Releases_2020, by = "ReleaseSite")

#Increase WMA_Releases_by_Yr by 1 SD for the hypothetical release change streams
mean(X20_21_Chp2$WMA_Releases_by_Yr) #10.304
sd(X20_21_Chp2$WMA_Releases_by_Yr) #across all 640 Chp2 streams, the mean num-
#ber of fish released within 40km of all streams is 8.577, and the SD is 20.836.
#I will increase the number of fish released by 20.3836 (million) fish for each
#stream within a hypothetical release change (Crawfish Inlet, Port Asumcion,
#and Freshwater Bay) because that seems like a sensible number to increase by
hypRel$SUM_Releases_in_millions <- hypRel$SUM_Releases_in_millions + 20.836


#Now attach data from other covariates; use 2020-2021 mean values for each stream
mean_Cons_A <- X20_21_Chp2 %>% group_by(StreamName) %>%
  summarise(Mean_Cons_A = mean(Cons_Abundance))
hypRel2 <- left_join(hypRel, mean_Cons_A, by = "StreamName")
hypRel3 <- left_join(hypRel2, X20_21_Chp2, by = "StreamName") #to get CV_flow
hypRel3 <- hypRel3 %>% select(-c(5:6,10:12,15:17))
hypRel3 <- hypRel3[!duplicated(hypRel3),]
colnames(hypRel3)[5] <- "Year"
hypRel_Master <- hypRel3[,c(4,5,8,9,7,6,10)]
colnames(hypRel_Master)[5] <- "Cons_Abundance"
colnames(hypRel_Master)[6] <- "WMA_Releases_by_Yr"


#Note that the 2020 releases for the 3 hypothetical sites are not WMA values, so
#you are making inferences for 4 years later (5 yo fish) from 2020; 2024 onwards
#assuming that releases have remained constant at 20.836 million fish since 2020
#within 40km of the given stream 





#9.2. High-conf. hypothetical release stream attractiveness predictions ========
sapply(hypRel_Master, function(x) sum(is.na(x))) #Cons_A has NAs for 54 sites.
#Remove these for the high-confidence predictions
hypRel_MasterHC <- hypRel_Master[complete.cases(hypRel_Master),]
rownames(hypRel_MasterHC) <- 1:nrow(hypRel_MasterHC) #57 total
sapply(hypRel_MasterHC, function(x) sum(is.na(x))) #No NAs

### Scale covariates for modeling ###
XhypRelHC <- apply(hypRel_MasterHC[ , c(5:7)], 2, scale.default)
scaled_hypRelHC <- cbind.data.frame(hypRel_MasterHC[ , c(1:4)], XhypRelHC)


### Make predictions ###
#Note that I have a random effect of year in the model. Unfortunately, by pre-
#dicting into the future, I don't have any way to estimate random intercepts for
#new years. I will deal with this by bootstrapping predictions around the mean
#intercept. Use SD of the random intercepts to create my bootstrapping 
#distribution. Use the objects created above in section 8.3:
rRE_HC #distribution of possible random intercepts for straying model based on
#mean and SD of 2008-2019 observed random effects

#distributions of each covariate for the high confidence model:
rWMA_releas
rCons_A
rCV_f
rCV_f2 

run_mod_hypRelHC <- function(R.Intercept, WMA, CA, CVf, CVf2){ 
  y <- exp(R.Intercept +  (WMA * scaled_hypRelHC$WMA_Releases_by_Yr) -
             (CA * scaled_hypRelHC$Cons_Abundance) +
             (CVf * scaled_hypRelHC$CV_flow) +
             (CVf2 * (scaled_hypRelHC$CV_flow^2)))
} 


#Create empty df to store predictions from each model iteration
length(scaled_hypRelHC$StreamName) #3
mod_preds_hypRelHC <- data.frame(matrix(ncol = 1000, nrow = 3))
#For loop to run model!
for (i in 1:1000) {
  R.Intercept <- sample(rRE_HC, 1, replace = T)
  WMA <- sample(rWMA_releas, 1, replace = T)
  CA <- sample(rCons_A, 1, replace = T)
  CVf <- sample(rCV_f, 1, replace = T)
  CVf2 <- sample(rCV_f2, 1, replace = T)
  mod_preds_hypRelHC[,i] <- run_mod_hypRelHC(R.Intercept, WMA, CA, CVf, CVf2)
}
mod_preds_hypRelHC
mod_preds_hypRelHC$Mean_bs <- rowMeans(mod_preds_hypRelHC)
mod_preds_hypRelHC$SD_bs <- apply(mod_preds_hypRelHC, 1, sd)


#Link stream and year information to mean and SD of bootstrapped predictions
preds_hypRelHC <-
  cbind.data.frame(scaled_hypRelHC[,c(1:4)], mod_preds_hypRelHC[,c(1001:1002)])
preds_hypRelHC$CV <- preds_hypRelHC$SD_bs/preds_hypRelHC$Mean_bs
preds_hypRelHC$CV_percent <- preds_hypRelHC$CV*100





#9.3. Low-confidence 2020-2021 stream attractiveness predictions ===============
hypRel_MasterHC #High-confidence streams are in this df. Remove them from low
#confidence set:
hypRel_MasterLC <- anti_join(hypRel_Master, hypRel_MasterHC, by = "StreamName")
rownames(hypRel_MasterLC) <- 1:nrow(hypRel_MasterLC) 
sapply(hypRel_MasterLC, function(x) sum(is.na(x))) #No NAs (except for Cons_A)
hypRel_MasterLC <- hypRel_MasterLC %>% select(-Cons_Abundance)

### Scale covariates for modeling ###
XhypRelLC <- apply(hypRel_MasterLC[ , c(5:6)], 2, scale.default)
scaled_hypRelLC <- cbind.data.frame(hypRel_MasterLC[ , c(1:4)], XhypRelLC)



### Make predictions ###
rRE_LC #distribution of possible random intercepts for straying model based on
#mean and SD of 2008-2019 observed random effects

#distributions of each covariate for the low confidence model:
rWMA_releasLC
rCV_fLC
rCV_f2LC 

run_mod_hypRelLC <- function(R.Intercept, WMA, CVf, CVf2){ 
  y <- exp(R.Intercept +  (WMA * scaled_hypRelLC$WMA_Releases_by_Yr) +
             (CVf * scaled_hypRelLC$CV_flow) +
             (CVf2 * (scaled_hypRelLC$CV_flow^2)))
} 


#Create empty df to store predictions from each model iteration
length(scaled_hypRelLC$StreamName) #54
mod_preds_hypRelLC <- data.frame(matrix(ncol = 1000, nrow = 54))
#For loop to run model!
for (i in 1:1000) {
  R.Intercept <- sample(rRE_LC, 1, replace = T)
  WMA <- sample(rWMA_releasLC, 1, replace = T)
  CVf <- sample(rCV_fLC, 1, replace = T)
  CVf2 <- sample(rCV_f2LC, 1, replace = T)
  mod_preds_hypRelLC[,i] <- run_mod_hypRelLC(R.Intercept, WMA, CVf, CVf2)
}
mod_preds_hypRelLC
mod_preds_hypRelLC$Mean_bs <- rowMeans(mod_preds_hypRelLC)
mod_preds_hypRelLC$SD_bs <- apply(mod_preds_hypRelLC, 1, sd)


#Link stream and year information to mean and SD of bootstrapped predictions
preds_hypRelLC <-
  cbind.data.frame(scaled_hypRelLC[,c(1:4)], mod_preds_hypRelLC[,c(1001:1002)])
preds_hypRelLC$CV <- preds_hypRelLC$SD_bs/preds_hypRelLC$Mean_bs
preds_hypRelLC$CV_percent <- preds_hypRelLC$CV*100





#9.4. Visualize effects of hypothetical release scenarios on straying ==========
### Add on data for other streams in region. Assume status quo from 2021 onwards:
#High-confidence streams:
head(CV_HC20_21)
#In preds_hypRelHC, remove Year column and move Lat & Long to match CV_HC20_21:
preds_hypRelHC <- preds_hypRelHC %>% select(-Year)
preds_hypRelHC <- preds_hypRelHC[c(1,4:7,2,3)]
Final_HC_hypRel <- anti_join(CV_HC20_21, preds_hypRelHC, by = "StreamName")
Final_HC_hypRel <- rbind.data.frame(preds_hypRelHC, Final_HC_hypRel)


#Low confidence streams:
head(CV_LC20_21)
#In preds_hypRelLC, remove Year column and move Lat & Long to match CV_LC20_21:
preds_hypRelLC <- preds_hypRelLC %>% select(-Year)
preds_hypRelLC <- preds_hypRelLC[c(1,4:7,2,3)]
Final_LC_hypRel <- anti_join(CV_LC20_21, preds_hypRelLC, by = "StreamName")
Final_LC_hypRel <- rbind.data.frame(preds_hypRelLC, Final_LC_hypRel)



#Map!
Hyp_Release_map <- ggmap(myMap) + geom_point(aes(x = LONGITUDE, y = LATITUDE,
                                             size = Mean_bs,
                                             fill = CV_percent),
                                         colour = "black", pch = 24,
                                         data = Final_LC_hypRel) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = Mean_bs,
                 fill = CV_percent), colour = "black", pch = 21,
             data = Final_HC_hypRel) +
  labs(x = "Latitude", y = "Longitude", size = "Predicted Index",
       fill = "Prediction CV") +
  guides(fill = "none", size = "none") + #removes the legend for this plot. I will
  #specify the legend in the zoomed in plots below instead so that I can have the 
  #legen positioned properly
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
  scale_fill_gradientn(colours = c("#062635", "#0A4C6A", "#12719E", "#1696D2",
                                   "#46ABDB", "#73BFE2", "#A2D4EC", "#CFE8F3"))+
  #values = rescale(c(0,10,25,50,75,200))) + #additional
  #code here to add rectangles to map to show areas you are zooming into:
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

Hyp_Release_map






save.image("Making_predictions_objects.RData")
load("Making_predictions_objects.RData")






