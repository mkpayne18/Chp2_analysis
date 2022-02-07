
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
#dataset for modeling so that I would only include streams with actual data (not
#interpolated) for each covariate. 

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



#3. Visualization of predicted attractiveness for high confidence streams ######
### Map of chp2 predictions
#First find averages by site
mean_predsChp2 <- Chp2_predictions %>% group_by(StreamName) %>%
  mutate(mean(Chp2_predictions)) #%>% ungroup
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
a <- left_join(Chp2_predictions, All_strms_predictions, by = c("StreamName", "Year"))
plot(a$Chp2_predictions ~ a$Predicted_strays) #relationship looks very linear
cor.test(a$Chp2_predictions, a$Predicted_strays) #indeed they are, however it is
#not a 1:1. The lower confidence stream predictions are way huger than the high
#confidence streams. You should use percentiles of predicted attractiveness for
#low and high confidence stream predictions if you want to be able to include them
#in the same tool or show them in the same figures




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
                                             fill = Percentile), size = 4,
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
  geom_rect(aes(xmin = -132.29,
                xmax = -131.45,
                ymin = 55.5,
                ymax = 55.97),
            fill = "transparent",
            color = "black", size = 1)
  
strays_map1

### Code to create inset map was here. See section 3 above for that code if you 
#want it

#add to original figure
strays_map2 <- strays_map1 + inset(grob = ggplotGrob(alaska), xmin = -133, xmax = -130,
                                   ymin = 58.2, ymax = 59.55) 
strays_map2

#add scale bar and north arrow
strays_map3 <- strays_map2 + scalebar(x.min = -136.8, x.max = -134.8, y.min = 54.8,
                                      y.max = 55, dist = 50, dist_unit = "km",
                                      transform = T, height = 0.5, st.dist = 0.6,
                                      st.size = 5)
strays_map3


#6.1. "Zoom-in" region #1: Lynn Canal, Amalga Harbor area ======================
zoom1_map <- get_stamenmap(location <- c(-135.55, 58.15, -134.4, 58.75), zoom = 9,
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
  scale_fill_gradientn(colours = c("#CFE8F3", "#A2D4EC", "#73BFE2",
                                   "#46ABDB", "#1696D2", "#12719E", "#0A4C6A",
                                   "#062635"), values = rescale(c(0,0.1,0.25,1)))
Amalga_map


#6.2. "Zoom-in" region #2: Neets Bay ===========================================
zoom2_map <- get_stamenmap(location <- c(-132.29, 55.5, -131.45, 55.97), zoom = 10,
                           maptype = "terrain-background", crop = TRUE)
ggmap(zoom2_map)

Neets_map <- ggmap(zoom2_map) + geom_point(aes(x = LONGITUDE, y = LATITUDE,
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
  scale_fill_gradientn(colours = c("#CFE8F3", "#A2D4EC", "#73BFE2",
                                   "#46ABDB", "#1696D2", "#12719E", "#0A4C6A",
                                   "#062635"), values = rescale(c(0,0.1,0.25,1)))
Neets_map


library(gridExtra)
library(ggpubr)

right_side <- ggarrange(Amalga_map, Neets_map, ncol = 1,
                        common.legend = T, legend = "right")

whole_map <- ggarrange(strays_map3, right_side, align = "v",
                       common.legend = T, legend = "right")
whole_map


ggarrange(strays_map3, Amalga_map, Neets_map,
          layout_matrix = matrix(c(1,3,2,3), nrow = 2),
          common.legend = TRUE, legend="bottom")

(1,1,2,3)
(1,2,3,3)
(3,3,2,1)
          
          
          
          heights = c(2,1), common.legend = TRUE, legend="bottom")
  

layout_matrix = rbind(c(1, 2, NA),
                      c(3, 3, 4))


save.image("Making_predictions_objects.RData")
load("Making_predictions_objects.RData")



