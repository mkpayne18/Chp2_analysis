
#Note that in this script I often reference "high" and "low" confidence predictions.
#The high confidence predictions are predicted attractiveness indices for streams
#which have Cons_Abundance data (n = 82), and I therefore have more confidence
#in their predictions due to their having all required covariates. The low conf-
#idence streams lack Cons_Abundance data but I still predicted their attractive-
#ness indices using WMA_Releases_by_Yr and CV_flow

#Get data to use in maps and figures
load("Making_predictions_objects.RData")



#0. Figure 0! Added after I made all the others ################################
#Show that the model predicts relative attractiveness well
load("~/Documents/CHUM_THESIS/Analysis/Mod_fit3.RData")
head(mean_bm1u_pred) #contains mean predictions over time for each Chp 1 stream
plot(Mean_obs_strays ~ Mean_pred_strays, data = mean_bm1u_pred)

pred_ordered <- mean_bm1u_pred[order(mean_bm1u_pred$Mean_pred_strays,
                                     decreasing = T),]
pred_ordered$Preds_rank <- 1:nrow(pred_ordered)

obs_ordered <- pred_ordered[order(pred_ordered$Mean_obs_strays, decreasing = T),]
obs_ordered$Obs_rank <- 1:nrow(obs_ordered)
plot(Obs_rank ~ Preds_rank, data = obs_ordered) #nice!


### Make nice ggplot2 versions of both plots
ObsPred_vals_plot <- ggplot(mean_bm1u_pred) +
  geom_point(aes(x = Mean_pred_strays, y = Mean_obs_strays)) +
  geom_abline(linetype = 2) + theme_bw() +
  labs(x = "Predicted Attractiveness Index",
       y = "Observed Attractiveness Index") +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman"))
  
ObsPred_vals_plot


ObsPred_rank_plot <- ggplot(obs_ordered) +
  geom_point(aes(x = Preds_rank, y = Obs_rank)) + geom_abline(linetype = 2) +
  theme_bw() +
  labs(x = "Predicted Attractiveness Rank",
       y = "Observed Attractiveness Rank") +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman"))
  
ObsPred_rank_plot

Chp1_Obs_Pred <- ggarrange(ObsPred_vals_plot, ObsPred_rank_plot, ncol = 2)
Chp1_Obs_Pred

#Export as high-res figure
tiff("Chp1_Obs_Pred.tiff", width = 9, height = 5, pointsize = 12, units = 'in',
     res = 300)
Chp1_Obs_Pred
dev.off( )








#1. Visualization of predicted attractiveness for HIGH confidence streams ######
### Map of chp2 predictions for 2008-2019 SE AK stream attractiveness for high
#confidence sites only (n = 82)

#The model predictions displayed in this map are combined with the low confidence
#predictions (low confidence predictions map in next section) to show all points
#across high + low confidence. See the combined map in section 3 of this script

#First find averages by site
mean_predsChp2 <- Mod1_Chp2_predictions %>% group_by(StreamName) %>%
  mutate(mean(Predictions)) #%>% ungroup
mean_predsChp2 <- mean_predsChp2 %>% select(-c(7,8))
colnames(mean_predsChp2)[7] <- "Mean_pred_strays"
mean_predsChp2 <- mean_predsChp2[!duplicated(mean_predsChp2$Mean_pred_strays),]

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
  labs(x = "Longitude", y = "Latitude", size = "Predicted Index",
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








#2. Visualization of predicted attractiveness for LOW confidence streams #######
### Map of chp2 predictions for 2008-2019 SE AK stream attractiveness for low
#confidence sites only (n = 558)

#The model predictions displayed in this map are combined with the high confidence
#predictions (high confidence predictions map in previous section) to show all
#points across high + low confidence. See the combined map in section 3 of this
#script


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



### Map of mean predicted strays by site for low confidence predictions
strays_map1a <- ggmap(myMap) + geom_point(aes(x = LONGITUDE, y = LATITUDE,
                                              size = Mean_pred_strays,
                                              fill = CV_percent),
                                          colour = "black", pch = 21,
                                          data = lowr_conf_preds) +
  labs(x = "Longitude", y = "Latitude", size = "Predicted Index",
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
                                     ymin = 58.2, ymax = 59.55) #ggplotGrob(alaska)
#gives the inset map of Alaska in the corner of your map. The code to create the
#inset map is in section 1 above (approx. line 65) if you wish to see it
strays_map2a

#add scale bar and north arrow
strays_map3a <- strays_map2a + scalebar(x.min = -137.2, x.max = -135.2, y.min = 54.8,
                                        y.max = 55, dist = 50, dist_unit = "km",
                                        transform = T, height = 0.5, st.dist = 0.6,
                                        st.size = 5)
north2(strays_map3a, x = 0.18, y = 0.20, symbol = 3)







#3. Create multi-panel map #####################################################
#Combine high and low confidence stream attractiveness predictions for 2008-2019
#(data in sections 1 and 2 above). Also zoom in on two specific areas of SE AK
#to create a multi-panel map. This is for part 1 of chapter 2 and is most likely
#your figure 1 of your chapter 2 manuscript


#Read in hatchery release site location data
H_Release_Locations <-
  read.csv("~/Documents/CHUM_THESIS/Data Sources/Release_Sites_Age/H_Release_Locations.csv")
#remove Crawfish Inlet release site from df because no fish are released from 
#there in years that would correspond with years the Crawfish Inlet area streams
#were surveyed, hence plotting the release site on the 2008-2019 map is misleading
H_Release_Locations <- H_Release_Locations %>%
  filter(ReleaseSite != "CRAWFISH INLET 113-33")
H_Release_Locations <- H_Release_Locations %>%
  filter(ReleaseSite != "THOMAS BAY 110-12")

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
  labs(x = "Longitude", y = "Latitude", size = "Predicted
Index",
       fill = "Prediction
CV") +
  #guides(fill = "none", size = "none") + #removes the legend for this plot. I will
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
            color = "white", size = 1) +
  annotate("text", x = -135.68, y = 58.7, label = "1", fontface = 2, size = 5,
           col = "white") +
  geom_rect(aes(xmin = -134.05,
                xmax = -132.7,
                ymin = 56.8,
                ymax = 57.5),
            fill = "transparent",
            color = "white", size = 1) +
  annotate("text", x = -134.2, y = 57.45, label = "2", fontface = 2, size = 5,
           col = "white") +
  theme(plot.margin = unit(c(0,-0.5,0,1), "cm"))

strays_map1 #warning message about true north not being meaningful. This is bc
#I included the north arrow as an annotation on the plot, rather than using data
#to locate it. You can ignore

-134.1, 56.68, -132.6, 57.5
annotate("text", x = -135.65, y = 56.95, label = "2", fontface


#add to original figure
strays_map2 <- strays_map1 + inset(grob = ggplotGrob(alaska), xmin = -133, xmax = -130,
                                   ymin = 58.2, ymax = 59.55) 
strays_map2

#add scale bar
strays_map3 <- strays_map2 + scalebar(x.min = -136.9, x.max = -134.9, y.min = 54.75,
                                      y.max = 54.95, dist = 50, dist_unit = "km",
                                      transform = T, height = 0.4, st.dist = 0.6,
                                      st.size = 4)
strays_map3





#3.1. "Zoom-in" region #1: Lynn Canal, Amalga Harbor area ======================
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
Index", fill = "Prediction
CV") +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman")) +
  theme(legend.box = "horizontal") +
  annotate("text", x = -135.45, y = 58.68, label = "1", fontface = 2, size = 8,
           col = "white") +
  theme(plot.margin = unit(c(1,0,0,-0.5), "cm")) + #the plot.margin positioning
  #specified here makes the plot have no margin space at the bottom or on its left
  #side, and I did that so that it would be closer to the Crawfish_map below it
  #and the SEAK strays map to its left when I put them together in ggarrange()
  scalebar(x.min = -135.44, x.max = -134.8, y.min = 58.2, y.max = 58.24,
           transform = T, dist_unit = "km", dist = 20, height = 0.6,
           st.dist = 0.6, st.size = 4.5) +
  scale_fill_gradientn(colours = c("#062635", "#0A4C6A", "#12719E", "#1696D2",
                                   "#46ABDB", "#73BFE2", "#A2D4EC", "#CFE8F3"))
#values = rescale(c(0,0.1,0.25,1)))
Amalga_map


addSmallLegend <- function(myPlot, pointSize = 1, textSize = 11, spaceLegend = 0.4) {
  myPlot +
    guides(shape = guide_legend(override.aes = list(size = pointSize))) +
    theme(legend.title = element_text(size = textSize), 
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "cm"))
}

Amalga_map2 <- addSmallLegend(Amalga_map)








#3.2. "Zoom-in" region #2: Dry Bay Creek area ==================================
#Previously I had zoomed in on the Neets Bay area. Here is the map for that if
#you wish to return to using that area again:
#zoom2_map <- get_stamenmap(location <- c(-132.29, 55.5, -131.43, 55.97), zoom = 10,
#maptype = "terrain-background", crop = TRUE)
#ggmap(zoom2_map)

#Other option for previous zoom area; Crawfish Inlet:
#zoom3_map <- get_stamenmap(location <- c(-135.53, 56.6, -134.73, 57.03), zoom = 10,
                           #maptype = "terrain-background", crop = TRUE)
#ggmap(zoom3_map)

#For Dry Bay Creek, so that you can show more contrast in the map, you should
#filter out the really attractive streams (Dry Bay Creek is moderately attractive
#compared to many unattractive sites around it)
HC_preds_under30 <- mean_predsChp2 %>% filter(Mean_pred_strays < 30)
LC_preds_under30 <- lowr_conf_preds %>% filter(Mean_pred_strays < 30)
#Make Dry Bay Creek its own point in its own df:
only_DryBay <- HC_preds_under30 %>% filter(StreamName == "Dry Bay Creek")
HC_preds_under30 <- HC_preds_under30 %>% filter(StreamName != "Dry Bay Creek")



zoom4_map <- get_stamenmap(location <- c(-134.1, 56.68, -132.6, 57.5), zoom = 10,
                           maptype = "terrain-background", crop = TRUE)
ggmap(zoom4_map)

DryBay_map <- ggmap(zoom4_map) + geom_point(aes(x = LONGITUDE, y = LATITUDE,
                                                  size = Mean_pred_strays,
                                                  fill = CV_percent),
                                              colour = "black", pch = 24,
                                              data = LC_preds_under30) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = Mean_pred_strays,
                 fill = CV_percent), colour = "white", pch = 21,
             data = only_DryBay) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = Mean_pred_strays,
                 fill = CV_percent), colour = "black", pch = 21,
             data = HC_preds_under30) +
  geom_point(aes(x = Longitude, y = Latitude), shape = 22, size = 4,
             fill = "darkred", data = H_Release_Locations) +
  labs(x = "Longitude", y = "", size = "Predicted
Index", fill = "Prediction
CV")  +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman")) +
  theme(legend.box = "horizontal") +
  annotate("text", x = -134.0, y = 57.41, label = "2", fontface = 2, size = 8,
           col = "white") +
  annotate("text", x = -133.45, y = 57.07, label = "Dry Bay Creek", fontface = 2,
           size = 4, col = "white") +
  theme(plot.margin = unit(c(0,0,1,-0.5), "cm")) + #the plot.margin positioning
  #specified here makes the plot have no margin space at the top or on its left
  #side, and I did that so that it would be closer to the Amalga_map above it
  #and the SEAK strays map to its left when I put them together in ggarrange()
  scalebar(x.min = -134.0, x.max = -133.1, y.min = 56.76, y.max = 56.81,
           transform = T, dist_unit = "km", dist = 20, height = 0.6,
           st.dist = 0.7, st.size = 4.5) +
  scale_fill_gradientn(colours = c("#062635", "#0A4C6A", "#12719E", "#1696D2",
                                   "#46ABDB", "#73BFE2", "#A2D4EC", "#CFE8F3"))
#values = rescale(c(0,0.1,0.25,1)))
DryBay_map

DryBay_map2 <- addSmallLegend(DryBay_map)





#3.3. Put whole map together ===================================================
library(gridExtra)
library(ggpubr)

right_side <- ggarrange(Amalga_map2, DryBay_map2, ncol = 1)
                        #common.legend = T, legend = "right")

#whole_map <- ggarrange(strays_map3, right_side, align = "v")
                       #common.legend = T, legend = "right")

### Now that I am showing the zoomed in map on the Dry Bay Creek area (zoom in
#are #2; section 3.2 above), the predicted index scale of the legend is different.
#As a result, I am showing a different legend for zoom in area #2 and so now the
#maps and legends no longer fit nicely all in one figure. So, instead I am going
#to make the overall SEAK map one figure with its one legend, and the two zoomed-
#in maps another figure, and I will put them side-by-side in the paper

#Export as high-res figure
tiff("Amalga_DryBay.tiff", width = 7, height = 6, pointsize = 12, units = 'in',
     res = 300)
right_side #graph that you want to export
dev.off( )


#Export as high-res figure
tiff("SEAK_map_Fig1.tiff", width = 6, height = 5, pointsize = 12, units = 'in',
     res = 300)
strays_map3 #graph that you want to export
dev.off( )









#4. Data for range of attractiveness table 2008-2019 predictions ###############
#This range of attractiveness table is still for the 2008-2019 predictions! There
#is an additional section below that creates an IDENTICAL table but for 2020-
#2021 predictions

head(mean_predsChp2) #high confidence mean predictions by stream are in this df
head(lowr_conf_preds) #lower confidence mean predictions by stream are in this df
length(mean_predsChp2$StreamName) #82 
length(lowr_conf_preds$StreamName) #558
0.10*82 #10% of 82 is 8.2, so take the top 8 streams as most attractive and 
#the bottom 41 (50%) as least attractive
0.10*558 #10% of 558 is 55.8, so take the top 56 streams as most attractive
#and the bottom 279 as least attractive




#4.1. High confidence streams range of attractiveness table 2008-2019 ==========
### Show the top 10, middle 40, and bottom 50% of streams in order of attractive-
#ness in the table

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





#4.2. Lower confidence streams range of attractiveness table 2008-2019 =========
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






#4.3. Data for range of attractiveness table 2020-2021 predictions #############
head(CV_HC20_21)
head(CV_LC20_21)




#4.4. High confidence streams range of attractiveness table 2008-2019 ==========
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



#4.5. Low confidence streams range of attractiveness table 2008-2019 ===========
head(CV_LC20_21) #lower confidence mean predictions by stream are in this df
length(CV_LC20_21$StreamName) #558
0.10*558 #10% of 558 is 55.8, so take the top 56 streams as most attractive
#and the bottom 279 (50%) as least attractive

#lc_bot50 <- CV_LC20_21 %>% slice_min(order_by = Mean_bs, n = 279)
#does not work because there are matching values in Mean_pred_strays for several
#streams (due to streams having the same CV_flow vals). Instead use:
ord_lowr_conf2 <- as.data.frame(CV_LC20_21[order(CV_LC20_21$Mean_bs,
                                                 decreasing = T),])
rownames(ord_lowr_conf2) <- 1:nrow(ord_lowr_conf2)

lc_top10_2021 <- ord_lowr_conf2 %>% slice_head(n = 56)
lc_bot50_2021 <- ord_lowr_conf2 %>% slice_tail(n = 279)
lc_mid40_2021 <- ord_lowr_conf2 %>% slice(c(57:279))


lc_n <- c(length(lc_top10_2021$Mean_bs),
          length(lc_mid40_2021$Mean_bs),
          length(lc_bot50_2021$Mean_bs))
lc_df <- cbind.data.frame(PercentsHC, lc_n) #use same "PercentsHC" object as above


### Append mean and range of covariate data to each percentile grouping
length(X2020_2021_LC$WMA_Releases_by_Yr) #contains 558 streams (1116 rows), 
#including the 82 high-confidence streams. These need to be removed
lc_covariate_dat2021 <- X2020_2021_LC %>% group_by(StreamName) %>%
  summarize(across(c(WMA_Releases_by_Yr, CV_flow), mean))

lc_top10_2021a <- left_join(lc_top10_2021, lc_covariate_dat2021, by = "StreamName")
lc_mid40_2021a <- left_join(lc_mid40_2021, lc_covariate_dat2021, by = "StreamName")
lc_bot50_2021a <- left_join(lc_bot50_2021, lc_covariate_dat2021, by = "StreamName")


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






#4.6. W Crawfish NE Arm Hd and W Crawfish N Arm NE table =======================
### Data to manually type into W Crawfish table in word doc:
lowr_conf_junk <- #2008-2019 data
  lowr_conf_preds[order(lowr_conf_preds$Mean_pred_strays, decreasing = T),]
rownames(lowr_conf_junk) <- 1:nrow(lowr_conf_junk)
which(lowr_conf_junk$StreamName == "W Crawfish N Arm NE") #ranked 134th in rela-
#tive attractiveness during 2008-2019

ord_lowr_conf2 <- as.data.frame(CV_LC20_21[order(CV_LC20_21$Mean_bs,
                                                 decreasing = T),])
rownames(ord_lowr_conf2) <- 1:nrow(ord_lowr_conf2)
#rownames(LC20_21_junk) <- 1:nrow(LC20_21_junk)
which(LC20_21_junk$StreamName == "W Crawfish N Arm NE") #ranked 79th in rela-
#tive attractiveness during 2020-2021

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






#5. W Crawfish NE Arm Hd indices over time figure ==============================
### Inclusive of 2008-2019 and 2020-2021 data

#Include obs attractiveness indices for W Crawfish NE Arm Hd and its predicted
#relative attractiveness (ranking) from chapter 1 (barchart). Add a line showing 
#the increase in WMA_Releases_by_Yr as well. Include 2020 and 2021 predicted data 
head(Chp1_Master) #contains obs. attractiveness index for W Crawfish 
head(Mod1_Chp2_predictions) #contains pred. index (and relative attractiveness
#by extension) for W Crawfish


#Collate observed data:
WCraw_obs <- Chp1_Master %>% filter(StreamName == "W Crawfish NE Arm Hd")
WCraw_obs <- WCraw_obs[,-c(1,4:8,10:14,16:19)]
colnames(WCraw_obs)[3] <- "Index"
surveydat_WCraw18_19 <- read.csv("Data/Chum salmon otolith data in D113.csv") 
#sent to me by Lorna Wilson (2/10/22 email). Contains # H strays data for 2018
#and 2019 in W Crawfish. See rows 12-13 (2 surveys in 2018) and 37-38 (2 surveys
#in 2019) in the "Marked" column
print(surveydat_WCraw18_19[c(12:13),]) #In 2018, survey #1 found 57 strays and 
#survey #2 found 86 strays ("Marked" column), so the observed attractiveness in-
#dex in W Crawfish NE Arm Hd in 2018 is 
(57+86)/2 #71.5
#And in 2019:
print(surveydat_WCraw18_19[c(37:38),])
(5+89)/2 #47
#Add these attractiveness indices to the observed data for W Crawfish NE Arm Hd:
newrow2018 <- c("2018", "W Crawfish NE Arm Hd", "71.5", "9.250540365") #9.25 is
#the WMA_Releases_by_Yr val for W Crawfish NE Arm Hd in 2018. See Chp2_MasterDat
newrow2019 <- c("2019", "W Crawfish NE Arm Hd", "47", "22.57044722")
#add 2020 and 2021 "NA" rows
newrow2020 <- c("2020", "W Crawfish NE Arm Hd", "NA", "24.86835")
newrow2021 <- c("2021", "W Crawfish NE Arm Hd", "NA", "25.37685")
WCraw_obs <- rbind.data.frame(WCraw_obs, newrow2018, newrow2019, newrow2020,
                              newrow2021)
WCraw_obs #won't add the 2020 and 2021 cells for some reason
WCraw_obs$Year <- as.character(WCraw_obs$Year)
WCraw_obs[8,1] <- "2020"
WCraw_obs[9,1] <- "2021"
WCraw_obs$Year <- as.factor(WCraw_obs$Year)
str(WCraw_obs)
WCraw_obs$Index <- as.numeric(WCraw_obs$Index) #there we go. "2020" & "2021" added


#Determine relative predicted attractiveness by year (i.e., "rank"; how attractive
#were the Crawfish streams relative to all other streams in the dataset)
sort_fxn <- function(dat){
  dat2 <- as.data.frame(dat[order(dat$Predictions, decreasing = T),])
  which(dat2$StreamName == "W Crawfish NE Arm Hd")
}

WC_split <- Mod1_Chp2_predictions %>% group_by(Year) %>% group_split()
WC_pred_rank <- as.data.frame(purrr::map(WC_split, sort_fxn))
HW_years <- seq(2008, 2019)
HW_years <- HW_years[-c(5,9)] #remove 2012 and 2016
WC_pred <- rbind.data.frame(WC_pred_rank, HW_years)
WC_pred2 <- t(WC_pred)
WC_pred2 <- as.data.frame(WC_pred2[,c(2,1)])
rownames(WC_pred2) <- rownames(1:nrow(WC_pred2))
WC_pred2 <- WC_pred2 %>% rename("Year" = "V1", "Rank" = "V2")
#only keep years that have observed data to compare to
WC_pred3 <- WC_pred2 %>% filter(Year %in% c("2008", "2009", "2013", "2014",
                                            "2015", "2018", "2019"))

#Add 2020 and 2021 predictions as well
head(bs_preds_2021HC) #2020 and 2021 predictions by year
#2020:
WCraw2020 <- bs_preds_2021HC %>% filter(Year == "2020")
ord_WCraw2020 <- as.data.frame(WCraw2020[order(WCraw2020$Mean,
                                               decreasing = T),])
which(ord_WCraw2020$StreamName == "W Crawfish NE Arm Hd") #23rd!
#2021:
WCraw2021 <- bs_preds_2021HC %>% filter(Year == "2021")
ord_WCraw2021 <- as.data.frame(WCraw2021[order(WCraw2021$Mean,
                                               decreasing = T),])
which(ord_WCraw2021$StreamName == "W Crawfish NE Arm Hd") #25th!

newrowsWC <- as.data.frame(matrix(data = c("2020", "2021", "23", "25"),
                                  ncol = 2, nrow = 2))
newrowsWC <- newrowsWC %>% rename("Year" = "V1", "Rank" = "V2")
WC_pred4 <- rbind.data.frame(WC_pred3, newrowsWC)


#WMA_Releases_by_Yr for W Crawfish NE Arm Hd
Craw_WMA_2008 <- Chp2_MasterDat %>% filter(StreamName == "W Crawfish NE Arm Hd")
Craw_WMA_2008 <- Craw_WMA_2008 %>% select(c(7,9))
Craw_WMA_2020 <- X20_21_Chp2 %>% filter(StreamName == "W Crawfish NE Arm Hd")
Craw_WMA_2020 <- Craw_WMA_2020 %>% select(c(7,9))
Craw_WMA <- rbind.data.frame(Craw_WMA_2008, Craw_WMA_2020)



### Create barplots ###
#W Crawfish NE Arm Hd OBSERVED values:
#add small amount to 2009 "0" observed so that a bar will show up
WCraw_obs[2,3] <- 0.5
#make plot:
WCraw_obs_plot <- ggplot(WCraw_obs, aes(Year, Index)) +
  labs(y = "Observed Attractiveness Index") +
  geom_bar(stat = "identity", fill = "gray18") + labs(x = "") + theme_bw() +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman"))
WCraw_obs_plot


#W Crawfish NE Arm Hd predicted values (rank, i.e., relative attractiveness):
WCraw_pred_plot <- ggplot(WC_pred4, aes(Year, Rank)) +
  geom_bar(stat = "identity", fill = "gray35") +
  scale_y_discrete(limits=rev) +
  labs(x = "", y = "Rank (Relative Attractiveness)") + theme_bw() +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman"))
WCraw_pred_plot

#WMA_Releases_by_Yr LINE plot
Craw_WMA_plot <- ggplot(Craw_WMA, aes(Year, WMA_Releases_by_Yr, group = 1)) +
  geom_line(size = 1) +
  labs(y = "Number of fish released
within 40 km") +
  theme_bw() +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman"))
Craw_WMA_plot


W_Crawfish_plot <- ggarrange(WCraw_obs_plot, WCraw_pred_plot, Craw_WMA_plot,
                             ncol = 1)
W_Crawfish_plot

#Export as high-res figure:
tiff("WCrawfishNE_plot.tiff", width = 7, height = 8, pointsize = 12,
     units = 'in', res = 300)
W_Crawfish_plot #graph that you want to export
dev.off( )







#6. Visualize effects of hypothetical release scenarios on straying ============
### Add on data for other streams in region. Assume status quo from 2021 onwards:

#High-confidence streams:
head(CV_HC20_21)
CV_HC20_21a <- CV_HC20_21 %>% rename("CV" = "Mean_CV")
#In preds_hypRelHC, remove Year column and move Lat & Long to match CV_HC20_21:
preds_hypRelHC <- preds_hypRelHC %>% select(-Year)
preds_hypRelHC <- preds_hypRelHC[c(1,4,6,7)]
Final_HC_hypRel <- anti_join(CV_HC20_21a, preds_hypRelHC, by = "StreamName")
Final_HC_hypRel <- rbind.data.frame(preds_hypRelHC, Final_HC_hypRel)


#Low confidence streams:
head(CV_LC20_21)
CV_LC20_21a <- CV_LC20_21 %>% rename("CV" = "Mean_CV")
#In preds_hypRelLC, remove Year column and move Lat & Long to match CV_LC20_21:
preds_hypRelLC <- preds_hypRelLC %>% select(-Year)
preds_hypRelLC <- preds_hypRelLC[c(1,4,6,7)]
Final_LC_hypRel <- anti_join(CV_LC20_21a, preds_hypRelLC, by = "StreamName")
Final_LC_hypRel <- rbind.data.frame(preds_hypRelLC, Final_LC_hypRel)


#Attach location data to hypothetical release site predictions
head(Chp2_MasterDat) #2008-2020 lat & long included here
llocs_2008_2020 <- Chp2_MasterDat %>% select(StreamName, LATITUDE, LONGITUDE) %>%
  distinct()
Final_HC_hypRel <- left_join(Final_HC_hypRel, llocs_2008_2020, by = "StreamName")
Final_LC_hypRel <- left_join(Final_LC_hypRel, llocs_2008_2020, by = "StreamName")
sapply(Final_LC_hypRel, function(x) sum(is.na(x)))





#6.1. Map version which includes entire SE AK map ==============================
Hyp_Release_map <- ggmap(myMap) + geom_point(aes(x = LONGITUDE, y = LATITUDE,
                                                 size = Mean_bs,
                                                 fill = CV_percent),
                                             colour = "black", pch = 24,
                                             data = Final_LC_hypRel) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = Mean_bs,
                 fill = CV_percent), colour = "black", pch = 21,
             data = Final_HC_hypRel) +
  labs(x = "Longitude", y = "Latitude", size = "Predicted Index",
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
  scalebar(x.min = -137.2, x.max = -134.9, y.min = 54.8,
           y.max = 55, dist = 50, dist_unit = "km",
           transform = T, height = 0.5, st.dist = 0.6,
           st.size = 5) +
  geom_rect(aes(xmin = -135.3,
                xmax = -134.6,
                ymin = 57.65,
                ymax = 58.05),
            fill = "transparent",
            color = "black", size = 1) +
  annotate("text", x = -135.43, y = 58.0, label = "1", fontface = 2, size = 5) +
  geom_rect(aes(xmin = -135.5,
                xmax = -134.75,
                ymin = 56.59,
                ymax = 57),
            fill = "transparent",
            color = "black", size = 1) +
  annotate("text", x = -135.65, y = 56.95, label = "2", fontface = 2, size = 5) +
  theme(plot.margin = unit(c(0,-0.5,0,1), "cm"))

Hyp_Release_map


### Zoom-in region #1 (Freshwater Bay) ###
#Add hypothetical release site locations
H_releases_hypothetical <- hypRel %>% select(ReleaseSite, Release_site_LAT,
                                             Release_site_LONG)
H_releases_hypothetical <-
  H_releases_hypothetical[!duplicated(H_releases_hypothetical),]


FWBay_map <- get_stamenmap(location <- c(-135.4, 57.65, -134.63, 58.02), zoom = 9,
                           maptype = "terrain-background", crop = TRUE)
ggmap(FWBay_map)

Freshwater_map <- ggmap(FWBay_map) + geom_point(aes(x = LONGITUDE, y = LATITUDE,
                                                    size = Mean_bs,
                                                    fill = CV_percent),
                                                colour = "black", pch = 24,
                                                data = Final_LC_hypRel) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = Mean_bs,
                 fill = CV_percent), colour = "black", pch = 21,
             data = Final_HC_hypRel) +
  geom_point(aes(x = Release_site_LONG, y = Release_site_LAT), size = 5,
             shape = 22, fill = "darkred", data = H_releases_hypothetical) +
  labs(x = "Longitude", y = "Latitude", size = "Predicted
Index",
       fill = "Prediction
CV") +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman")) +
  annotate("text", x = -135.35, y = 57.98, label = "1", fontface = 2, size = 8) +
  theme(plot.margin = unit(c(1,0,0,-0.5), "cm")) + #the plot.margin positioning
  #specified here makes the plot have no margin space at the bottom or on its left
  #side, and I did that so that it would be closer to the Crawfish_map below it
  #and the SEAK strays map to its left when I put them together in ggarrange()
  scalebar(x.min = -135.41, x.max = -134.7, y.min = 57.69, y.max = 57.72,
           transform = T, dist_unit = "km", dist = 20, height = 0.4,
           st.dist = 0.6, st.size = 4.5) +
  scale_fill_gradientn(colours = c("#062635", "#0A4C6A", "#12719E", "#1696D2",
                                   "#46ABDB", "#73BFE2", "#A2D4EC", "#CFE8F3")) +
  scale_size_continuous(range = c(3,10))
#values = rescale(c(0,0.1,0.25,1)))
Freshwater_map



### Zoom-in region #2 (Crawfish Inlet) ###
CF_map <- get_stamenmap(location <- c(-135.53, 56.63, -134.73, 57.03), zoom = 10,
                        maptype = "terrain-background", crop = TRUE)
ggmap(CF_map)

Crawfish_map2 <- ggmap(CF_map) + geom_point(aes(x = LONGITUDE, y = LATITUDE,
                                                size = Mean_bs,
                                                fill = CV_percent),
                                            colour = "black", pch = 24,
                                            data = Final_LC_hypRel) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = Mean_bs,
                 fill = CV_percent), colour = "black", pch = 21,
             data = Final_HC_hypRel) +
  geom_point(aes(x = Release_site_LONG, y = Release_site_LAT), size = 5,
             shape = 22, fill = "darkred", data = H_releases_hypothetical) +
  labs(x = "Longitude", y = "Latitude", size = "Predicted
Index",
       fill = "Prediction
CV") +
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
  scalebar(x.min = -135.5, x.max = -134.83, y.min = 56.66, y.max = 56.68,
           transform = T, dist_unit = "km", dist = 20, height = 0.6,
           st.dist = 0.7, st.size = 4.5) +
  scale_fill_gradientn(colours = c("#062635", "#0A4C6A", "#12719E", "#1696D2",
                                   "#46ABDB", "#73BFE2", "#A2D4EC", "#CFE8F3")) +
  scale_size_continuous(range = c(3,10))
#values = rescale(c(0,0.1,0.25,1)))
Crawfish_map2



### Put whole map together
FW_Craw_together <- ggarrange(Freshwater_map, Crawfish_map2, ncol = 1,
                              common.legend = T, legend = "right")

whole_map2 <- ggarrange(Hyp_Release_map, FW_Craw_together, align = "v",
                        common.legend = T, legend = "right")
whole_map2 

#Export as high-res figure
tiff("hyp_release_map.tiff", width = 9, height = 6, pointsize = 12, units = 'in',
     res = 300)
whole_map2 #graph that you want to export
dev.off( )







#6.2. Map version which show zoomed in panels only for B & A comparison ========
#Show FW Bay and Crawfish Inlet before and after
FWBay_map <- get_stamenmap(location <- c(-135.4, 57.65, -134.63, 58.02), zoom = 9,
                           maptype = "terrain-background", crop = TRUE)
ggmap(FWBay_map)

#add location data to 2021 predictions
locations_data_2021HC <- left_join(CV_HC20_21a, llocs_2008_2020, by = "StreamName")
locations_data_2021LC <- left_join(CV_LC20_21a, llocs_2008_2020, by = "StreamName")

FW_Map_BEFORE <- ggmap(FWBay_map) + geom_point(aes(x = LONGITUDE, y = LATITUDE,
                                                   size = Mean_bs,
                                                   fill = CV_percent),
                                               colour = "black", pch = 24,
                                               data = locations_data_2021LC) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = Mean_bs,
                 fill = CV_percent), colour = "black", pch = 21,
             data = locations_data_2021HC) +
  labs(x = "", y = "Latitude") + guides(size = "none", fill = "none") +
  ggtitle("BEFORE") +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman")) +
  #annotate("text", x = -135.35, y = 57.98, label = "1", fontface = 2, size = 8) +
  #theme(plot.margin = unit(c(1,0,0,-0.5), "cm")) + #the plot.margin positioning
  #specified here makes the plot have no margin space at the bottom or on its left
  #side, and I did that so that it would be closer to the Crawfish_map below it
  #and the SEAK strays map to its left when I put them together in ggarrange()
  scalebar(x.min = -135.41, x.max = -134.7, y.min = 57.69, y.max = 57.72,
           transform = T, dist_unit = "km", dist = 20, height = 0.5,
           st.dist = 0.6, st.size = 4.5) +
  scale_fill_gradientn(colours = c("#062635", "#0A4C6A", "#12719E", "#1696D2",
                                   "#46ABDB", "#73BFE2", "#A2D4EC", "#CFE8F3")) +
  scale_size_continuous(range = c(3,10))
#values = rescale(c(0,0.1,0.25,1)))
FW_Map_BEFORE





FW_Map_AFTER <- ggmap(FWBay_map) + geom_point(aes(x = LONGITUDE, y = LATITUDE,
                                                  size = Mean_bs,
                                                  fill = CV_percent),
                                              colour = "black", pch = 24,
                                              data = Final_LC_hypRel) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = Mean_bs,
                 fill = CV_percent), colour = "black", pch = 21,
             data = Final_HC_hypRel) +
  geom_point(aes(x = Release_site_LONG, y = Release_site_LAT), size = 5,
             shape = 22, fill = "darkred", data = H_releases_hypothetical) +
  labs(x = "", y = "", size = "Predicted
Index",
       fill = "Prediction
CV") + ggtitle("AFTER") +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman")) +
  #annotate("text", x = -135.35, y = 57.98, label = "1", fontface = 2, size = 8) +
  #theme(plot.margin = unit(c(1,0,0,-0.5), "cm")) + #the plot.margin positioning
  #specified here makes the plot have no margin space at the bottom or on its left
  #side, and I did that so that it would be closer to the Crawfish_map below it
  #and the SEAK strays map to its left when I put them together in ggarrange()
  scalebar(x.min = -135.41, x.max = -134.7, y.min = 57.69, y.max = 57.72,
           transform = T, dist_unit = "km", dist = 20, height = 0.5,
           st.dist = 0.6, st.size = 4.5) +
  scale_fill_gradientn(colours = c("#062635", "#0A4C6A", "#12719E", "#1696D2",
                                   "#46ABDB", "#73BFE2", "#A2D4EC", "#CFE8F3")) +
  scale_size_continuous(range = c(3,10))
#values = rescale(c(0,0.1,0.25,1)))
FW_Map_AFTER




#Crawfish map before and after
CF_map <- get_stamenmap(location <- c(-135.53, 56.63, -134.73, 57.03), zoom = 10,
                        maptype = "terrain-background", crop = TRUE)

CF_Map_BEFORE <- ggmap(CF_map) + geom_point(aes(x = LONGITUDE, y = LATITUDE,
                                                size = Mean_bs,
                                                fill = CV_percent),
                                            colour = "black", pch = 24,
                                            data = locations_data_2021LC) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = Mean_bs,
                 fill = CV_percent), colour = "black", pch = 21,
             data = locations_data_2021HC) +
  labs(x = "Longitude", y = "Latitude") +
  guides(size = "none", fill = "none") +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman")) +
  scalebar(x.min = -135.5, x.max = -134.83, y.min = 56.66, y.max = 56.68,
           transform = T, dist_unit = "km", dist = 20, height = 0.6,
           st.dist = 0.7, st.size = 4.5) +
  #annotate("text", x = -135.35, y = 57.98, label = "1", fontface = 2, size = 8) +
  #theme(plot.margin = unit(c(1,0,0,-0.5), "cm")) + #the plot.margin positioning
  #specified here makes the plot have no margin space at the bottom or on its left
  #side, and I did that so that it would be closer to the Crawfish_map below it
  #and the SEAK strays map to its left when I put them together in ggarrange()
  scale_fill_gradientn(colours = c("#062635", "#0A4C6A", "#12719E", "#1696D2",
                                   "#46ABDB", "#73BFE2", "#A2D4EC", "#CFE8F3")) +
  scale_size_continuous(range = c(3,10))
CF_Map_BEFORE





CF_Map_AFTER <- ggmap(CF_map) + geom_point(aes(x = LONGITUDE, y = LATITUDE,
                                               size = Mean_bs,
                                               fill = CV_percent),
                                           colour = "black", pch = 24,
                                           data = Final_LC_hypRel) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = Mean_bs,
                 fill = CV_percent), colour = "black", pch = 21,
             data = Final_HC_hypRel) +
  geom_point(aes(x = Release_site_LONG, y = Release_site_LAT), size = 5,
             shape = 22, fill = "darkred", data = H_releases_hypothetical) +
  labs(x = "Longitude", y = "", size = "Predicted
Index",
       fill = "Prediction
CV") +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman")) +
  #annotate("text", x = -135.48, y = 57, label = "2", fontface = 2, size = 8) +
  #theme(plot.margin = unit(c(0,0,1,-0.5), "cm")) + #the plot.margin positioning
  #specified here makes the plot have no margin space at the top or on its left
  #side, and I did that so that it would be closer to the Amalga_map above it
  #and the SEAK strays map to its left when I put them together in ggarrange()
  scalebar(x.min = -135.5, x.max = -134.83, y.min = 56.66, y.max = 56.68,
           transform = T, dist_unit = "km", dist = 20, height = 0.6,
           st.dist = 0.7, st.size = 4.5) +
  scale_fill_gradientn(colours = c("#062635", "#0A4C6A", "#12719E", "#1696D2",
                                   "#46ABDB", "#73BFE2", "#A2D4EC", "#CFE8F3")) +
  scale_size_continuous(range = c(3,10))
CF_Map_AFTER



### Arrange map
before_FW_CF <- ggarrange(FW_Map_BEFORE, CF_Map_BEFORE, ncol = 1)
after_FW_CF <- ggarrange(FW_Map_AFTER, CF_Map_AFTER, ncol = 1,
                         common.legend = T, legend = "right")

before_after_hypRel_map <- ggarrange(before_FW_CF, after_FW_CF, align = "v")
before_after_hypRel_map

#Export as high-res figure
tiff("hyp_release_mapBA.tiff", width = 9, height = 6.5, pointsize = 12, units = 'in',
     res = 300)
before_after_hypRel_map #graph that you want to export
dev.off( )







#7. Table S3 data (all HC streams + obs data for the 56 that have it) ##########
head(mean_predsChp2)
head(Chp1_Master)
means_for_tabS3 <- Chp1_Master %>% group_by(StreamName) %>%
  summarise(Mean_obs = mean(Avg_number_strays))
tabS3 <- left_join(mean_predsChp2, means_for_tabS3, by = "StreamName")
TableS3 <- tabS3[,c(3,5:7,9,10)]
TableS3 <- TableS3[order(TableS3$Mean_pred_strays,
                     decreasing = T),]
rownames(TableS3) <- 1:nrow(TableS3)
write.csv(TableS3, "TableS3.csv")





save.image("Chp2_figures.RData")
load("Chp2_figures.RData")




