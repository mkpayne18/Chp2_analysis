#-------------------------------------------------------------------------------
# Project title: Predicting out-of-sample stream attractiveness to stray
# hatchery-origin chum salmon in Southeast Alaska (MS thesis chapter 2)

# Molly K Payne

# Purpose of this script: Create figures for chapter 2 thesis + manuscript

# Last updated: May 30, 2022
#-------------------------------------------------------------------------------
library(ggplot2)
library(ggpubr)
library(ggmap)
library(reshape2)
library(scales)
library(ggspatial) #for more modern version of north arrow with easier code
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(grid)
library(ggsn)
library(gridExtra)
library(ggpubr)
library(dplyr)


#1. Chp 1 model preds and obs values ===========================================
#This is currently figure 1 of the thesis + chp2 manuscript
mean_bm1_pred <- readRDS("output/mean_bm1_pred.rds") #this object is from chp 1
#and contains the mean predicted and observed attractiveness index for each stream


#1.1. Data tailoring ===========================================================
#I'm going to create a 2-panel figure, where the second panel shows the relative
#predicted and observed attractiveness. To create the 2nd panel, order the pred
#and obs vals and assign them ranks (i.e., most attractive stream has rank #1,
#second most attractive stream has rank #2, etc.)
#Predicted vals ranks:
pred_ordered <- mean_bm1_pred[order(mean_bm1_pred$Mean_pred_strays,
                                    decreasing = T),]
pred_ordered$Preds_rank <- 1:nrow(pred_ordered)

#Obs vals ranks:
obs_ordered <- pred_ordered[order(pred_ordered$Mean_obs_strays, decreasing = T),]
obs_ordered$Obs_rank <- 1:nrow(obs_ordered)


#1.2. Create the plot ==========================================================
ObsPred_vals_plot <- ggplot(mean_bm1_pred) +
  geom_point(aes(x = Mean_pred_strays, y = Mean_obs_strays)) +
  geom_abline() + theme_bw() +
  labs(x = "Predicted attractiveness index",
       y = "Observed attractiveness index") +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman"))

ObsPred_vals_plot


ObsPred_rank_plot <- ggplot(obs_ordered) +
  geom_point(aes(x = Preds_rank, y = Obs_rank)) + geom_abline() +
  theme_bw() +
  labs(x = "Predicted attractiveness rank",
       y = "Observed attractiveness rank") +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman"))

ObsPred_rank_plot

Chp1_Obs_Pred <- ggarrange(ObsPred_vals_plot, ObsPred_rank_plot, ncol = 2)
Chp1_Obs_Pred

#Export
tiff("figs/Chp1_Obs_Pred.tiff", width = 9, height = 5, pointsize = 12,
     units = 'in',
     res = 300)
Chp1_Obs_Pred
dev.off( )

#Remove uneeded items
rm(obs_ordered, pred_ordered, ObsPred_rank_plot, ObsPred_vals_plot)






#2. Attractiveness map 2008-2019 predictions ===================================
#Create a multi-panel map which shows all of SEAK predictions in one central map,
#then zooms in to 2 smaller maps to show more detail

### Read in necessary objects
mean_preds_08_19 <- readRDS("output/means_preds_08_19.rds") #chp 2 attractiveness
#predictions
StreamPoints <- read.csv("data/Chp2_StreamPoints.csv") #stream locations
H_Release_Locations <- read.csv("data/H_Release_Locations.csv") #hatchery release
#site locations



#2.1. Data tailoring ===========================================================
#Remove Crawfish Inlet release and Thomas Bay release sites from hatchery release
#df bc no fish were released from there in years that would correspond with years
#that 2008-2019 streams were surveyed. Hence plotting those release sites on the
#2008-2019 map is misleading
H_Release_Locations2 <- H_Release_Locations %>%
  filter(ReleaseSite != "CRAWFISH INLET 113-33")
H_Release_Locations2 <- H_Release_Locations2 %>%
  filter(ReleaseSite != "THOMAS BAY 110-12")


#Add location data to chp2 predictions
mean_preds_08_19X <- left_join(mean_preds_08_19, StreamPoints,
                                     by = "StreamName")
mean_preds_08_19X <- mean_preds_08_19X[,c(1:3,9,10)]
join_fail <- anti_join(mean_preds_08_19, StreamPoints,
                       by = "StreamName") #not sure why these three streams
#didn't join. They appear to be spelled correctly in both dfs. Manually enter
#their location data instead:
mean_preds_08_19X[320,4] <- 57.68192 #Marble Creek-Angoon
mean_preds_08_19X[320,5] <- -134.7011
mean_preds_08_19X[571,4] <- 57.54642 #Ushk
mean_preds_08_19X[571,5] <- -135.6901
mean_preds_08_19X[587,4] <- 55.25929 #W Arm Chol Sd Head
mean_preds_08_19X[587,5] <- -132.4723
sapply(mean_preds_08_19X, function(x) sum(is.na(x))) #no NAs

# Reduce the attractiveness of Sullivan Creek and Barlow Cove W Shore a little so 
#that you can see some contrast on the map (but still make these two creeks be
#way more attractive than the rest)
mean_preds_08_19X[mean_preds_08_19X$StreamName ==
                          "Sullivan Creek", 2] <- 600
mean_preds_08_19X[mean_preds_08_19X$StreamName ==
                          "Barlow Cove W Shore", 2] <- 550

#Remove unneeded items
rm(join_fail, mean_preds_08_19)



#2.2. Create central map =======================================================
myMap <- get_stamenmap(location <- c(-137, 54.5, -130, 59.5), zoom = 6,
                       maptype = "terrain-background", crop = TRUE)
ggmap(myMap)
strays_map1 <- ggmap(myMap) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = Mean_prediction,
                 fill = Mean_CV), colour = "black", pch = 21,
             data = mean_preds_08_19X) +
  labs(x = "Longitude (°W)", y = "Latitude (°N)", size = "Predicted
index",
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
  #annotate("text", x = -135.68, y = 58.7, label = "1", fontface = 2, size = 5,
  # col = "white") +
  geom_rect(aes(xmin = -134.05,
                xmax = -132.7,
                ymin = 56.8,
                ymax = 57.5),
            fill = "transparent",
            color = "white", size = 1) +
  #annotate("text", x = -134.2, y = 57.45, label = "2", fontface = 2, size = 5,
  #col = "white") +
  theme(plot.margin = unit(c(0,-0.5,0,1), "cm"))

strays_map1 #warning message about true north not being meaningful. This is bc
#I included the north arrow as an annotation on the plot, rather than using data
#to locate it. You can ignore



### Create inset map
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
#add inset map to figure
strays_map2 <- strays_map1 + inset(grob = ggplotGrob(alaska), xmin = -133, xmax = -130,
                                   ymin = 58.2, ymax = 59.55) 
strays_map2

#add scale bar
strays_map3 <- strays_map2 + scalebar(x.min = -136.9, x.max = -134.9, y.min = 54.75,
                                      y.max = 54.95, dist = 50, dist_unit = "km",
                                      transform = T, height = 0.4, st.dist = 0.6,
                                      st.size = 4)
strays_map3




#2.3. "Zoom-in" map panel #1 ===================================================
#Zoom into Lynn Canal, Amalga Harbor area 
zoom1_map <- get_stamenmap(location <- c(-135.55, 58.15, -134.41, 58.74),
                           zoom = 9,
                           maptype = "terrain-background",
                           crop = TRUE)
ggmap(zoom1_map)

Amalga_map <- ggmap(zoom1_map) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = Mean_prediction,
                 fill = Mean_CV), colour = "black", pch = 21,
             data = mean_preds_08_19X) +
  geom_point(aes(x = Longitude, y = Latitude), shape = 22, size = 4,
             fill = "darkred", data = H_Release_Locations2) +
  labs(x = "", y = "", size = "Predicted
index", fill = "Prediction
CV") +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman")) +
  theme(legend.box = "horizontal") +
  #annotate("text", x = -135.45, y = 58.68, label = "1", fontface = 2, size = 8,
  #col = "white") +
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
Amalga_map2




#2.4. "Zoom-in" map panel #2 ===================================================
#Zoom into Dry Bay Creek area SE of bottom of Admiralty Island
zoom4_map <- get_stamenmap(location <- c(-134.1, 56.68, -132.6, 57.5), zoom = 10,
                           maptype = "terrain-background", crop = TRUE)
ggmap(zoom4_map)

#Previously I had zoomed in on the Neets Bay area. Here is the map for that if
#you wish to return to using that area again:
#zoom2_map <- get_stamenmap(location <- c(-132.29, 55.5, -131.43, 55.97),
#zoom = 10, maptype = "terrain-background", crop = TRUE)
#ggmap(zoom2_map)

#Other option for previous zoom area; Crawfish Inlet:
#zoom3_map <- get_stamenmap(location <- c(-135.53, 56.6, -134.73, 57.03),
#zoom = 10, maptype = "terrain-background", crop = TRUE)
#ggmap(zoom3_map)

### For Dry Bay Creek, so that you can show more contrast in the map, you should
#filter out the really attractive streams (Dry Bay Creek is moderately attractive
#compared to many unattractive sites around it)
preds_under_30 <- mean_preds_08_19X %>% filter(Mean_prediction < 30)
#Make Dry Bay Creek its own point in its own df:
only_DryBay <- preds_under_30 %>% filter(StreamName == "Dry Bay Creek")
preds_under_30 <- preds_under_30 %>% filter(StreamName != "Dry Bay Creek")


### Make the map with data!
DryBay_map <- ggmap(zoom4_map) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = Mean_prediction,
                 fill = Mean_CV), colour = "white", pch = 21,
             data = only_DryBay) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = Mean_prediction,
                 fill = Mean_CV), colour = "black", pch = 21,
             data = preds_under_30) +
  geom_point(aes(x = Longitude, y = Latitude), shape = 22, size = 4,
             fill = "darkred", data = H_Release_Locations2) +
  labs(x = "Longitude (°W)", y = "", size = "Predicted
index", fill = "Prediction
CV")  +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 11.5)) +
  theme(legend.title = element_text(size = 14)) +
  theme(text=element_text(family="Times New Roman")) +
  theme(legend.box = "horizontal") +
  #annotate("text", x = -134.0, y = 57.41, label = "2", fontface = 2, size = 8,
  # col = "white") +
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




#2.5. Put whole map together ===================================================
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
tiff("figs/Amalga_DryBay.tiff", width = 7, height = 6, pointsize = 12,
     units = 'in',
     res = 300)
right_side #graph that you want to export
dev.off( )


#Export as high-res figure
tiff("figs/SEAK_map_Fig1.tiff", width = 6, height = 5, pointsize = 12,
     units = 'in',
     res = 300)
strays_map3 #graph that you want to export
dev.off( )

#Remove unneeded items
rm(usa_can, world, DryBay_map, Amalga_map, strays_map1, strays_map2)






#3. Before & after hypothetical release map ====================================
#Create a 4-panel figure which shows Freshwater Bay and Crawfish Inlet before and
#after the addition of a new hatchery release (or increase in releases in the
#case of Crawfish Inlet)

#Read in necessary data
mean_preds_20_21 <- readRDS("output/mean_preds_20_21.rds")
mean_preds_hypRel <- readRDS("output/mean_preds_hypRel.rds")




#3.1. Data tailoring ===========================================================
### Attach location data to 2020-2021 predictions
mean_preds_20_21X <- left_join(mean_preds_20_21, mean_preds_08_19X[,c(1,4:5)],
                               by = "StreamName") #attach to the 2008-2019 data
#from above so as to avoid the weird problem of the 2 creeks not attaching to
#their location data
join_fail <- anti_join(mean_preds_20_21, mean_preds_08_19X[,c(1,4:5)],
                       by = "StreamName") 
sapply(mean_preds_20_21X, function(x) sum(is.na(x))) #Success
####


### Attach location data to hypothetical release site df
mean_preds_hypRelX <- left_join(mean_preds_hypRel, mean_preds_08_19X[,c(1,4:5)],
                                by = "StreamName") #attach to the 2008-2019 data
#from above so as to avoid the weird problem of the 2 creeks not attaching to
#their location data
join_fail <- anti_join(mean_preds_hypRel, mean_preds_08_19X[,c(1,4:5)],
                       by = "StreamName") 
sapply(mean_preds_hypRelX, function(x) sum(is.na(x))) #Success
####



#Create df that contains the predictions for the streams within 40km of a new
#hypothetical release site combined with the 2020-2021 mean predictions for other
#streams, given that those predictions will not differ outside of 40km from a
#new release site:
dat_for_hypRel <- anti_join(mean_preds_20_21X, mean_preds_hypRel,
                            by = "StreamName")
colnames(mean_preds_hypRelX)[2] <- "Mean_prediction"
colnames(mean_preds_hypRelX)[3] <- "Mean_CV"

dat_for_hypRel2 <- rbind.data.frame(dat_for_hypRel, mean_preds_hypRelX)
#Correct Sullivan Creek and Barlow Cove W Shore predictions
dat_for_hypRel2[dat_for_hypRel2$StreamName ==
                  "Sullivan Creek", 2] <- 700
dat_for_hypRel2[dat_for_hypRel2$StreamName ==
                  "Barlow Cove W Shore", 2] <- 650

sapply(dat_for_hypRel2, function(x) sum(is.na(x)))


# Read in hypothetical release site location data
Releases_2020 <- read.csv("data/Releases_2020.csv") #ignore warning, df is fine






#3.2. Freshwater Bay BEFORE new release site ===================================
#Show FW Bay and Crawfish Inlet before and after
FWBay_map <- get_stamenmap(location <- c(-135.4, 57.65, -134.63, 58.02), zoom = 9,
                           maptype = "terrain-background", crop = TRUE)
ggmap(FWBay_map)

FW_Map_BEFORE <- ggmap(FWBay_map) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = Mean_prediction,
                 fill = Mean_CV), colour = "black", pch = 21,
             data = mean_preds_20_21X) +
  labs(x = "", y = "Latitude (°N)") + guides(size = "none", fill = "none") +
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




#3.3. Freshwater Bay AFTER new release site ====================================
FW_Map_AFTER <- ggmap(FWBay_map) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = Mean_prediction,
                 fill = Mean_CV), colour = "black", pch = 21,
             data = dat_for_hypRel2) +
  geom_point(aes(x = Longitude, y = Latitude), shape = 22, size = 4,
             fill = "darkred", data = Releases_2020) +
  labs(x = "", y = "", size = "Predicted
index",
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






#3.4. Crawfish Inlet BEFORE increased releases =================================
CF_map <- get_stamenmap(location <- c(-135.52, 56.63, -134.74, 57.03), zoom = 10,
                        maptype = "terrain-background", crop = TRUE)

CF_Map_BEFORE <- ggmap(CF_map) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = Mean_prediction,
                 fill = Mean_CV), colour = "black", pch = 21,
             data = mean_preds_20_21X) +
  labs(x = "Longitude (°W)", y = "Latitude (°N)") +
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





#3.5. Crawfish Inlet AFTER increased releases =================================
CF_Map_AFTER <- ggmap(CF_map) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE, size = Mean_prediction,
                 fill = Mean_CV), colour = "black", pch = 21,
             data = dat_for_hypRel2) +
  geom_point(aes(x = Longitude, y = Latitude), shape = 22, size = 4,
             fill = "darkred", data = Releases_2020) +
  labs(x = "Longitude (°W)", y = "", size = "Predicted
index",
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





#3.6. Put before & after map together ==========================================
### Arrange map
before_FW_CF <- ggarrange(FW_Map_BEFORE, CF_Map_BEFORE, ncol = 1)
after_FW_CF <- ggarrange(FW_Map_AFTER, CF_Map_AFTER, ncol = 1,
                         common.legend = T, legend = "right")

before_after_hypRel_map <- ggarrange(before_FW_CF, after_FW_CF, align = "v")
before_after_hypRel_map

#Export
tiff("figs/before_after_map.tiff", width = 9, height = 6.5, pointsize = 12,
     units = 'in',
     res = 300)
before_after_hypRel_map
dev.off( )

