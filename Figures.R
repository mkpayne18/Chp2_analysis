

#1. Visualization of predicted attractiveness for high confidence streams ######
### Map of chp2 predictions for 2008-2019 SE AK stream attractiveness (First re-
#sult of part 1 of chapter 2, most likely is figure 1)

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
