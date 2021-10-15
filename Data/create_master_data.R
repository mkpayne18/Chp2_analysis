#Create chapter master dataset. I.e., bring together all covariate data into 1 file
getwd()
setwd("~/Documents/CHUM_THESIS/CHAPTER_2/Chp2_analysis/Data")
Cons_Abundance <- read.csv("Cons_Abundance_Chp2.csv")
Pink_Abundance <- read.csv("Pink_Abundance_Chp2.csv")
Flow <- read.csv("Flow_dat_Chp2.csv")
WMA_Releases <- read.csv("WMA_Releas_chp2.csv")

View(Cons_Abundance) #need to pivot longer
View(Pink_Abundance) #need to pivot longer
View(Flow)
View(WMA_Releases)

library(tidyverse)
Cons_A_long <- Cons_Abundance %>% pivot_longer(cols = starts_with("X"),
                                               names_to = "Year",
                                               names_prefix = "X")
colnames(Cons_A_long)[6] <- "Cons_Abundance"
View(Cons_A_long)

Pink_A_long <- Pink_Abundance %>% pivot_longer(cols = starts_with("X"),
                                               names_to = "Year",
                                               names_prefix = "X")
colnames(Pink_A_long)[14] <- "Pink_Abundance"
View(Pink_A_long)


#Select only the relevant columns from each df to join together
Cons_A_long <- Cons_A_long[,c(3,5,6)]
colnames(Cons_A_long)[1] <- "StreamName"
colnames(Cons_A_long)[3] <- "Cons_Abundance"
Pink_A_long <- Pink_A_long[,c(1,4,6,13,14)] #keep additional data cols (subregion
#+ AWC #) from this one
Flow2 <- Flow[,c(4,11,12)]
#WMA_Releases already fine just need to rename columns
colnames(WMA_Releases)[1] <- "StreamName"
colnames(WMA_Releases)[2] <- "Year"
colnames(WMA_Releases)[3] <- "WMA_Releases_in_millions"

library(dplyr)
#start with Pink_A_long bc it has the additional data cols
Cons_A_long$Year <- as.factor(Cons_A_long$Year)
Pink_A_long$Year <- as.factor(Pink_A_long$Year)
WMA_Releases$Year <- as.factor(WMA_Releases$Year)
Master_dataset <- left_join(Pink_A_long, Cons_A_long, by=c('StreamName','Year')) %>%
  left_join(., Flow2, by='StreamName') %>% left_join(., WMA_Releases,
                                                               by=c('StreamName','Year'))
View(Master_dataset)
Master_dataset <- Master_dataset[,c(1:4,6,5,9,7,8)]
sum(is.na(Master_dataset$Pink_Abundance))
