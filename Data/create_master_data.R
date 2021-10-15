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
