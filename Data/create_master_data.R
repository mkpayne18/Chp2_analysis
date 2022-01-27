#Create chp 2 master dataset. I.e., bring together all covariate data into 1 file
getwd()
setwd("~/Documents/CHUM_THESIS/CHAPTER_2/Chp2_analysis/Data")
Cons_Abundance <- read.csv("Cons_Abundance_Chp2.csv")
Pink_Abundance <- read.csv("Pink_Abundance_Chp2.csv")
Flow <- read.csv("Flow_dat_Chp2.csv")
WMA_Releases <- read.csv("WMA_Releas_chp2.csv")

library(tidyverse)
library(dplyr)
#Tailor Cons_Abundance =========================================================
View(Cons_Abundance) #need to pivot longer
Cons_A_long <- Cons_Abundance %>% pivot_longer(cols = starts_with("X"),
                                               names_to = "Year",
                                               names_prefix = "X")
View(Cons_A_long)
colnames(Cons_A_long)[6] <- "Cons_Abundance"
colnames(Cons_A_long)[3] <- "StreamName"
#Select only the relevant columns from df to join together
Cons_A_long <- Cons_A_long[,c(2,3,5,6)]
#Finally, make sure year is a factor
Cons_A_long$Year <- as.factor(Cons_A_long$Year)


#Tailor Pink_Abundance =========================================================
View(Pink_Abundance) #need to pivot longer
Pink_A_long <- Pink_Abundance %>% pivot_longer(cols = starts_with("X"),
                                               names_to = "Year",
                                               names_prefix = "X")
View(Pink_A_long)
colnames(Pink_A_long)[14] <- "Pink_Abundance"
#Select only the relevant columns from df to join together
Pink_A_long <- Pink_A_long[,c(1,3,4,6:8,13,14)] #keep additional data cols (subregion
#+ AWC #) from this one
colnames(Pink_A_long)[1] <- "Subregion"
Pink_A_long$Year <- as.factor(Pink_A_long$Year)


#Tailor flow data ==============================================================
View(Flow)
Flow2 <- Flow[,c(4,6,11,12)]
#remove Flow2 streams with NA values (no flow data)
Flow2 <- Flow2[complete.cases(Flow2),]



#Tailor WMA_Releases_within_40km data ==========================================
View(WMA_Releases)
#WMA_Releases already fine just need to rename columns
colnames(WMA_Releases)[1] <- "StreamName"
colnames(WMA_Releases)[2] <- "Year"
colnames(WMA_Releases)[3] <- "WMA_Releases_in_millions"
#remove Herman Creek (NSE Inside) because there is no Herman Creek in this sub-
#region in Pink_Abundance data and it doesn't have flow data anyway
WMA_Releases <- WMA_Releases[WMA_Releases$StreamName != "Herman Creek",]
#remove 10 random empty rows I noticed within WMA_Releases
WMA_Releases <- WMA_Releases[complete.cases(WMA_Releases), ]
#year as factor
WMA_Releases$Year <- as.factor(WMA_Releases$Year)



#Create the master dataset #####################################################
#BEGIN JOINING WITH PINK_A_LONG BC IT HAS THE ADDITIONAL INFO COLS
#Somewhere along the way an extra 10 rows appeared (length(Pink_Abundance should
#equal 6640, not 6650) when I did the first join using "Stream_Name" instead of
#"Stream_Number". Using "Stream_Number" seems to have solved it
Master_dataset <- left_join(Pink_A_long, Cons_A_long, by=c("Stream_Number",'Year'))
colnames(Master_dataset)[3] <- "StreamName" #not "StreamName.x"

Master_dataset <- left_join(Master_dataset, Flow2[,c(2:4)], by = c("AWC_CODE"))
Master_dataset <- left_join(Master_dataset, WMA_Releases, by=c('StreamName','Year'))

View(Master_dataset)
#Note that I join by different columns for each dataset^^ (Stream_Number, AWC,
#or StreamName). This is bc when I was checking for correct match-ups in the 
#section below, using StreamName for all 3 datasets would yield some incorrect
#match-ups for reasons that were puzzling to me. But, I found that using the join
#columns I did here yielded 100% correct match-ups to year and stream for each obs.
#Cons_Abundance, Pink_Abundance, and WMA_Releases_by_Yr needed to be joined by 
#'Year' as well


################################################################################
#Make sure everyone got correctly matched up (all values used for each covariate)
sum(is.na(Master_dataset$Pink_Abundance)) #should = 20 because there are 2 streams
#from Cons_Abundance data (Berners River and Kalinin Cove Hd) that do not have
#pink abundance data. I entered these as NA for pink_abundance data vals
length(Master_dataset$Pink_Abundance) #6640 is the full length of the dataset (10
#years of data for each of 664 streams)

#Did all streams correctly match up between pink_abundance data and cons_abundance?
which(ifelse(Master_dataset$StreamName == Master_dataset$StreamName.y, 0, 1) == 1)
#returns rows that do not match stream names^^. No mismatched streams

### Check data values
#Cons_Abundance
sum(is.na(Cons_A_long$Cons_Abundance)) #none
length(Cons_A_long$Cons_Abundance) #870
length(Master_dataset$Cons_Abundance[!is.na(Master_dataset$Cons_Abundance)]) #870
#GOOD


#WMA_Releases_in_millions
sum(is.na(WMA_Releases$WMA_Releases_in_millions)) #none
length(WMA_Releases$WMA_Releases_in_millions)  #1680
length(Master_dataset$WMA_Releases_in_millions[!is.na(
  Master_dataset$WMA_Releases_in_millions)]) #1680

#Flow
sum(is.na(Flow2$mean_flow)) #none
length(Flow2$mean_flow[!is.na(Flow2$mean_flow)]) #640
664-640 #=24 = total # of streams that are NA for flow data, which means there 
#should be 240 rows of NA for flow data in the full dataset: 6640-240=6400
length(Master_dataset$mean_flow[!is.na(Master_dataset$mean_flow)]) #6400
#observations not matching
#Should be the same for CV_flow
length(Master_dataset$CV_flow[!is.na(Master_dataset$CV_flow)]) #6400

#Example of how I previously checked for the streams that did not properly match
#up between datasets when joining. I would identify the streams that DID correctly
#match (success_XXX), and then follow up that with an anti_join to see what streams
#were missed. In the example here, there were initially two streams that did not
#match when I tried to join flow data to the Master_dataset (with pink_abundance
#stream ID info). The names seemed to match exactly so I couldn't figure out why
#the two streams didn't join. But, joining the datasets by AWC_Code solved the 
#issue
success_flow <- inner_join(Master_dataset, Flow2,
                           by = c("AWC_CODE", "mean_flow", "CV_flow"))
fail_flow <- anti_join(Flow2, success_flow, by = c("AWC_CODE", "mean_flow", "CV_flow"))
View(fail_flow) #Marble Creek-Angoon and Ushk. Unclear why matching did not hap-
#pen as the names appear to be the same in Flow2 and the Master_dataset created
#from Pink_A_long stream names. Maybe try joining by StreamName and AWC in the 
#next iteration



#Alright awesome, we finally have it. Export to .csv:
Master_dataset <- Master_dataset[ , c(1:8, 10, 13, 11, 12)]
setwd("~/Documents/CHUM_THESIS/CHAPTER_2/Chp2_analysis")
write.csv(Master_dataset, "Chp2_Master_dataset.csv")


