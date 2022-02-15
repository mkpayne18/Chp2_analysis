#Read in data listing release sites that have chp 2 streams within 40km of them
setwd("~/Documents/CHUM_THESIS/CHAPTER_2/Chp2_analysis/Data")
r40 <- read.csv("Releases_within40km_chp2.csv")
str(r40)
r40$ReleaseSite <- as.factor(r40$ReleaseSite)
r40$Stream_within_40km <- as.factor(r40$Stream_within_40km)


#Get a list of release sites by stream, instead of streams by release sites
library(tidyverse)
library(dplyr)
dat <- r40 %>% group_by(Stream_within_40km) %>% distinct(ReleaseSite)
dat <- dat[,c(2,1)]


#Calculate mean age distribution of all known hatchery strays from your 2008-2019
#data ##########################################################################
X2008_2019_HW_Data <-
  read_csv("~/Documents/CHUM_THESIS/Data Sources/Stray_Data/2008-2019_HW_Data.csv")
HW_Data <- X2008_2019_HW_Data[,c(1:3,18,20)]
marks_yr <- HW_Data %>% group_by(Year, MarkId) %>% summarize(n = n())
#remove NAs (the wild fish which don't have marks)
marks_yr <- marks_yr[complete.cases(marks_yr),]
#also remove the exp_mark and unknown mark rows bc we have no age data for those
p <- marks_yr %>% filter(MarkId != "EXP_MARK")
p2 <- p %>% filter(MarkId != "unknown mark")
marks_yr <- p2 %>% filter(MarkId != "UNKNOWN")
View(marks_yr)

#extract the brood year from the markID
marks_yr$Brood_year <- as.numeric(str_extract(marks_yr$MarkId, "[0-9]+"))
marks_yr$Brood_year <- marks_yr$Brood_year + 2000
#remove Nitinat hatchery fish (the one in BC)
marks_yr <- marks_yr %>% filter(MarkId != "NITH2010CHUM32H")
marks_yr <- marks_yr %>% filter(MarkId != "NITH2011CHUM32H")
#determine age
marks_yr$Age <- marks_yr$Year - marks_yr$Brood_year
#repeat each row (each age) the number of times specified in the "n" column, which
#indicates how many fish there were of that age in that year (so you can get a 
#total of each age group in each year)
marks_yr_expanded <- marks_yr[rep(row.names(marks_yr), marks_yr$n), ]
View(marks_yr_expanded)
#now determine total numbers of each age fish from 2008-2019
ages <- table(marks_yr_expanded$Age)
ages <- as.data.frame(ages)
View(ages) #in the entire HW dataset, all of the H strays range from 3-7yo. There
#are very few 6 or 7 yo though, so I will limit my age distribution calculation
#to 3, 4, and 5yo proportions out of the total of 3+4+5yo only
ages <- ages[-c(4,5),] #remove 6 and 7yo row
total345 <- sum(ages$Freq)
ages$Proportion_345only <- ages$Freq/total345



#Add release site data by year to each stream ##################################
s_yrs <- seq(2008, 2021, 1) #"s" for survey year, or year H fish were sampled.
#2020 and 2021 are included for making additional predictions in part 2 of chapter
#2, so there are 12 total years instead of 10
s_yrs <- s_yrs[-c(5,9)] #remove 2016 and 2012
#repeat each line of streams w/ release sites df length(s_yrs) times
dat2 <- dat[rep(seq_len(nrow(dat)), each = length(s_yrs)), ]
dat3 <- cbind.data.frame(dat2, s_yrs)
#now add corresponding release years to each individual sampling year (2, 3, & 
#4 years ago)
#rep each row 3 times (to be able to add 3 years)
dat4 <- dat3[rep(seq_len(nrow(dat2)), each = 3), ]
dat4$subtract <- c(2,3,4)
dat4$YearReleased <- dat4$s_yrs - dat4$subtract
dat4[!complete.cases(dat4), ] #no NAs. Good


#Read in # of fish released by year for each release site df
Releases_site_year <- read.csv("Releases_thru2019.csv")

#join releases by year to dat4
dat5 <- left_join(dat4, Releases_site_year, by = c("ReleaseSite", "YearReleased"))
dat5 <- dat5[,c(1:5,7)]
#bind proportions of each age to corresponding year. 0.038 for 3yo, 0.612 for 
#4yo, and 0.3495 for 5yo as specified in ages
dat5$props <- ages$Proportion_345only
#calculate proportion of each year's release for each release site based on fish age
dat5$prop_released <- dat5$SUM_Releases_in_millions * dat5$props
#sum every 3 rows of the prop_released column (proportion of releases 3, 4, and 
#5 years ago weighted by how many 3, 4, and 5yos there were over time)
#first replace NAs with 0s in the prop_released column so that the WMA won't re-
#turn NAs for release sites that didn't release in all years
dat5$prop_released[is.na(dat5$prop_released)] <- 0

dat5$three_yr_index <- c(0, rep(1:(nrow(dat5)-1)%/%3))
dat6 <- group_by(dat5, three_yr_index) %>%
  summarise(WMA_Release_Total = sum(prop_released))
View(dat6) #nice. This gives the WMA of fish released at each release site 2, 3,
#and 4 years earlier based on the age distribution of hatchery strays (3, 4, and 
#5yrs old)
View(dat3)
dat7 <- cbind.data.frame(dat3, dat6$WMA_Release_Total)
#sum WMA_releases from each release site by stream. Es decir, for streams that 
#have more than 1 release site within 40km, sum up the WMA_Release total for those
#sites
WMA_Releas_chp2 <- aggregate(dat7[,-c(1:3)],dat7[c("Stream_within_40km", "s_yrs")],sum)
View(WMA_Releas_chp2)                         
write.csv(WMA_Releas_chp2, "WMA_Releas_chp2.csv")

