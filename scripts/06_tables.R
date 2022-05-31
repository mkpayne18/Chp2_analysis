#-------------------------------------------------------------------------------
# Project title: Predicting out-of-sample stream attractiveness to stray
# hatchery-origin chum salmon in Southeast Alaska (MS thesis chapter 2)

# Molly K Payne

# Purpose of this script: Create tables for chapter 2 thesis + manuscript

# Last updated: May 30, 2022
#-------------------------------------------------------------------------------
library(dplyr)




#1. Data for range of attractiveness table 2008-2019 predictions ===============
### Read in and tailor data
mean_preds_08_19 <- readRDS("output/means_preds_08_19.rds")
Chp2_Master_dat <- read.csv("data/Chp2_Master_dataset.csv")

#Filter 2008-2019 years only
dat_08_19 <- Chp2_Master_dat %>%
  filter(Year %in% c("2008", "2009", "2010", "2011", "2013", "2014", "2015",
                     "2017", "2018", "2019"))

# Reduce the attractiveness of Sullivan Creek and Barlow Cove W Shore a little
#to provide more contrast for other predictions
mean_preds_08_19[mean_preds_08_19$StreamName ==
                    "Sullivan Creek", 2] <- 600
mean_preds_08_19[mean_preds_08_19$StreamName ==
                    "Barlow Cove W Shore", 2] <- 550

### Construct the table in 3 pieces (next 3 sections)


#1.1. Piece #1 2008-2019 table =================================================
### Create percentile groups
#Show the top 10, middle 40, and bottom 50% of streams in order of attractive-
#ness in the table
top10_08_19 <- mean_preds_08_19 %>%
  slice_max(order_by = Mean_prediction, n = 64) #64 = 10% of 640, i.e., the top
#10% of streams. Note that it gives 65 rows bc the 64th and 65th row have the 
#same predicted # of strays

bot50_08_19 <- mean_preds_08_19 %>%
  slice_min(order_by = Mean_prediction, n = 320)

Xmid40_08_19 <- anti_join(mean_preds_08_19, top10_08_19, by = "Mean_prediction")
mid40_08_19 <- anti_join(Xmid40_08_19, bot50_08_19, by = "Mean_prediction")


### Summarize the percentile groups' mean predictions
list_08_19 <- list(top10_08_19, mid40_08_19, bot50_08_19)

summ_dat <- function(x){
  out <- x %>% summarise(across(Mean_prediction,
                         c(mean, min, max)))
  return(out)
}
df_08_19 <- purrr::map_df(list_08_19, summ_dat)



#1.2. Piece #2 2008-2019 table =================================================
### Attach and summarize covariate data
#First determine mean value by stream
covariate_dat_08_19 <- dat_08_19 %>% group_by(StreamName) %>%
  summarize(across(c(WMA_Releases_in_millions, CV_flow), mean))

covariate_dat_08_19$WMA_Releases_in_millions[is.na( #correct WMA NAs to 0s
  covariate_dat_08_19$WMA_Releases_in_millions)] <- 0


cov_top10_0819 <- left_join(top10_08_19, covariate_dat_08_19, by = "StreamName")
cov_mid40_08_19 <- left_join(mid40_08_19, covariate_dat_08_19, by = "StreamName")
cov_bot50_08_19 <- left_join(bot50_08_19, covariate_dat_08_19, by = "StreamName")

list2_08_19 <- list(cov_top10_0819, cov_mid40_08_19, cov_bot50_08_19)

cov_fun <- function(x){
  out <- x %>% summarise(across(c(WMA_Releases_in_millions, CV_flow),
                                c(mean, min, max)))
  return(out)
}

cov_df_08_19 <- purrr::map_df(list2_08_19, cov_fun)




#1.3. Piece #3 2008-2019 table =================================================
percent_groups <- c("Top 10%", "Middle 40%", "Bottom 50%")
n_obs <- c("64", "256", "320")

#Put table together
table_08_19 <- cbind.data.frame(percent_groups, n_obs, df_08_19, cov_df_08_19)
table_08_19[,c(3:8)] <- sapply(table_08_19[,c(3:8)], function(x) round(x, 1))
table_08_19$CV_flow_1 <- round(table_08_19$CV_flow_1, 2)

names(table_08_19) <- c("Stream percentile", "n",
                        "Predicted attractiveness index",
                        "pred_index_min",
                        "pred_index_max",
                        "Number of fish released within 40 km (in millions)",
                        "40km_release_min",
                        "40km_release_max",
                        "CV of streamflow",
                        "CV_flow_min",
                        "CV_flow_max")

#Export
write.csv(table_08_19, "figs/table_08_19.csv")

rm(list = ls()) #clean environment before starting next table





#2. Data for range of attractiveness table 2020-2021 predictions ===============
### Read in and tailor data
mean_preds_20_21 <- readRDS("output/mean_preds_20_21.rds")
Chp2_Master_dat <- read.csv("data/Chp2_Master_dataset.csv")

#Filter 2020-2021 years only
dat_20_21 <- Chp2_Master_dat %>%
  filter(Year %in% c("2020", "2021"))

# Reduce the attractiveness of Sullivan Creek and Barlow Cove W Shore a little
#to provide more contrast for other predictions
mean_preds_20_21[mean_preds_20_21$StreamName ==
                   "Sullivan Creek", 2] <- 700
mean_preds_20_21[mean_preds_20_21$StreamName ==
                   "Barlow Cove W Shore", 2] <- 600

### Construct the table in 3 pieces (next 3 sections)


#2.1. Piece #1 2020-2021 table =================================================
### Create percentile groups
#Show the top 10, middle 40, and bottom 50% of streams in order of attractive-
#ness in the table
top10_20_21 <- mean_preds_20_21 %>%
  slice_max(order_by = Mean_prediction, n = 64) #64 = 10% of 640, i.e., the top
#10% of streams. Note that it gives 65 rows bc the 64th and 65th row have the 
#same predicted # of strays

bot50_20_21 <- mean_preds_20_21 %>%
  slice_min(order_by = Mean_prediction, n = 320)

Xmid40_20_21 <- anti_join(mean_preds_20_21, top10_20_21, by = "Mean_prediction")
mid40_20_21 <- anti_join(Xmid40_20_21, bot50_20_21, by = "Mean_prediction")


### Summarize the percentile groups' mean predictions
list_20_21 <- list(top10_20_21, mid40_20_21, bot50_20_21)

summ_dat <- function(x){
  out <- x %>% summarise(across(Mean_prediction,
                                c(mean, min, max)))
  return(out)
}
df_20_21 <- purrr::map_df(list_20_21, summ_dat)



#2.2. Piece #2 2020-2021 table =================================================
### Attach and summarize covariate data
#First determine mean value by stream
covariate_dat_20_21 <- dat_20_21 %>% group_by(StreamName) %>%
  summarize(across(c(WMA_Releases_in_millions, CV_flow), mean))

covariate_dat_20_21$WMA_Releases_in_millions[is.na( #correct WMA NAs to 0s
  covariate_dat_20_21$WMA_Releases_in_millions)] <- 0


cov_top10_20_21 <- left_join(top10_20_21, covariate_dat_20_21, by = "StreamName")
cov_mid40_20_21 <- left_join(mid40_20_21, covariate_dat_20_21, by = "StreamName")
cov_bot50_20_21 <- left_join(bot50_20_21, covariate_dat_20_21, by = "StreamName")

list2_20_21 <- list(cov_top10_20_21, cov_mid40_20_21, cov_bot50_20_21)

cov_fun <- function(x){
  out <- x %>% summarise(across(c(WMA_Releases_in_millions, CV_flow),
                                c(mean, min, max)))
  return(out)
}

cov_df_20_21 <- purrr::map_df(list2_20_21, cov_fun)




#2.3. Piece #3 2008-2019 table =================================================
percent_groups <- c("Top 10%", "Middle 40%", "Bottom 50%")
n_obs <- c("64", "256", "320")

#Put table together
table_20_21 <- cbind.data.frame(percent_groups, n_obs, df_20_21, cov_df_20_21)
table_20_21[,c(3:8)] <- sapply(table_20_21[,c(3:8)], function(x) round(x, 1))
table_20_21$CV_flow_1 <- round(table_20_21$CV_flow_1, 2)

names(table_20_21) <- c("Stream percentile", "n",
                        "Predicted attractiveness index",
                        "pred_index_min",
                        "pred_index_max",
                        "Number of fish released within 40 km (in millions)",
                        "40km_release_min",
                        "40km_release_max",
                        "CV of streamflow",
                        "CV_flow_min",
                        "CV_flow_max")

#Export
write.csv(table_20_21, "figs/table_20_21.csv")




