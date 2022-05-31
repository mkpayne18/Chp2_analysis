NSEI <- read_csv("data/Cons_A_RawDat/Chum NSEI Summer - 1960-2021.csv")
NSEO <- read_csv("data/Cons_A_RawDat/Chum NSEO Summer - 2021.csv")
SSE <- read_csv("data/Cons_A_RawDat/Chum SSE Summer - 1960-2021.csv")

#only keep years 2008-2021
NSEI <- NSEI[ , -c(3:50)]
NSEO <- NSEO[ , -c(3:28)]
SSE <- SSE[ , -c(3:50)]

#also remove 2012 and 2016
NSEI <- NSEI[ , -c(7,11)]
NSEO <- NSEO[ , -c(7,11)]
SSE <- SSE[ , -c(7,11)]

NSEI$Subregion <- rep("NSE Inside", length(NSEI$`Stream Name`))
NSEI <- NSEI[,c(15,1:14)]

NSEO$Subregion <- rep("NSE Outside", length(NSEO$`Stream Name`))
NSEO <- NSEO[,c(15,1:14)]

SSE$Subregion <- rep("SSE", length(SSE$`Stream Name`))
SSE <- SSE[,c(15,1:14)]

Cons_A <- rbind.data.frame(NSEI, NSEO, SSE)
colnames(Cons_A)[2] <- "StreamName"
colnames(Cons_A)[3] <- "Stream_Number"

write.csv(Cons_A, "data/Cons_Abundance_Chp2.csv")

