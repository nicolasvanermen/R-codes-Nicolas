#Read data
setwd("C:/Users/nicolas_vanermen/Desktop/DATA INVOER/2023/UPDATES")
CAMPAIGNS <- read.csv("CAMPAIGNS_BE_ICES_update_feb_2024.csv", fileEncoding = "UTF-8")
SAMPLES <- read.csv("SAMPLES_BE_ICES_update_feb_2024.csv", fileEncoding = "UTF-8")
POSITIONS <- read.csv("POSITIONS_BE_ICES_update_feb_2024.csv", fileEncoding = "UTF-8")
OBSERVATIONS <- read.csv("OBSERVATIONS_BE_ICES_update_feb_2024.csv", fileEncoding = "UTF-8")

#Add RecordTypes
CAMPAIGNS$RecordType <- "EC"
CAMPAIGNS <- CAMPAIGNS[,c(6,1:5)]

SAMPLES$RecordType <- "ES"
SAMPLES <- SAMPLES[,c(16,1:15)]

POSITIONS$RecordType <- "EP"
POSITIONS <- POSITIONS[,c(16,1:15)]

OBSERVATIONS$RecordType <- "EO"
OBSERVATIONS <- OBSERVATIONS[,c(18,1:17)]

#As matrices
FILE_INFORMATION <- matrix(nrow=1,ncol=18)
FILE_INFORMATION[1,1:3] <- c("FI","202","BE")

CAMPAIGNS_screen <- matrix(nrow=nrow(CAMPAIGNS),ncol=18)
CAMPAIGNS_screen[,1:6] <- as.matrix(CAMPAIGNS)

SAMPLES_screen <- matrix(nrow=nrow(SAMPLES),ncol=18)
SAMPLES_screen[,1:16] <- as.matrix(SAMPLES)

POSITIONS_screen <- matrix(nrow=nrow(POSITIONS),ncol=18)
POSITIONS_screen[,1:16] <- as.matrix(POSITIONS)

OBSERVATIONS_screen <- matrix(nrow=nrow(OBSERVATIONS),ncol=18)
OBSERVATIONS_screen[,1:18] <- as.matrix(OBSERVATIONS)

#Bind matrices
ESAS_2_ICES_DB <- rbind(FILE_INFORMATION,
                      CAMPAIGNS_screen,
                      SAMPLES_screen,
                      POSITIONS_screen,
                      OBSERVATIONS_screen)

ESAS_2_ICES_DB[is.na(ESAS_2_ICES_DB)] <- ""
ESAS_2_ICES_DB <- as.data.frame(ESAS_2_ICES_DB)
head(ESAS_2_ICES_DB)

#Write data
write.table(ESAS_2_ICES_DB, "ESAS_INBO_2024_02_23.csv", sep="\t", row.names=F, col.names=F, quote=F, 
            fileEncoding = "UTF-8")
