library(ggplot2)
library(tidyverse)
library(lubridate)
library(geosphere)
library(readxl)

setwd("C:/Users/nicolas_vanermen/Desktop/DATA INVOER/2023/")
SAS_pos_data <- read_excel("SASBASE2023 no macro.xlsx", sheet = "BASE", guess_max = 10000)
SAS_bird_data <- read_excel("SASBASE2023 no macro.xlsx", sheet = "BIRD", guess_max = 10000)

SAS_pos_data <- SAS_pos_data %>% 
  filter(Ship == "SBE2" & Date > "2023-05-01") %>% 
  select(c(1:15))
  
SAS_bird_data <- SAS_bird_data %>% 
  filter(Ship == "SBE2"& Date > "2023-05-01")

UW_data <- read.csv("./UW DATA/Belgica 2023 21 25/Belgica_2023_21_25.csv")

getwd()
BIRDALL_1992_2023_b <- read.csv("./UPDATES/BIRDALL_MERGE_1992_2023_b.csv", fileEncoding = "UTF-8")
BASEALL_1992_2023_b <- read.csv("./UPDATES/BASEALL_MERGE_1992_2023_b.csv", fileEncoding = "UTF-8")
TRIPALL_1992_2023_b <- read.csv("./UPDATES/TRIPALL_MERGE_1992_2023_b.csv", fileEncoding = "UTF-8")

#DateTime aanmaken
SAS_pos_data <- SAS_pos_data %>% 
 mutate(Date = format(Date, format = "%Y-%m-%d"),
        Time = format(Time, format = "%H:%M:%S"),
        DateTimeUCT = with_tz(paste(Date, Time, sep = " "), tz = "UCT"),
        TimeUCT = format(DateTimeUCT, format = "%H:%M:%S"))

SAS_bird_data <- SAS_bird_data %>%
  mutate(Date = format(Date, format = "%Y-%m-%d"),
         Time = format(Time, format = "%H:%M:%S"),
         DateTimeUCT = with_tz(paste(Date, Time, sep = " "), tz = "UCT"),
         TimeUCT = format(DateTimeUCT, format = "%H:%M:%S"))

UW_data <- UW_data %>%
  mutate(DateTimeUCT = ymd_hms(DateTime),
         DateTimeUCT = round_date(DateTimeUCT, unit = "minute"))

#check  
head(SAS_pos_data[month(SAS_pos_data$DateTimeUCT) == 10, c("Date", "Time", "DateTimeUCT", "TimeUCT")])
head(SAS_pos_data[month(SAS_pos_data$DateTimeUCT) == 12, c("Date", "Time", "DateTimeUCT", "TimeUCT")])

#geen duplicates:
sum(duplicated(UW_data$DateTimeUCT))

#zoeken naar missende datetimes
SAS_pos_data %>% 
  filter(!(DateTimeUCT %in% UW_data$DateTimeUCT)) %>% pull(DateTimeUCT)
#none!

#merge maken en verwijder kolom Time om verwarring the vermijden
SAS_UW_data <- 
  left_join(SAS_pos_data, UW_data, join_by("DateTimeUCT")) %>%
  select(-Time)

#distance, area, midlongitude & midlatitude berekenen:
SAS_UW_data <- SAS_UW_data %>% 
  arrange(Date, Ship, TimeUCT) %>% 
  mutate(Distance = c(distHaversine(cbind(Longitude[-1], Latitude[-1]),
                                    cbind(Longitude[-n()], Latitude[-n()])), NA),
         Distance = round(Distance/1000, digits = 3),
         Area = Distance * Transect/1000,
         MidLatitude = c(Latitude[-n()] + 1/2*(Latitude[-1] - Latitude[-n()]), NA),
         MidLongitude = c(Longitude[-n()] + 1/2*(Longitude[-1] - Longitude[-n()]), NA))
SAS_UW_data <- as.data.frame(SAS_UW_data)
head(SAS_UW_data[,c("Latitude","Longitude","Distance","Area","MidLatitude","MidLongitude")])
summary(SAS_UW_data[,c("Latitude","Longitude","Distance","Area","MidLatitude","MidLongitude")])

#weggooien van records met duration 0 (stopsDateTime#weggooien van records met duration 0 (stops)
sum(SAS_UW_data$Dur == 0)
SAS_UW_data <-  SAS_UW_data[SAS_UW_data$Dur != 0,]
#afstand van meer dan 1 km is onwaarschijnlijk
SAS_UW_data %>% filter(Distance > 1) %>% pull(DateTime)
#check!

summary(SAS_UW_data[,c("Latitude","Longitude","Distance","Area","MidLatitude","MidLongitude")])

#grafische checks:
ggplot(SAS_UW_data, aes(Distance)) + geom_histogram()
ggplot(SAS_UW_data, aes(MidLongitude, MidLatitude, col = WP)) + geom_point()

for (i in unique(as.character(SAS_UW_data$Date)))
{
  print(ggplot(SAS_UW_data[SAS_UW_data$Date == i,], aes(MidLongitude, MidLatitude, col = WP)) + 
          labs(title = i) + geom_point())
}

#omzetting naar formaat compatibel met dat van SAS applicatie:
#TRIPALL:
TRIPALL <- aggregate(DateTime ~ Date + Ship + Observers + Survey, data = SAS_UW_data, FUN = length)

TRIPALL <- TRIPALL %>% 
  arrange(Date, Ship) %>%
  mutate(Tripkey = max(TRIPALL_1992_2023_b$Tripkey) + c(1:nrow(TRIPALL))) %>%
  select(names(TRIPALL_1992_2023_b[,-1]))
TRIPALL

#BASEALL:
BASEALL <- merge(SAS_UW_data,TRIPALL[,c("Tripkey","Date","Ship","Survey")])

BASEALL <- BASEALL %>% 
  arrange(Date, Ship, TimeUCT) %>%
  mutate(Poskey = max(BASEALL_1992_2023_b$Poskey) + c(1:nrow(BASEALL)))  %>%
  rename(SpeciesCounted = Species,
         TransectWidth = Transect,
         CountMethod = Count,
         WaveHeight = Wave) %>%
  mutate(WP = ifelse(WP %in% c("wp"), 1, 0)) 

head(BASEALL)

#BIRDALL:
BIRDALL <- merge(SAS_bird_data, BASEALL[,c("Poskey","Tripkey","Ship","Date","TimeUCT")], all.x = T)
BIRDALL[is.na(BIRDALL$Poskey),]

BIRDALL <- BIRDALL %>% 
  arrange(Poskey, Group) %>%
  mutate(Obskey = max(BIRDALL_1992_2023_b$Obskey) + c(1:nrow(BIRDALL))) %>%
  select(names(BIRDALL_1992_2023_b[,-1]))
  
head(BIRDALL)

#Notes checken
BIRDALL[!is.na(BIRDALL$Notes),]$Notes
#Check!

#SAMEN ZETTEN MET VORIGE UPDATE
BASEALL <- BASEALL %>%
  rename(Time = TimeUCT) %>%
  select(names(BASEALL_1992_2023_b[,-1]))

#TRIPALL_MERGE
TRIPALL_MERGE <- rbind(TRIPALL_1992_2023_b[,-1],TRIPALL)

#BASEALL_MERGE
BASEALL_MERGE <- rbind(BASEALL_1992_2023_b[,-1],BASEALL)

#BIRDALL_MERGE
BIRDALL_MERGE <- rbind(BIRDALL_1992_2023_b[,-1],BIRDALL)

#characters omzetten naar factoren en "" naar NA values
TRIPALL_MERGE[sapply(TRIPALL_MERGE, is.character)] <- lapply(TRIPALL_MERGE[sapply(TRIPALL_MERGE, is.character)], as.factor)
TRIPALL_MERGE[TRIPALL_MERGE == ""] <- NA
TRIPALL_MERGE <- droplevels(TRIPALL_MERGE)

BASEALL_MERGE[sapply(BASEALL_MERGE, is.character)] <- lapply(BASEALL_MERGE[sapply(BASEALL_MERGE, is.character)], as.factor)
BASEALL_MERGE[BASEALL_MERGE == ""] <- NA
BASEALL_MERGE <- droplevels(BASEALL_MERGE)

BIRDALL_MERGE[sapply(BIRDALL_MERGE, is.character)] <- lapply(BIRDALL_MERGE[sapply(BIRDALL_MERGE, is.character)], as.factor)
BIRDALL_MERGE[BIRDALL_MERGE == ""] <- NA
BIRDALL_MERGE <- droplevels(BIRDALL_MERGE)

# summary(TRIPALL_MERGE)
# summary(BASEALL_MERGE)
# summary(BIRDALL_MERGE)

#wegschrijven:
setwd("C:/Users/nicolas_vanermen/Desktop/DATA INVOER/2023/UPDATES")
write.csv(TRIPALL_MERGE,"TRIPALL_MERGE_1992_2023_b.csv", fileEncoding = "UTF-8")
write.csv(BASEALL_MERGE,"BASEALL_MERGE_1992_2023_b.csv", fileEncoding = "UTF-8")
write.csv(BIRDALL_MERGE,"BIRDALL_MERGE_1992_2023_b.csv", fileEncoding = "UTF-8")

