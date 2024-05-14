library(tidyverse)
library(lubridate)
library(readxl)

setwd("C:/Users/nicolas_vanermen/Desktop/SAS DB 2024 (back-up @ PRJ ZEEVOGELS)/update feb 2024")
TRP <- read.csv("TRIPALL_MERGE_1992_2023_c.csv", fileEncoding = "UTF-8")
POS <- read.csv("BASEALL_MERGE_1992_2023_c.csv", fileEncoding = "UTF-8")
OBS <- read.csv("BIRDALL_MERGE_1992_2023_c.csv", fileEncoding = "UTF-8")

table(TRP$Survey)

TRP <- TRP %>% filter(Survey %in% c("WINMON","CODEVCO","WINMON/LTMON")) 
POS <- POS %>% filter(Tripkey %in% TRP$Tripkey)
OBS <- OBS %>% filter(Poskey %in% POS$Poskey)

TRP_IDOD <- TRP %>% select(Tripkey, Ship, Date, Observers, Survey)

POS_IDOD <- POS %>% select(Tripkey, Poskey, Date, Time, MidLatitude, MidLongitude, TransectWidth, Distance, 
                           Area, SpeciesCounted, CountMethod, WaveHeight, Beaufort, Visibility)

OBS_IDOD <- OBS %>% 
  filter(Transect == 1  & !Behaviour %in% c("X")) %>%
  group_by(Poskey, Species) %>%
  summarise(Number = sum(Count))

setwd("C:/Users/nicolas_vanermen/Desktop/SAS DB 2024 (back-up @ PRJ ZEEVOGELS)/queries")
write.csv(TRP_IDOD,"TRP_IDOD_2014_2023.csv")
write.csv(POS_IDOD,"POS_IDOD_2014_2023.csv")
write.csv(OBS_IDOD,"OBS_IDOD_2014_2023.csv")
