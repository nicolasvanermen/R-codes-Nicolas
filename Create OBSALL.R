library(tidyverse)
library(ggplot2)

setwd("C:/Users/nicolas_vanermen/Desktop/SAS DB 2024/update feb 2024")
TRIPALL <- read.csv("TRIPALL_MERGE_1992_2023_c.csv")
BASEALL <- read.csv("BASEALL_MERGE_1992_2023_c.csv")
BIRDALL <- read.csv("BIRDALL_MERGE_1992_2023_c.csv")

#check trips 2023:
ggplot(BASEALL[grepl("2023", BASEALL$Date),], aes(x=MidLongitude, y=MidLatitude, colour=as.factor(WP))) + geom_point()

#create OBSALL
soortselectie <- c(20,30,59,90,220,710,720,2130,5690,5780,5820,5900,5910,5920,5926,5927,6000,6020,6110,6150,6240,6340,6345,6360,63510)

BIRDALLselectie <- BIRDALL %>%
  filter(Species %in% soortselectie, 
         !(Behaviour %in% c("X")))
         
BASE <- BASEALL %>% expand(Poskey, soortselectie) %>%
  rename(Species = soortselectie)
  
SOM <- BIRDALLselectie %>%
  group_by(Poskey, Species) %>%
  summarise(Count = sum(Count)) %>%
  mutate(Species = as.numeric(Species))

BASE_SOM <- left_join(BASE, SOM) %>%
  mutate(Count = ifelse(is.na(Count), 0, Count)) %>%
  spread(Species, Count) %>%
  arrange(Poskey)

BASE_SOM <- as.data.frame(BASE_SOM)
head(BASE_SOM)

OBSALL <- left_join(BASEALL[,c("Poskey","Date","Time","Area","MidLatitude","MidLongitude","CountMethod","SpeciesCounted")], 
                    BASE_SOM) %>%
  mutate(Year = year(date(Date)),
         Month = month(date(Date)),
         Day = day(date(Date))) %>%
  arrange(Year, Month, Day, Time)

#take in account 'species counted':
table(OBSALL$SpeciesCounted)
OBSALL[OBSALL$SpeciesCounted == 4, c("220","5780","5820","5900","5910","5920","X5926","X5927","6000","6020")] <- NA

#check:
head(OBSALL)
summary(OBSALL)

#export:
setwd("C:/Users/nicolas_vanermen/Desktop/SAS DB 2024/update feb 2024/R exports")
write.csv(OBSALL, "OBSALL_update_2024_02.csv")

#test:
test <- read.csv("C:/Users/nicolas_vanermen/Desktop/SAS DB 2023/update mei 2023/OBSALL_NoCorr_update_2023_05.csv")

summary(test$X5910)
summary(OBSALL[OBSALL$Poskey<=max(test$Poskey),]$`5910`)

summary(test$X5910) == 
summary(OBSALL[OBSALL$Poskey<=max(test$Poskey),]$`5910`)

summary(test$Area)
summary(OBSALL[OBSALL$Poskey<=max(test$Poskey),]$Area)

summary(test$Area) ==
summary(OBSALL[OBSALL$Poskey<=max(test$Poskey),]$Area)
