#Dit is een distance correctie-script volgens de methode die gebruikt werd om de IHD referentie-aantallen te berekenen, 
#en kan aldus als basis dienen voor de 6-jaarlijkse rapportages teneinde ook telkens op dezelfde referentie-aantallen
#uit te komen

library(tidyverse)
library(Distance)
library(mrds)
library(ggplot2)
library(sf)
library(arsenal)

setwd("C:/Users/nicolas_vanermen/Desktop/SAS DB 2024/update feb 2024")
TRIPALL <- read.csv("TRIPALL_MERGE_1992_2023_c.csv")
BASEALL <- read.csv("BASEALL_MERGE_1992_2023_c.csv")
BIRDALL <- read.csv("BIRDALL_MERGE_1992_2023_c.csv")

#truukje om duikers samen te voegen:
GAVIA <- BIRDALL %>% 
  filter(Species %in% c(20,30,59)) %>% 
  mutate(Species = 59)

#selectie soorten
soorten <- c(90,710,5780,5900,5910,5920,6000,6020,6110,6150,6340,6360)
BIRDALL <- BIRDALL %>% filter(Species %in% soorten)
BIRDALL <- rbind(BIRDALL,GAVIA)

#selectie BPNS
setwd("C:/Users/nicolas_vanermen/Desktop/SAS DB 2023/Shapes/")
BPNS <- st_read("BPNS_polygon.shp")
ggplot() + geom_sf(data = BPNS)
BPNS <- st_transform(BPNS, crs = 4326)

BASEALL_sf <- st_as_sf(BASEALL, coords = c("MidLongitude","MidLatitude"), crs = 4326)
BASEALL_BPNS <-  BASEALL_sf[BPNS,]

BASEALL_BPNS <- cbind(st_drop_geometry(BASEALL_BPNS), st_coordinates(BASEALL_BPNS))
rm(BASEALL_sf)

BIRDALL_BPNS <- BIRDALL %>% 
  filter(Poskey %in% BASEALL_BPNS$Poskey) %>%
  mutate(Species = as.integer(Species))
  
#DISTANCE files aanmaken
#merge BIRDALL & BASEALL
DISTANCE <- left_join(BIRDALL_BPNS, 
                     BASEALL_BPNS %>% select(Poskey, Tripkey, Date, Time, TransectWidth, WaveHeight, Beaufort))
DISTANCE <- left_join(DISTANCE,
                     TRIPALL %>% select(Tripkey, Ship, Observers))

DISTANCE <- DISTANCE %>% filter(Ship %in% c("SST","SBE","SZA","SZH","SZL","SBE2"))

#DISTANCE_swim aanmaken
DISTANCE_swim <- DISTANCE %>% filter(
  TransectWidth == 300 & 
  Distance %in% c("A","B","C","D","U") &
  Transect == 1 &
  !(Behaviour %in% c("X")))

#DISTANCE_fly aanmaken
DISTANCE_fly <- DISTANCE %>% filter(
  TransectWidth == 300 & 
  Distance %in% c("1","2") &
  Transect == 1 &
  !(Behaviour %in% c("X")))

DISTANCE_fly <- DISTANCE_fly %>% 
  select(Poskey, Species, Count, WaveHeight, Beaufort) %>%
  rename(Sample.Label = Poskey,
         Size = Count) %>%
  mutate(detection = 1)

#Opsplitsing groepen vs. niet-groepen
DISTANCE_swim <- DISTANCE_swim %>% mutate(CRIT = is.na(Group))

DISTANCE_swim_nongroup <- DISTANCE_swim %>% 
  filter(CRIT == T) %>%
  select(Poskey, Species, Distance, Count)

DISTANCE_swim_group <- DISTANCE_swim %>% 
  filter(CRIT == F) %>%
  group_by(Poskey, Distance, Group, Species) %>%
  summarise(Count = sum(Count)) %>%
  arrange(Poskey, Group) %>% 
  ungroup() %>%
  select(Poskey, Species, Distance, Count)

DISTANCE_swim <- rbind(DISTANCE_swim_group, DISTANCE_swim_nongroup)
rm(DISTANCE_swim_group, DISTANCE_swim_nongroup)

DISTANCE_swim <- DISTANCE_swim %>% arrange(Species, Poskey)
head(DISTANCE_swim)

DISTANCE_swim <- DISTANCE_swim %>%
  mutate(Distance = recode(Distance, "A" = 0.025, "B" = 0.075, "C" = 0.150, "D" = 0.250, "U" = 0.150))

#Hier moeten we nu een geschikte 'flatfile' van maken
BASEALL <- BASEALL[,-1] %>% rename(Effort = Distance)
DISTANCE_swim <- left_join(DISTANCE_swim, 
                           BASEALL %>% select(Poskey, Effort, WaveHeight, Beaufort))

DISTANCE_swim <- DISTANCE_swim %>% 
  rename(Sample.Label = Poskey,
         Size = Count,
         distance = Distance) %>%
  mutate(Region.Label = "BPNS",
         Area = 1, 
         observer = 1,
         detected = 1,
         object = 1:nrow(DISTANCE_swim)) 

summary(DISTANCE_swim)

#final step
DISTANCE_swim <- create_bins(data = DISTANCE_swim, cutpoints = c(0,0.05,0.1,0.2,0.3))
head(DISTANCE_swim)

#IHD MODELLEN
#species formulas
formula_59 <- "~log(Size) + Beaufort"
data_59 <- DISTANCE_swim %>% filter(Species == 59 & !is.na(Beaufort))

formula_90   <- "~log(Size) + Beaufort"
data_90 <- DISTANCE_swim %>% filter(Species == 90 & !is.na(Beaufort))

formula_710   <- "~log(Size) + WaveHeight"
data_710 <- DISTANCE_swim %>% filter(Species == 710 & !is.na(WaveHeight))

formula_5780   <- "~log(Size) + Beaufort"
data_5780 <- DISTANCE_swim %>% filter(Species == 5780 & !is.na(Beaufort))

formula_5900   <- "~log(Size) + Beaufort"
data_5900 <- DISTANCE_swim %>% filter(Species == 5900 & !is.na(Beaufort))

formula_5910   <- "~log(Size) + Beaufort"
data_5910 <- DISTANCE_swim %>% filter(Species == 5910 & !is.na(Beaufort))

formula_5920    <- "~log(Size) + Beaufort"
data_5920 <- DISTANCE_swim %>% filter(Species == 5920 & !is.na(Beaufort))

formula_6000    <- "~log(Size) + Beaufort"
data_6000 <- DISTANCE_swim %>% filter(Species == 6000 & !is.na(Beaufort))

formula_6020   <- "~log(Size) + WaveHeight"
data_6020 <- DISTANCE_swim %>% filter(Species == 6020 & !is.na(WaveHeight))

formula_6110    <- "~log(Size)"
data_6110 <- DISTANCE_swim %>% filter(Species == 6110)

formula_6150    <- "~log(Size)"
data_6150 <- DISTANCE_swim %>% filter(Species == 6150)

formula_6340    <- "~Size + Beaufort"
data_6340 <- DISTANCE_swim %>% filter(Species == 6340 & !is.na(Beaufort))

formula_6360   <- "~log(Size) + Beaufort"
data_6360 <- DISTANCE_swim %>% filter(Species == 6360 & !is.na(Beaufort))

MetaData <- list(point = FALSE, binned = TRUE, width = 0.3, breaks = c(0,0.05,0.1,0.2,0.3))

soorten <- c(59, soorten)

#loop met distance modellen:
distance_model_list <- vector('list', 13)
ptm <- proc.time()
for (i in c(1:13))
{
  soort <- soorten[i]
  distance_model_list[[i]] <- 
    ddf(method = "ds", data = get(paste("data_", soort, sep = "")), 
    dsmodel = ~mcds(key = "hr", formula = get(paste("formula_", soort, sep = ""))),
    meta.data = MetaData)
}
proc.time() - ptm
#deze loop neemt een goeie 10 minuten in beslag...

#gemiddelde detectie-probabiliteit:
probabilities <- as.data.frame(matrix(nrow = 13, ncol = 3))
for (i in c(1:13))
{
  probabilities[i,2]  <- summary(distance_model_list[[i]])$average.p
}

#detectie-probabiliteit voor solitaire vogel bij 4 Bft
newdata <- data.frame("Size" = 1, "Beaufort" = 4, "WaveHeight" = 1)
for (i in c(1:13))
{
  probabilities[i,3]  <- predict(distance_model_list[[i]], newdata = newdata)
}

colnames(probabilities) <- c("Species","Detection_P_AVG", "Detection_P_single_bird")

probabilities <- probabilities %>% mutate(
  Species = soorten,
  Detection_P_AVG = signif(as.numeric(paste(Detection_P_AVG)), digits = 2),
  Detection_P_single_bird = signif(as.numeric(paste(Detection_P_single_bird)), digits = 2))
probabilities

#export:
setwd("C:/Users/nicolas_vanermen/Desktop/SAS DB 2024/update feb 2024/R exports")
write.csv(probabilities, "probabilities_revised_script.csv")

#test:
test <- read.csv("probabilities_original_script.csv")
test
probabilities
summary(comparedf(probabilities, test[,c(2:4)]))
#check!!

#predicties per record:
DISTANCE_swim$detection <- NA
str(DISTANCE_swim)

for (i in c(1:13))
{
  soort <- soorten[i]
  
  soort_data <- get(paste("data_", soort, sep = ""))
  
  DISTANCE_swim[DISTANCE_swim$Species == soort & 
                DISTANCE_swim$Sample.Label %in% soort_data$Sample.Label,]$detection <- 
    unlist(predict(distance_model_list[[i]], newdata = soort_data))
}

summary(DISTANCE_swim)
#1224 NA waarden

#NA waarden oplossen:
DISTANCE_swim <- DISTANCE_swim[,c("Sample.Label","Species","WaveHeight","Beaufort","Size","detection")]
nrow(DISTANCE_swim[is.na(DISTANCE_swim$detection),])
table(DISTANCE_swim[is.na(DISTANCE_swim$detection),]$Species)

for (i in c(1:9,12:13))
{
  soort <- soorten[i]
  
  DISTANCE_swim[is.na(DISTANCE_swim$detection) & 
                DISTANCE_swim$Species == soort,]$detection <- 
    summary(distance_model_list[[i]])$average.p
}
table(DISTANCE_swim[is.na(DISTANCE_swim$detection),]$Species)
#check

#Alles terug samen zetten:
TOTALL_corr <- rbind(DISTANCE_swim,DISTANCE_fly[,colnames(DISTANCE_swim)])
length(unique(TOTALL_corr$Sample.Label))

#Gecorrigeerde aantallen berekenen:
TOTALL_corr <- TOTALL_corr %>%
  mutate(number_corr = Size / detection)

TOTALL_corr <- TOTALL_corr %>%
  group_by(Sample.Label, Species) %>%
  summarise(number_corr = sum(number_corr)) %>%
  spread(Species, number_corr, fill = 0)

#Nu ook tellingen zonder waarnemingen in transect meenemen:
Zero_samples <- left_join(BASEALL_BPNS[,c(2:29)],TRIPALL[,c(2:6)])
Zero_samples <- Zero_samples %>%
  filter(Ship %in% c("SST","SBE","SZA","SZH","SZL","SBE2"),
         TransectWidth == 300) %>%
  select(Poskey)

TOTALL_corr_merge <- left_join(Zero_samples, TOTALL_corr, join_by("Poskey" == "Sample.Label"))
summary(TOTALL_corr_merge)
#telkens 33992 missing values

#NA's omzetten naar 0
TOTALL_corr_merge[is.na(TOTALL_corr_merge)] <- 0  
summary(TOTALL_corr_merge)

#merge met TOTALL
TOTALL_corr_final <- right_join(BASEALL[,c("Poskey","Date","Time","Area","MidLatitude","MidLongitude",
                            "SpeciesCounted","CountMethod")], TOTALL_corr_merge) %>%
  mutate(Year = year(date(Date)),
         Month = month(date(Date)),
         Day = day(date(Date)))

TOTALL_corr_final[,c(9:21)] <- round(TOTALL_corr_final[,c(9:21)], digits=4)
head(TOTALL_corr_final)

setwd("C:/Users/nicolas_vanermen/Desktop/SAS DB 2024/update feb 2024/R exports")
write.csv(TOTALL_corr_final, "TOTALL_Corr_update_2024_02_IHD.csv")

#vergelijken met vorige output
test1 <- read.csv("C:/Users/nicolas_vanermen/Desktop/SAS DB 2023/update feb 2023/TOTALL_Corr_2023_IHD_herziening.csv")
test2 <- TOTALL_corr_final[TOTALL_corr_final$Poskey <= max(test1$Poskey),]
#verschil in aantal records?

test2_poskeys <- test2[!(test2$Poskey %in% test1$Poskey),]$Poskey
#8 rijen
test1_poskeys <- test1[!(test1$Poskey %in% test2$Poskey),]$Poskey
#5 rijen

BASEALL_sf <- st_as_sf(BASEALL, coords = c("MidLongitude","MidLatitude"), crs = 4326)
ggplot() + geom_sf(data = BPNS) + geom_sf(data = BASEALL_sf[BASEALL_sf$Poskey %in% test2_poskeys,])
ggplot() + geom_sf(data = BPNS) + geom_sf(data = BASEALL_sf[BASEALL_sf$Poskey %in% test1_poskeys,])
#randgevallen!!! subtiel verschil in overlay door overschakeling naar sf package?
#de referentie-aantallen zullen als gevolg hiervan een beetje verschillen

summary(test1$X5910)
summary(test2$`5910`)

summary(test1$X6340)
summary(test2$`6340`)
