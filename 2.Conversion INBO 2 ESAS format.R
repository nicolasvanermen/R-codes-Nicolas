library(tidyverse)
library(lubridate)
library(readxl)

setwd("C:/Users/nicolas_vanermen/Desktop/DATA INVOER/2023/UPDATES")
TRP <- read.csv("TRIPALL_MERGE_1992_2023_c.csv", fileEncoding = "UTF-8")
POS <- read.csv("BASEALL_MERGE_1992_2023_c.csv", fileEncoding = "UTF-8")
OBS <- read.csv("BIRDALL_MERGE_1992_2023_c.csv", fileEncoding = "UTF-8")

TRP <- TRP[ymd(TRP$Date) > "2022-01-01" & TRP$Ship == "SBE2",] 
POS <- POS[POS$Tripkey %in% TRP$Tripkey,]
OBS <- OBS[OBS$Poskey %in% POS$Poskey,]

#CAMPAIGNS
#Conversie
CAMPAIGNS <- TRP
CAMPAIGNS <- CAMPAIGNS %>% mutate("ICES_CampaignID" = Tripkey)
CAMPAIGNS <- CAMPAIGNS %>% mutate("ICES_DataAccess" = "Public")
CAMPAIGNS <- CAMPAIGNS %>% mutate("ICES_StartDate" = Date)
CAMPAIGNS <- CAMPAIGNS %>% mutate("ICES_EndDate" = Date)
CAMPAIGNS <- CAMPAIGNS %>% mutate("ICES_Notes" = NA)
#select column and trim column names
CAMPAIGNS <- CAMPAIGNS %>% select(starts_with("ICES_"))
colnames(CAMPAIGNS) <- str_remove(colnames(CAMPAIGNS), "ICES_")


#SAMPLES
#Conversie:
SAMPLES <- TRP
SAMPLES <- SAMPLES %>% mutate("ICES_CampaignID" = Tripkey)
SAMPLES <- SAMPLES %>% mutate("ICES_SampleID" = Tripkey)
SAMPLES <- SAMPLES %>% mutate("ICES_Date" = Date)
SAMPLES <- SAMPLES %>% mutate("ICES_PlatformCode" = "11BU")
SAMPLES <- SAMPLES %>% mutate("ICES_PlatformClass" = 30)
SAMPLES <- SAMPLES %>% mutate("ICES_PlatformSide" = NA)
SAMPLES <- SAMPLES %>% mutate("ICES_PlatformHeight" = NA)
SAMPLES <- SAMPLES %>% mutate("ICES_TransectWidth" = 300)
SAMPLES <- SAMPLES %>% mutate("ICES_SamplingMethod" = 1)
SAMPLES <- SAMPLES %>% mutate("ICES_PrimarySampling" = TRUE)
SAMPLES <- SAMPLES %>% mutate("ICES_TargetTaxa" = 1)
SAMPLES <- SAMPLES %>% mutate("ICES_DistanceBins" = "0|50|100|200|300")
SAMPLES <- SAMPLES %>% mutate("ICES_UseOfBinoculars" = 3)
SAMPLES <- SAMPLES %>% mutate("ICES_NumberOfObservers" = NA)
SAMPLES <- SAMPLES %>% mutate("ICES_Notes" = NA)
#select column and trim column names
SAMPLES <- SAMPLES %>% select(starts_with("ICES_"))
colnames(SAMPLES) <- str_remove(colnames(SAMPLES), "ICES_")


#POSITIONS
#Round coordinates
POS$MidLatitude_rev <- sprintf("%.4f", round(POS$MidLatitude, 4))
POS$MidLongitude_rev <- sprintf("%.4f", round(POS$MidLongitude, 4))
POS$Distance_rev <- sprintf("%.4f", round(POS$Distance, 4))
POS$Area_rev <- sprintf("%.4f", round(POS$Area, 4))

#Conversie:
POSITIONS <- POS
POSITIONS <- POSITIONS %>% mutate("ICES_SampleID" = Tripkey)
POSITIONS <- POSITIONS %>% mutate("ICES_PositionID" = Poskey)
POSITIONS <- POSITIONS %>% mutate("ICES_Time" = Time)
POSITIONS <- POSITIONS %>% mutate("ICES_Latitude" = MidLatitude_rev)
POSITIONS <- POSITIONS %>% mutate("ICES_Longitude" = MidLongitude_rev)
POSITIONS <- POSITIONS %>% mutate("ICES_Distance" = Distance_rev)
POSITIONS <- POSITIONS %>% mutate("ICES_Area" = Area_rev)
POSITIONS <- POSITIONS %>% mutate("ICES_WindForce" = Beaufort)
POSITIONS <- POSITIONS %>% mutate("ICES_Visibility" = Visibility)
POSITIONS <- POSITIONS %>% mutate("ICES_Glare" = Glare)
POSITIONS <- POSITIONS %>% mutate("ICES_SunAngle" = NA)
POSITIONS <- POSITIONS %>% mutate("ICES_CloudCover" = NA)
POSITIONS <- POSITIONS %>% mutate("ICES_Precipitation" = Precipitation)
POSITIONS <- POSITIONS %>% mutate("ICES_IceCover" = 0)
POSITIONS <- POSITIONS %>% mutate("ICES_ObservationConditions" = NA)
#select column and trim column names
POSITIONS <- POSITIONS %>% select(starts_with("ICES_"))
colnames(POSITIONS) <- str_remove(colnames(POSITIONS), "ICES_")

#OBSERVATIONS
#distance U omzetten in "W"
table(OBS$Distance, OBS$Transect)
OBS <- OBS %>% mutate(
  Distance = ifelse(Distance %in% c("U"), "W", Distance),
  Distance = ifelse(Distance %in% c("1","2","3"), "F", Distance),
  Distance = ifelse(Distance%in%c("E") & Transect == 1, "W", Distance))
table(OBS$Distance)
levels(as.factor(OBS$Distance))

#Map columns with new vocabulary
setwd("C:/Users/nicolas_vanermen/Desktop/DATA INVOER/CODING")
ASS <-  as.data.frame(read_excel("BE_Association.xlsx"))
BEH <-  as.data.frame(read_excel("BE_Behaviour.xlsx"))

#Association
OBS$Association_recoded <- plyr::mapvalues(OBS$Association, 
                             from = ASS$Association_INBO, to = ASS$Association_ESAS)
table(OBS$Association)
table(OBS$Association_recoded)
sum(table(OBS$Association))==sum(table(OBS$Association_recoded))

#Behaviour
OBS$Behaviour_recoded <- plyr::mapvalues(OBS$Behaviour, 
                          from = BEH$Behaviour_INBO, to = BEH$Behaviour_ESAS)
table(OBS$Behaviour)
table(OBS$Behaviour_recoded)
sum(table(OBS$Behaviour))==sum(table(OBS$Behaviour_recoded))

#Transect
OBS$Transect <- as.logical(OBS$Transect)
table(OBS$Transect)

#Species nog beetje pimpen:
table(OBS$Species)
OBS <- OBS[OBS$Species!=9999,]
table(OBS$Species)

#Conversie:
OBSERVATIONS  <- OBS
OBSERVATIONS <- OBSERVATIONS %>% mutate("ICES_PositionID" = Poskey)
OBSERVATIONS <- OBSERVATIONS %>% mutate("ICES_ObservationID" = Obskey)
OBSERVATIONS <- OBSERVATIONS %>% mutate("ICES_GroupID" = Group)
OBSERVATIONS <- OBSERVATIONS %>% mutate("ICES_Transect" = Transect)
OBSERVATIONS <- OBSERVATIONS %>% mutate("ICES_SpeciesCodeType" = "ESAS")
OBSERVATIONS <- OBSERVATIONS %>% mutate("ICES_SpeciesCode" = Species)
OBSERVATIONS <- OBSERVATIONS %>% mutate("ICES_Count" = Count)
OBSERVATIONS <- OBSERVATIONS %>% mutate("ICES_ObservationDistance" = Distance)
OBSERVATIONS <- OBSERVATIONS %>% mutate("ICES_LifeStage" = Age)
OBSERVATIONS <- OBSERVATIONS %>% mutate("ICES_Moult" = NA)
OBSERVATIONS <- OBSERVATIONS %>% mutate("ICES_Plumage" = Plumage)
OBSERVATIONS <- OBSERVATIONS %>% mutate("ICES_Sex" = Sex)
OBSERVATIONS <- OBSERVATIONS %>% mutate("ICES_TravelDirection" = Direction)
OBSERVATIONS <- OBSERVATIONS %>% mutate("ICES_Prey" = Prey)
OBSERVATIONS <- OBSERVATIONS %>% mutate("ICES_Association" = Association_recoded)
OBSERVATIONS <- OBSERVATIONS %>% mutate("ICES_Behaviour" = Behaviour_recoded)
OBSERVATIONS <- OBSERVATIONS %>% mutate("ICES_Notes" = Notes)
#select column and trim column names
OBSERVATIONS <- OBSERVATIONS %>% select(starts_with("ICES_"))
colnames(OBSERVATIONS) <- str_remove(colnames(OBSERVATIONS), "ICES_")

#WEGSCHRIJVEN
setwd("C:/Users/nicolas_vanermen/Desktop/DATA INVOER/2023/UPDATES")
write.csv(CAMPAIGNS,"CAMPAIGNS_BE_ICES_update_feb_2024.csv", row.names = F, fileEncoding = "UTF-8")
write.csv(SAMPLES,"SAMPLES_BE_ICES_update_feb_2024.csv", row.names = F, fileEncoding = "UTF-8")
write.csv(POSITIONS,"POSITIONS_BE_ICES_update_feb_2024.csv", row.names = F, fileEncoding = "UTF-8")
write.csv(OBSERVATIONS,"OBSERVATIONS_BE_ICES_update_feb_2024.csv", row.names = F, fileEncoding = "UTF-8")
