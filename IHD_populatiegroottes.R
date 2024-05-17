library(tidyverse)
library(sf)
library(ggplot2)
library(data.table)
library(zoo)

setwd("C:/Users/nicolas_vanermen/Desktop/SAS DB 2024/update feb 2024/R exports")
TOTALL <- read.csv("TOTALL_Corr_update_2024_02_IHD.csv")

#test verschil met vorige update
TOTALL_old <- read.csv("TOTALL_Corr_2023_IHD_herziening.csv")
cbind(TOTALL$X59[1:100],TOTALL_old$X59_c[1:100])
#hele kleine verschillen agv update in distancemodellen! -> geeft igv duiker sp. ook klein verschil in ref populatie

setwd("C:/Users/nicolas_vanermen/Desktop/SAS DB 2024/shapes")
BPNS <- read_sf("BPNS_polygon_zones.shp")
ggplot() + geom_sf(data = BPNS)

#coordinatensystemen goed zetten
BPNS  <- st_transform(BPNS, crs = 4326)
TOTALL <- st_as_sf(TOTALL, coords = c("MidLongitude","MidLatitude"))
st_crs(TOTALL) <- st_crs(BPNS)

#spatial join
TOTALL_BPNS <-  st_join(TOTALL, BPNS)

TOTALL <- st_drop_geometry(TOTALL)
TOTALL_BPNS <- st_drop_geometry(TOTALL_BPNS)

table(TOTALL_BPNS$TAG)

OppDeelzones <- aggregate(AREA ~ TAG, FUN = mean, data = TOTALL_BPNS)
OppDeelzones <- OppDeelzones %>% rename(Zone_area = AREA, 
                                        Zone = TAG)

#seizoenen definieren:
TOTALL_BPNS <- TOTALL_BPNS %>% 
  mutate(
  Season = ifelse(Month %in% c(12,1,2),"WINTER", "SPRING"),
  Season = ifelse(Month %in% c(6,7,8),"SUMMER", Season),
  Season = ifelse(Month %in% c(9,10,11),"AUTUMN", Season),
  Year2 = ifelse(Month == 12, Year + 1, Year)) %>%
  filter(Year2 > 1992 & Year2 < 2024)

table(TOTALL_BPNS$Season,TOTALL_BPNS$Year2)

#berekening totalen uitgemiddeld per seizoen, zone & jaar:
soortselectie <- c("X59","X90","X710","X5780","X5900","X5910","X5920","X6000","X6020",
                   "X6340","X6360","Area")
soortselectie2 <- soortselectie[!soortselectie %in% c("Area")]

TOTALL_agg_year <- TOTALL_BPNS %>% group_by(TAG, Year2, Season) %>%
  summarise_at(soortselectie, sum)

sum(TOTALL_agg_year$Area < 10)
# verwijderen van 20 combinaties tag-jaar-seizoen met minder dan 10 km²
TOTALL_agg_year <- TOTALL_agg_year[TOTALL_agg_year$Area > 10,]

# berekening dichtheden
DENSALL_agg_year <- cbind(TOTALL_agg_year[, c(1:3)], TOTALL_agg_year[, soortselectie2] / TOTALL_agg_year$Area)
head(DENSALL_agg_year[,1:10])

DENSALL_agg_year <- merge(DENSALL_agg_year %>% rename(Zone = TAG), OppDeelzones)

#Totalen per zone
TOTALEN_YEAR_temp <- cbind(DENSALL_agg_year[, c(1:3)], 
                           DENSALL_agg_year[, soortselectie2] * DENSALL_agg_year$Zone_area)
nrow(TOTALEN_YEAR_temp)
#4 seizoenen, 3 zones, 31 jaar
4*3*31
372-323
#er ontbreken 49 combinaties
BASE <- expand.grid(Year2 = c(1993:2023),
                    Zone = c("Zone 1","Zone 2","Zone 3"),
                    Season = c("WINTER","SPRING","SUMMER","AUTUMN"))
TOTALEN_YEAR <- left_join(BASE, TOTALEN_YEAR_temp) 
TOTALEN_YEAR[1:12,1:12]
summary(TOTALEN_YEAR)
#telkens 49 NA waarden

#deze totalen aggregeren per gebiedsselectie
TOTALEN_YEAR_agg_1 <- TOTALEN_YEAR %>% filter(Zone == "Zone 1") %>% 
  group_by(Year2, Season) %>%
  summarise_at(soortselectie2, sum) %>% ungroup()

TOTALEN_YEAR_agg_2 <- TOTALEN_YEAR %>% filter(Zone == "Zone 2") %>% 
  group_by(Year2, Season) %>%
  summarise_at(soortselectie2, sum) %>% ungroup()

TOTALEN_YEAR_agg_3 <- TOTALEN_YEAR %>% filter(Zone == "Zone 3") %>% 
  group_by(Year2, Season) %>%
  summarise_at(soortselectie2, sum) %>% ungroup()

TOTALEN_YEAR_agg_12 <- cbind(TOTALEN_YEAR_agg_1 %>% select(1:2), 
                             TOTALEN_YEAR_agg_1 %>% select(3:13) + TOTALEN_YEAR_agg_2 %>% select(3:13))

TOTALEN_YEAR_agg_23 <- cbind(TOTALEN_YEAR_agg_1 %>% select(1:2), 
                             TOTALEN_YEAR_agg_2 %>% select(3:13) + TOTALEN_YEAR_agg_3 %>% select(3:13))

TOTALEN_YEAR_agg_123 <- cbind(TOTALEN_YEAR_agg_1 %>% select(1:2), 
                              TOTALEN_YEAR_agg_1 %>% select(3:13) + TOTALEN_YEAR_agg_2 %>% select(3:13) + 
                                TOTALEN_YEAR_agg_3 %>% select(3:13))


#POPULATIESCHATTING OBV MSFD selectie zone / seizoen
MSFD_zone_seizoen <- matrix(nrow = 11, ncol = 3, dimnames = list(c(1:11), c("Species", "Season", "Zone")))
MSFD_zone_seizoen <- as.data.frame(MSFD_zone_seizoen)

MSFD_zone_seizoen <- MSFD_zone_seizoen %>% 
  mutate(Species = soortselectie2,
         
         Season = ifelse(Species %in% c("X59","X90","X5900","X6000","X6020","X6340","X6360"), "WINTER", NA),
         Season = ifelse(Species %in% c("X5780","X5910","X5920"), "SPRING", Season),
         Season = ifelse(Species %in% c("X710"), "AUTUMN", Season),
         
         Zone = ifelse(Species %in% c("X59","X5780","X5910","X5920"), "12", NA),
         Zone = ifelse(Species %in% c("X90"), "1", Zone),
         Zone = ifelse(Species %in% c("X710","X6020","X6340","X6360"), "23", Zone),
         Zone = ifelse(Species %in% c("X5900","X6000"), "123", Zone))
         
MSFD_zone_seizoen

IHD_year <- as.data.frame(matrix(nrow = 11, ncol = 31))
colnames(IHD_year) <- c(1993:2023)
rownames(IHD_year) <- c("DUIKER","FUU","JVG","DWM","STM","KLM","ZM","GM","DTM","ZK","ALK")
IHD_year

for(i in c(1:11))
{
  Species_loop <- MSFD_zone_seizoen[i,]$Species
  Season_loop <- MSFD_zone_seizoen[i,]$Season
  DF <- get(paste("TOTALEN_YEAR_agg", MSFD_zone_seizoen[i,]$Zone, sep="_"))
  
  IHD_year[i,] <- t(round(DF %>% filter(Season == Season_loop) %>% select(Species_loop)))
}

IHD_year

IHD_year <- IHD_year %>% 
  add_column(Selection = c("W12","W1","A23","SP12","W123","SP12","SP12",
                                      "W123","W23","W23","W23"), .before="1993") %>%
  mutate(REF_population = rowMeans(IHD_year[,as.character(c(1993:2009))], na.rm = T))

IHD_year

#moving average berekenen:
IHD_year_mov_avg <- IHD_year[,as.character(c(1993:2023))]

for(i in c(1:11))
{
  IHD_year_mov_avg[i,] <- round(frollmean(t(IHD_year[i, as.character(c(1993:2023))]), 6, na.rm = T))
}
IHD_year_mov_avg

#grenswaarde bepalen voor min aantal waarden in de moving average
IHD_year_NA <- as.data.frame(ifelse(is.na(IHD_year), 0, 1))
IHD_year_mov_sum_NA <- IHD_year_NA[, as.character(c(1993:2023))]
IHD_year_mov_sum_NA

for(i in c(1:11))
{
  IHD_year_mov_sum_NA[i,6:31] <- rollapply(t(IHD_year_NA[i, as.character(c(1993:2023))]), 6, sum, na.rm = T)
}
IHD_year_NA[,as.character(c(1993:2009))]
IHD_year_mov_sum_NA[as.character(c(1993:2009))]

#tabel met moving averages omzetten, met voorwaarde tot min 3 positieve waarden
IHD_year_mov_sum_NA[,as.character(c(1998:2023))]<3
sum(IHD_year_mov_sum_NA[,as.character(c(1998:2023))]<3)
#slechts 2 moving averages zijn bepaald obv <3 waarden (2020 & 2021 voor JVG)

IHD_year_mov_avg
for(i in c(1:11))
{
  IHD_year_mov_avg[i, as.character(c(1998:2023))] <- ifelse(IHD_year_mov_sum_NA[i,as.character(c(1998:2023))] > 2,
                                                                IHD_year_mov_avg[i,as.character(c(1998:2023))], NA)
}
IHD_year_mov_avg


#range natuurlijke variatie bepalen adhv min & max moving average
IHD_year_mov_avg[,as.character(c(1993:2009))]
IHD_year$Min_mov_avg  <- apply(IHD_year_mov_avg[,as.character(c(1993:2009))], 1, min, na.rm=T)
IHD_year$Max_mov_avg  <- apply(IHD_year_mov_avg[,as.character(c(1993:2009))], 1, max, na.rm=T)
IHD_year

#samenvattende kolommen juist afronden
for (i in c("REF_population","Min_mov_avg","Max_mov_avg"))
{
  IHD_year[,i] <- ifelse(IHD_year[,i] < 1000, 10 * round(IHD_year[,i] / 10), 100 * round(IHD_year[,i] / 100))
  IHD_year[,i] <- ifelse(IHD_year[,i] > 10000, 1000 * round(IHD_year[,i] / 1000), IHD_year[,i])
}
IHD_year


#alles samenzetten
IHD_year_temp <- as.data.frame(matrix(nrow=11, ncol=35))
colnames(IHD_year_temp) <- c("Selection",c(1993:2023),"REF_population","Min_mov_avg","Max_mov_avg")
rownames(IHD_year_temp) <- c("DUIKER_mov_avg","FUU_mov_avg","JVG_mov_avg","DWM_mov_avg","STM_mov_avg",
                             "KLM_mov_avg","ZM_mov_avg","GM_mov_avg","DTM_mov_avg","ZK_mov_avg","ALK_mov_avg")

IHD_year_temp[1:11,as.character(c(1993:2023))] <- IHD_year_mov_avg
IHD_year_temp

IHD_ALL <- rbind(IHD_year, IHD_year_temp)
IHD_ALL

setwd("C:/Users/nicolas_vanermen/Desktop/SAS DB 2024/queries/exports")
write.csv(IHD_ALL,"IHD_actualisatie_populatiegroottes_versie2024.csv")
