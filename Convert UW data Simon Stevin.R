library(tidyverse)
library(lubridate)
library(ggplot2)

setwd("C:/Users/nicolas_vanermen/Desktop/DATA INVOER/2023/UW DATA/Simon Stevin/")
pathway <- "C:/Users/nicolas_vanermen/Desktop/DATA INVOER/2023/UW DATA/Simon Stevin/"
filenames_SST <- list.files(pathway, pattern = "LW_Underway_data", full.names = TRUE)

for (i in 1:length(filenames_SST)) {
  split <- str_remove(filenames_SST[i], pattern = pathway)
  name <- str_remove(split, pattern = ".tab")
  file <- read.table(filenames_SST[i], sep="\t", header=T)
  assign(x = name, value = file)
  }
ls(pattern = "LW_Underway_data")
Navigatie <- do.call("rbind",mget(ls(pattern = "LW_Underway_data")))

rm(list = ls(pattern = "LW_Underway_data"))
rm(file)

#kolommen selecteren:
names(Navigatie)
Navigatie <- Navigatie %>%
  select(!c(Conductivity,Speedlog,Gyro,Winddirection,Windspeed))
names(Navigatie)

#kolomnamen aanpassen
names(Navigatie) <- c("DateTime","Latitude","Longitude","WaterTemperature","WaterSalinity","WaterDepth","ChlA","SOG",
                      "WindDirection","WindSpeed","Heading","AirTemperature","AirPressure",
                      "AirHumidity")

#volgorde kolomnamen goed zetten:
Navigatie <- Navigatie[,c("DateTime","Latitude","Longitude","Heading","SOG","AirTemperature","AirPressure","AirHumidity",
                          "WindDirection","WindSpeed","WaterDepth","WaterSalinity","WaterTemperature","ChlA")]  

#uitzondering voor 27/01:
library(readxl)
Navigatie27jan <- read_excel("Nav_230127.xlsx", sheet = "Nav_230127_revised")
Meteo27jan <- read_excel("Nav_230127.xlsx", sheet = "meteo_revised")
Navigatie27jan <- left_join(Navigatie27jan,Meteo27jan)
Navigatie27jan$DateTime <- paste(Navigatie27jan$Date, format(Navigatie27jan$Time, format = "%H:%M:%S"), sep = " ")

Navigatie27jan <- Navigatie27jan %>%
  select(c(DateTime,Latitude,Longitude,CourseMadeGood,SpeedMadeGood,AWSTemperature,AWSAirPressure,AWSRelHumidity,
           AWSTrueWindDirection,AWSTrueWindSpeed,OdomDepth200khz)) %>%
  mutate(WaterSalinity = NA,
         WaterTemperature = NA,
         ChlA = NA)
names(Navigatie27jan)
names(Navigatie27jan) <- c("DateTime","Latitude","Longitude","Heading","SOG","AirTemperature","AirPressure","AirHumidity",
                           "WindDirection","WindSpeed","WaterDepth","WaterSalinity","WaterTemperature","ChlA")

sum(names(Navigatie)==names(Navigatie27jan))

#rbind??
Navigatie_all <- rbind(Navigatie, Navigatie27jan)
                      
#UW data cleaning:
summary(Navigatie_all)
hist(Navigatie_all$ChlA)

Navigatie_all <- Navigatie_all %>% mutate(
  Heading = ifelse(Heading == -999, NA, Heading),
  AirPressure = 1000*AirPressure,
  WaterDepth = ifelse(WaterDepth < -500, NA, -WaterDepth),
  ChlA = ifelse(ChlA == -999, NA, ChlA))

summary(Navigatie_all)
ggplot(Navigatie_all, aes(x=Longitude, y=Latitude)) + geom_point()

getwd()
write.csv(Navigatie_all, "SST_2023.csv")

