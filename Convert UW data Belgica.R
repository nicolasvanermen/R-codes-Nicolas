#test renaming in Github
library(tidyverse) 
library(lubridate)

setwd("C:/Users/nicolas_vanermen/Desktop/DATA INVOER/2023/UW DATA/Belgica 2023 21 25")
Belgica_2023 <- read.csv("vanermen_2023-21_2023-25.csv")

str(Belgica_2023)
Belgica_2023[,c(2:30)] <- sapply(Belgica_2023[,c(2:30)], as.numeric)
str(Belgica_2023)

#Select UW columns
as.data.frame(names(Belgica_2023))
#Ter info: voorheen gebruikten we True.heading..deg. ipv Seapath.true.heading..deg.
Belgica_2023$AirHumidity <- NA
Belgica_2023 <- Belgica_2023[,c("PHENOMENON_TIME_START..UTC0.","latitude", "longitude",
                                            "Seapath.true.heading..deg.", "Seapath.groundspeed..kn.", 
                                            "Air.temperature..degC.","Air.pressure..hPa.","AirHumidity",
                                            "True.Wind.direction..deg.", "True.Wind.speed..m.s.",
                                            "Depth.from.surface..m.","Salinity..PSU.",
                                            "Temperature.SBE38..degC.","CHL.Eco.triplet..ug.l.")]

#Rename columns
colnames(Belgica_2023) <- c("DateTime","Latitude","Longitude",
                                      "Heading","SOG",
                                      "AirTemperature","AirPressure","AirHumidity",
                                      "WindDirection","WindSpeed",
                                      "WaterDepth","WaterSalinity",
                                      "WaterTemperature","ChlA")

#checks
summary(Belgica_2023)

for(i in colnames(Belgica_2023[,c(4:7,9:14)]))
{
  print(ggplot(Belgica_2023, aes(get(i))) + geom_histogram() + labs(x=paste(i)))
}

#clean
Belgica_2023$WaterDepth <- -Belgica_2023$WaterDepth
Belgica_2023$WindSpeed <- ifelse(Belgica_2023$WindSpeed==0, NA, Belgica_2023$WindSpeed)
ggplot(Belgica_2023, aes(WaterDepth)) + geom_histogram() + labs(x="WaterDepth")
ggplot(Belgica_2023, aes(WindSpeed)) + geom_histogram() + labs(x="WindSpeed")
                           
# ggplot(Belgica_2023, aes(Latitude, Longitude)) + geom_point()                                 

write.csv(Belgica_2023,"Belgica_2023_21_25.csv")   


       
