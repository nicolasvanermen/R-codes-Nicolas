setwd("C:/Users/nicolas_vanermen/Desktop/Hilbran")

# install.packages(c("checklist", "INBOmd"))

library(tidyverse)
library(leaflet)
library(sf)
library(viridis)
library(htmlwidgets)
library(webshot)
library(adehabitatHR)
library(data.table)
library(MESS)
library(suncalc)
library(lubridate)

W <- read.csv2("Wulp2024.csv", sep = ";")
head(W)

summary(ymd_hms(W$UTC_datetime))

W <- W %>%
  mutate(UTC_datetime = ymd_hms(UTC_datetime)) %>%
  filter(!is.na(UTC_datetime))


W$Latitude <- as.numeric(W$Latitude)
W$Longitude <- as.numeric(W$Longitude)

W$month <- month(W$UTC_datetime)
W$year <- year(W$UTC_datetime)

# Vliegende beesten eruit
W <- W[W$speed_km_h < 10,]

#resampling tot 30 min (cut-off van 28.5 minuten)
W <- W %>%
  arrange(device_id, UTC_datetime) %>%
  group_by(device_id) %>%
  mutate(diff_sec = UTC_datetime - lag(UTC_datetime, default = UTC_datetime[1]))

head(W[,c("UTC_datetime","diff_sec")])

ggplot(W[W$diff_sec<10000,], aes(diff_sec)) + geom_histogram()
ggplot(W[W$diff_sec<2500,], aes(diff_sec)) + geom_histogram() + scale_x_continuous(limits=c(0,2500))

W_30min <- setDT(W)[, .SD[.N], cumsumbinning(diff_sec, 28.5*60, cutwhenpassed = TRUE)]

W_30min <- W_30min %>%
  arrange(device_id, UTC_datetime) %>%
  group_by(device_id) %>%
  mutate(diff_sec = UTC_datetime - lag(UTC_datetime, default = UTC_datetime[1]))

ggplot(W_30min[W_30min$diff_sec<2500,], aes(diff_sec)) + geom_histogram() + scale_x_continuous(limits=c(0,2500))

#zonsopgang/zonsondergang
efemeriden <- getSunlightTimes(date = seq.Date(as.Date("2020-01-01"), as.Date("2023-12-31"), by = 1),
                               keep = c("sunrise", "sunset"), lat=50.90, lon=5.12,tz="UTC")
head(efemeriden)
efemeriden$date.chr <- as.character(efemeriden$date)

W_30min <- merge(W_30min, efemeriden[,c("date.chr","sunrise","sunset")], by.x="UTC_date", by.y="date.chr")

#"day" definieren als alles tussen 1 uur voor zonsopgang en 1 uur na zonsondergang
#hier stond een foutje denk ik en doe nu sunset + 3600 ipv sunset - 3600
W_30min$day <- ifelse(W_30min$UTC_datetime > (W_30min$sunrise-3600) &
                      W_30min$UTC_datetime < (W_30min$sunset+3600), TRUE, FALSE)
summary(W_30min$day)

#KERNEL LOOP
table(W$Naam,W$year)
W_30min$Name_year <- paste(W_30min$Naam, W_30min$year)
Name_year <- unique(W_30min$Name_year)
Name_year

BS_data <- matrix(nrow=17, ncol=2, dimnames=list(Name_year,c("Start_BS","End_BS")))
BS_data["Herlaar 2020",] <- c("2020-05-01","2020-06-01")
BS_data["Henk-Jan 2020",] <- c("2020-05-06","2020-06-09")
BS_data["Peter 2020",] <- c("2020-05-28","2020-07-05")
BS_data["Peter 2021",] <- c("2021-04-01","2021-06-27")
BS_data["Marc 2021",] <- c("2021-04-01","2021-06-09")
BS_data["Marcel 2021",] <- c("2021-06-10","2021-07-01")
BS_data["Peter 2022",] <- c("2022-04-01","2022-06-20")
BS_data["Marc 2022",] <- c("2022-04-01","2022-06-09")
BS_data["Jorg 2022",] <- c("2022-05-04","2022-06-15")
BS_data["Ronnie 2022",] <- c("2022-05-12","2022-05-30")
BS_data["Tonia 2022",] <- c("2022-05-13","2022-06-03")
BS_data["Geva 2022",] <- c("2022-05-18","2022-05-24")
BS_data["Ronnie 2023",] <- c("2023-05-01","2023-06-01")
BS_data["Peter 2023",] <- c("2023-05-01","2023-06-01")
BS_data["Geva 2023",] <- c("2023-05-01","2023-06-01")
BS_data["Marc 2023",] <- c("2023-05-01","2023-06-01")
BS_data["Jorg 2023",] <- c("2023-05-01","2023-06-01")
BS_data

BS_data <- as.data.frame(BS_data)
BS_data$Name_year <- rownames(BS_data)
BS_data

W_30min <- merge(W_30min, BS_data)
# W_30min_BS <- W_30min %>% filter(ymd(UTC_date) < ymd(End_BS) & ymd(UTC_date) > ymd(Start_BS))

pal <- colorNumeric(c("orangered4","orangered"), 1:2)
pal(1:2)

#select dag of nacht (indien enkel dagwaarden relevant: bovenste lijn aktief zetten, indien nachtawaarden relevant: tweede lijn aktief zetten)
W_30min_loop <- W_30min[W_30min$day==TRUE,]
# W_30min_loop <- W_30min[W_30min$day==FALSE,]
# W_30min_loop <- W_30min

#er loopt hier iets mis met de coördinaten, zowel latitude als longitude zijn 0:
summary(W_30min_loop[,c("Longitude", "Latitude")])
W_30min_BS[W_30min_loop$Latitude==0, c("Longitude", "Latitude")]
# ggplot(W_30min_BS, aes(Longitude, Latitude)) + geom_point()
# ggplot(W_30min_loop, aes(Longitude, Latitude)) + geom_point()
#deze waarnemingen verwijderen!
W_30min_loop <- W_30min_loop[W_30min_loop$Latitude != 0,]

for (i in Name_year)
{
  W_i <- W_30min_loop %>% filter(Name_year==i)
  W_i <- W_i %>% filter(ymd(UTC_date) < ymd(End_BS) & ymd(UTC_date) > ymd(Start_BS))

  coordinates(W_i) <- c("Longitude", "Latitude")
  proj4string(W_i) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

  Homerange <- kernelUD(W_i, grid=500, extent=1)
  HR_50 <- getverticeshr(Homerange, percent = 50)
  HR_95 <- getverticeshr(Homerange, percent = 95)

  W_i_map <- W_i %>%
    leaflet() %>%
    addTiles(group = "OSM") %>%
    addCircles(color = "#FF4500", weight = 3) %>%
    addPolygons(data = HR_50, fill = TRUE, color = ~pal(1), weight = 2) %>%
    addPolygons(data = HR_95, fill = TRUE, color = ~pal(2), weight = 2) %>%
    addControl(paste(i), position = "topright") %>%
    addLegend("topright", colors = pal(1:2), labels = c("50% Kernel","95% Kernel"), opacity = 1)

  shape_name_50 <- paste("Wulp ",i, " kernel 50", sep="")
  st_write(st_as_sf(HR_50), paste(shape_name_50), driver = "ESRI Shapefile")

  shape_name_95 <- paste("Wulp ",i, " kernel 95", sep="")
  st_write(st_as_sf(HR_95), paste(shape_name_95), driver = "ESRI Shapefile")

  filename_html <- paste("Wulp ",i,".html", sep="")
  saveWidget(W_i_map, file = filename_html)

  filename_png <- paste("Wulp ",i,".png", sep="")
  webshot(filename_html, file = filename_png,
          cliprect = "viewport")

  filename_html <- paste("Wulp ",i,".html", sep="")
  filename_png <- paste("Wulp ",i,".png", sep="")

  saveWidget(W_i_map, file=filename_html)
  webshot(filename_html, file = filename_png,
          cliprect = "viewport")
}


# overzichtskaart alle wulpen binnen BS_data
factpal <- colorFactor(viridis(12), W_30min_loop$jaar)

for (i in c(2020:2022))
{
  W_i <- W_30min_loop %>% filter(Naam == i)
  W_i <- W_i %>% filter(ymd(UTC_date) < ymd(End_BS) & ymd(UTC_date) > ymd(Start_BS))
  
  W_i_map <- W_i %>%
    leaflet() %>%
    addTiles(group = "OSM") %>%
    addCircles(lng = W_i$Longitude, lat = W_i$Latitude, color = ~factpal(W_i$jaar),radius=10) %>%
    # addPolygons(data = HR_50, fill = TRUE, color = ~pal(1), weight = 2) %>%
    # addPolygons(data = HR_95, fill = TRUE, color = ~pal(2), weight = 2) %>%
    addLegend(pal = factpal, values = ~jaar, opacity = 1)
  
  W_i_map

  filename_html <- paste("Wulp ",i, "_cumul_years.html", sep = "")
  filename_png <- paste("Wulp ",i, "_cumul_years.png", sep = "")
  
  saveWidget(W_i_map, file = filename_html)
  webshot(filename_html, file = filename_png, cliprect = "viewport")
}

Broed_zit_dag <- W_30min_loop %>% filter(ymd(UTC_date) < ymd(End_BS) & ymd(UTC_date) > ymd(Start_BS))
write.csv2(Broed_zit_dag, "Broed_zit_dag.csv", row.names = FALSE)

# #test vliegend/zittend
# library(ggplot2)
# ggplot(W, aes(speed_km_h)) + geom_histogram()
# ggplot(W, aes(month)) + geom_bar(stat="count")
# ggplot(W[W$month %in% c(2),], aes(speed_km_h)) + geom_histogram()
# ggplot(W[W$month %in% c(5),], aes(speed_km_h)) + geom_histogram()
# 
# sum(W$speed_km_h>10)/length(W$speed_km_h)
# sum(W$speed_km_h>5)/length(W$speed_km_h)
# #2.3% met snelheid hoger dan 10 km/h
# #3.2% met snelheid hoger dan 5 km/h
# 
# #een selectie op snelheid zal vermoedelijk heel weinig veranderen in de kernel resultaten

