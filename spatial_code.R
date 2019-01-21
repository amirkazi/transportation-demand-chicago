
# TRAIN LATITUTE & LONGITUDE CODE
train_stations <- read.csv(here::here("data", "CTA_-_System_Information_-_List_of__L__Stops.csv"))
train_stations <- select(train_stations, STOP_NAME, STATION_NAME, MAP_ID, Location)
train_stations <- rename(train_stations, station_id = MAP_ID)
train_stations <- train_stations %>% extract(Location, c("Latitude", "Longitude"), "\\(([^,]+), ([^)]+)\\)")
train_locations <- left_join(train_trips, train_stations, by = "station_id")

library(sp) #spatial data wrangling & analysis
library(rgdal) #spatial data wrangling
library(rgeos) #spatial data wrangling & analytics
library(tmap) #modern data visualizations
library(leaflet) #modern data visualizations

train_locations$Latitude <- as.numeric(train_locations$Latitude)
train_locations$Longitude <- as.numeric(train_locations$Longitude)
train_locations <- filter(train_locations,  !is.na(Longitude))

top30 <- train_locations %>%
  group_by(station_id) %>%
  summarise(count = sum(rides)) %>%
  top_n(30)

top30 <- inner_join(top30, train_stations, by = "station_id")
top30 <- top30[!duplicated(top30$station_id),]

stops <- SpatialPointsDataFrame(top30[,c('Latitude','Longitude')],top30)
proj4string(stops) <- CRS("+init=epsg:28992")
plot(stops)

length(stops)

train_stations$Latitude <- as.numeric(train_stations$Latitude)
train_stations$Longitude <- as.numeric(train_stations$Longitude)
stops <- SpatialPointsDataFrame(train_stations[,c('Latitude','Longitude')],train_stations)
proj4string(stops) <- CRS("+init=epsg:28992")
plot(stops)

length(stops)

tmap_mode('plot')

tm_shape(stops) + tm_dots("STOP_NAME",style="cat",size=1)

tmap_mode('view')
tm_shape(stops) + tm_dots("STOP_NAME",style="cat",size=0.1)

latlong

# PLOT 2