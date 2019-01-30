library (tidyverse)
library (here)

bus_trips <- read.csv (here("data", "bus_data.csv"))
train_trips <- read.csv (here("data", "train_data.csv"))
weather <- read.csv(here("data", "weather.csv"))

# lubridate masks object 'here' from package 'here'.
library(lubridate)

train_trips$date <- ymd(train_trips$date)
train_trips$dayofweek <- wday(train_trips$date, label=TRUE)  
train_trips$day <- day(train_trips$date)
train_trips$month <- month(train_trips$date, label = TRUE)
train_trips$year <- year(train_trips$date)

bus_trips$date <- ymd(bus_trips$date)
bus_trips$dayofweek <- wday(bus_trips$date, label=TRUE)  
bus_trips$day <- day(bus_trips$date)
bus_trips$month <- month(bus_trips$date, label = TRUE)
bus_trips$year <- year(bus_trips$date)

train_trips$trip_identity = 'train'
bus_trips$trip_identity = 'bus'

train_trips$route = NA
bus_trips$station_id = NA
bus_trips$stationname = NA

public_transport = rbind(train_trips,bus_trips)

# PLOT 1

# Average passengers over a three year period (2015 - 2017)
# Divide by 3 to get average yearly data 
public_transport %>%
  group_by(month, trip_identity) %>%
  summarise (count = sum(rides)) %>%
  ggplot() + 
  geom_bar(mapping = aes(x = month, y=count/3, fill = trip_identity), position = "dodge",
             stat = "identity" , colour="black",  size=.7, width=0.7)  +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian( ylim = c(13000000, 24500000)) + 
  labs(title ="Monthly Usage of Public Transport in Chicago Declines in Winter", 
       subtitle = "Both buses & trains experience fall in usage",
       x = "Month", y = "Total Number of Passengers",
       caption = "CTA Ridership Data 2015-2017") +
  guides(fill=guide_legend(title="")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0, size = 10, face = "bold"), 
                                  plot.subtitle = element_text(size = 9, face = "bold"))


# PLOT 2

# Variation in usage by day of week
# 3*52.19 == Number of weeks in 3 years
public_transport %>%
  group_by(dayofweek, trip_identity) %>%
  summarise(count = sum(rides)/(3*52.19))  %>%
  ggplot(aes(x=dayofweek, y=count)) + 
  geom_point(aes(col=trip_identity), size = 5, stroke = 1) + 
  geom_line(mapping = aes(x = dayofweek, y=count),  alpha = 0.3)  +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian( ylim = c(200000, 900000)) + 
  labs(title ="Usage of Public Transport in Chicago is Highest on Weekdays", 
       subtitle = "Weekends see a significant drop in riders",
       x = "Day of Week", y = "Average Weekly Passengers",
       caption = "CTA Ridership Data 2015-2017") +
  theme_bw()  + 
  theme(plot.title = element_text(hjust = -0, size = 11, face = "bold"), 
        plot.subtitle = element_text(size = 9, face = "bold"), 
        legend.title=element_blank(),
        legend.key = element_rect(fill = "white")) +
  coord_flip()


# PLOT 3

# Most common bus routes and train stations
# Dividing by 3*365 to get daily average
top10_bus <- bus_trips %>%
  group_by(route) %>%
  summarise(count = sum(rides)/(3*365)) %>%
  top_n(10)

top10_train <- train_trips %>%
  group_by(stationname) %>%
  summarise(count = sum(rides)/(3*365)) %>%
  top_n(10)

library(ggrepel)

ggplot() +
  geom_label_repel(data = top10_train, aes(x = "Train Stations", y = count, label = stationname),
            check_overlap = FALSE, size = 4, angle = 0, colour = "black")  +
  geom_label_repel(data = top10_bus, aes(x = "Bus Routes", y = count, label = route),
            check_overlap = FALSE, size = 4, angle = 0, colour = "black")  +
  scale_y_continuous(labels = scales::comma) + 
  labs(title ="Top 10 Bus Routes & Train Stations in Chicago", 
       subtitle = "Busiest train stations are in downtown, buses common on non-L routes.",
       x = "", y = "Average Daily Passengers",
       caption = "CTA Ridership Data 2015-2017") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = -0, size = 11, face = "bold"), 
        plot.subtitle = element_text(size = 9, face = "bold"))


################ 
################
################

# PLOT 4: DIAGRAM FOR LATER
# Average passengers over a three year period (2015 - 2018)
# Divide by 3 to get average yearly data 
public_transport %>%
  group_by(month, trip_identity) %>%
  summarise (count = sum(rides)) %>%
  ggplot() + 
  geom_line(mapping = aes(x = month, y=count/3)) +
  geom_point(mapping = aes(x = month, y=count/3), position = "dodge",
             stat = "identity" , colour="black", size=.6)  +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian( ylim = c(10000000, 25000000)) + 
  labs(title =" Difference between Bus & Train usage", 
       x = "Month", y = "Total Number of Passengers",
       caption = "CTA L Train Ridership 2015-2018") +
  coord_flip() 


# PLOT 5: DIAGRAM FOR LATER
# Difference in demand
x <- public_transport %>%
  group_by(month, trip_identity) %>%
  summarise (count = sum(rides)) %>%
  mutate (Diff = count - lag(count)) 

ggplot(x, aes (x = month, y = count)) +
  geom_jitter() 

ggplot(x, aes(x=month, y=abs(Diff) )) + 
  geom_point() + geom_smooth() +
  scale_y_continuous(labels = scales::comma) 







```{r, include= FALSE, echo= FALSE}
# EXTRA PLOT
# AVERAGE MONTHLY 


# Average passengers over a three year period (2015 - 2017)
# Divide by 3 to get average yearly data 
public_transport %>%
  group_by(month, trip_identity) %>%
  summarise (count = sum(rides)) %>%
  ggplot(mapping = aes(x = month, y=(count/3)/1000000, colour = trip_identity)) + 
  geom_point() +
  geom_line(mapping = aes( group = trip_identity)) +
  theme(legend.title = element_blank()) + 
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian( ylim = c(13, 25)) + 
  labs(title ="Monthly Usage of Public Transport in Chicago Declines in Winter", 
       subtitle = "Both buses & trains experience fall in usage",
       x = "Month", y = "Total Number of Passengers (Millions)",
       caption = "CTA Ridership Data 2015-2017") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0, size = 12, face = "bold"), 
        plot.subtitle = element_text(size = 10, face = "bold"),
        legend.title=element_blank())


```








```{r, include = FALSE, echo = FALSE}
library (pacman)
p_load ("rgdal", "maptools", "plyr")
install.packages("ggmap")
library(rgdal)     # R wrapper around GDAL/OGR
library(ggplot2)   # for general plotting
library(ggmap)    # for fortifying shapefiles

# First read in the shapefile, using the path to the shapefile and the shapefile name minus the
# extension as arguments
shapefile <- readOGR(dsn=path.expand("~/Desktop/transportation-demand-chicago//ChiComArea.shp"))

# Next the shapefile has to be converted to a dataframe for use in ggplot2
shapefile_df <- fortify(shapefile)

# Now the shapefile can be plotted as either a geom_path or a geom_polygon.
# Paths handle clipping better. Polygons can be filled.
# You need the aesthetics long, lat, and group.
map <- ggplot() +
  geom_path(data = shapefile_df, 
            aes(x = long, y = lat, group = group),
            color = 'gray', fill = 'white', size = .2)

print(map) 

# Using the ggplot2 function coord_map will make things look better and it will also let you change
# the projection. But sometimes with large shapefiles it makes everything blow up.
map_projected <- map +
  coord_map()

print(map_projected)

```

