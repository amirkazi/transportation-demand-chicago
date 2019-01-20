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
             stat = "identity" , colour="black", size=.7, width=0.7)  +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian( ylim = c(13000000, 24500000)) + 
  labs(title =" Average Monthly Users of Public Transport in Chicago", 
       subtitle = "Usage declines during winter months",
       x = "Month", y = "Total Number of Passengers",
       caption = "CTA Ridership Data 2015-2017") +
  guides(fill=guide_legend(title="")) +
  theme(plot.title = element_text(hjust = 1, size = 13)) 


# PLOT 2



public_transport %>%
  group_by(month, trip_identity) %>%
  summarise (count = sum(rides)) %>%
  mutate (Diff = count - lag(count)) %>%
  ggplot(aes(x=month, y=abs(Diff) )) + 
  geom_point() + geom_line() +
  scale_y_continuous(labels = scales::comma) 

+ 
  coord_flip() 


ggplot(economics_long, aes(date, value01, colour = variable)) +
  geom_line()



df %>%
  group_by(group) %>%
  mutate(Diff = value - lag(value))

# PLOT 2

+ theme_dark() +
  scale_fill_brewer(palette = "Greys") 




# Average passengers over a three year period (2015 - 2018)
# Divide by 3 to get average yearly data 
public_transport %>%
  group_by(month, trip_identity) %>%
  summarise (count = sum(rides)) %>%
  ggplot() + 
  geom_line(mapping = aes(x = month, y=count/3)) +
  geom_point(mapping = aes(x = month, y=count/3, ), position = "dodge",
           stat = "identity" , colour="black", size=.6)  +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian( ylim = c(10000000, 25000000)) + 
  labs(title =" Difference between Bus & Train usage", 
       x = "Month", y = "Total Number of Passengers",
       caption = "CTA L Train Ridership 2015-2018")


public_transport %>%
  group_by(dayofweek, trip_identity) %>%
  summarise (count = sum(rides)) %>%
  
ggplot (data = public_transport, mapping = aes(x = dayofweek)) +
  geom_dotplot(aes(fill = rides))


theta, start, direction 





public_transport %>%
  group_by(month, trip_identity) %>%
  summarise(sum = sum(public_transport$rides)) %>%
  ggplot() + 
  geom_bar(mapping = aes(x = month, y=sum/3, fill = trip_identity), position = "dodge",
           stat = "identity" , colour="black", size=.6)  +
  coord_cartesian( ylim = c(1500000, 200000)) + 
  labs(title =" Average Monthly Users of Public Transport", x = "Month", y = "Total Number of Passengers")

ggplot(data = public_transport) + 
  geom_bar(mapping = aes(x = month, fill = dayofweek), position = "dodge")   +
  coord_cartesian(ylim = c(2500, 4000)) 










detach("package:lubridate", unload=TRUE)


# IDEAS
# Top bus routes
# top train stations
# days when highest bus and train usage
  