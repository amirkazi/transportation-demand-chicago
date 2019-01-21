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


# Variation in demand by day
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



ggplot(x) +
  geom_ribbon(aes(x = dayofweek, ymin = count - 1, ymax = count + 1), fill = "grey70") +
  geom_line(aes(x = dayofweek, y = count))

ggplot(x, aes(x=dayofweek, y=count)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=dayofweek, 
                   xend=dayofweek, 
                   y=0, 
                   yend=count,
                   colour = trip_identity,
                   position = 'dodge',
                   linetype=trip_identity)) + 
  labs(title="Lollipop Chart", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") 

install.packages('ggthemes')
library(ggthemes)

ggplot(x, aes(dayofweek, count)) + 
  geom_tufteboxplot() + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Tufte Styled Boxplot", 
       subtitle="City Mileage grouped by Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")

ggplot(x, aes(x=dayofweek)) + 
  geom_line(aes(y=count, col=trip_identity)) + 
ggplot(x, aes(x=dayofweek)) + 
  geom_area(aes(y=count, fill="psavert")) + 
  geom_area(aes(y=uempmed, fill="uempmed"))


geom_freqpoly()
x, y, alpha, color, linetype, size
b + geom_freqpoly(aes(y = ..density..)) 

x <- public_transport %>%
  group_by(dayofweek, trip_identity) %>%
  summarise(count = sum(rides)) 

ggplot(data = x) +
  geom_text(aes(x = dayofweek, y = count, label = trip_identity))


ggplot(x, aes(x=dayofweek, y=count, group=2)) +
  geom_point(stat='summary', fun.y=sum) +
  stat_summary(fun.y=sum, geom="line")


ggplot(x, aes(dayofweek, count)) +
  geom_line(aes(linetype = trip_identity))


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











ggplot(data = public_transport) + 
  geom_bar(mapping = aes(x = month, fill = dayofweek), position = "dodge")   +
  coord_cartesian(ylim = c(2500, 4000)) 





# Difference in demand
x <- public_transport %>%
  group_by(month, trip_identity) %>%
  summarise (count = sum(rides)) %>%
  mutate (Diff = count - lag(count)) 

ggplot(data = x, aes (x = month, y = count)) +
  geom_jitter() 

%>%
  ggplot(aes(x=month, y=abs(Diff) )) + 
  geom_point() + geom_smooth() +
  scale_y_continuous(labels = scales::comma) 



detach("package:lubridate", unload=TRUE)


# IDEAS
# Top bus routes
# top train stations
# days when highest bus and train usage
  