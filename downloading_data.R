library("RSocrata")
library("devtools")
library(here)
library (tidyverse)
#devtools::install_github("Chicago/RSocrata")

# Data between 1st January 2015 and 1st January 2018
# API docs: https://dev.socrata.com/docs/datatypes/floating_timestamp.html

# PUBLIC TRANSPORT
bus_trips <- read.socrata("https://data.cityofchicago.org/resource/jyb9-n7fm.json?$where=date between '2015-01-01T00:00:00' and '2018-01-01T00:00:00'")
train_trips <- read.socrata("https://data.cityofchicago.org/resource/5neh-572f.json?$where=date between '2015-01-01T00:00:00' and '2018-01-01T00:00:00'")

write.csv(bus_trips, 'bus_data.csv')
write.csv(train_trips, 'train_data.csv')

# WEATHER
weather <- read.csv(file="data/weather_description.csv", header=TRUE, sep=",")
write.csv (select (weather, datetime, Chicago), here("data", "weather.csv"))

# TAXI
#taxi_trips <- read.socrata("https://data.cityofchicago.org/resource/wrvz-psew.json?$where=trip_start_timestamp between '2015-01-01T00:00:00' and '2018-01-01T00:00:00'")
