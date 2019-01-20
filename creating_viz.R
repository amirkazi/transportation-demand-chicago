library (tidyverse)
library (here)


bus_trips <- read.csv (here("data", "bus_data.csv"))
train_trips <- read.csv (here("data", "train_data.csv"))
weather <- read.csv(here("data", "weather.csv"))


library(lubridate)
