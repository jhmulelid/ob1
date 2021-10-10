## Obligatory assignment - Script ##

#### Load packages to use ####
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

#### Data wrangling: For Oslo ####
#Creating dataset with aggregated number of trips, daily cases, wind and rain for each day for Oslo

# Loading all monthly csv-files
apr_19 <- read_csv("apr_19.csv")
may_19 <- read_csv("may_19.csv")
jun_19 <- read_csv("jun_19.csv")
jul_19 <- read_csv("jul_19.csv")
aug_19 <- read_csv("aug_19.csv")
sep_19 <- read_csv("sep_19.csv")
mar_20 <- read_csv("mar_20.csv")
apr_20 <- read_csv("apr_20.csv")
may_20 <- read_csv("may_20.csv")
jun_20 <- read_csv("jun_20.csv")
jul_20 <- read_csv("jul_20.csv")
aug_20 <- read_csv("aug_20.csv")
sep_20 <- read_csv("sep_20.csv")
mar_21 <- read_csv("mar_21.csv")
apr_21 <- read_csv("apr_21.csv")
may_21 <- read_csv("may_21.csv")
jun_21 <- read_csv("jun_21.csv")
jul_21 <- read_csv("jul_21.csv")
aug_21 <- read_csv("aug_21.csv")

# Merging into one dataset, called "citybike", and selecting variables of interest
citybike_full <- rbind(apr_19,may_19,jun_19,jul_19,aug_19,sep_19,
                  mar_20,apr_20,may_20,jun_20,jul_20,aug_20,sep_20,
                  mar_21,apr_21,may_21,jun_21,jul_21,aug_21) %>%  
  separate(1, c("year","month", "day")) %>% 
  select(c(1,2,3,5,6,11))

# Loading the other datasets
weather_oslo <- read_delim("~/R/Oblig/weather_oslo.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
daily_infections_oslo <- read_delim("~/R/Oblig/daily_infections_oslo.csv", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)
citybike_full <- read_csv("~/R/Oblig/Bysykkel/citybike_full.csv") # Only load if it is not the the environment

# Data wrangling, shaping the dataset into aggregate number of trips, for each date.
bike_group<-citybike_full %>% 
  group_by(year,month,day) %>%  # group into rows with the same value of year, month and date
  summarise(Count=n()) %>%  # COunt number of trips per day
  mutate(year=as.character(year)) %>% # Creating "year" into the form of "characters"
  mutate(month=as.character(month)) %>% # Creating "month" into the form of "characters"
  mutate(day=as.character(day)) %>% # Creating "day" into the form of "characters"
  unite(col=date, c(day,month,year), sep = ".") # Unite into one date on form dd.mm.yyyy, need to have all as characters to do that

# Selecting date and daily cases of covid-19 infections, because these are the variables we care about
infections<-daily_infections_oslo %>% 
  select(c(1,3))
names(infections)<-c('date','infection') # Changing the names

# Selecting date, rain and wind for weather-data 
weather<-weather_oslo %>% 
  select(c(3,4,5)) 
names(weather)<-c('date','wind','rain') # Changing the names

# Merging all datasets into one, where we have date, wind, rain, bike trips and daily cases as variables for our analysis
bike_weather<-left_join(weather,bike_group)
oslo_data<-left_join(bike_weather,infections)
names(oslo_data) <- c('date','wind',"rain",'bike_trips','daily_cases') # Changing the names

# We want the "date" to be understood as a real date, not just a number
oslo_data <- oslo_data %>% 
  mutate(dmy(oslo_data$date)) %>% # use function dmy from package lubridate to convert it to a date
  select(2,3,4,5,6)
names(oslo_data) <- c('wind',"rain",'bike_trips','daily_cases','date') # Changing the names

#### Data wrangling: For The University of Oslo ####
# Creating dataset with aggregated number of trips to the University of Oslo, daily cases, wind and rain for each day for Oslo


# Only trips to the University of Oslo, so that end_station_id is one of the following (with id):
# Fysikkbyggningen (496)
# Georg Morgenstiernes hus (580)
# Bak Niels Treschows hus nord (619)
# Bak Niels Treschows hus sør (618)
# Blindern studentparkering (474)

# Data wrangling, shaping the dataset into aggregate number of trips, for each date.
bike_uni<-citybike_full %>% 
  filter(end_station_id==c(496,580,619,618,474)) %>% 
  group_by(year,month,day) %>% # group into rows with the same value of year, month and date
  summarise(Count=n())%>%  # COunt number of trips per day
  mutate(year=as.character(year)) %>% # Creating "year" into the form of "characters"
  mutate(month=as.character(month)) %>% # Creating "month" into the form of "characters"
  mutate(day=as.character(day)) %>% # Creating "day" into the form of "characters"
  unite(col=date, c(day,month,year), sep = ".") # Unite into one date on form dd.mm.yyyy, need to have all as characters to do that

# Merging all datasets into one, where we have date, wind, rain, bike trips and daily cases as variables for our analysis
bike_uni_weather<-left_join(weather,bike_uni)
data_university<-left_join(bike_uni_weather,infections)
names(data_university) <- c('date','wind',"rain",'bike_trips','daily_cases') # Changing the names

# We want the "date" to be understood as a real date, not just a number
data_university <- data_university %>% 
  mutate(dmy(data_university$date)) %>% # use function dmy from package lubridate to convert it to a date
  select(2,3,4,5,6)
names(data_university) <- c('wind',"rain",'bike_trips','daily_cases','date') # Changing the names

#### Visualisation ####

## We want to visualize our two datasets; oslo_data and data_university
# Visualisation for all three years
ggplot(oslo_data) +
  geom_line(aes(date,bike_trips)) +
  labs(x = "Days", y = "Trips in Oslo") +
  geom_line(data=data_university, aes(x=date, y=bike_trips*1000), colour = 'red')+
  scale_y_continuous(limits=c(0, 35000),
                     sec.axis = sec_axis(~ . *0.001, name = "Trips to the University"))


# Visualisation for 2019
oslo_data_2019<-oslo_data %>% 
  filter(date>= "2019-04-03" & date<= "2019-09-30")

data_university_2019<-data_university %>% 
  filter(date>= "2019-04-03" & date<= "2019-09-30")

ggplot(oslo_data_2019) +
  geom_line(aes(date,bike_trips), size = 1) +
  labs(x = "Days", y = "Trips in Oslo") +
  geom_line(data=data_university_2019, aes(x=date, y=bike_trips*1000),colour = 'red', size = 1)+
  scale_y_continuous(limits=c(0, 35000),
                     sec.axis = sec_axis(~ . *0.001, name = "Trips to the University"))

# Visualisation for 2020
oslo_data_2020<-oslo_data %>% 
  filter(date>= "2020-03-18" & date<= "2020-09-30")

data_university_2020<-data_university %>% 
  filter(date>= "2020-03-18" & date<= "2020-09-30")

ggplot(oslo_data_2020) +
  geom_line(aes(date,bike_trips), size = 1) +
  labs(x = "Days", y = "Trips in Oslo") +
  geom_line(data=data_university_2020, aes(x=date, y=bike_trips*1000),colour = 'red', size = 1)+
  scale_y_continuous(limits=c(0, 35000),
                     sec.axis = sec_axis(~ . *0.001, name = "Trips to the University"))

# Visualisation for 2021
oslo_data_2021<-oslo_data %>% 
  filter(date>= "2021-03-01" & date<= "2021-08-31")

data_university_2021<-data_university %>% 
  filter(date>= "2021-03-01" & date<= "2021-08-31")

ggplot(oslo_data_2021) +
  geom_line(aes(date,bike_trips), size = 1) +
  labs(x = "Days", y = "Trips in Oslo") +
  geom_line(data=data_university_2021, aes(x=date, y=bike_trips*1000),colour = 'red', size = 1)+
  scale_y_continuous(limits=c(0, 20000),
                     sec.axis = sec_axis(~ . *0.001, name = "Trips to the University"))

## We want to compare data for university for 2019 and 2020
ggplot(data_university_2019) +
  geom_line(aes(date,bike_trips), size = 1) +
  labs(x = "Days", y = "Trips in Oslo") +
  geom_line(data=data_university_2020, aes(x=date, y=bike_trips),colour = 'red', size = 1)+
  scale_y_continuous(limits=c(0, 35),
                     sec.axis = sec_axis(~ . *1, name = "Trips to the University"))

# TEST, does not work
data_university_2019_longer<-data_university %>% 
  filter(date>= "2019-03-18" & date<= "2019-09-30")
difference_uni_2019_2020 <- data_university_2019_longer$bike_trips - data_university_2020$bike_trips

## See number of infected and number of trips in Oslo and the university
# Oslo
ggplot(oslo_data_2020) +
  geom_line(aes(date,bike_trips), size = 1) +
  labs(x = "Days", y = "Trips in Oslo") +
  geom_line(data=oslo_data_2020, aes(x=date, y=daily_cases*200),colour = 'red', size = 1)+
  scale_y_continuous(limits=c(0, 22000),
                     sec.axis = sec_axis(~ . *0.005, name = "Daily cases"))

# University
ggplot(data_university_2020) +
  geom_line(aes(date,bike_trips), size = 1) +
  labs(x = "Days", y = "Trips to the University of Oslo") +
  geom_line(data=data_university_2020, aes(x=date, y=daily_cases*(1/2)),colour = 'red', size = 1)+
  scale_y_continuous(limits=c(0, 50),
                     sec.axis = sec_axis(~ . *2, name = "Daily cases"))


