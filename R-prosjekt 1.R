library(tidyverse)
library(dplyr)
library(ggplot2)
library(fpp2) # Sjekke om trengs

##### Comparing Covid versus not Covid from/to UiO ----

# Setting up two tables:

popular_notcorona <- citybikes_univ %>% 
  mutate(date = mday(started_at)) %>% 
  filter(start_station_name != end_station_name) %>%
  filter(year<2020) %>%
  group_by(date) %>%
  summarise(numbers = n()) %>%
  arrange(desc(numbers)) 

popular_incorona <- citybikes_univ %>% 
  mutate(date = mday(started_at)) %>% 
  filter(start_station_name != end_station_name) %>%
  filter(year==2021 & year==2020) %>%
  filter() %>%
  group_by(date) %>%
  summarise(numbers = n()) %>%
  arrange(desc(numbers)) 

comparing_table_trips <- tibble(popular_incorona, popular_notcorona)
comparing_table

popular_notcorona_duration <- citybikes_univ %>% 
  mutate(date = mday(started_at)) %>% 
  filter(start_station_name != end_station_name) %>%
  filter(year<2020) %>%
  group_by(date) %>%
  summarise(numbers = mean(duration)) %>%
  arrange(desc(numbers)) 
  
popular_incorona_duration <- citybikes_univ %>% 
  mutate(date = mday(started_at)) %>% 
  filter(start_station_name != end_station_name) %>%
  filter(year==2021 & year==2020) %>%
  filter() %>%
  group_by() %>%
  summarise(duration = mean(duration)) %>%
  arrange(desc(duration)) 
  
comparing_table_trips <- tibble(popular_incorona, popular_notcorona)
comparing_table_trips

# Deviations from non-corona og corona (moving average)
  
##### Regressions ----

# Fix the data before regressing

.. 

# City bike trips and infection rates and weather (standard mlm)
# Using the "Oslo" dataset

regr_oslo_infectionrates %>%
  lm(formula = trips ~ infectionrates, data = citybikes_oslo) %>%
  summary(regr_oslo_infectionrates) %>%
  ggplot(data = citybikes_oslo, aes(x = infectionrates, y = trips))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)+
  ylab("Number of trips")+
  xlab("Infection rates")+
  ggtitle("Regression on infection rates on number of trips")
  
regr_oslo_weather %>%
  lm(formula = trips ~ rain + wind, data = citybikes_oslo) %>%
  summary(regr_oslo_weather) 

regr_oslo_weather_trimmed %>%
  lm(formula = trips ~ rain + wind, data = citybikes_oslo) %>%
  filter(data = rain, rain > 0.9)
  summary(regr_oslo_weather)
  
regr_oslo_combined %>%
  lm(formula = trips ~ infectionrates + rain + wind, data = citybikes_oslo) %>%
  summary(regr_oslo_combined)
  
stargazer(regr_oslo_infectionrates, regr_oslo_weather, regr_oslo_weather_trimmed, regr_oslo_combined, 
          out='Estimates of regressions of "Standard mlm".tex')

anova(regr_oslo_infectionrates, regr_oslo_weather_trimmed, regr_oslo_combined)

# City bike trips and infection rates and weather (more regressions)
# Using the "Oslo" dataset

regr_oslo_infectionrates_2 %>%
  lm(formula = trips ~ infectionrates + infectionrates^2, data = citybikes_oslo) %>%
  summary(citybikes_oslo) %>%
  ggplot(data = df_regresjon_hverdag, aes(x = infectionrates, y = trips))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x + x^2)+
  ylab("Number of trips")+
  xlab("Infection rates")+
  ggtitle("Regression on infection rates on number of trips")

regr_oslo_weather_2 %>%
  lm(formula = trips ~ rain + rain^2 + wind, data = citybikes_oslo) %>%
  filter(data = rain, rain > 0.9) %>%
  summary(regr_oslo_weather) 

regr_oslo_weather_trimmed_2 %>%
  lm(formula = trips ~ rain + + rain^2 + wind + wind^2, data = citybikes_oslo) %>%
  filter(data = rain, rain > 0.9)
summary(regr_oslo_weather)

regr_oslo_combined_2 %>%
  lm(formula = trips ~ infectionrates + rain + rain^2 + wind + wind^2, data = citybikes_oslo) %>%
  summary(regr_oslo_combined)

stargazer(regr_oslo_infectionrates_2, regr_oslo_weather_2, regr_oslo_weather_trimmed_2, regr_oslo_combined_2, 
          out='Estimates of some more regressions.tex')

# City bike trips and infection rates and weather (standard mlm)
# Using the "University" dataset

regr_univ_infectionrates %>%
  lm(formula = trips ~ infectionrates, data = citybikes_oslo) %>%
  summary(regr_oslo_infectionrates) %>%
  ggplot(data = citybikes_oslo, aes(x = infectionrates, y = trips))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ x)+
  ylab("Number of trips")+
  xlab("Infection rates")+
  ggtitle("Regression on infection rates on number of trips")

regr_univ_weather %>%
  lm(formula = trips ~ rain + wind, data = citybikes_oslo) %>%
  summary(regr_oslo_weather) 

regr_univ_weather_trimmed %>%
  lm(formula = trips ~ rain + wind, data = citybikes_oslo) %>%
  filter(data = rain, rain > 0.9)
summary(regr_oslo_weather)

regr_univ_combined %>%
  lm(formula = trips ~ infectionrates + rain + wind, data = citybikes_oslo) %>%
  summary(regr_oslo_combined)

## Comparing regressions - could we make a loop?

stargazer(regr_oslo_infectionrates, reg_univ_infectionrates, 
          out='Comparing regressions with Oslo and the UiO.tex')

comparing <- function(compare){
  stargazer(regr_oslo_$variable,regr_oslo_$variable, out='Comparing regressions.tex')
}
variable <- c(infectionrates, weather, weather_trimmed, combined)
compare()   ## Is it possible to make a loop??

# City bike trips and infection rates and weather (exponential model)
# Using the "Oslo" dataset

regr_univ_infectionrates_exp %>%
  lm(formula = log(trips) ~ infectionrates, data = citybikes_oslo) %>%
  summary(regr_oslo_infectionrates_exp) %>%
  ggplot(data = citybikes_oslo, aes(x = infectionrates, y = trips))+
  geom_point()+
  geom_smooth(method = "lm", formula = exp(y) ~ x)+
  ylab("Number of trips")+
  xlab("Infection rates")+
  ggtitle("Regression on infection rates on number of trips")

regr_oslo_weather_exp %>%
  lm(formula = log(trips) ~ rain + rain^2 + wind, data = citybikes_oslo) %>%
  filter(data = rain, rain > 0.9) %>%
  summary(regr_oslo_weather) 

regr_oslo_weather_trimmed_exp %>%
  lm(formula = log(trips) ~ rain + + rain^2 + wind + wind^2, data = citybikes_oslo) %>%
  filter(data = rain, rain > 0.9)
summary(regr_oslo_weather)

regr_oslo_combined_exp %>%
  lm(formula = log(trips) ~ infectionrates + rain + rain^2 + wind + wind^2, data = citybikes_oslo) %>%
  summary(regr_oslo_combined)

stargazer(regr_oslo_infectionrates_exp, regr_oslo_weather_exp, regr_oslo_weather_trimmed_exp, regr_oslo_combined_exp, 
          out='Estimates of exponential regressions.tex')


##### Estimation function ----

regr_oslo_combined %>%
  lm(formula = trips ~ infectionrates + rain + wind, data = citybikes_oslo) 

b0 <- coef(regr_oslo_combined)[1]
b1 <- coef(regr_oslo_combined)[2]
b3 <- coef(regr_oslo_combined)[3]

estimate_trips <- function(infectionrates, rain, wind) {
  return(b0 + b1*infectionrates + b2*rain + b3*wind)
}

estimate_trips(200,0,2) # Type in your values here

##### Plotting the series ----

citybikes_oslo %>%
  ggplot(data = citybikes_oslo, aes(x = date, y = trips)) +
  ggseasonplot() +
  ylab("N")+
  xlab("I")+
  ggtitle("R")

citybikes_oslo %>%
  ggplot(data = citybikes_oslo, aes(x = date, y = trips)) +
  ggsubseriesplot() +
  ylab("N")+
  xlab("I")+
  ggtitle("R")

citybikes_oslo %>%
  ggplot(data = citybikes_oslo, aes(x = date, y = trips)) +
  gglagplot() +
  ylab("N")+
  xlab("I")+
  ggtitle("R")

##### Forecasts ----

# Try to apply "Naive"-forecasting

citybikes_trips_fc <- naive(citybikes_trips[2], h = 20) %>%
  autoplot(citybikes_trips_fc) %>%
  summary(citybikes_trips_fc) %>%
  ggplot(data = Citybike_trips_TS)  %>%
  labs(title = "Projections of number of city bike trips", 
       subtitle = "From xx to xx", y="Number of trips")

citybikes_trips_fc %>% naive() %>% checkresiduals() # Check the residuals from the naive forecasts applied to the goog series
citybikes_trips_fc <- TRUE # Do they look like white noise? (T/F)

citybikes_trips_fC <- naive(citybikes_trips[2], h = 20) %>%
  autoplot(citybikes_trips_FC) %>%
  summary(citybikes_trips_FC) %>%
  ggplot(data = Citybike_trips_TS)  %>%
  labs(title = "Projections of number of city bike trips", 
       subtitle = "From xx to xx", y="Number of trips")

citybikes_trips_fc %>% naive() %>% checkresiduals() # Check the residuals from the naive forecasts applied to the goog series
citybikes_trips_fc <- TRUE # Do they look like white noise? (T/F)

# Try to apply "Snaive"-forecasting

citybikes_trips_sfc <- snaive(citybikes_trips[2], h = 20) %>%
  autoplot(citybikes_trips_fc) %>%
  summary(citybikes_trips_fc) %>%
  ggplot(data = Citybike_trips_TS)  %>%
  labs(title = "Projections of number of city bike trips", 
       subtitle = "From xx to xx", y="Number of trips")

citybikes_trips_sfc <- snaive(citybikes_trips[2], h = 20) %>%
  autoplot(citybikes_trips_sfc) %>%
  summary(citybikes_trips_sfc) %>%
  ggplot(data = Citybike_trips_TS)  %>%
  labs(title = "Projections of number of city bike trips", 
       subtitle = "From xx to xx", y="Number of trips")

# Try to apply "ARIMA"-forecasting

citybikes_trips_ts %>% ts(Citybikes_trips[2], start = c(2016,1), frequency=12, end=c(2022,12))

citybikes_trips_ts %>%
  auto.arima() %>%
  forecast(h=) %>%
  ggplot(data = Citybike_trips_TS)  %>%
  labs(title = "Projections of number of city bike trips", 
       subtitle = "From xx to xx", y="Number of trips")

citybikes_aggduration_ts %>%
  auto.arima() %>%
  forecast() %>%
  ggplot(data = Citybike_aggduration_TS) %>%
  labs(title = "Projections of aggregate duration of city bike trips", 
       subtitle = "From xx to xx", y="Time spend of all trips") 
  
# Graph all the methods

Graph_trips <- grid.arrange(citybikes_trips_fc, citybikes_trips_sfc, citybikes_trips_ts, nrow = 1)
Graph_trips

Graph_duration <- grid.arrange(citybikes_trips_fc, citybikes_trips_sfc, citybikes_trips_ts, nrow = 1)
Graph_duration



    







