# ******************************************************************************
# Chapter   : 2 Time series graphics
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/24
# URL       : https://otexts.com/fpp3/graphics.html
# ******************************************************************************

# ＜概要＞





# 0 準備 ---------------------------------------------------------------------------------------

library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)
library(fpp3)


# 2.1 tsibble objects --------------------------------------

y <- tsibble(
  Year = 2015:2019,
  Observation = c(123, 39, 78, 52, 110),
  index = Year
)

y

y %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month)

olympic_running


olympic_running %>% distinct(Sex)



PBS

PBS %>%
  filter(ATC2 == "A10")


PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost)

PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost))


PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost)) %>%
  mutate(Cost = TotalC / 1e6)


PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost)) %>%
  mutate(Cost = TotalC / 1e6) -> a10


prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")
prison <- prison %>%
  mutate(Quarter = yearquarter(Date)) %>%
  select(-Date) %>%
  as_tsibble(key = c(State, Gender, Legal, Indigenous),
             index = Quarter)

prison


# 2.2 Time plots ---------------------------


melsyd_economy <- ansett %>%
  filter(Airports == "MEL-SYD", Class == "Economy")
autoplot(melsyd_economy, Passengers) +
  labs(title = "Ansett economy class passengers",
       subtitle = "Melbourne-Sydney")

autoplot(a10, Cost) +
  labs(y = "$ million", title = "Antidiabetic drug sales")


# 2.4 Seasonal plots -----------------


a10 %>%
  gg_season(Cost, labels = "both") +
  labs(y = "$ million",
       title = "Seasonal plot: antidiabetic drug sales")


vic_elec %>% gg_season(Demand, period = "day") +
  theme(legend.position = "none")


vic_elec %>% gg_season(Demand, period = "week") +
  theme(legend.position = "none")

vic_elec %>% gg_season(Demand, period = "year")


# 2.5 Seasonal subseries plots -------------------------------

a10 %>%
  gg_subseries(Cost) +
  labs(
    y = "$ million",
    title = "Seasonal subseries plot: antidiabetic drug sales"
  )


holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))

holidays

autoplot(holidays, Trips) +
  labs(y = "thousands of trips",
       title = "Australian domestic holiday nights")


gg_season(holidays, Trips) +
  labs(y = "thousands of trips",
       title = "Australian domestic holiday nights")

holidays %>%
  gg_subseries(Trips) +
  labs(y = "thousands of trips",
       title = "Australian domestic holiday nights")


# 2.6 Scatterplots --------------------------------------

vic_elec %>%
  filter(year(Time) == 2014) %>%
  autoplot(Demand) +
  labs(
    y = "Demand (GW)",
    title = "Half-hourly electricity demand: Victoria"
  )


vic_elec %>%
  filter(year(Time) == 2014) %>%
  autoplot(Temperature) +
  labs(
    y = "Temperature (degrees Celsius)",
    title = "Half-hourly temperatures: Melbourne, Australia"
  )

vic_elec %>%
  filter(year(Time) == 2014) %>%
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  labs(y = "Demand (GW)", x = "Temperature (degrees Celsius)")



visitors <- tourism %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))
visitors %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State), scales = "free_y") +
  labs(y = "Number of visitor nights each quarter (millions)")


visitors %>%
  pivot_wider(values_from=Trips, names_from=State) %>%
  GGally::ggpairs(columns = 2:9)



# 2.7 Lag plots ----------------------------------------------------

recent_production <- aus_production %>%
  filter(year(Quarter) >= 2000)
recent_production %>% gg_lag(Beer, geom = "point")


# 2.8 Autocorrelation -------------------------------------------


recent_production %>% ACF(Beer, lag_max = 9)


recent_production %>%
  ACF(Beer) %>%
  autoplot()

a10 %>%
  ACF(Cost, lag_max = 48) %>%
  autoplot()



# 2.9 White noise ----------------------------------------

set.seed(30)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)
y %>% autoplot(wn) + labs(title = "White noise")

y %>%
  ACF(wn) %>%
  autoplot()



