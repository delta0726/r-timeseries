# ******************************************************************************
# Chapter   : 8 Exponential smoothing
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/24
# URL       : https://otexts.com/fpp3/expsmooth.html
# ******************************************************************************

# ＜概要＞





# 0 準備 ---------------------------------------------------------------------------------------

library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)


# 8.1 Simple exponential smoothing  -------------------------------------------

algeria_economy <- global_economy %>%
  filter(Country == "Algeria")
algeria_economy %>%
  autoplot(Exports) +
  labs(y="Exports (% of GDP)")

# Estimate parameters
fit <- algeria_economy %>%
  model(ETS(Exports ~ error("A") + trend("N") + season("N")))
fc <- fit %>%
  forecast(h = 5)

autoplot(fc) +
  geom_line(aes(y = Exports, col="Data"), data = algeria_economy) +
  geom_line(aes(y = .fitted, col="Fitted"), data = augment(fit)) +
  labs(y="Exports (% of GDP)") +
  scale_color_manual(values=c(Data="black",Fitted="red")) +
  guides(colour = guide_legend("Series"))


# 8.2 Methods with trend --------------------------------------

aus_economy <- global_economy %>%
  filter(Code == "AUS") %>%
  mutate(Pop = Population / 1e6)
autoplot(aus_economy, Pop) + labs(y = "Millions")


fit <- aus_economy %>%
  model(AAN = ETS(Pop ~ error("A") + trend("A") + season("N")))
fc <- fit %>% forecast(h = 10)


aus_economy %>%
  model(
    `Holt's method` = ETS(Pop ~ error("A") + trend("A") + season("N")),
    `Damped Holt's method` = ETS(Pop ~ error("A") + trend("Ad", phi = 0.9) + season("N"))
  ) %>%
  forecast(h = 15) %>%
  autoplot(aus_economy, level = NULL) +
  labs(title = "Forecasts from Holt's method",
       y = "Population of Australia (millions)") +
  guides(colour = guide_legend(title = "Forecast"))


www_usage <- as_tsibble(WWWusage)
www_usage %>% autoplot(value) +
  labs(x="Minute", y="Number of users")


www_usage %>%
  stretch_tsibble(.init = 10) %>%
  model(
    SES = ETS(value ~ error("A") + trend("N") + season("N")),
    Holt = ETS(value ~ error("A") + trend("A") + season("N")),
    Damped = ETS(value ~ error("A") + trend("Ad") + season("N"))
  ) %>%
  forecast(h = 1) %>%
  accuracy(www_usage)


fit <- www_usage %>%
  model(Damped = ETS(value ~ error("A") + trend("Ad") + season("N")))
# Estimated parameters:
tidy(fit)

fit %>%
  forecast(h = 10) %>%
  autoplot(www_usage) +
  labs(x="Minute", y="Number of users")



# 8.3 Methods with seasonality -----------------------------------------------


aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  summarise(Trips = sum(Trips)/1e3)
fit <- aus_holidays %>%
  model(
    additive = ETS(Trips ~ error("A") + trend("A") + season("A")),
    multiplicative = ETS(Trips ~ error("M") + trend("A") + season("M"))
  )
fc <- fit %>% forecast(h = "3 years")

fc %>%
  autoplot(aus_holidays, level = NULL) +
  labs(y="Overnight trips (millions)")

ETS(y ~ error("M") + trend("Ad") + season("M"))

sth_cross_ped <- pedestrian %>%
  filter(Date >= "2016-07-01", Sensor == "Southern Cross Station") %>%
  index_by(Date) %>%
  summarise(Count = sum(Count))
sth_cross_ped %>%
  filter(Date <= "2016-07-31") %>%
  model(hw = ETS(Count ~ error("M") + trend("Ad") + season("M"))) %>%
  forecast(h = "2 weeks") %>%
  autoplot(sth_cross_ped %>% filter(Date <= "2016-08-14"))


# 8.6 Estimation and model selection ---------------------------------

aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  summarise(Trips = sum(Trips)/1e3)
fit <- aus_holidays %>%
  model(ETS(Trips))
report(fit)


components(fit) %>%
  autoplot() +
  labs(title = "ETS(M,N,A) components")

augment(fit)


# 8.7 Forecasting with ETS models -------------------------------


fit %>%
  forecast(h = 8) %>%
  autoplot(aus_holidays) +
  labs(y="Domestic holiday visitors in Australia (thousands)")
