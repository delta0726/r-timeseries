# ******************************************************************************
# Chapter   : 10 Dynamic regression models
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/24
# URL       : https://otexts.com/fpp3/dynamic.html
# ******************************************************************************

# ＜概要＞





# 0 準備 ---------------------------------------------------------------------------------------

library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)




# 1 10.2 Regression with ARIMA errors using fable -----------------------------------------

us_change %>%
  gather("var", "value", Consumption, Income) %>%
  ggplot(aes(x = Quarter, y = value)) +
  geom_line() +
  facet_grid(vars(var), scales = "free_y") +
  labs(y = "",
       title = "Quarterly changes in US consumption and personal income")


fit <- us_change %>%
  model(ARIMA(Consumption ~ Income))
report(fit)


bind_rows(
    `Regression Errors` = as_tibble(residuals(fit, type = "regression")),
    `ARIMA Errors` = as_tibble(residuals(fit, type = "innovation")),
    .id = "type"
  ) %>%
  ggplot(aes(x = Quarter, y = .resid)) +
  geom_line() +
  facet_grid(vars(type), scales = "free_y") +
  labs(y = "")


fit %>% gg_tsresiduals()


augment(fit) %>%
  features(.innov, ljung_box, dof = 5, lag = 8)


# 10.3 Forecasting ----------------------------------------------------


us_change_future <- new_data(us_change, 8) %>%
  mutate(Income = mean(us_change$Income))
forecast(fit, new_data = us_change_future) %>%
  autoplot(us_change) +
  labs(y = "Percentage change")


vic_elec_daily <- vic_elec %>%
  filter(year(Time) == 2014) %>%
  index_by(Date = date(Time)) %>%
  summarise(
    Demand = sum(Demand) / 1e3,
    Temperature = max(Temperature),
    Holiday = any(Holiday)
  ) %>%
  mutate(Day_Type = case_when(
    Holiday ~ "Holiday",
    wday(Date) %in% 2:6 ~ "Weekday",
    TRUE ~ "Weekend"
  ))

vic_elec_daily %>%
  ggplot(aes(x = Temperature, y = Demand, colour = Day_Type)) +
  geom_point() +
  labs(y = "Electricity demand (GW)", x = "Maximum daily temperature")


fit <- vic_elec_daily %>%
  model(ARIMA(Demand ~ Temperature + I(Temperature^2) + (Day_Type == "Weekday")))
fit %>% gg_tsresiduals()



augment(fit) %>%
  features(.innov, ljung_box, dof = 8, lag = 14)

vic_elec_future <- new_data(vic_elec_daily, 14) %>%
  mutate(
    Temperature = 26,
    Holiday = c(TRUE, rep(FALSE, 13)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )
forecast(fit, vic_elec_future) %>%
  autoplot(vic_elec_daily) + labs(y = "Electricity demand (GW)")


# 10.4 Stochastic and deterministic trends

aus_airpassengers %>%
  autoplot(Passengers) +
  labs(x = "Year", y = "millions of people",
       title = "Total annual air passengers")

fit_deterministic <- aus_airpassengers %>%
  model(deterministic = ARIMA(Passengers ~ 1 + trend() + pdq(d = 0)))
report(fit_deterministic)


fit_stochastic <- aus_airpassengers %>%
  model(stochastic = ARIMA(Passengers ~ pdq(d = 1)))
report(fit_stochastic)


aus_airpassengers %>%
  autoplot(Passengers) +
  autolayer(fit_stochastic %>% forecast(h = 20),
    colour = "blue", alpha = 0.5, level = 95) +
  autolayer(fit_deterministic %>% forecast(h = 20),
    colour = "red", alpha = 0.5, level = 95) +
  labs(x = "Year", y = "Air passengers (millions)",
       title = "Forecasts from trend models")



# 10.5 Dynamic harmonic regression -----------------------------------------

aus_cafe <- aus_retail %>%
  filter(
    Industry == "Cafes, restaurants and takeaway food services",
    year(Month) %in% 2004:2018
  ) %>%
  summarise(Turnover = sum(Turnover))

fit <- aus_cafe %>%
  model(
    `K = 1` = ARIMA(log(Turnover) ~ fourier(K = 1) + PDQ(0, 0, 0)),
    `K = 2` = ARIMA(log(Turnover) ~ fourier(K = 2) + PDQ(0, 0, 0)),
    `K = 3` = ARIMA(log(Turnover) ~ fourier(K = 3) + PDQ(0, 0, 0)),
    `K = 4` = ARIMA(log(Turnover) ~ fourier(K = 4) + PDQ(0, 0, 0)),
    `K = 5` = ARIMA(log(Turnover) ~ fourier(K = 5) + PDQ(0, 0, 0)),
    `K = 6` = ARIMA(log(Turnover) ~ fourier(K = 6) + PDQ(0, 0, 0))
  )

fit %>%
  forecast(h = "2 years") %>%
  autoplot(aus_cafe, level = 95) +
  facet_wrap(vars(.model), ncol = 2) +
  guides(colour = FALSE, fill = FALSE, level = FALSE) +
  geom_label(
    aes(x = yearmonth("2007 Jan"), y = 4250, label = paste0("AICc = ", format(AICc))),
    data = glance(fit)
  )


# 10.6 Lagged predictors ----------------------------------------


insurance %>%
  pivot_longer(Quotes:TVadverts) %>%
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(vars(name), scales = "free_y") +
  labs(y = "", title = "Insurance advertising and quotations")


fit <- insurance %>%
  # Restrict data so models use same fitting period
  mutate(Quotes = c(NA, NA, NA, Quotes[4:40])) %>%
  # Estimate models
  model(
    lag0 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts),
    lag1 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts)),
    lag2 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts) + lag(TVadverts, 2)),
    lag3 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts) + lag(TVadverts, 2) + lag(TVadverts, 3))
  )


glance(fit)


fit_best <- insurance %>%
  model(ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts)))
report(fit_best)

insurance_future <- new_data(insurance, 20) %>%
  mutate(TVadverts = 8)
fit_best %>%
  forecast(insurance_future) %>%
  autoplot(insurance) +
  labs(y = "Quotes",
       title = "Forecast quotes with future advertising set to 8")

