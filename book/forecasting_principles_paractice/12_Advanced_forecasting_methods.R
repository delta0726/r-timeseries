# ******************************************************************************
# Chapter   : 13 Some practical forecasting issues
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/24
# URL       : https://otexts.com/fpp3/advanced.html
# ******************************************************************************

# ＜概要＞





# 0 準備 ---------------------------------------------------------------------------------------

library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)

bank_calls %>%
  fill_gaps() %>%
  autoplot(Calls) +
  labs(y = "Call volume", x = "Date")


calls <- bank_calls %>%
  mutate(t = row_number()) %>%
  update_tsibble(index = t, regular = TRUE)

calls %>%
  model(STL(sqrt(Calls) ~ season(period = 169) + season(period = 5*169),
            robust = TRUE)) %>%
  components() %>%
  autoplot() + labs(x = "Observation")


# Forecasts from STL+ETS decomposition
my_dcmp_spec <- decomposition_model(
  STL(sqrt(Calls) ~ season(period = 169) + season(period = 5*169),
      robust = TRUE),
  ETS(season_adjust ~ season("N"))
)
fc <- calls %>%
  model(my_dcmp_spec) %>%
  forecast(h = 5 * 169)

# Add correct time stamps to fable
fc_with_times <- bank_calls %>%
  new_data(n = 7 * 24 * 60 / 5) %>%
  mutate(time = format(DateTime, format = "%H:%M:%S")) %>%
  filter(
    time %in% format(bank_calls$DateTime, format = "%H:%M:%S"),
    wday(DateTime, week_start = 1) <= 5
  ) %>%
  mutate(t = row_number() + max(calls$t)) %>%
  left_join(fc, by = "t") %>%
  as_fable(response = "Calls", distribution = Calls)

# Plot results with last 3 weeks of data
fc_with_times %>%
  fill_gaps() %>%
  autoplot(bank_calls %>% tail(14 * 169) %>% fill_gaps()) +
  labs(x = "Date", y = "Call volume")


fit <- calls %>%
  model(
    dhr = ARIMA(sqrt(Calls) ~ PDQ(0, 0, 0) + pdq(d = 0) +
                fourier(period = 169, K = 10) + fourier(period = 5*169, K = 5))
  )
fc <- fit %>% forecast(h = 5 * 169)

# Add correct time stamps to fable
fc_with_times <- bank_calls %>%
  new_data(n = 7 * 24 * 60 / 5) %>%
  mutate(time = format(DateTime, format = "%H:%M:%S")) %>%
  filter(
    time %in% format(bank_calls$DateTime, format = "%H:%M:%S"),
    wday(DateTime, week_start = 1) <= 5
  ) %>%
  mutate(t = row_number() + max(calls$t)) %>%
  left_join(fc, by = "t") %>%
  as_fable(response = "Calls", distribution = Calls)

# Plot results with last 3 weeks of data
fc_with_times %>%
  fill_gaps() %>%
  autoplot(bank_calls %>% tail(14 * 169) %>% fill_gaps()) +
  labs(x = "Date", y = "Call volume")

vic_elec %>%
  pivot_longer(Demand:Temperature, names_to = "Series") %>%
  ggplot(aes(x = Time, y = value)) +
  geom_line() +
  facet_grid(rows = vars(Series), scales = "free_y") +
  labs(x = "Time", y = "")



elec <- vic_elec %>%
  mutate(
    DOW = wday(Date, label = TRUE),
    Working_Day = !Holiday & !(DOW %in% c("Sat", "Sun")),
    Cooling = pmax(Temperature, 18)
  )
elec %>%
  ggplot(aes(x = Temperature, y = Demand, col = Working_Day)) +
  geom_point(alpha = 0.6) +
  labs(x = "Temperature (degrees Celsius)", y = "Demand (MWh)")


fit <- elec %>%
  model(
    ARIMA(Demand ~ PDQ(0, 0, 0) + pdq(d = 0) +
          Temperature + Cooling + Working_Day +
          fourier(period = "day", K = 10) +
          fourier(period = "week", K = 5) +
          fourier(period = "year", K = 3))
  )

emps <- tail(elec, 2 * 48)
fc <- fit %>%
  forecast(new_data = tail(elec, 2 * 48))
fc %>%
  autoplot(elec %>% tail(10 * 48)) +
  labs(x = "Date", y = "Demand (MWh)")



fit %>% gg_tsresiduals()


# 12.2 Prophet model -----------------------------------------------

library(fable.prophet)
cement <- aus_production %>%
  filter(year(Quarter) >= 1988)
train <- cement %>%
  filter(year(Quarter) <= 2007)
fit <- train %>%
  model(
    arima = ARIMA(Cement),
    ets = ETS(Cement),
    prophet = prophet(Cement ~ season(period = 4, order = 2, type = "multiplicative"))
  )

fc <- fit %>% forecast(h = "2 years 6 months")
fc %>% autoplot(cement)


fc %>% accuracy(cement)


fit <- elec %>%
  model(
    prophet(Demand ~ Temperature + Cooling + Working_Day +
            season(period = "day", order = 10) +
            season(period = "week", order = 5) +
            season(period = "year", order = 3))
  )
fit %>%
  components() %>%
  autoplot()

fit %>% gg_tsresiduals()

temps <- tail(elec, 2 * 48)
fc <- fit %>%
  forecast(new_data = tail(elec, 2 * 48))
fc %>%
  autoplot(elec %>% tail(10 * 48)) +
  labs(x = "Date", y = "Demand (MWh)")



# 12.3 Vector autoregressions ----------------------------------------------


fit <- us_change %>%
  model(
    aicc = VAR(vars(Consumption, Income)),
    bic = VAR(vars(Consumption, Income), ic = "bic")
  )
fit

glance(fit)


fit %>%
  augment() %>%
  ACF(.innov) %>%
  autoplot()



fit %>%
  select(aicc) %>%
  forecast() %>%
  autoplot(us_change %>% filter(year(Quarter) > 2010))


# 12.4 Neural network models ----------------------------------------------

sunspots <- sunspot.year %>% as_tsibble()
sunspots %>%
  model(NNETAR(sqrt(value))) %>%
  forecast(h = 30) %>%
  autoplot(sunspots)



fit %>%
  generate(times = 9, h = 30) %>%
  autoplot(.sim) +
  autolayer(sunspots, value) +
  theme(legend.position = "none")



# 12.5 Bootstrapping and bagging ----------------------------------------------

cement <- aus_production %>%
  filter(year(Quarter) >= 1988) %>%
  select(Quarter, Cement)
cement_stl <- cement %>%
  model(stl = STL(Cement))
cement_stl %>%
  components() %>%
  autoplot()


cement_stl %>%
  generate(new_data = cement, times = 10, bootstrap_block_size = 8) %>%
  autoplot(.sim) +
  autolayer(cement, Cement) +
  guides(colour = "none") +
  labs(y = "Bootstrapped series")



sim <- cement_stl %>%
  generate(new_data = cement, times = 100, bootstrap_block_size = 8) %>%
  select(-.model, -Cement)


ets_forecasts <- sim %>%
  model(ets = ETS(.sim)) %>%
  forecast(h = 12)
ets_forecasts %>%
  update_tsibble(key = .rep) %>%
  autoplot(.mean) +
  autolayer(cement, Cement) +
  guides(col = FALSE)


bagged <- ets_forecasts %>%
  summarise(bagged_mean = mean(.mean))
cement %>%
  model(ets = ETS(Cement)) %>%
  forecast(h = 12) %>%
  autoplot(cement) +
  autolayer(bagged, bagged_mean, col = "red")


