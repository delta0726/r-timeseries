# ******************************************************************************
# Chapter   : 9 ARIMA models
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/24
# URL       : https://otexts.com/fpp3/arima.html
# ******************************************************************************

# ＜概要＞





# 0 準備 ---------------------------------------------------------------------------------------

library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)



# 9.1 Stationarity and differencing --------------------------------------------------

google_2015 %>%
  mutate(diff_close = difference(Close)) %>%
  features(diff_close, ljung_box, lag = 10)

PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost)/1e6) %>%
  transmute(
    `Sales ($million)` = Cost,
    `Log sales` = log(Cost),
    `Annual change in log sales` = difference(log(Cost), 12),
    `Doubly differenced log sales` = difference(difference(log(Cost), 12), 1)
  ) %>%
  pivot_longer(-Month, names_to="Type", values_to="Sales") %>%
  mutate(
    Type = factor(Type, levels = c(
      "Sales ($million)",
      "Log sales",
      "Annual change in log sales",
      "Doubly differenced log sales"))
  ) %>%
  ggplot(aes(x = Month, y = Sales)) +
  geom_line() +
  facet_grid(vars(Type), scales = "free_y") +
  labs(title = "Corticosteroid drug sales", y = NULL)



google_2015 %>%
  features(Close, unitroot_kpss)
#> # A tibble: 1 x 3
#>   Symbol kpss_stat kpss_pvalue
#>   <chr>      <dbl>       <dbl>
#> 1 GOOG        3.56        0.01


google_2015 %>%
  mutate(diff_close = difference(Close)) %>%
  features(diff_close, unitroot_kpss)
#> # A tibble: 1 x 3
#>   Symbol kpss_stat kpss_pvalue
#>   <chr>      <dbl>       <dbl>
#> 1 GOOG      0.0989         0.1

google_2015 %>%
  features(Close, unitroot_ndiffs)
#> # A tibble: 1 x 2
#>   Symbol ndiffs
#>   <chr>   <int>
#> 1 GOOG        1

aus_total_retail <- aus_retail %>%
  summarise(Turnover = sum(Turnover))
aus_total_retail %>%
  mutate(log_turnover = log(Turnover)) %>%
  features(log_turnover, unitroot_nsdiffs)
#> # A tibble: 1 x 1
#>   nsdiffs
#>     <int>
#> 1       1

aus_total_retail %>%
  mutate(log_turnover = difference(log(Turnover), 12)) %>%
  features(log_turnover, unitroot_ndiffs)
#> # A tibble: 1 x 1
#>   ndiffs
#>    <int>
#> 1      1


# 9.5 Non-seasonal ARIMA models --------------------------------


global_economy %>%
  filter(Code == "EGY") %>%
  autoplot(Exports) +
  labs(y = "Percentage of GDP", title = "Egyptian Exports")

fit <- global_economy %>%
  filter(Code == "EGY") %>%
  model(ARIMA(Exports))
report(fit)

fit %>% forecast(h=10) %>% autoplot(global_economy)

global_economy %>% filter(Code == "EGY") %>% ACF(Exports) %>% autoplot()


global_economy %>% filter(Code == "EGY") %>% PACF(Exports) %>% autoplot()


fit2 <- global_economy %>%
  filter(Code == "EGY") %>%
  model(ARIMA(Exports ~ pdq(4,0,0)))
report(fit2)


# 9.7 ARIMA modelling in fable -----------------------------------------------

global_economy %>%
  filter(Code == "CAF") %>%
  autoplot(Exports)


global_economy %>%
  filter(Code == "CAF") %>%
  gg_tsdisplay(difference(Exports), plot_type='partial')


caf_fit <- global_economy %>%
  filter(Code == "CAF") %>%
  model(
    arima210 = ARIMA(Exports ~ pdq(2,1,0)),
    arima013 = ARIMA(Exports ~ pdq(0,1,3)),
    stepwise = ARIMA(Exports),
    search = ARIMA(Exports, stepwise=FALSE)
  )
caf_fit

caf_fit %>%
  select(search) %>%
  gg_tsresiduals()

augment(caf_fit) %>%
  filter(.model=='search') %>%
  features(.innov, ljung_box, lag = 10, dof = 3)


caf_fit %>%
  forecast(h=5) %>%
  filter(.model=='search') %>%
  autoplot(global_economy)


gg_arma(caf_fit %>% select(Country, search))



# 9.9 Seasonal ARIMA models ----------------------------------------

leisure <- us_employment %>%
  filter(Title == "Leisure and Hospitality", year(Month) > 2000) %>%
  mutate(Employed = Employed/1000) %>%
  select(Month, Employed)
autoplot(leisure, Employed) +
  labs(y="Millions of people")


leisure %>%
  gg_tsdisplay(difference(Employed, 12), plot_type='partial', lag=36) +
  labs(y="Seasonally differenced")


leisure %>%
  gg_tsdisplay(difference(Employed, 12) %>% difference(), plot_type='partial', lag=36) +
  labs(y="Double differenced")


fit <- leisure %>%
  model(
    arima012011 = ARIMA(Employed ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(Employed ~ pdq(2,1,0) + PDQ(0,1,1)),
    auto = ARIMA(Employed, stepwise=FALSE, approximation=FALSE)
  )
fit


fit %>% select(auto) %>% gg_tsresiduals(lag=36)


augment(fit) %>% features(.innov, ljung_box, lag=24, dof=4)

fit %>% forecast(h=36) %>% filter(.model=='auto') %>% autoplot(leisure)


h02 <- PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost)/1e6)
h02 %>%
  mutate(log(Cost)) %>%
  pivot_longer(-Month) %>%
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(y="", title="Cortecosteroid drug scripts (H02)")

h02 %>% gg_tsdisplay(difference(log(Cost), 12), plot_type='partial', lag_max = 24)


fit <- h02 %>%
  model(ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2)))
report(fit)


fit %>% gg_tsresiduals(lag_max=36)


augment(fit) %>%
  features(.innov, ljung_box, lag = 36, dof = 6)



h02 %>%
  model(ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2))) %>%
  forecast() %>%
  autoplot(h02) +
    labs(y="H02 sales (million scripts)")


# 9.10 ARIMA vs ETS -----------------------------------------------------

aus_economy <- global_economy %>% filter(Code == "AUS") %>%
  mutate(Population = Population/1e6)

aus_economy %>%
  slice(-n()) %>%
  stretch_tsibble(.init = 10) %>%
  model(
    ETS(Population),
    ARIMA(Population)
  ) %>%
  forecast(h = 1) %>%
  accuracy(aus_economy)


aus_economy %>%
  model(ETS(Population)) %>%
  forecast(h = "5 years") %>%
  autoplot(aus_economy)


cement <- aus_production %>% filter_index("1988 Q1" ~ .)
train <- cement %>% filter_index(. ~ "2007 Q4")


fit_arima <- train %>% model(ARIMA(Cement))
report(fit_arima)

gg_tsresiduals(fit_arima, lag_max = 16)

augment(fit_arima) %>%
  features(.innov, ljung_box, lag = 16, dof = 6)


fit_ets <- train %>% model(ETS(Cement))
report(fit_ets)


fit_ets %>% gg_tsresiduals(lag_max = 16)


augment(fit_ets) %>%
  features(.innov, ljung_box, lag = 16, dof = 6)

bind_rows(
  fit_arima %>% accuracy(),
  fit_ets %>% accuracy(),
  fit_arima %>% forecast(h = "2 years 6 months") %>%
    accuracy(cement),
  fit_ets %>% forecast(h = "2 years 6 months") %>%
    accuracy(cement)
)


cement %>%
  model(ARIMA(Cement)) %>%
  forecast(h="3 years") %>%
  autoplot(cement)