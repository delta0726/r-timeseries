# ******************************************************************************
# Chapter   : 13 Some practical forecasting issues
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/24
# URL       : https://otexts.com/fpp3/practical.html
# ******************************************************************************

# ＜概要＞





# 0 準備 ---------------------------------------------------------------------------------------

library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)


# 13.1 Weekly, daily and sub-daily data ------------------------------------------------------

my_dcmp_spec <- decomposition_model(
  STL(Barrels),
  ETS(season_adjust ~ season("N"))
)
us_gasoline %>%
  model(stl_ets = my_dcmp_spec) %>%
  forecast(h = "2 years") %>%
  autoplot(us_gasoline) + labs(y = "Millions of barrels per day")


gas_dhr <- us_gasoline %>%
  model(dhr = ARIMA(Barrels ~ PDQ(0, 0, 0) + fourier(K = 6)))
gas_dhr %>%
  forecast(h = "2 years") %>%
  autoplot(us_gasoline)


# 13.2 Time series of counts --------------------------------------------------------------

j06 <- PBS %>%
  filter(ATC2 == "J06") %>%
  summarise(Scripts = sum(Scripts)) %>%
  fill_gaps(Scripts = 0)
j06 %>% autoplot(Scripts)

j06 %>%
  model(CROSTON(Scripts)) %>%
  forecast(h = 6)


# 13.3 Ensuring forecasts stay within limits -------------------------------------------

egg_prices <- prices %>% filter(!is.na(eggs))
egg_prices %>%
  model(ETS(log(eggs) ~ trend("A"))) %>%
  forecast(h = 50) %>%
  autoplot(egg_prices)

scaled_logit <- function(x, lower = 0, upper = 1) {
  log((x - lower) / (upper - x))
}
inv_scaled_logit <- function(x, lower = 0, upper = 1) {
  (upper - lower) * exp(x) / (1 + exp(x)) + lower
}
my_scaled_logit <- new_transformation(scaled_logit, inv_scaled_logit)
egg_prices %>%
  model(ETS(my_scaled_logit(eggs, lower = 50, upper = 400) ~ trend("A"))) %>%
  forecast(h = 50) %>%
  autoplot(egg_prices)


# 13.4 Forecast combinations -----------------------------------------------


auscafe <- aus_retail %>%
  filter(stringr::str_detect(Industry, "Takeaway")) %>%
  summarise(Turnover = sum(Turnover))
train <- auscafe %>%
  filter(year(Month) <= 2013)
STLF <- decomposition_model(
  STL(log(Turnover) ~ season(window = Inf)),
  ETS(season_adjust ~ season("N"))
)
cafe_models <- train %>%
  model(
    ets = ETS(Turnover),
    stlf = STLF,
    arima = ARIMA(log(Turnover))
  ) %>%
  mutate(combination = (ets + stlf + arima) / 3)
cafe_fc <- cafe_models %>%
  forecast(h = "5 years")

cafe_fc %>%
  autoplot(auscafe %>% filter(year(Month) > 2008), level = NULL) +
  labs(y = "$ billion", title = "Australian monthly expenditure on eating out")


cafe_fc %>%
  accuracy(auscafe) %>%
  arrange(RMSE)

cafe_fc %>% filter(Month == min(Month))

cafe_futures <- cafe_models %>%
  # Generate 1000 future sample paths
  generate(h = "5 years", times = 1000) %>%
  # Compute forecast distributions from future sample paths
  as_tibble() %>%
  group_by(Month, .model) %>%
  summarise(dist = distributional::dist_sample(list(.sim))) %>%
  ungroup() %>%
  # Create fable object
  as_fable(index = Month, key = .model, distribution = dist, response = "Turnover")

# Forecast distributions for h=1
cafe_futures %>% filter(Month == min(Month))


cafe_futures %>%
  filter(.model == "combination") %>%
  autoplot(auscafe %>% filter(year(Month) > 2008)) +
  labs(y = "$ billion", title = "Australian monthly expenditure on eating out")



cafe_futures %>% accuracy(auscafe, measures = interval_accuracy_measures, level = 95)


# 13.5 Prediction intervals for aggregates ------------------------------------------

fit <- auscafe %>%
  # Fit a model to the data
  model(ETS(Turnover))
futures <- fit %>%
  # Simulate 10000 future sample paths, each of length 12
  generate(times = 10000, h = 12) %>%
  # Sum the results for each sample path
  as_tibble() %>%
  group_by(.rep) %>%
  summarise(.sim = sum(.sim)) %>%
  # Store as a distribution
  summarise(total = distributional::dist_sample(list(.sim)))


futures %>%
  mutate(
    mean = mean(total),
    pi80 = hilo(total, 80),
    pi95 = hilo(total, 95)
  )


forecast(fit, h = 12) %>%
  as_tibble() %>%
  summarise(total = sum(.mean))



# 13.6 Backcasting -----------------------------------------------------------

backcasts <- auscafe %>%
  mutate(reverse_time = rev(row_number())) %>%
  update_tsibble(index = reverse_time) %>%
  model(ets = ETS(Turnover ~ season(period = 12))) %>%
  forecast(h = 15) %>%
  mutate(Month = auscafe$Month[1] - (1:15)) %>%
  as_fable(index = Month, response = "Turnover", distribution = "Turnover")
backcasts %>%
  autoplot(auscafe %>% filter(year(Month) < 1985)) +
  labs(title = "Backcasts from ETS model")


# 13.7 Very long and very short time series ----------------------------

m3totsibble <- function(z) {
  bind_rows(
    as_tsibble(z$x) %>% mutate(Type = "Training"),
    as_tsibble(z$xx) %>% mutate(Type = "Test")
  ) %>%
    mutate(
      st = z$st,
      type = z$type,
      period = z$period,
      description = z$description,
      sn = z$sn,
    ) %>%
    as_tibble()
}
short <- Mcomp::M3 %>%
  subset("yearly") %>%
  purrr::map_dfr(m3totsibble) %>%
  group_by(sn) %>%
  mutate(n = max(row_number())) %>%
  filter(n <= 20) %>%
  ungroup() %>%
  as_tsibble(index = index, key = c(sn, period, st))

short_fit <- short %>%
  model(arima = ARIMA(value))



# 13.8 Forecasting on training and test sets ---------------------------------


training <- auscafe %>% filter(year(Month) <= 2013)
test <- auscafe %>% filter(year(Month) > 2013)
cafe_fit <- training %>%
  model(ARIMA(log(Turnover)))
cafe_fit %>%
  forecast(h = 60) %>%
  autoplot(auscafe)


fits12 <- fitted(cafe_fit, h = 12)
training %>%
  autoplot(Turnover) +
  autolayer(fits12, .fitted, col = "red")


cafe_fit %>%
  refit(test) %>%
  accuracy()


# 13.9 Dealing with outliers and missing values -------------------------------.

tourism %>%
  filter(Region == "Adelaide Hills", Purpose == "Visiting") %>%
  autoplot(Trips)

ah_decomp <- tourism %>%
  filter(Region == "Adelaide Hills", Purpose == "Visiting") %>%
  # Fit a non-seasonal STL decomposition
  model(stl = STL(Trips ~ season(period = 1), robust = TRUE)) %>%
  components()
ah_decomp %>% autoplot()


outliers <- ah_decomp %>%
  filter(
    remainder < quantile(remainder, 0.25) - 3 * IQR(remainder) |
    remainder > quantile(remainder, 0.75) + 3 * IQR(remainder)
  )
outliers



ah_miss <- tourism %>%
  filter(Region == "Adelaide Hills", Purpose == "Visiting") %>%
  # Remove outlying observations
  anti_join(outliers) %>%
  # Replace with missing values
  fill_gaps()
ah_fill <- ah_miss %>%
  # Fit ARIMA model to the data containing missing values
  model(ARIMA(Trips)) %>%
  # Estimate Trips for all periods
  interpolate(ah_miss)
ah_fill %>%
  # Only show outlying periods
  right_join(outliers %>% select(-Trips))