# ******************************************************************************
# Chapter   : 5 The forecaster’s toolbox
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/24
# URL       : https://otexts.com/fpp3/toolbox.html
# ******************************************************************************

# ＜概要＞





# 0 準備 ---------------------------------------------------------------------------------------

library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)



# 5.1 A tidy forecasting workflow ---------------------------------------------

gdppc <- global_economy %>%
  mutate(GDP_per_capita = GDP / Population)


gdppc %>%
  filter(Country == "Sweden") %>%
  autoplot(GDP_per_capita) +
  labs(y = "$US", title = "GDP per capita for Sweden")



TSLM(GDP_per_capita ~ trend())



fit <- gdppc %>%
  model(trend_model = TSLM(GDP_per_capita ~ trend()))

fit

fit %>% forecast(h = "3 years")


fit %>%
  forecast(h = "3 years") %>%
  filter(Country == "Sweden") %>%
  autoplot(gdppc) +
  labs(y = "$US", title = "GDP per capita for Sweden")


# 5.2 Some simple forecasting methods ---------------------------------------

bricks <- aus_production %>%
  filter_index("1970 Q1" ~ "2004 Q4")

bricks %>% model(MEAN(Bricks))

bricks %>% model(NAIVE(Bricks))

bricks %>% model(SNAIVE(Bricks ~ lag("year")))



bricks %>% model(RW(Bricks ~ drift()))


# Set training data from 1992 to 2006
train <- aus_production %>%
  filter_index("1992 Q1" ~ "2006 Q4")
# Fit the models
beer_fit <- train %>%
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer)
  )
# Generate forecasts for 14 quarters
beer_fc <- beer_fit %>% forecast(h = 14)
# Plot forecasts against actual values
beer_fc %>%
  autoplot(train, level = NULL) +
  autolayer(
    filter_index(aus_production, "2007 Q1" ~ .),
    color = "black"
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))


# Re-index based on trading days
google_stock <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2015) %>%
  mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE)
# Filter the year of interest
google_2015 <- google_stock %>% filter(year(Date) == 2015)
# Fit the models
google_fit <- google_2015 %>%
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = NAIVE(Close ~ drift())
  )
# Produce forecasts for the trading days in January 2016
google_jan_2016 <- google_stock %>%
  filter(yearmonth(Date) == yearmonth("2016 Jan"))
google_fc <- google_fit %>%
  forecast(new_data = google_jan_2016)
# Plot the forecasts
google_fc %>%
  autoplot(google_2015, level = NULL) +
  autolayer(google_jan_2016, Close, color = "black") +
  labs(x = "Day", y = "Closing Price (US$)",
       title = "Google stock prices (Jan 2015 - Jan 2016)") +
  guides(colour = guide_legend(title = "Forecast"))



# 5.3 Fitted values and residuals ---------------------------------

augment(beer_fit)


# 5.4 Residual diagnostics -------------------------------


autoplot(google_2015, Close) +
  labs(x = "Day", y = "Closing Price (US$)",
       title = "Google Stock in 2015")


aug <- google_2015 %>%
  model(NAIVE(Close)) %>%
  augment()
autoplot(aug, .innov) +
  labs(x = "Day", y = "Residual",
       title = "Residuals from naïve method")


aug %>%
  ggplot(aes(x = .innov)) +
  geom_histogram() +
  labs(title = "Histogram of residuals")


aug %>%
  ACF(.innov) %>%
  autoplot() +
  labs(title = "ACF of residuals")


google_2015 %>%
  model(NAIVE(Close)) %>%
  gg_tsresiduals()

aug %>% features(.innov, box_pierce, lag = 10, dof = 0)
aug %>% features(.innov, ljung_box, lag = 10, dof = 0)

fit <- google_2015 %>% model(RW(Close ~ drift()))
tidy(fit)


augment(fit) %>% features(.innov, ljung_box, lag=10, dof=1)



# 5.5 Distributional forecasts and prediction intervals

google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10) %>%
  hilo()

google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10) %>%
  autoplot(google_2015)


fit <- google_2015 %>%
  model(NAIVE(Close))
sim <- fit %>% generate(h = 30, times = 5, bootstrap = TRUE)
sim


google_2015 %>%
  ggplot(aes(x = day)) +
  geom_line(aes(y = Close)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)),
    data = sim) +
  labs(title = "Google closing stock price") +
  guides(col = FALSE)


fc <- fit %>% forecast(h = 30, bootstrap = TRUE)
fc


autoplot(fc, google_2015) +
  labs(title = "Google closing stock price")



google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10, bootstrap = TRUE, times = 1000) %>%
  hilo()

# 5.6 Forecasting using transformations ----------------------------

prices %>%
  filter(!is.na(eggs)) %>%
  model(RW(log(eggs) ~ drift())) %>%
  forecast(h = 50) %>%
  autoplot(prices %>% filter(!is.na(eggs)),
    level = 80, point_forecast = lst(mean, median)
  )



# 5.7 Forecasting with decomposition ---------------------------

us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade")
dcmp <- us_retail_employment %>%
  model(STL(Employed ~ trend(window = 7), robust = TRUE)) %>%
  components() %>%
  select(-.model)
dcmp %>%
  model(NAIVE(season_adjust)) %>%
  forecast() %>%
  autoplot(dcmp) +
  labs(y = "New orders index",
       title = "Naive forecasts of seasonally adjusted data")


fit_dcmp <- us_retail_employment %>%
  model(stlf = decomposition_model(
    STL(Employed ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))
fit_dcmp %>%
  forecast() %>%
  autoplot(us_retail_employment)

fit_dcmp %>% gg_tsresiduals()



# 5.8 Evaluating point forecast accuracy


aus_production %>% filter(year(Quarter) >= 1995)

aus_production %>%
  slice(n()-19:0)


aus_retail %>%
  group_by(State, Industry) %>%
  slice(1:12)


recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)
beer_train <- recent_production %>%
  filter(year(Quarter) <= 2007)

beer_fit <- beer_train %>%
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer),
    Drift = RW(Beer ~ drift())
  )

beer_fc <- beer_fit %>%
  forecast(h = 10)

beer_fc %>%
  autoplot(
    aus_production %>% filter(year(Quarter) >= 1992),
    level = NULL
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))


google_fit <- google_2015 %>%
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = RW(Close ~ drift())
  )

google_fc <- google_fit %>%
  forecast(google_jan_2016)

google_fc %>%
  autoplot(bind_rows(google_2015, google_jan_2016),
    level = NULL) +
  labs(x = "Day", y = "Closing Price (US$)",
       title = "Google stock prices (Jan 2015 - Jan 2016)") +
  guides(colour = guide_legend(title = "Forecast"))

accuracy(google_fc, google_stock)


# 5.9 Evaluating distributional forecast accuracy

google_fc %>%
  filter(.model == "Naïve") %>%
  autoplot(bind_rows(google_2015, google_jan_2016), level=80)


google_fc %>%
  filter(.model == "Naïve", Date == "2016-01-04") %>%
  accuracy(google_stock, list(qs=quantile_score), probs=0.10)


google_fc %>%
  filter(.model == "Naïve", Date == "2016-01-04") %>%
  accuracy(google_stock,
    list(winkler = winkler_score), level = 80)


google_fc %>%
  accuracy(google_stock, list(crps = CRPS))

google_fc %>%
  accuracy(google_stock, list(skill = skill_score(CRPS)))


# 5.10 Time series cross-validation

# Time series cross-validation accuracy
google_2015_tr <- google_2015 %>%
  stretch_tsibble(.init = 3, .step = 1)
google_2015_tr


# TSCV accuracy
google_2015_tr %>%
  model(RW(Close ~ drift())) %>%
  forecast(h = 1) %>%
  accuracy(google_2015)
# Training set accuracy
google_2015 %>%
  model(RW(Close ~ drift())) %>%
  accuracy()




google_2015_tr <- google_2015 %>%
  stretch_tsibble(.init = 3, .step = 1)
fc <- google_2015_tr %>%
  model(RW(Close ~ drift())) %>%
  forecast(h = 8) %>%
  group_by(.id) %>%
  mutate(h = row_number()) %>%
  ungroup()
fc %>%
  accuracy(google_2015, by = c("h", ".model")) %>%
  ggplot(aes(x = h, y = RMSE)) +
  geom_point()