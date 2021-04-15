# ******************************************************************************
# Chapter   : 11 Forecasting hierarchical and grouped time series
# Title     : 
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/24
# URL       : https://otexts.com/fpp3/hts.html
# ******************************************************************************


# ＜概要＞





# 0 準備 ---------------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)
library(fabletools)


# データ準備
data(tourism)
prison_raw <- read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")

# データ確認
tourism %>% as_tibble()
tourism %>% glimpse()

# データ構造
tourism %>% tally()
tourism %>% select(-Trips) %>% map(table)


# 1 Hierarchical and grouped time series Single level approaches --------------------------------

# データ加工
# --- 州を略称表記
tourism <-
  tourism %>%
  mutate(State = recode(State,
                        `New South Wales` = "NSW",
                        `Northern Territory` = "NT",
                        `Queensland` = "QLD",
                        `South Australia` = "SA",
                        `Tasmania` = "TAS",
                        `Victoria` = "VIC",
                        `Western Australia` = "WA"))

# 集計系列の追加
# --- Aggregated
tourism_hts <-
  tourism %>%
    aggregate_key(State / Region, Trips = sum(Trips))

# プロット作成
# --- State別
tourism_hts %>%
  filter(is_aggregated(Region)) %>%
  autoplot(Trips) +
  labs(y = "Trips ('000)",
       title = "Australian tourism: national total and states") +
  facet_wrap(vars(State), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")





prison <-
  prison_raw %>%
    mutate(Quarter = yearquarter(Date)) %>%
    select(-Date)  %>%
    as_tsibble(key = c(Gender, Legal, State, Indigenous), index = Quarter) %>%
    relocate(Quarter)


prison_gts <-
  prison %>%
    aggregate_key(Gender * Legal * State, Count = sum(Count)/1e3)


prison_gts %>%
  filter(!is_aggregated(Gender), is_aggregated(Legal), is_aggregated(State)) %>%
  autoplot(Count) +
  labs(y = "Number of prisoners ('000)")


prison_gts %>%
  filter(!is_aggregated(Gender), !is_aggregated(Legal), !is_aggregated(State)) %>%
  mutate(Gender = as.character(Gender)) %>%
  ggplot(aes(x = Quarter, y = Count, group = Gender, colour=Gender)) +
  stat_summary(fun = sum, geom = "line") +
  labs(title = "Prison population by state and gender",
       y = "Number of prisoners ('000)") +
  facet_wrap(~ as.character(State), nrow = 1, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


tourism_full <- tourism %>%
  aggregate_key((State / Region) * Purpose, Trips = sum(Trips))



# 2 Single level approaches ---------------------------------

tourism_states <-
  tourism %>%
    aggregate_key(State, Trips = sum(Trips))


fcasts_state <-
  tourism_states %>%
    filter(!is_aggregated(State)) %>%
    model(ets = ETS(Trips)) %>%
    forecast()


# Sum bottom-level forecasts to get top-level forecasts
fcasts_national <-
  fcasts_state %>%
    summarise(value = sum(Trips), .mean = mean(value))

tourism_states %>%
  model(ets = ETS(Trips)) %>%
  reconcile(bu = bottom_up(ets)) %>%
  forecast()





# 4 Forecasting Australian domestic tourism ---------------------------------

# データ集計
tourism_full <-
  tourism %>%
    aggregate_key((State / Region) * Purpose, Trips = sum(Trips))

#
fit <-
  tourism_full %>%
    filter(year(Quarter) <= 2015) %>%
    model(base = ETS(Trips)) %>%
    reconcile(bu = bottom_up(base),
              ols = min_trace(base, method = "ols"),
              mint = min_trace(base, method = "mint_shrink"))



fc <- fit %>% forecast(h = "2 years")


# プロット作成
fc %>%
  filter(is_aggregated(Region), is_aggregated(Purpose)) %>%
  autoplot(tourism_full %>% filter(year(Quarter) >= 2011),
           level = NULL) +
  labs(y = "Trips ('000)") +
  facet_wrap(vars(State), scales = "free_y")

# プロット作成
fc %>%
  filter(is_aggregated(State), !is_aggregated(Purpose)) %>%
  autoplot(tourism_full %>% filter(year(Quarter) >= 2011),
           level = NULL) +
  labs(y = "Trips ('000)") +
  facet_wrap(vars(Purpose), scales = "free_y")



fc %>%
  filter(is_aggregated(State), is_aggregated(Purpose)) %>%
  accuracy(data = tourism_full,
           measures = list(rmse = RMSE, mase = MASE)) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase))


# 6 Forecasting Australian prison population ------------------------------------------------

fit <-
  prison_gts %>%
    filter(year(Quarter) <= 2014) %>%
    model(base = ETS(Count)) %>%
    reconcile(bottom_up = bottom_up(base),
              MinT = min_trace(base, method = "mint_shrink"))

fc <- fit %>% forecast(h = 8)

fc %>%
  filter(is_aggregated(State), is_aggregated(Gender), is_aggregated(Legal)) %>%
  autoplot(prison_gts, alpha = 0.7, level = 90) +
  labs(y = "Number of prisoners ('000)",
       title = "Australian prison population (total)")


fc %>%
  filter(
    .model %in% c("base", "MinT"),
    !is_aggregated(State), is_aggregated(Legal), is_aggregated(Gender)
  ) %>%
  autoplot(
    prison_gts %>% filter(year(Quarter) >= 2010),
    alpha = 0.7, level = 90
  ) +
  labs(title = "Prison population (by state)",
       y = "Number of prisoners ('000)") +
  facet_wrap(vars(State), scales = "free_y", ncol = 4) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


fc %>%
  filter(is_aggregated(State), is_aggregated(Gender), is_aggregated(Legal)) %>%
  accuracy(data = prison_gts, measures = list(
    mase = MASE,
    ss = skill_score(CRPS)
  )) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), sspc = mean(ss) * 100)
