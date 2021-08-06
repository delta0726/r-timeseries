# ***************************************************************************************
# Library   : modeltime
# Function  : arima_boost
# Created on: 2021/8/7
# URL       : https://business-science.github.io/modeltime/reference/arima_boost.html
# ***************************************************************************************


# ＜概要＞
# - ARIMAブーストモデルのインターフェース


# ＜構文＞
# arima_boost(
#   mode = "regression",
#   seasonal_period = NULL,
#   non_seasonal_ar = NULL,
#   non_seasonal_differences = NULL,
#   non_seasonal_ma = NULL,
#   seasonal_ar = NULL,
#   seasonal_differences = NULL,
#   seasonal_ma = NULL,
#   mtry = NULL,
#   trees = NULL,
#   min_n = NULL,
#   tree_depth = NULL,
#   learn_rate = NULL,
#   loss_reduction = NULL,
#   sample_size = NULL,
#   stop_iter = NULL
# )


# ＜引数＞



# ＜使用例＞
# 0 準備
# 1 ARIMA Boost


# 0 準備 --------------------------------------------------------------------------------

# ライブラリ
library(dplyr)
library(parsnip)
library(rsample)
library(timetk)
library(modeltime)


# データ準備
# --- 1系列を抽出
m750 <- m4_monthly %>% filter(id == "M750")

# データ確認
m750 %>% print()

# データ分割
splits <- m750 %>% initial_time_split(prop = 0.8)


# 1 ARIMA Boost -------------------------------------------------------------------------

# モデル構築
model_spec <-
  arima_boost(seasonal_period = 12,
              non_seasonal_ar = 0,
              non_seasonal_differences = 1,
              non_seasonal_ma = 1,
              seasonal_ar     = 0,
              seasonal_differences = 1,
              seasonal_ma     = 1,
              tree_depth = 6,
              learn_rate = 0.1) %>%
    set_engine(engine = "arima_xgboost")

# 学習
model_fit <-
  model_spec %>%
    fit(value ~ date + as.numeric(date) + month(date, label = TRUE),
        data = training(splits))

# 確認
model_spec %>% print()
model_fit %>% print()
