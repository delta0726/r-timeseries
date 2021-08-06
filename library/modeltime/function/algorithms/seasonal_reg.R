# ***************************************************************************************
# Library   : modeltime
# Function  : seasonal_reg
# Created on: 2021/8/7
# URL       : https://business-science.github.io/modeltime/reference/seasonal_reg.html
# ***************************************************************************************


# ＜概要＞
# - 複数の季節性回帰モデルのインターフェイス
#   --- TBATS
#   --- STLM


# ＜構文＞
# seasonal_reg(
#   mode = "regression",
#   seasonal_period_1 = NULL,
#   seasonal_period_2 = NULL,
#   seasonal_period_3 = NULL
# )


# ＜引数＞


# ＜使用例＞
# 0 準備
# 1 STLM ETS
# 2 STLM ARIMA


# 0 準備 --------------------------------------------------------------------------------

# ライブラリ
library(dplyr)
library(parsnip)
library(rsample)
library(timetk)
library(modeltime)

# データ確認
taylor_30_min %>% print()

# データ分割
splits <- taylor_30_min %>% initial_time_split(prop = 0.8)


# 1 STLM ETS ---------------------------------------------------------------------------

# モデル構築
model_spec <-
  seasonal_reg() %>%
    set_engine("stlm_ets")

# 学習
model_fit <-
  model_spec %>%
    fit(log(value) ~ date, data = training(splits))

# 確認
model_spec %>% print()
model_fit %>% print()


# 2 STLM ARIMA -------------------------------------------------------------------------

# モデル構築
model_spec <-
  seasonal_reg() %>%
    set_engine("stlm_arima")

# 学習
model_fit <-
  model_spec %>%
    fit(log(value) ~ date, data = training(splits))

# 確認
model_spec %>% print()
model_fit %>% print()
