# ***************************************************************************************
# Library   : modeltime
# Function  : arima_reg
# Created on: 2021/8/7
# URL       : https://business-science.github.io/modeltime/reference/arima_reg.html
# ***************************************************************************************


# ＜概要＞
# - ARIMAモデルのインターフェース


# ＜構文＞
# arima_reg(
#   mode = "regression",
#   seasonal_period = NULL,
#   non_seasonal_ar = NULL,
#   non_seasonal_differences = NULL,
#   non_seasonal_ma = NULL,
#   seasonal_ar = NULL,
#   seasonal_differences = NULL,
#   seasonal_ma = NULL
# )


# ＜引数＞


# ＜使用例＞
# 0 準備
# 1 AUTO ARIMA
# 2 STANDARD ARIMA


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


# 1 AUTO ARIMA -------------------------------------------------------------------------

# モデル構築
model_spec <-
  arima_reg() %>%
    set_engine("auto_arima")

# 学習
model_fit <-
  model_spec %>%
    fit(log(value) ~ date, data = training(splits))

# 確認
model_spec %>% print()
model_fit %>% print()


# 2 STANDARD ARIMA ----------------------------------------------------------------------

# モデル構築
model_spec <-
  arima_reg(seasonal_period          = 12,
            non_seasonal_ar          = 3,
            non_seasonal_differences = 1,
            non_seasonal_ma          = 3,
            seasonal_ar              = 1,
            seasonal_differences     = 0,
            seasonal_ma              = 1) %>%
    set_engine("arima")

# 学習
model_fit <-
  model_spec %>%
    fit(log(value) ~ date, data = training(splits))

# 確認
model_spec %>% print()
model_fit %>% print()
