# ***************************************************************************************
# Library   : modeltime
# Function  : naive_reg
# Created on: 2021/8/7
# URL       : https://business-science.github.io/modeltime/reference/naive_reg.html
# ***************************************************************************************


# ＜概要＞
# - NAIVE予測モデルのインターフェイス


# ＜構文＞
# naive_reg(mode = "regression", id = NULL, seasonal_period = NULL)


# ＜引数＞


# ＜使用例＞
# 0 準備
# 1 NAIVE
# 2 SEASONAL NAIVE


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


# 1 NAIVE -----------------------------------------------------------------------------

# モデル構築
model_spec <-
  naive_reg() %>%
    set_engine("naive")

# 学習
model_fit <-
  model_spec %>%
    fit(log(value) ~ date, data = training(splits))

# 確認
model_spec %>% print()
model_fit %>% print()


# 2 SEASONAL NAIVE ----------------------------------------------------------------------

# モデル構築
model_spec <-
  naive_reg(id = "id",
            seasonal_period = 12) %>%
    set_engine("snaive")

# 学習
model_fit <-
  model_spec %>%
    fit(log(value) ~ date, data = training(splits))

# 確認
model_spec %>% print()
model_fit %>% print()
