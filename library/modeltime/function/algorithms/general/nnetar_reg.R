# ***************************************************************************************
# Library   : modeltime
# Function  : nnetar_reg
# Created on: 2021/8/7
# URL       : https://business-science.github.io/modeltime/reference/nnetar_reg.html
# ***************************************************************************************


# ＜概要＞
# - ニューラルネットワークによるカオス時系列の予測


# ＜構文＞
# nnetar_reg(
#   mode = "regression",
#   seasonal_period = NULL,
#   non_seasonal_ar = NULL,
#   seasonal_ar = NULL,
#   hidden_units = NULL,
#   num_networks = NULL,
#   penalty = NULL,
#   epochs = NULL
# )


# ＜引数＞


# ＜使用例＞
# 0 準備
# 1 NNETAR


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


# 1 NNETAR ------------------------------------------------------------------------------

# モデル構築
model_spec <-
  nnetar_reg() %>%
    set_engine("nnetar")

# 学習
set.seed(123)
model_fit <-
  model_spec %>%
    fit(log(value) ~ date, data = training(splits))

# 確認
model_spec %>% print()
model_fit %>% print()
