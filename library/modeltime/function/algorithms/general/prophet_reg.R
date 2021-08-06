# ***************************************************************************************
# Library   : modeltime
# Function  : prophet_reg
# Created on: 2021/8/7
# URL       : https://business-science.github.io/modeltime/reference/prophet_reg.html
# ***************************************************************************************


# ＜概要＞
# - プロフェット回帰モデル


# ＜構文＞
# prophet_reg(
#   mode = "regression",
#   growth = NULL,
#   changepoint_num = NULL,
#   changepoint_range = NULL,
#   seasonality_yearly = NULL,
#   seasonality_weekly = NULL,
#   seasonality_daily = NULL,
#   season = NULL,
#   prior_scale_changepoints = NULL,
#   prior_scale_seasonality = NULL,
#   prior_scale_holidays = NULL,
#   logistic_cap = NULL,
#   logistic_floor = NULL
# )


# ＜引数＞


# ＜使用例＞
# 0 準備
# 1 モデル構築
# 2 モデル学習


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


# 1 モデル構築 -------------------------------------------------------------------------

# モデル構築
model_spec <-
  prophet_reg() %>%
    set_engine("prophet")

# 確認
model_spec %>% print()


# 2 モデル学習 --------------------------------------------------------------------------

# 学習
model_fit <-
  model_spec %>%
    fit(log(value) ~ date, data = training(splits))

# 確認
model_fit %>% print()
