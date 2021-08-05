# ***************************************************************************************
# Library   : modeltime
# Function  : modeltime_residuals
# Created on: 2021/8/6
# URL       : https://business-science.github.io/modeltime/reference/modeltime_residuals.html
# ***************************************************************************************


# ＜概要＞
# - モデルテーブルをunnestして残差データを取得するためのユーティリティ関数
#   --- 残差データの計算自体はmodeltime_calibrate()で行う


# ＜構文＞
# modeltime_residuals(object, new_data = NULL, quiet = TRUE, ...)


# ＜使用例＞
# 0 準備
# 1 モデル構築
# 2 予測


# 0 準備 --------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(lubridate)
library(timetk)
library(parsnip)
library(rsample)
library(modeltime)


# データ準備
# --- 1系列
m750 <- m4_monthly %>% filter(id == "M750")

# データ分割
splits <- m750 %>% initial_time_split(prop = 0.9)


# 1 モデル構築 ---------------------------------------------------------------------------

# モデル構築＆学習
# --- Auto Arima
model_fit_arima <-
  arima_reg() %>%
    set_engine(engine = "auto_arima") %>%
    fit(value ~ date, data = training(splits))

# モデルテーブルに登録
models_tbl <-
  modeltime_table(model_fit_arima)


# 2 予測 -------------------------------------------------------------------------------

# 残差データの取得
# --- インサンプル
model_residuals_tr <-
  models_tbl %>%
    modeltime_calibrate(new_data = training(splits)) %>%
    modeltime_residuals()

# 残差データの取得
# --- アウトサンプル
model_residuals_te <-
  models_tbl %>%
    modeltime_calibrate(new_data = testing(splits)) %>%
    modeltime_residuals()

# プロット作成
model_residuals_tr %>% plot_modeltime_residuals(.interactive = FALSE)
model_residuals_te %>% plot_modeltime_residuals(.interactive = FALSE)
