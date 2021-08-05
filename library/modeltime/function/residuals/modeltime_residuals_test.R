# ***************************************************************************************
# Library   : modeltime
# Function  : modeltime_residuals_test
# Created on: 2021/8/6
# URL       : https://business-science.github.io/modeltime/reference/modeltime_residuals_test.html
# ***************************************************************************************


# ＜概要＞
# - 残差データに対して検定統計量のp値を計算する
#   --- シャピロ・ウィルク検定（残差の正規性）
#   --- Box-Pierce検定（残差の自己相関）
#   --- Ljung-Box検定（残差の自己相関）
#   --- ダービン-ワトソン検定（残差の自己相関）


# ＜構文＞
# modeltime_residuals_test(object, new_data = NULL, lag = 1, fitdf = 0, ...)


# ＜検定＞
# シャピロ・ウィルク検定
# - 残差の正規性をテストする
# - 帰無仮説は、残差が正規分布しているというもの
# - 特定の有意水準を下回る低いP値は、値が正規分布していないことを示す

# Box-Pierce検定およびLjung-Box検定
# - 残差の自己相関の欠如を検定する方法
# - 特定の有意水準を下回る低いp値は、値が自己相関していることを示す

# ダービン-ワトソン検定
# - 残差の自己相関の欠如を検定する方法
# - ダービンワトソン検定は、0から4までの値で検定統計量を出力する
# - 2は自己相関なし（良い）
# - 0から<2は正の自己相関です（時系列データで一般的）
# - 2から4は負の自己相関です（時系列データではあまり一般的ではありません）


# ＜使用例＞
# 0 準備
# 1 モデル構築
# 2 残差データの統計的検定


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
splits <- initial_time_split(m750, prop = 0.9)


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


# 2 残差データの統計的検定 -----------------------------------------------------------------

# 検定統計量の出力
# --- インサンプル
models_tbl %>%
  modeltime_calibrate(new_data = training(splits)) %>%
  modeltime_residuals() %>%
  modeltime_residuals_test()

# 検定統計量の出力
# --- アウトサンプル
models_tbl %>%
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_residuals() %>%
  modeltime_residuals_test()
