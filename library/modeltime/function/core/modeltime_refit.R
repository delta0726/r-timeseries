# ***************************************************************************************
# Library   : modeltime
# Function  : modeltime_refit
# Created on: 2021/8/7
# URL       : https://business-science.github.io/modeltime/reference/modeltime_refit.html
# ***************************************************************************************


# ＜概要＞
# - 新しいデータでモデルテーブルの登録モデルを再学習する
#   --- モデルを更新する作業のみで、新しいモデルでのキャリブレーションは別途行う必要がある


# ＜構文＞
#.modeltime_refit(object, data, ..., control = control_refit())


# ＜使用例＞
# 0 準備
# 1 モデル構築
# 2 モデルテーブルの登録
# 3 検証用データの作成
# 4 新しいデータで再学習
# 5 再学習データで検証用データを作成


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(lubridate)
library(timetk)
library(parsnip)
library(rsample)
library(modeltime)

# データ準備
m750 <- m4_monthly %>% filter(id == "M750")

# データ分割
splits <- m750 %>% initial_time_split(prop = 0.9)


# 1 モデル構築 ---------------------------------------------------------------------

# モデル構築＆学習
# --- Auto Arima
model_fit_arima <-
  arima_reg() %>%
    set_engine(engine = "auto_arima") %>%
    fit(value ~ date, data = training(splits))


# 2 モデルテーブルの登録 --------------------------------------------------------------

# テーブル登録
models_tbl <-
  modeltime_table(model_fit_arima)


# 3 検証用データの作成 ---------------------------------------------------------------

# キャリブレーション
calibration_tbl <-
  models_tbl %>%
    modeltime_calibrate(new_data = testing(splits))

# 確認
calibration_tbl %>% print()

# データ確認
#   --- .prediction ⇒ modeltime_accuracy()で使用
#   --- .residuals  ⇒ modeltime_forecast()で使用
calibration_tbl$.calibration_data[1]


# 4 新しいデータで再学習 ----------------------------------------------------------------

# ＜ポイント＞
# - リフィットは新しいデータでモデルを更新する作業（.model列）
#   --- キャリブレーションデータはこの時点で更新されていない点に注意（.calibration_data列）


# リフィット
# --- データを変更して再学習
refit_tbl <-
  calibration_tbl %>%
    modeltime_refit(data = m750)

# 確認
# --- 表面上はリフィット前と変化がない
calibration_tbl %>% print()
refit_tbl %>% print()

# 確認
# --- モデルはちゃんと更新されている
calibration_tbl$.model[1]
refit_tbl$.model[1]

# 確認
# --- リフィット時点では、.calibration_dataは更新されていない！
calibration_tbl$.calibration_data[1]
refit_tbl$.calibration_data[1]


# 5 再学習データで検証用データを作成 ------------------------------------------------------

# ＜ポイント＞
# - リフィット後のデータを用いて予測精度検証や予測をしたければ、再度キャリブレーションする必要がある


# リフィット後のデータで検証データを作成
calibration_tbl_refit <-
  refit_tbl %>%
    modeltime_calibrate(new_data = testing(splits))

# 確認
calibration_tbl$.calibration_data[1]
calibration_tbl_refit$.calibration_data[1]
