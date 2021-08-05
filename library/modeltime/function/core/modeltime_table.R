# ***************************************************************************************
# Library   : modeltime
# Function  : modeltime_table
# Created on: 2021/8/6
# URL       : https://business-science.github.io/modeltime/reference/modeltime_table.html
# ***************************************************************************************


# ＜概要＞
# - {modeltime}のコアコンセプトであるモデルテーブルを定義する
# - モデルテーブルはtidyrのNested Dataframeを使用している
# - 複数モデルを同時実行/並列処理するのに適したフレームワーク


# ＜構文＞
# modeltime_table(...)
# as_modeltime_table(.l)


# ＜使用例＞
# 0 準備
# 1 モデル構築
# 2 モデルテーブルの作成
# 3 その他のワークフロー


# 0 準備 -------------------------------------------------------------------------

# ライブラリ
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)
library(parsnip)
library(rsample)


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


# 2 モデルテーブルの作成 ------------------------------------------------------------

# ＜ポイント＞
# - モデルテーブルには学習済モデルを登録する


# テーブル登録
models_tbl <-
  modeltime_table(model_fit_arima)

# テーブル登録
# --- リスト化から変換
list(model_fit_arima) %>% as_modeltime_table()


# 3 その他のワークフロー ------------------------------------------------------------

# 検証用データの計算
calibration_tbl <-
  models_tbl %>%
    modeltime_calibrate(new_data = testing(splits))

# 予測精度の計算
calibration_tbl %>% modeltime_accuracy()

# 予測
df_forecast <-
  calibration_tbl %>%
      modeltime_forecast(new_data    = testing(splits),
                          actual_data = m750)

# 確認
df_forecast %>% group_by(.key) %>% tally()
