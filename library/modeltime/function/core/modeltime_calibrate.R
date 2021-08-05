# ***************************************************************************************
# Library   : modeltime
# Function  :
# Created on: 2021/8/
# URL       :
# ***************************************************************************************


# ＜概要＞
# - キャリブレーションは各種検証をするための基礎データを作成するプロセス
#   --- 信頼区間推定（残差データ）⇒ modeltime_forecast()で使用
#   --- 予測精度計算（予測値/実績値）⇒ modeltime_accuracy()で使用


# ＜構文＞
# modeltime_calibrate(object, new_data, id = NULL, quiet = TRUE, ...)


# ＜使用例＞
# 0 準備
# 1 モデル構築
# 2 キャリブレーション


# 0 準備 -----------------------------------------------------------------------------------

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


# 1 モデル構築 ------------------------------------------------------------------------------

# モデル定義＆学習
model_fit_arima <-
  arima_reg() %>%
    set_engine(engine = "auto_arima") %>%
    fit(value ~ date, data = training(splits))

# モデルテーブルの登録
models_tbl <-
  modeltime_table(model_fit_arima)


# 2 キャリブレーション ----------------------------------------------------------------------

# 検証用データの作成
# --- テストデータを用いてデータ作成するのがセオリー
calibration_tbl <-
  models_tbl %>%
    modeltime_calibrate(new_data = testing(splits))


# 確認
calibration_tbl %>% print()

# データ確認
#   --- .residuals  ⇒ modeltime_forecast()で使用
#   --- .prediction ⇒ modeltime_accuracy()で使用
calibration_tbl$.calibration_data[1]


# 3 その他のワークフロー --------------------------------------------------------------------

# モデル精度の検証
calibration_tbl %>% modeltime_accuracy()

# 予測値の作成
# --- Actual/Predictionを結合したデータ
calibration_tbl %>%
    modeltime_forecast(new_data    = testing(splits),
                        actual_data = m750)
