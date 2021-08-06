# ***************************************************************************************
# Library   : modeltime
# Function  : modeltime_forecast
# Created on: 2021/8/7
# URL       : https://business-science.github.io/modeltime/reference/modeltime_forecast.html
# ***************************************************************************************


# ＜概要＞
# - モデルテーブルに登録した各モデルの予測を行う
#   --- モデルテーブルは複数モデルを登録できるが、予測用データは同じものを渡す（同じ期間を異なるモデルで予測）


# ＜構文＞
# modeltime_forecast(
#   object,
#   new_data = NULL,
#   h = NULL,
#   actual_data = NULL,
#   conf_interval = 0.95,
#   conf_by_id = FALSE,
#   keep_data = FALSE,
#   arrange_index = FALSE,
#   ...
# )


# ＜引数＞
# object       ：モデルテーブル（キャリブレーション前/後の両方を使用可能、後を使うのがセオリー）
# new_data     ：予測する将来データ（NULLの場合はキャリブレーションのデータを使用）
# h            ：外部変数のない時系列モデルの場合は使用可能（キャリブレーションデータの日付をh期間延長）
# actual_data  ：実際データを指定すると、予測データに結合して出力
# conf_interval：信頼区間の値
# conf_by_id   ：パネルデータのキーid（キャリブレーションの段階で指定する必要がリ）


# ＜使用例＞
# 0 準備
# 1 モデル構築
# 2 モデルテーブルの登録
# 3 検証用データの作成
# 4 予測精度の検証
# 5 将来データの予測


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


# 4 予測精度の検証 ---------------------------------------------------------------------

# メトリックの算出
calibration_tbl %>% modeltime_accuracy()


# 5 将来データの予測 ------------------------------------------------------------------

# ＜ポイント＞
# - モデルテーブルから作成するが、キャリブレーションの有無で出力が異なる（信頼区間の有無）
# - plot_modeltime_forecast()でのプロット作成を想定して実際データを結合して出力することが可能


# キャリブレーションからの予測
# --- 信頼区間あり（残差データから作成）
# --- actual_data引数を渡すと、予測データと実際データが結合される（予測期間はデータが重複）
calibration_tbl %>%
    modeltime_forecast(new_data    = testing(splits),
                       actual_data = m750)

# モデルテーブルから予測
# --- 信頼区間なし
# --- actual_data引数を渡すと、予測データと実際データが結合される（予測期間はデータが重複）
models_tbl %>%
    modeltime_forecast(new_data    = testing(splits),
                       actual_data = m750)

# キャリブレーションからの予測
calibration_tbl %>%
    modeltime_forecast(new_data  = testing(splits),
                       keep_data = TRUE)
