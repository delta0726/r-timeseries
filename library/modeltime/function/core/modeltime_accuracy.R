# ***************************************************************************************
# Library   : modeltime
# Function  : modeltime_accuracy
# Created on: 2021/8/
# URL       : https://business-science.github.io/modeltime/reference/modeltime_accuracy.html
# ***************************************************************************************


# ＜概要＞
# - キャリブレーションされたモデルテーブルから予測精度のメトリックを算出する
#   --- メトリックは{yardstick}のものを用いる


# ＜構文＞
# modeltime_accuracy(
#   object,
#   new_data = NULL,
#   metric_set = default_forecast_accuracy_metric_set(),
#   acc_by_id = FALSE,
#   quiet = TRUE,
#   ...
# )


# ＜引数＞
# metric_set ：yardstick::metric_set()で指定（デフォルトはdefault_forecast_accuracy_metric_set）
# acc_by_id  ：idごとに予測精度を検証（キャリブレーションでidを指定する必要あり）


# ＜使用例＞
# 0 準備
# 1 モデル構築
# 2 モデル精度の検証


# 0 準備 ----------------------------------------------------------------------------------

# ライブラリ
library(modeltime)
library(tidymodels)
library(tidyverse)
library(lubridate)
library(timetk)

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


# 2 モデル精度の検証 -------------------------------------------------------------------------

# モデル精度の検証
# --- 事前にキャリブレーションを行う
models_tbl %>%
    modeltime_calibrate(new_data = testing(splits)) %>%
    modeltime_accuracy(metric_set = metric_set(mae, rmse, rsq))
