# ***************************************************************************************
# Library   : modeltime
# Function  : table_modeltime_accuracy
# Created on: 2021/8/
# URL       : https://business-science.github.io/modeltime/reference/table_modeltime_accuracy.html
# ***************************************************************************************


# ＜概要＞
# - modeltime::accuracy()のオブジェクトをインタラクティブなテーブルに変換する


# ＜構文＞
# table_modeltime_accuracy(
#   .data,
#   .round_digits = 2,
#   .sortable = TRUE,
#   .show_sortable = TRUE,
#   .searchable = TRUE,
#   .filterable = FALSE,
#   .expand_groups = TRUE,
#   .title = "Accuracy Table",
#   .interactive = TRUE,
#   ...
# )


# ＜使用例＞
# 0 準備
# 1 データ分割
# 2 学習
# 3 予測精度の検証


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(lubridate)
library(timetk)
library(parsnip)
library(rsample)


# データ準備
# --- 1系列のみ抽出
m750 <- m4_monthly %>% filter(id == "M750")


# データ確認
m750 %>% print()


# 1 データ分割 ------------------------------------------------------------------------------

# データ分割
splits <- m750 %>% initial_time_split(prop = 0.9)

# データ確認
splits %>% training() %>% tk_summary_diagnostics() %>% select(1:4)
splits %>% testing() %>% tk_summary_diagnostics() %>% select(1:4)


# 2 学習 ------------------------------------------------------------------------------------

# 学習済モデルの構築
# --- モデル定義＆学習
model_fit_arima <-
  arima_reg() %>%
    set_engine(engine = "auto_arima") %>%
    fit(value ~ date, data = training(splits))

# モデルテーブルに登録
models_tbl <-
  modeltime_table(model_fit_arima)


# 3 予測精度の検証 ----------------------------------------------------------------------------

# 予測精度をテーブル表示
# --- キャリブレーションで検証用データを作成
# --- メトリックの出力
# --- インタラクティブテーブルに変換
models_tbl %>%
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy()
