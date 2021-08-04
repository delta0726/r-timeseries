# ***************************************************************************************
# Library   : modeltime
# Function  : default_forecast_accuracy_metric_set
# Created on: 2021/8/5
# URL       : https://business-science.github.io/modeltime/reference/default_forecast_accuracy_metric_set.html
# ***************************************************************************************


# ＜概要＞
# - modeltime_accuracy()で使用するデフォルトのメトリックを指定して、予測精度計算の関数を定義する
#   --- メトリックは{yardstick}の関数として定義されたものを使用する
#   --- ｢関数｣を作る関数なので珍しいパターン（yardstick::metric_set()のラッパー関数）


# ＜構文＞
# default_forecast_accuracy_metric_set(...)


# ＜引数＞
# ... ： {yardstick}のメトリック


# ＜詳細＞
# - 初期設定のデフォルトメトリックは以下のとおり
# - 時系列予測の評価には回帰系のメトリックを使用する

# 1 mae    numeric_metric minimize
# 2 mape   numeric_metric minimize
# 3 mase   numeric_metric minimize
# 4 smape  numeric_metric minimize
# 5 rmse   numeric_metric minimize
# 6 rsq    numeric_metric maximize


# ＜使用例＞
# 0 準備
# 1 デフォルト状態で使用
# 2 カスタマイズして使用


# 0 準備 ------------------------------------------------------------------------------------

# ライブラリ
library(tibble)
library(dplyr)
library(timetk)
library(yardstick)
library(modeltime)


# データ準備
fake_data <-
  tibble(y    = c(1:12, 2*1:12),
         yhat = c(1 + 1:12, 2*1:12 - 1))

# データ確認
fake_data %>% print()


# 1 デフォルト状態で使用 -----------------------------------------------------------------------------

# デフォルトメトリックの確認
default_forecast_accuracy_metric_set()

# 予測精度計算の関数定義
# --- デフォルト状態
calc_default_metrics <- default_forecast_accuracy_metric_set()

# 予測精度の計算
fake_data %>% calc_default_metrics(y, yhat)


# 2 カスタマイズして使用 -----------------------------------------------------------------------------

# カスタムメトリックを作成
# --- mase() with seasonality = 12 (monthly)
mase12 <- metric_tweak(.name = "mase12", .fn = mase, m = 12)

# 予測精度計算の関数定義
# --- カスタムメトリックを追加
my_metric_set <- default_forecast_accuracy_metric_set(mase12)
my_metric_set

# 予測精度の計算
fake_data %>% my_metric_set(y, yhat)
