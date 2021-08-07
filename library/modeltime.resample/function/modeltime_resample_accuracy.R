# ***************************************************************************************
# Library   : modeltime.resample
# Function  : modeltime_resample_accuracy
# Created on: 2021/8/8
# URL       : https://business-science.github.io/modeltime.resample/reference/modeltime_resample_accuracy.html
# ***************************************************************************************


# ＜概要＞
# - クロスバリデーションのFoldごとの予測精度を計算して、モデルごとに集計した値を出力する
#   --- 集計関数を指定するとモデル単位で出力
#   --- 集計関数を指定しないとFold単位で出力


# ＜構文＞
# modeltime_resample_accuracy(
#   object,
#   summary_fns = mean,
#   metric_set = default_forecast_accuracy_metric_set(),
#   ...
# )


# ＜引数＞
# object      ：モデルテーブル
# summary_fns ：集計関数
# metric_set  ：評価メトリック(yardstick)


# ＜使用例＞
# 0 準備
# 1 予測精度の算出


# 0 準備 --------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)
library(modeltime)
library(modeltime.resample)


# データ確認
# --- モデルテーブルでCVを実行済
m750_training_resamples_fitted

# 要素確認
m750_training_resamples_fitted$.resample_results[1]


# 1 予測精度の算出 --------------------------------------------------------------------------

# 予測精度の算出
# --- モデル単位に集計（デフォルトは平均）
m750_training_resamples_fitted %>%
    modeltime_resample_accuracy()

# 予測精度の算出
# --- Fold単位に出力
m750_training_resamples_fitted %>%
    modeltime_resample_accuracy(summary_fns = NULL)
