# ***************************************************************************************
# Library   : modeltime.resample
# Function  : unnest_modeltime_resamples
# Created on: 2021/8/8
# URL       : https://business-science.github.io/modeltime.resample/reference/unnest_modeltime_resamples.html
# ***************************************************************************************


# ＜概要＞
# - モデルテーブルに生成されたクロスバリデーション結果をデータフレームに変換する


# ＜構文＞
# unnest_modeltime_resamples(object)


# ＜引数＞
# object ： '.resample_results'の列を含むモデルテーブル


# ＜使用例＞
# 0 準備
# 1 モデルテーブルのフラット化


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


# 1 モデルテーブルのフラット化 ---------------------------------------------------------

# ＜ポイント＞
# - モデルテーブルのリサンプリングデータをフラット化する

# フラット化
m750_training_resamples_fitted %>%
  unnest_modeltime_resamples()
