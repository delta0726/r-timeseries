# ***************************************************************************************
# Library   : timetk
# Function  : tk_augment_holiday_signature
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/tk_get_holiday.html
# ***************************************************************************************


# ＜概要＞
# - 日付インデックスからカレンダーテーブルを生成する


# ＜構文＞
# tk_get_holiday_signature(
#   idx,
#   holiday_pattern = ".",
#   locale_set = c("all", "none", "World", "US", "CA", "GB", "FR", "IT", "JP", "CH",
#     "DE"),
#   exchange_set = c("all", "none", "NYSE", "LONDON", "NERC", "TSX", "ZURICH")
# )


# ＜目次＞
# 0 準備
# 1 カレンダーの生成
# 2 特定カレンダーの抽出


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidyquant)
library(timetk)


# 日付ベクトルの生成
idx <- tk_make_timeseries("2017-01-01", "2017-12-31", by = "day")
idx


# 1 カレンダーの生成 ----------------------------------------------------------

# カレンダーの出力
idx %>% tk_get_holiday_signature()

# カレンダー一覧
# ローカルカレンダー
idx %>% tk_get_holiday_signature() %>% names() %>% sort()


# 2 特定カレンダーの抽出 -------------------------------------------------------

# 指定カレンダーを抽出
idx %>%
  tk_get_holiday_signature(holiday_pattern = "(US_NewYears)|(US_Christmas)|(US_Thanks)",
                           locale_set      = "none",
                           exchange_set    = "none")
