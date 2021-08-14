# ***************************************************************************************
# Library   : timetk
# Function  : tk_get_timeseries_summary
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/tk_augment_timeseries.html
# ***************************************************************************************


# ＜概要＞
# - 日付インデックスからサマリーを作成する
# - Date型以外の日付オブジェクトからもサマリーの作成が可能


# ＜構文＞
# tk_get_timeseries_summary(idx)


# ＜引数＞
# idx : A time-series index that is a vector of dates or datetimes


# ＜目次＞
# 0 準備
# 1 日付サマリーの取得


# 0 準備 ------------------------------------------------------------

# ライブラリ
library(dplyr)
library(tidyquant)
library(timetk)


# データ準備
FB_tbl <- FANG %>% filter(symbol == "FB")
FB_tbl %>% print()


# 1 日付サマリーの取得 ---------------------------------------------------

# 日付インデックスの取得
FB_idx <- FB_tbl %>% tk_index()

# 日付サマリーの取得
FB_idx %>% tk_get_timeseries_summary()
