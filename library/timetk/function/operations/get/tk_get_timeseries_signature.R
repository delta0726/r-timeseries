# ***************************************************************************************
# Library   : timetk
# Function  : tk_get_timeseries_signature
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/tk_augment_timeseries.html
# ***************************************************************************************


# ＜概要＞
# - 日付インデックスから日付要素を分解したデータフレームを作成する
#   --- データフレームから直接操作する場合はtk_augument_timeseries_signature()を使う


# ＜構文＞
# tk_get_timeseries_signature(idx)


# ＜引数＞
# idx : A time-series index that is a vector of dates or datetimes



# ＜目次＞
# 0 準備
# 1 日付属性テーブルの作成
# 2 週次データの属性取得


# 0 準備 ------------------------------------------------------------

# ライブラリ
library(dplyr)
library(tidyquant)
library(timetk)


# ローケール設定変更
# --- 日付を扱うものではローケールをEnglishにしておくほうがよい
Sys.setlocale("LC_TIME", "English")

# データ準備
FB_tbl <- FANG %>% filter(symbol == "FB")
FB_tbl %>% print()


# 1 日付属性テーブルの作成 ---------------------------------------------

# 日付インデックスの取得
FB_idx <- FB_tbl %>% tk_index()

# 日付属性テーブルの作成
FB_idx %>% tk_get_timeseries_signature()
FB_idx %>% tk_get_timeseries_signature() %>% glimpse()

# 参考：日付列から直接作成することも可能
FB_tbl$date %>% tk_get_timeseries_signature()


# 2 週次データの属性取得 ----------------------------------------------

# 週次データの作成
idx_weekly <- seq.Date(from = ymd("2016-01-01"), by = 'week', length.out = 6)
idx_weekly %>% print()

# 日付属性の取得
# --- 日付が予め絞られているので、wdayがフィルタのかかった状態のようになる
idx_weekly %>% tk_get_timeseries_signature()
idx_weekly %>% tk_get_timeseries_signature() %>% glimpse()
