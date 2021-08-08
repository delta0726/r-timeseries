# ***************************************************************************************
# Library   : timetk
# Function  : tk_augment_timeseries_signature
# Created on: 2021/8/8
# URL       : https://business-science.github.io/timetk/reference/tk_augment_timeseries.html
# ***************************************************************************************


# ＜ポイント＞
# - 日付データを持つデータフレームに日付属性を追加する


# ＜構文＞
# tk_augment_timeseries_signature(.data, .date_var = NULL)


# ＜引数＞
# .data     ：A time-based tibble / time-series object
# .date_var ：tibbleの場合に日付列を指定


# ＜使用例＞
# 0 準備
# 1 日付属性を追加


# 0 準備 ------------------------------------------------------------------------------

# ＜ポイント＞
# - tk_augment_timeseries_signature()を用いる場合はローケールを英語表記にしておく

# ライブラリ
library(dplyr)
library(timetk)


# ローケール設定変更
Sys.setlocale("LC_TIME", "English")
Sys.getlocale("LC_TIME")

# データ確認
m4_daily %>% print()
m4_daily %>% group_by(id) %>% tally()


# 1 日付属性を追加 ----------------------------------------------------------------------

# 日付属性を追加
m4_daily %>%
  group_by(id) %>%
  tk_augment_timeseries_signature(.date_var = date)


# 項目確認
m4_daily %>%
  group_by(id) %>%
  tk_augment_timeseries_signature(.date_var = date) %>%
  glimpse()
