# ***************************************************************************************
# Library   : timetk
# Function  : tk_make_timeseries
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/tk_make_timeseries.html
# ***************************************************************************************


# ＜概要＞
# - 元の日付ベクトルを元に、将来の日付ベクトルを生成する
#   --- 開始日を指定しない点に注意


# ＜構文＞
# tk_make_future_timeseries(
#   idx,
#   length_out,
#   inspect_weekdays = FALSE,
#   inspect_months = FALSE,
#   skip_values = NULL,
#   insert_values = NULL,
#   n_future = NULL
# )


# ＜目次＞
# 0 準備
# 1 将来時間の取得
# 2 将来日付の取得
# 3 実践的な使用例


# 0 準備 -------------------------------------------------------------------------

# ライブラリ
library(dplyr)
library(tidyquant)
library(timetk)


# 時間ベクトルの作成
# --- これを基準に将来日付を生成する
idx_sec <- tk_make_timeseries(start_date = "2016-01-01 00:00:00", by = "3 sec", length_out = 3)
idx_sec

# 日付ベクトルの作成
# --- これを基準に将来日付を生成する
idx_day <- tk_make_timeseries(start_date = "2016-01-01", by = "1 month", length_out = "12 months")
idx_day


# 1 将来時間の取得 ----------------------------------------------------------------

# 要素数で指定
idx_sec %>% tk_make_future_timeseries(length_out = 3)

# 期間キーワードで指定
idx_sec %>% tk_make_future_timeseries(length_out = "6 sec")


# 2 将来日付の取得 ----------------------------------------------------------------

# 要素数で指定
idx_day %>% tk_make_future_timeseries(length_out = 3)

# 期間キーワードで指定
idx_day %>% tk_make_future_timeseries(length_out = "12 months")


# 3 実践的な使用例 ----------------------------------------------------------------

# データ準備
FB_tbl <- FANG %>% filter(symbol == "FB")

# データ確認
FB_tbl %>% print()
FB_tbl %>% tk_summary_diagnostics() %>% select(1:4)

# 休日ベクトルの取得
holidays <- tk_make_holiday_sequence(start_date = "2017-01-01",
                                     end_date   = "2017-12-31",
                                     calendar   = "NYSE")

# 将来日付のベクトルを取得
# --- 元データは2016-12-30まで
FB_tbl %>%
  tk_index() %>%
  tk_make_future_timeseries(length_out       = "1 month",
                            inspect_weekdays = TRUE,
                            skip_values      = holidays)
