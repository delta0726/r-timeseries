# ***************************************************************************************
# Library   : timetk
# Function  : tk_make_timeseries
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/tk_make_timeseries.html
# ***************************************************************************************


# ＜概要＞
# - 指定した期間の日付ベクトルを作成する


# ＜構文＞
# tk_make_timeseries(
#   start_date,
#   end_date,
#   by,
#   length_out = NULL,
#   include_endpoints = TRUE,
#   skip_values = NULL,
#   insert_values = NULL
# )


# ＜目次＞
# 0 準備
# 1 単純な日付出力
# 2 ベクトルの長さを指定
# 3 長さをキーワードで指定
# 4 時間ベクトルの出力
# 5 除外日付の指定
# 6 実践的な指定


# 0 準備 -------------------------------------------------------------------------

# ライブラリ
library(dplyr)
library(tidyquant)
library(timetk)


# 1 単純な日付出力 ------------------------------------------------------------------

# 日付で期間指定
tk_make_timeseries(start_date = "2017-01-01", end_date = "2017-12-31")

# 年のみ指定
# --- 指定した年の全ての日付
tk_make_timeseries(start_date = "2017")

# 年月を指定
# --- 指定した年月の全ての日付
tk_make_timeseries(start_date = "2017-02")


# 2 ベクトルの長さを指定 ---------------------------------------------------------------

# 年のみ指定
# --- ベクトルの長さを指定
tk_make_timeseries(start_date = "2012", length_out = 6)

# 年のみ指定
# --- 頻度とベクトルの長さを指定
tk_make_timeseries(start_date = "2012", by = "1 month", length_out = 6)


# 3 長さをキーワードで指定 ------------------------------------------------------------------

# 期間をlength outで指定
# --- 開始日を基準に出力
# --- include_endpoints = FALSEで18か月分を出力
tk_make_timeseries(start_date = "2012", by = "1 month",
                   length_out = "1 year 6 months", include_endpoints = FALSE)

# 期間をlength outで指定
# --- 開始日を基準に出力
# --- include_endpoints = FALSEで19か月分を出力
tk_make_timeseries(start_date = "2012", by = "1 month",
                   length_out = "1 year 6 months", include_endpoints = TRUE)

# 期間をlength outで指定
# --- 終了日を基準に出力
# --- include_endpoints = FALSEで18か月分を出力
tk_make_timeseries(end_date = "2012-03-01", by = "1 month",
                   length_out = "1 year 6 months", include_endpoints = FALSE)


# 4 時間ベクトルの出力 ------------------------------------------------------------------

# 秒単位の出力
tk_make_timeseries("2016-01-01 01:01:02", "2016-01-01 01:01:04")

# 分単位の出力
tk_make_timeseries("2017-01-01", "2017-01-02", by = "10 min")

# 時間単位の出力
tk_make_timeseries("2017-01-01", by = "30 min", length_out = "6 hours")


# 5 除外日付の指定 ------------------------------------------------------------------

# 除外日付の指定
tk_make_timeseries(start_date = "2011-01-01", length_out = 5,
                   skip_values   = "2011-01-05", insert_values = "2011-01-06")


# 6 実践的な指定 --------------------------------------------------------------------

# 毎週金曜日
tk_make_timeseries(start_date = "2021-04-02", by = "1 week", length_out = 12)

# 毎月30日
tk_make_timeseries(start_date = "2021-04-30", by = "1 month", length_out = 12)

# 毎月の月末日（最終日）
tk_make_timeseries(start_date = "2021-01-01", by = "1 month", length_out = 12) %-time% "1 day"

# 毎月の月末日（最終営業日）
weekend <- tk_make_weekend_sequence(start_date = "2021")
tk_make_timeseries(start_date = "2021-01-01", by = "1 month",
                   length_out = 12, skip_values = weekend) %-time% "1 day"

# 毎月の月末日（最終営業日/除く休日）
# --- うまく動作していない
holiday <- tk_make_holiday_sequence(start_date = "2021", calendar = "NYSE")
weekend <- tk_make_weekend_sequence(start_date = "2021")
tk_make_timeseries(start_date = "2021-01-01", by = "1 month",
                   length_out = 12, skip_values = c(holiday, weekend)) %-time% "1 day"
