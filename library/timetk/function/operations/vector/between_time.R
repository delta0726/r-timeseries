# ***************************************************************************************
# Library   : timetk
# Function  : between_time
# Created on: 2021/8/13
# URL       : https://business-science.github.io/timetk/reference/between_time.html
# ***************************************************************************************


# ＜ポイント＞
# - 日付データや時間データが指定した範囲に入るかをTRUE/FALSEで判定
# - 日付指定の際に日付キーワードを使用することが可能


# ＜構文＞
# between_time(index, start_date = "start", end_date = "end")


# ＜目次＞
# 0 準備
# 1 日付判定
# 2 日付フィルタリング


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidyquant)
library(timetk)


# データロード
data("FANG")

# データ作成
# --- 日付ベクトル
index_daily <- tk_make_timeseries("2016-01-01", "2017-01-01", by = "day")
index_min   <- tk_make_timeseries("2016-01-01", "2017-01-01", by = "min")


# 1 日付判定 ------------------------------------------------------------------

# 日付の判定
# --- between_time()で指定した期間に該当したらTRUE、しなければFALSE
index_daily %>% between_time("start", "2016-01")

# 期間数を演算
# --- TRUEを合計することで期間数を調べることができる
index_daily %>%
  between_time("start", "2016-01") %>%
  sum()


# 2 日付スライシング ------------------------------------------------------------

# 日付抽出
# --- 日付判定の結果でスライシング
index_daily[index_daily %>% between_time("start", "2016-01")]
index_daily[index_daily %>% between_time("2016-03", "2016-06-15")]


# 3 データフレームでの活用 ------------------------------------------------------------------

FANG %>%
  group_by(symbol) %>%
  filter(between_time(date, "2016-01", "2016-01"))
