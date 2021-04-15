# Title     : between_time
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/6
# URL       : https://business-science.github.io/timetk/reference/between_time.html



# ＜ポイント＞
# - 日付データや時間データが指定した範囲に入るかをTRUE/FALSEで判定



# ＜構文＞
# between_time(index, start_date = "start", end_date = "end")



# 1.準備 ------------------------------------------------------------------

library(tidyverse)
library(tidyquant)
library(timetk)


data("FANG")


# 2.日付操作 ------------------------------------------------------------------

# 日付ベクトルの作成
index_daily <- tk_make_timeseries("2016-01-01", "2017-01-01", by = "day")
index_daily %>% print()


# 日付の判定
# --- between_time()で指定した期間に該当したらTRUE、しなければFALSE
index_daily %>% between_time("start", "2016-01")


# 期間数を演算
# --- TRUEを合計することで期間数を調べることができる
index_daily %>%
  between_time("start", "2016-01") %>%
  sum()


# 日付の抽出
index_daily %>%
  between_time("start", "2016-01") %>%
  index_daily[.]




# 3.時間操作 ------------------------------------------------------------------

# 時間ベクトルの作成
index_min <- tk_make_timeseries("2016-01-01", "2016-01-03", by = "hour")
index_min %>% print()


index_min %>%
  between_time("2016-01-01 12:00", "2016-01-02 13:00") %>%
  index_min[.]



# 4.データフレームでの活用 ------------------------------------------------------------------

FANG %>%
  group_by(symbol) %>%
  filter(date %>% between_time("2016-01", "2016-01"))