# Title     : tk_get_timeseries_signature
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/4
# URL       : https://business-science.github.io/timetk/reference/tk_augment_timeseries.html



# ＜ポイント＞
# - 日付ベクトルに日付属性を追加してデータフレームにする
# - データフレームから直接操作する場合はtk_augument_timeseries_signature()を使う



# ＜構文＞
# tk_get_timeseries_signature(idx)


# ＜引数＞
# idx : A time-series index that is a vector of dates or datetimes




# 1.準備 -----------------------------------------------------------------------

library(dplyr)
library(tidyquant)
library(timetk)



# 2.データフレームからの取得 ----------------------------------------------

# データ確認
FB_tbl <- FANG %>% filter(symbol == "FB")
FB_tbl %>% print()


# 日付インデックスの取得
FB_idx <- FB_tbl %>% tk_index()


# 日付インデックスの確認
FB_idx %>% glimpse()
FB_idx %>% summary()
FB_idx %>% length()


# 日付属性の取得
FB_idx %>% tk_get_timeseries_signature()


# 別の方法
# --- データフレームから日付をベクトルで取得して変換することも可能
# --- データフレームのまま操作する場合は｢tk_augument_timeseries_signature()｣を使う
FB_tbl$date %>% tk_get_timeseries_signature()




# 3.週次データから属性取得 --------------------------------------------

# 週次データの作成
idx_weekly <- seq.Date(from = ymd("2016-01-01"), by = 'week', length.out = 6)
idx_weekly %>% print()


# 日付属性の取得
idx_weekly %>% tk_get_timeseries_signature()



# 4.他の日付オブジェクトから属性取得 ------------------------------------

# 日付オブジェクトの作成
# --- yearmonオブジェクト
idx_yearmon <-
  seq.Date(from = ymd("2016-01-01"),by = "month", length.out = 12) %>%
    as.yearmon()


# データ確認
idx_yearmon %>% print()
idx_yearmon %>% glimpse()


# 日付属性の取得
idx_yearmon %>% tk_get_timeseries_signature()


