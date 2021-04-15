# Title     : tk_get_timeseries_summary
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/4
# URL       : https://business-science.github.io/timetk/reference/tk_augment_timeseries.html



# ＜ポイント＞
# - 日付インデックスからサマリーを作成する
# - Date型以外の日付オブジェクトからもサマリーの作成が可能


# ＜構文＞
# tk_get_timeseries_summary(idx)


# ＜引数＞
# idx : A time-series index that is a vector of dates or datetimes




# 1.準備 -----------------------------------------------------------------------

library(dplyr)
library(tidyquant)
library(timetk)



# 2.データフレームからの取得 ----------------------------------------------

# ＜ポイント＞
# - 予め日付インデックスを取得してから日付サマリーを作成する


# データ確認
FB_tbl <- FANG %>% filter(symbol == "FB")
FB_tbl %>% print()


# 日付インデックスの取得
FB_idx <- FB_tbl %>% tk_index()


# 日付インデックスの確認
FB_idx %>% glimpse()
FB_idx %>% summary()


# 日付サマリーの取得
FB_idx %>% tk_get_timeseries_summary()



# 3.日付オブジェクトから属性取得 --------------------------------------------

# ＜ポイント＞
# - 他の日付オブジェクトからも日付サマリーを取得することが可能


# 日付オブジェクトの作成
# --- yearmonオブジェクト
idx_yearmon <-
  seq.Date(from = ymd("2016-01-01"),by = "month", length.out = 12) %>%
    as.yearmon()


# データ確認
idx_yearmon %>% print()
idx_yearmon %>% glimpse()
idx_yearmon %>% class()


# 日付サマリーの取得
idx_yearmon %>% tk_get_timeseries_summary()

