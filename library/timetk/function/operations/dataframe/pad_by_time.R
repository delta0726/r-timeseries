# Title     : pad_by_time
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/5
# URL       : https://business-science.github.io/timetk/reference/pad_by_time.html


# ＜ポイント＞
# - データセットの日付インデックスのパターンや期間で存在しない日付を追加する
#   --- カレンダーを指定することはできない(Seven Dayで補完される)
#   ---



# ＜構文＞
# pad_by_time(
#  .data,
#  .date_var,
#  .by = "auto",
#  .pad_value = NA,
#  .start_date = NULL,
#  .end_date = NULL
# )


# ＜引数＞
# - .by         : 周期の指定(autoを指定すると自動検知)
# - pad_value   : 補完する値の指定
# - .start_date : 元のデータセットよりも広く指定することも可能
# - .end_date   : 元のデータセットよりも広く指定することも可能



# 1.準備 --------------------------------------------------------------

library(tidyverse)
library(tidyquant)
library(timetk)


# データ作成
# --- 四半期ベース
# --- 特定日付を除外
missing_data_tbl <-
  tibble(date = tk_make_timeseries(start_date = "2014-01-01", end_date = "2015-01-01",  by = "quarter"),
         value = 1:5) %>%
    slice(-4)


# データ確認
# --- 2014-11-1が欠落している
missing_data_tbl %>% print()



# 2.日付を補完 ---------------------------------------------------

# 日付を補完
# --- 四半期ベース
missing_data_tbl %>% pad_by_time(date, .by = "quarter")


# 日付を補完
# --- 月次ベース
missing_data_tbl %>% pad_by_time(date, .by = "month")


# 日付を補完
# --- 自動検知
missing_data_tbl %>% pad_by_time()



# 2.補完する値を指定 ----------------------------------------------

# 特定の値で補完
# --- 自動検知
missing_data_tbl %>% pad_by_time(date, .by = "quarter", .pad_value = 0)


# 補完アルゴリズムを指定
# --- 一旦NAとしてから、別途アルゴリズムで追加
missing_data_tbl %>%
  pad_by_time(date, .by = "quarter") %>%
  mutate(value = ts_impute_vec(value, period = 1))



# 3.補完する期間を指定 ----------------------------------------------

# 準備：元データの期間を確認
missing_data_tbl %>% tk_index() %>% tk_get_timeseries_summary() %>% .[1:4]


# 開始日や終了日を指定
# --- 元の期間よりも広く指定することができる
missing_data_tbl %>%
   pad_by_time(date, .by = "quarter", .start_date = "2013", .end_date = "2015-07-01")


# 4.グループデータを補完 ----------------------------------------------

FANG %>%
  group_by(symbol) %>%
  pad_by_time(.by = "day")

