# ***************************************************************************************
# Library   : timetk
# Function  : summarise_by_time
# Created on: 2021/8/8
# URL       : https://business-science.github.io/timetk/reference/summarise_by_time.html
# ***************************************************************************************


# ＜ポイント＞
# - 日付インデックスを用いて時系列の集計を行うことができる
#   --- グループとは別にパーティションが生成されて集計計算が行われる
#   --- {dplyr}だと日付要素をmutateで抽出してからgroup_by()で集計する必要がある


# ＜構文＞
# summarise_by_time(
#   .data,
#   .date_var,
#   .by = "day",
#   ...,
#   .type = c("floor", "ceiling", "round"),
#   .week_start = NULL
# )


# ＜引数＞
# - .date_var : 日付インデックスとして使用する列を指定
# - .by       : 集計単位を指定（lubridateで指定する時系列フレーズの使用が可能）
# - ...       : 集計列と計算定義を指定


# ＜目次＞
# 0 準備
# 1 時系列サマリ
# 2 年ごとに集計


# 0 準備 ---------------------------------------------------------------------

# Libraries
library(timetk)
library(dplyr)


# データ確認
m4_daily %>% print()
m4_daily %>% group_by(id) %>% tally()


# 1 時系列サマリー -------------------------------------------------------------

# 各月の最初の値で集計
# --- dateは最初の日付で表示
m4_daily %>%
  group_by(id) %>%
  summarise_by_time(.date_var = date,
                    .by       = "month",
                    value     = first(value))


# 各月の最初の値で集計
# --- dateを月末最終日に変更
m4_daily %>%
  group_by(id) %>%
  summarise_by_time(.by   = "month",
                    value = last(value),
                    .type = "ceiling") %>%
  mutate(date = date %-time% "1 day")



# 2 年ごとに集計 ----------------------------------------------------

m4_daily %>%
  group_by(id) %>%
  summarise_by_time(.by = "year",
                    value = sum(value),
                    type = "ceiling")

# 検証
m4_daily %>%
  mutate(year = year(date)) %>%
  group_by(id, year) %>%
  summarise(value = sum(value)) %>%
  ungroup()

