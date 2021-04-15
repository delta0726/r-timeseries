# Title     : summarise_by_time
# Objective :
# Created by: Owner
# Created on: 2020/8/23
# URL       : https://business-science.github.io/timetk/reference/summarise_by_time.html


# ＜ポイント＞
# - インデックスを用いて時系列の集計を行うことができる
# - {dplyr}だと時間でgroup_by()をするが、summarise_by_time()では関数内でインデックスを指定する


# ＜構文＞
# summarise_by_time(
#   .data,
#   .date_var,
#   .by = "day",
#   ...,
#   .type = c("floor", "ceiling", "round")
# )


# ＜引数＞
# - .date_var : 日付インデックスとして使用する列を指定
# - .by       : 集計単位を指定（lubridateで指定する時系列フレーズの使用が可能）
# - ...       : 集計列と計算定義を指定


# 1.準備 --------------------------------------------------------

# Libraries
library(timetk)
library(dplyr)


# データ確認
m4_daily %>% print()


# グループ確認
m4_daily %>% group_by(id) %>% tally()



# 2.時系列サマリー -----------------------------------------------

# 各月の最初の値で集計
# --- dateは最初の日付で表示
m4_daily %>%
  group_by(id) %>%
  summarise_by_time(.date_var = date,
                    .by       = "month",
                    value  = first(value))



# 各月の最初の値で集計
# --- dateを月末最終日に変更
m4_daily %>%
  group_by(id) %>%
  summarise_by_time(.by   = "month",
                    value = last(value),
                    .type = "ceiling") %>%
  mutate(date = date %-time% "1 day")



# 3.年ごとに集計 ----------------------------------------------------

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

