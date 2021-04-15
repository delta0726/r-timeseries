# Title     : mutate_by_time
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/5
# URL       : https://business-science.github.io/timetk/reference/mutate_by_time.html


# ＜ポイント＞
# - 日付の期間単位の集計を行った列を追加する
#   --- group_by()で日付列を集計しなくても集計計算ができる


# ＜構文＞
# mutate_by_time(
#  .data,
#  .date_var,
#  .by = "day",
#  ...,
#  .type = c("floor", "ceiling", "round")
#)


# ＜引数＞
# - .date_var : 日付列の指定
# - .by       : 日付の集計単位
# - .type     : See lubridate::round_date.



# 1.準備 --------------------------------------------------------------

library(timetk)
library(dplyr)
library(tidyr)


# データ確認
m4_daily %>% print()
m4_daily %>% glimpse()



# 2.1系列の時系列データ ---------------------------------------------------

# 列の追加
# --- 日付の期間単位に追加
# --- 月初の値を追加
m4_daily_first_by_month_tbl <-
  m4_daily %>%
    group_by(id) %>%
    mutate_by_time(.date_var = date,
                   .by       = "month",
                   first_value_by_month  = first(value))


# 確認
m4_daily_first_by_month_tbl %>% print()


# プロット作成
# --- 月初の値は会談プロットのように表示される
m4_daily_first_by_month_tbl %>%
    pivot_longer(value:first_value_by_month) %>%
    plot_time_series(.date = date, .value = value, .color_var = name,
                     .facet_scale = "free", .facet_ncol = 2,
                     .smooth = FALSE, .interactive = FALSE)
