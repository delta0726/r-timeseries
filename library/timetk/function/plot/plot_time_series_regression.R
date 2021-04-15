# Title     : plot_time_series_regression
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/5
# URL       : https://business-science.github.io/timetk/reference/plot_time_series_regression.html


# ＜ポイント＞
# - 日付を含むデータフレームで簡単にggplot2ベースの時系列チャートを作成
# - ggplot2ベースなので追加的な操作も可能


# ＜構文＞
# plot_time_series_regression(
#  .data,
#  .date_var,
#  .formula,
#  .show_summary = FALSE,
#  ...
#)



# 1.準備 --------------------------------------------------------

library(dplyr)
library(lubridate)


# データ確認
m4_monthly %>% print()

# レコード件数
m4_monthly %>% group_by(id) %>% tally()



# 2.時系列回帰プロット --------------------------------------------------------

# 1系列
m4_monthly %>%
  filter(id == "M750") %>%
  plot_time_series_regression(
      .date_var     = date,
      .formula      = log(value) ~ as.numeric(date) + month(date, label = TRUE),
      .show_summary = TRUE,
      .facet_ncol   = 2,
      .interactive  = FALSE
  )


# 複数系列
m4_monthly %>%
    group_by(id) %>%
    plot_time_series_regression(
        .date_var    = date,
        .formula     = log(value) ~ as.numeric(date) + month(date, label = TRUE),
        .facet_ncol  = 2,
        .interactive = FALSE
    )