# Title     : plot_time_series
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/23
# URL       : https://business-science.github.io/timetk/reference/plot_time_series.html


# ＜ポイント＞
# - 日付を含むデータフレームで簡単にggplot2ベースの時系列チャートを作成
# - ggplot2ベースなので追加的な操作も可能


# ＜構文＞
# plot_time_series(
#  .data,
#  .date_var,
#  .value,
#  .color_var = NULL,
#  .facet_vars = NULL,
#  .facet_ncol = 1,
#  .facet_scales = "free_y",
#  .facet_collapse = TRUE,
#  .facet_collapse_sep = " ",
#  .line_color = "#2c3e50",
#  .line_size = 0.5,
#  .line_type = 1,
#  .line_alpha = 1,
#  .y_intercept = NULL,
#  .y_intercept_color = "#2c3e50",
#  .smooth = TRUE,
#  .smooth_period = "auto",
#  .smooth_message = FALSE,
#  .smooth_span = NULL,
#  .smooth_degree = 2,
#  .smooth_color = "#3366FF",
#  .smooth_size = 1,
#  .smooth_alpha = 1,
#  .legend_show = TRUE,
#  .title = "Time Series Plot",
#  .x_lab = "",
#  .y_lab = "",
#  .color_lab = "Legend",
#  .interactive = TRUE,
#  .plotly_slider = FALSE
# )





# 1.準備 --------------------------------------------------------

library(tidyverse)
library(tidyquant)
library(lubridate)
library(timetk)


# データ確認
FANG %>% print()


# 使用データ
FANG %>% select(symbol, date, adjusted)


# レコード件数
FANG %>% group_by(symbol) %>% tally()



# 2.基本的なチャート --------------------------------------------------------

# 1系列
# --- 自動でスムージングが入ってくる
FANG %>%
  filter(symbol == "FB") %>%
  plot_time_series(.date_var = date, .value = adjusted, .interactive = FALSE)


# 複数系列
# --- 予めグループ化しておく
# --- Facetで表示さえる（あまり多くの系列を表示するのは非現実的）
FANG %>%
  group_by(symbol) %>%
  plot_time_series(.date_var = date,
                   .value = adjusted,
                   .facet_ncol  = 2,
                   .interactive = FALSE)



# 2.グループ化なしのFacet作成 ----------------------------------------------

#
FANG %>%
  mutate(year = year(date)) %>%
  plot_time_series(date, adjusted,
                   .facet_vars   = c(symbol, year), # add groups/facets
                   .color_var    = year,            # color by year
                   .facet_ncol   = 4,
                   .facet_scales = "free",
                   .interactive  = FALSE)


# Can apply transformations to .value or .color_var
# - .value = log(adjusted)
# - .color_var = year(date)
FANG %>%
    plot_time_series(date, log(adjusted),
                     .color_var    = year(date),
                     .facet_vars   = contains("symbol"),
                     .facet_ncol   = 2,
                     .facet_scales = "free",
                     .y_lab        = "Log Scale",
                     .interactive  = FALSE)

