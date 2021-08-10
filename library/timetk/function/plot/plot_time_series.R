# ***************************************************************************************
# Library   : timetk
# Function  : plot_time_series
# Created on: 2021/8/8
# URL       : https://business-science.github.io/timetk/reference/plot_time_series.html
# ***************************************************************************************


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


# ＜目次＞
# 0 準備
# 1 単一系列のプロット
# 2 複数系列のプロット
# 3 グループ化なしでFacetプロットを作成
# 4 系列のカラーをカテゴリに応じて変更する


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidyquant)
library(lubridate)
library(timetk)


# データ確認
FANG %>% print()
FANG %>% group_by(symbol) %>% tally()

# 使用データ
FANG %>% select(symbol, date, adjusted)


# 1 単一系列のプロット --------------------------------------------------------

# プロット作成
FANG %>%
  filter(symbol == "FB") %>%
  plot_time_series(.date_var = date, 
                   .value = adjusted, 
                   .interactive = FALSE)


# 2 複数系列のプロット --------------------------------------------------------

# プロット作成
# --- 予めグループ化しておく
# --- Facetで表示される（あまり多くの系列を表示するのは非現実的）
FANG %>%
  group_by(symbol) %>%
  plot_time_series(.date_var = date,
                   .value = adjusted,
                   .facet_ncol  = 2,
                   .interactive = FALSE)



# 3 グループ化なしでFacetプロットを作成 ----------------------------------------

# プロット作成
# --- .facet_vars引数にキーを指定する
# --- ファセットのラベルにキーが表示されるので見やすい
FANG %>%
  mutate(year = year(date)) %>%
  plot_time_series(date, adjusted,
                   .facet_vars   = c(symbol, year), 
                   .color_var    = year, 
                   .facet_ncol   = 4,
                   .facet_scales = "free",
                   .interactive  = FALSE)



# 4 系列のカラーをカテゴリに応じて変更する -------------------------------------

FANG %>%
  plot_time_series(date, log(adjusted), 
                   .color_var    = year(date), 
                   .facet_vars   = contains("symbol"), 
                   .facet_ncol   = 2, 
                   .facet_scales = "free", 
                   .y_lab        = "Log Scale", 
                   .interactive  = FALSE)

