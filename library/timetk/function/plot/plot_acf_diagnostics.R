# ***************************************************************************************
# Library   : timetk
# Function  : plot_acf_diagnostics
# Created on: 2021/8/8
# URL       : https://business-science.github.io/timetk/reference/plot_acf_diagnostics.html
# ***************************************************************************************


# ＜ポイント＞
# - 指定した系列のACFとPACFを計算してプロットする
#   --- CCFについてはオプションで表示可能
#   --- グループ化で複数グループの同時プロットが可能（プロットエリアを考えると4系列が限界）


# ＜構文＞
# plot_acf_diagnostics(
#  .data,
#  .date_var,
#  .value,
#  .ccf_vars = NULL,
#  .lags = 1000,
#  .show_ccf_vars_only = FALSE,
#  .show_white_noise_bars = FALSE,
#  .facet_ncol = 1,
#  .facet_scales = "fixed",
#  .line_color = "#2c3e50",
#  .line_size = 0.5,
#  .line_alpha = 1,
#  .point_color = "#2c3e50",
#  .point_size = 1,
#  .point_alpha = 1,
#  .x_intercept = NULL,
#  .x_intercept_color = "#E31A1C",
#  .hline_color = "#2c3e50",
#  .white_noise_line_type = 2,
#  .white_noise_line_color = "#A6CEE3",
#  .title = "Lag Diagnostics",
#  .x_lab = "Lag",
#  .y_lab = "Correlation",
#  .interactive = TRUE,
#  .plotly_slider = FALSE
#)


# ＜目次＞
# 0 準備
# 1 時間データのACF分解
# 2 差分系列のACF分解
# 3 CCF分解


# 0 準備 -----------------------------------------------------------------------

#　ライブラリ
library(tidyverse)
library(timetk)


# データ準備
data("FANG")


# 1 時間データのACF分解 --------------------------------------------------------

# データ確認
m4_hourly %>% print()

# 自己相関プロットの作成
# --- デフォルトではACFとPACFが表示される
m4_hourly %>%
  group_by(id) %>%
  plot_acf_diagnostics(.date = date, .value = value,
                       .lags = "7 days",
                       .interactive = FALSE)


# 2 差分系列のACF分解 --------------------------------------------------------

# データ確認
m4_hourly %>% print()

# 自己相関プロットの作成
# --- デフォルトではACFとPACFが表示される
m4_hourly %>%
    group_by(id) %>%
    plot_acf_diagnostics(date, 
                         diff_vec(value, lag = 1), 
                         .lags        = 0:(24*7), 
                         .interactive = FALSE) +
    ggtitle("ACF Diagnostics",  subtitle = "1st Difference")


# 3 CCF分解 -----------------------------------------------------------------

# データ確認
walmart_sales_weekly %>% print()

# 自己相関プロットの作成
# --- CCFを含める
walmart_sales_weekly %>%
  select(id, Date, Weekly_Sales, Temperature, Fuel_Price) %>%
  group_by(id) %>%
  plot_acf_diagnostics(Date, Weekly_Sales, 
                       .ccf_vars    = c(Temperature, Fuel_Price), 
                       .lags        = "3 months", 
                       .interactive = FALSE)
