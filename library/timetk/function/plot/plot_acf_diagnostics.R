# Title     : plot_acf_diagnostics
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/4
# URL       : https://business-science.github.io/timetk/reference/plot_acf_diagnostics.html


# ＜ポイント＞
# - Seasonal-Trend-Loess分解による時系列データの分解を行う
#   --- stats::stl()を実装したもの
#   --- 観測系列から"season"と"trend"を抽出する
# - ggplot2ベースなので追加的な操作も可能


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






# 1.準備 --------------------------------------------------------

library(tidyverse)
library(timetk)

# データ準備
data("FANG")

# データ確認
taylor_30_min %>% print()
FB <- FANG %>% filter(symbol == "FB") %>% select(date, adjusted)

# データ確認
taylor_30_min %>% print()
FB %>% print()



# 2.30分毎のデータ --------------------------------------------------------

# 複数系列でSTL分解
# --- グループ化
# Visualize seasonality
m4_hourly %>%
  group_by(id) %>%
  plot_acf_diagnostics(.date = date, .value = value,
                       .lags = "7 days",
                       .interactive = FALSE
  )



# 2.日次データ --------------------------------------------------------


m4_hourly %>%
    group_by(id) %>%
    plot_acf_diagnostics(
        date,
        diff_vec(value, lag = 1), # Difference the value column
        .lags        = 0:(24*7),   # 7-Days of hourly lags
        .interactive = FALSE
    ) +
    ggtitle("ACF Diagnostics",  subtitle = "1st Difference")

walmart_sales_weekly %>%
    select(id, Date, Weekly_Sales, Temperature, Fuel_Price) %>%
    group_by(id) %>%
    plot_acf_diagnostics(
        Date, Weekly_Sales,                        # ACF & PACF
        .ccf_vars    = c(Temperature, Fuel_Price), # CCFs
        .lags        = "3 months", # 3 months of weekly lags
        .interactive = FALSE