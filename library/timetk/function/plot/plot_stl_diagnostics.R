# Title     : plot_stl_diagnostics
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/4
# URL       : https://business-science.github.io/timetk/reference/plot_stl_diagnostics.html


# ＜ポイント＞
# - Seasonal-Trend-Loess分解による時系列データの分解を行う
#   --- stats::stl()を実装したもの
#   --- 観測系列から"season"と"trend"を抽出する
# - ggplot2ベースなので追加的な操作も可能


# ＜構文＞
# plot_stl_diagnostics(
#   .data,
#   .date_var,
#   .value,
#   .facet_vars = NULL,
#   .feature_set = c("observed", "season", "trend", "remainder", "seasadj"),
#   .frequency = "auto",
#   .trend = "auto",
#   .message = TRUE,
#   .facet_scales = "free",
#   .line_color = "#2c3e50",
#   .line_size = 0.5,
#   .line_type = 1,
#   .line_alpha = 1,
#   .title = "STL Diagnostics",
#   .x_lab = "",
#   .y_lab = "",
#   .interactive = TRUE
# )



# 1.準備 --------------------------------------------------------

library(tidyverse)
library(timetk)


# データ確認
m4_hourly %>% print()

# レコード件数
m4_hourly %>% group_by(id) %>% tally()



# 2.基本的なチャート --------------------------------------------------------

# 1系列でSTL分解
m4_hourly %>%
  filter(id == "H10") %>%
  plot_stl_diagnostics(
      date, value,
      .feature_set = c("observed", "season", "trend", "remainder"),
      .frequency   = "24 hours",
      .trend       = "1 week",
      .interactive = FALSE)


# 複数系列でSTL分解
# --- グループ化
m4_hourly %>%
  group_by(id) %>%
  plot_stl_diagnostics(
      date, value,
      .feature_set = c("observed", "season", "trend"),
      .interactive = FALSE)

