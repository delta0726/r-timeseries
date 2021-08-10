# ***************************************************************************************
# Library   : timetk
# Function  : plot_stl_diagnostics
# Created on: 2021/8/8
# URL       : https://business-science.github.io/timetk/reference/plot_stl_diagnostics.html
# ***************************************************************************************


# ＜ポイント＞
# - Seasonal-Trend-Loess分解による時系列データの分解を行う
#   --- stats::stl()を実装したもの
#   --- 観測系列から"season"と"trend"を抽出する


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


# ＜目次＞
# 0 準備
# 1 単一系列のSTL分解
# 2 複数系列のSTL分解


# 0 準備 ----------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)


# データ確認
m4_hourly %>% print()

# レコード件数
m4_hourly %>% group_by(id) %>% tally()



# 1 単一系列のSTL分解 ---------------------------------------------------

# プロット作成
# --- STL分解
m4_hourly %>%
  filter(id == "H10") %>%
  plot_stl_diagnostics(.date_var    = date, 
                       .value       = value, 
                       .feature_set = c("observed", "season", "trend", "remainder"), 
                       .frequency   = "24 hours", 
                       .trend       = "1 week", 
                       .interactive = FALSE)


# 2 複数系列のSTL分解 ---------------------------------------------------

# プロット作成
# --- STL分解
m4_hourly %>%
  group_by(id) %>%
  plot_stl_diagnostics(.date_var    = date, 
                       .value       = value, 
                       .feature_set = c("observed", "season", "trend"),　
                       .interactive = FALSE)

