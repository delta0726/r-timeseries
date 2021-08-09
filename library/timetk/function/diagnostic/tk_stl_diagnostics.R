# ***************************************************************************************
# Library   : timetk
# Function  : tk_stl_diagnostics
# Created on: 2021/8/10
# URL       : https://business-science.github.io/timetk/reference/tk_stl_diagnostics.html
# ***************************************************************************************


# ＜概要＞
# - 指定した系列をseason / trend / remainder の3つに分解する


# ＜STL分解＞
# - LOESS回帰に基づくフィルタリングアルゴリズムを用いてシリーズを季節/トレンド/残余に分解する季節調整法
#   --- トレンド(Trend)　: 長期的に変動する要素
#   --- 季節性(Seasonal) : 一定の時間で周期的に変動する要素
#   --- 残差(Remainder)  : 残りの細かく変動する要素


# ＜構文＞
# - tk_stl_diagnostics(
#  .data,
#  .date_var,
#  .value,
#  .frequency = "auto",
#  .trend = "auto",
#  .message = TRUE
# )



# ＜目次＞
# 0 準備
# 1 日次データのSTL分解
# 2 週次データのSTL分解


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)


# データ確認
m4_daily %>% print()
m4_daily %>% group_by(id) %>% tally()


# データ確認
m4_weekly %>% print()
m4_weekly %>% group_by(id) %>% tally()


# 1 日次データのSTL分解 ------------------------------------------------------------------------

# STL分解
# --- 原系列をBox-Cox変換
# --- season / trend / remainder の3つに分解される
m4_daily %>%
    group_by(id) %>%
    tk_stl_diagnostics(.date_var = date, 
                       .value    = box_cox_vec(value))

# 検証
# --- observed = season + trend + remainder
m4_daily %>%
  group_by(id) %>%
  tk_stl_diagnostics(date, box_cox_vec(value)) %>%
  ungroup() %>%
  mutate(result = rowSums(.[4:6]))

# プロット
m4_hourly %>%
  filter(id == "H10") %>%
  plot_stl_diagnostics(date, value,　
                       .feature_set = c("observed", "season", "trend", "remainder"),　
                       .interactive = FALSE)


# 2 週次データのSTL分解 ------------------------------------------------------------------------

# STL分解
m4_weekly %>%
  group_by(id) %>%
  tk_stl_diagnostics(.date_var = date, 
                     .value    = box_cox_vec(value), 
                     .trend    = "2 quarters")

# 検証
# --- observed = season + trend + remainder
m4_weekly %>%
  group_by(id) %>%
  tk_stl_diagnostics(date, box_cox_vec(value)) %>%
  ungroup() %>%
  mutate(result = rowSums(.[4:6]))

# プロット
m4_weekly %>%
  group_by(id) %>%
  plot_stl_diagnostics(date, value,　
                       .feature_set = c("observed", "season", "trend", "remainder"),　
                       .interactive = FALSE)

