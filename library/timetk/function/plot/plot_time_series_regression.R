# ***************************************************************************************
# Library   : timetk
# Function  : plot_time_series_regression
# Created on: 2021/8/11
# URL       : https://business-science.github.io/timetk/reference/plot_time_series_regression.html
# ***************************************************************************************


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


# ＜目次＞
# 0 準備
# 1 単一系列の時系列回帰プロット
# 2 複数系列の時系列回帰プロット


# 0 準備 --------------------------------------------------------------

# ライブラリ
library(timetk)
library(tidyverse)
library(lubridate)


# データ確認
m4_monthly %>% print()
m4_monthly %>% group_by(id) %>% tally()


# 1 単一系列の時系列回帰プロット --------------------------------------

# プロット作成
# --- 単一系列
m4_monthly %>%
  filter(id == "M750") %>%
  plot_time_series_regression(.date_var     = date, 
                              .formula      = log(value) ~ as.numeric(date) + month(date, label = TRUE), 
                              .show_summary = TRUE, 
                              .facet_ncol   = 2, 
                              .interactive  = FALSE)


# 2 複数系列の時系列回帰プロット --------------------------------------

# プロット作成
# --- 複数系列
m4_monthly %>%
  group_by(id) %>%
  plot_time_series_regression(.date_var    = date, 
                              .formula     = log(value) ~ as.numeric(date) + month(date, label = TRUE), 
                              .facet_ncol  = 2, 
                              .interactive = FALSE)
