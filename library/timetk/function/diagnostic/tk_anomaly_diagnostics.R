# ***************************************************************************************
# Library   : timetk
# Function  : tk_anomaly_diagnostics
# Created on: 2021/8/8
# URL       : https://business-science.github.io/timetk/reference/tk_anomaly_diagnostics.html
# ***************************************************************************************


# ＜概要＞
# - STL分解を用いて季節性とトレンドを除去した上でIQR法により異常検知を行う
# - グループ化することで複数系列の異常検知を同時に行うことができる


# ＜構文＞
# tk_anomaly_diagnostics(
#   .data,
#   .date_var,
#   .value,
#   .frequency = "auto",
#   .trend = "auto",
#   .alpha = 0.05,
#   .max_anomalies = 0.2,
#   .message = TRUE
# )


# ＜目次＞
# 0 準備
# 1 単一系列の異常検知
# 2 複数系列の異常検知


# 0 準備 ---------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)


# データ確認
walmart_sales_weekly %>% print()


# 使用データ
walmart_sales_weekly %>% select(id, Date, Weekly_Sales)


# レコード件数
walmart_sales_weekly %>%
  filter(id %in% c("1_1", "1_3")) %>%
  group_by(id) %>%
  tally()


# 1 単一系列の異常検知 --------------------------------------------------------------

# 異常検知
# --- データ出力
walmart_single_anomaly <- 
  walmart_sales_weekly %>%
    filter(id == "1_1") %>%
    tk_anomaly_diagnostics(Date, Weekly_Sales)

# データ確認
walmart_single_anomaly %>% glimpse()

# プロット確認
walmart_sales_weekly %>%
  filter(id == "1_1") %>%
  plot_anomaly_diagnostics(Date, Weekly_Sales,
                           .message = FALSE,
                           .facet_ncol = 1,
                           .ribbon_alpha = 0.25,
                           .interactive = FALSE)


# 2 複数系列の異常検知 --------------------------------------------------------------

# 異常検知
# --- データ出力
walmart_multiple_anomaly <- 
  walmart_sales_weekly %>%
    filter(id %in% c("1_1", "1_3")) %>%
    group_by(id) %>%
    tk_anomaly_diagnostics(Date, Weekly_Sales)

# データ確認
walmart_multiple_anomaly %>% glimpse()

# プロット確認
walmart_sales_weekly %>%
  filter(id %in% c("1_1", "1_3")) %>%
  group_by(id) %>%
  plot_anomaly_diagnostics(Date, Weekly_Sales,
                           .message = FALSE,
                           .facet_ncol = 1,
                           .ribbon_alpha = 0.25,
                           .interactive = FALSE)
