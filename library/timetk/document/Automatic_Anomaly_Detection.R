# Title     : Automatic Anomaly Detection
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/3
# URL       : https://business-science.github.io/timetk/articles/TK08_Automatic_Anomaly_Detection.html



# ＜ポイント＞
# - tibble形式の時系列データセットで簡単に異常検知を行う
# - {anomalize}のラッパー関数




# 1 準備 ----------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)
library(rstudioapi)


# データ確認
walmart_sales_weekly %>% print()
walmart_sales_weekly %>% glimpse()


# クラス確認
walmart_sales_weekly %>% class()



# 2 異常検知 -------------------------------------------------------------

# 対象データ確認
walmart_sales_weekly %>%
  select(Store, Dept, Date, Weekly_Sales)


# グループごとのレコード数
walmart_sales_weekly %>%
  group_by(Store, Dept) %>%
  tally()


# アノマリー分析
# --- プロット出力
# --- Rstudioでしか動かない？
walmart_sales_weekly %>%
  group_by(Store, Dept) %>%
  plot_anomaly_diagnostics(Date, Weekly_Sales, .facet_ncol = 2)


# アノマリー分析
# --- データ出力
walmart_sales_weekly %>%
  group_by(Store, Dept) %>%
  tk_anomaly_diagnostics(Date, Weekly_Sales)


