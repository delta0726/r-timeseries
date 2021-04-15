# Title     : Visualize Time Series Data in 1-Line of Code
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/23
# URL       : https://www.business-science.io/code-tools/2020/06/05/timetk-vesion-2-announcement.html



# ＜ポイント＞




# 1 準備 ----------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(timetk)



# 2 1行コードでプロット作成 ------------------------------------------------------------


# データ確認
# --- 古典的な電力需要の時系列データ
taylor_30_min %>% print()
taylor_30_min %>% glimpse()


# クラス確認
# --- tibble型
taylor_30_min %>% class()


# プロット作成
# --- 静的プロット
taylor_30_min %>%
    plot_time_series(.date_var =  date, .value = value, .color_var = week(date),
                     .interactive = FALSE, .color_lab = "Week")


# プロット作成
# --- インタラクティブ(Plotly)
# --- 動作が遅いので注意
taylor_30_min %>%
    plot_time_series(.date_var =  date, .value = value, .color_var = week(date),
                     .interactive = TRUE, .color_lab = "Week")



# 3 異常検知を診断して可視化 ------------------------------------------------------------

# データ確認
# --- Walmartの週次売上高
walmart_sales_weekly %>% print()
walmart_sales_weekly %>% glimpse()


# クラス確認
# --- tibble型
walmart_sales_weekly %>% class()


# 異常検知の診断
# --- 店舗(Store)と部門(Dept)にグループ化
walmart_sales_weekly %>%
    group_by(Store, Dept) %>%
    plot_anomaly_diagnostics(Date, Weekly_Sales,
                             .facet_ncol = 3, .interactive = FALSE)




# 4 季節性を可視化 ------------------------------------------------------------

# データ確認
# --- 古典的な電力需要の時系列データ
taylor_30_min %>% print()
taylor_30_min %>% glimpse()


# クラス確認
# --- tibble型
taylor_30_min %>% class()


# プロット作成
# --- 静的プロット
taylor_30_min %>%
    plot_seasonal_diagnostics(date, value, .interactive = FALSE)



