# Title     : plot_seasonal_diagnostics
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/4
# URL       : https://business-science.github.io/timetk/reference/plot_seasonal_diagnostics.html


# ＜ポイント＞
# - Seasonal-Trend-Loess分解による時系列データの分解を行う
#   --- stats::stl()を実装したもの
#   --- 観測系列から"season"と"trend"を抽出する
# - ggplot2ベースなので追加的な操作も可能


# ＜構文＞
# plot_seasonal_diagnostics(
#  .data,
#  .date_var,
#  .value,
#  .facet_vars = NULL,
#  .feature_set = "auto",
#  .geom = c("boxplot", "violin"),
#  .geom_color = "#2c3e50",
#  .geom_outlier_color = "#2c3e50",
#  .title = "Seasonal Diagnostics",
#  .x_lab = "",
#  .y_lab = "",
#  .interactive = TRUE
# )






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


# FBのみ抽出
# --- 日次の株価データ




# 2.30分毎のデータ --------------------------------------------------------

# 原系列のプロット
taylor_30_min %>%
    plot_time_series(.date = date,
                     .value = value,
                     .interactive = FALSE)


# 複数系列でSTL分解
# --- グループ化
# Visualize seasonality
taylor_30_min %>%
    plot_seasonal_diagnostics(.date = date,.value =  value, .interactive = FALSE)



# 2.日次データ --------------------------------------------------------

# 原系列のプロット
FB %>%
    plot_time_series(.date = date,
                     .value = adjusted,
                     .interactive = FALSE)


# 複数系列でSTL分解
# --- グループ化
# Visualize seasonality
FB %>%
    plot_seasonal_diagnostics(.date = date,.value = adjusted, .interactive = FALSE)

