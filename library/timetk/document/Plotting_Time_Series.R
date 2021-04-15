# Title     : Plotting Time Series
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/23
# URL       : https://business-science.github.io/timetk/articles/TK04_Plotting_Time_Series.html



# ＜ポイント＞
# - tibble形式の時系列データからggplot2ベースのプロットを作成する
# -




# 1.準備 -------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(lubridate)
library(timetk)


# インタラクティブモード
# --- Plotly用
interactive <- TRUE




# 2.単一指標のプロット -------------------------------------------------------------


# データ確認
taylor_30_min %>% print()
taylor_30_min %>% glimpse()


# プロット作成
taylor_30_min %>% plot_time_series(date, value)





# 3.複数指標のプロット -------------------------------------------------------------

# データ確認
m4_daily %>% print()
m4_daily %>% glimpse()



m4_daily %>%
  group_by(id) %>%
  plot_time_series(date, value,
                   .facet_ncol = 2, .facet_scales = "free",
                   .interactive = interactive)

