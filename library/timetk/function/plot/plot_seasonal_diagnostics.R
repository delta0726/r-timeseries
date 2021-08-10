# ***************************************************************************************
# Library   : timetk
# Function  : plot_seasonal_diagnostics
# Created on: 2021/8/11
# URL       : https://business-science.github.io/timetk/reference/plot_seasonal_diagnostics.html
# ***************************************************************************************


# ＜ポイント＞
# - 時系列データの周期性(季節性)を確認する
#   --- 明確な周期性パターンがないとインプリケーションは得にくい


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


# ＜目次＞
# 0 準備
# 1 30分毎のデータ
# 2 日次データ


# 0 準備 ----------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidyquant)
library(timetk)


# ローケール設定変更
Sys.setlocale("LC_TIME", "English")


# データ準備
# --- 30分毎のデータ
taylor_30_min %>% print()

# データ準備
# --- 日次データ
data("FANG")
FB <- FANG %>% filter(symbol == "FB") %>% select(date, adjusted)
FB %>% print()



# 1 30分毎のデータ -----------------------------------------------------------------

# プロット確認
# --- 原系列
taylor_30_min %>%
  plot_time_series(.date = date, 
                   .value = value, 
                   .interactive = FALSE)

# 季節性分解
taylor_30_min %>%
  plot_seasonal_diagnostics(.date = date, 
                            .value =  value,
                            .interactive = FALSE)



# 2 日次データ --------------------------------------------------------------------

# プロット確認
# --- 原系列
FB %>%
  plot_time_series(.date = date, 
                   .value = adjusted, 
                   .interactive = FALSE)


# 季節性分解
FB %>% 
  plot_seasonal_diagnostics(.date = date, 
                            .value = adjusted, 
                            .interactive = FALSE)

