# ***************************************************************************************
# Library   : timetk
# Function  : fourier_vec
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/fourier_vec.html
# ***************************************************************************************


# ＜概要＞
# フーリエ系列の追加


# ＜構文＞
# fourier_vec(x, period, K = 1, type = c("sin", "cos"))


# ＜目次＞
# 0 準備
# 1 フーリエ系列の追加


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)


# データ準備
date_sequence <- tk_make_timeseries("2016-01-01", "2016-01-31", by = "hour")

# データ確認
date_sequence %>% print()


# 1 フーリエ系列の追加 -------------------------------------------------------

# データ作成
X_Plot <- 
  tibble(date = date_sequence) %>%
    mutate(C1_7 = fourier_vec(date, period = 7*24, K = 1, type = "cos"), 
           C2_7 = fourier_vec(date, period = 7*24, K = 2, type = "cos"))
         
# プロット作成
X_Plot %>% 
  pivot_longer(cols = contains("_"), names_to = "name", values_to = "value") %>%
  plot_time_series(date, value, .color_var = name,
                   .smooth = FALSE,
                   .interactive = FALSE,
                   .title = "7-Day Fourier Terms")
