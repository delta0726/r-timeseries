# ***************************************************************************************
# Library   : timetk
# Function  : tk_augment_fourier
# Created on: 2021/8/8
# URL       : https://business-science.github.io/timetk/reference/tk_augment_fourier.html
# ***************************************************************************************


# ＜ポイント＞
# - データフレームに一度に複数のフーリエ系列を作成する


# ＜構文＞
# tk_augment_fourier(.data, .date_var, .periods, .K = 1, .names = "auto")


# ＜使用例＞
# 0 準備
# 1 フーリエ系列の追加


# 0 準備 -------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)


# データ確認
m4_monthly %>% print()
m4_monthly %>% group_by(id) %>% tally()


# 1 フーリエ系列の追加 -------------------------------------------------------------------

# フーリエ系列の追加
m4_monthly_fourier <-
  m4_monthly %>%
    group_by(id) %>%
    tk_augment_fourier(date, .periods = c(6, 12), .K = 2)

# 確認
m4_monthly_fourier %>% glimpse()
