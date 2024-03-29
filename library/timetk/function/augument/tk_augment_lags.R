# ***************************************************************************************
# Library   : timetk
# Function  : tk_augment_lags
# Created on: 2021/8/8
# URL       : https://business-science.github.io/timetk/reference/tk_augment_lags.html
# ***************************************************************************************


# ＜ポイント＞
# - データフレームに複数のラグ系列を一度に作成する


# ＜構文＞
# tk_augment_lags(.data, .value, .lags = 1, .names = "auto")


# ＜引数＞
# - lags: プラスのベクトルで指定


# ＜使用例＞
# 0 準備
# 1 ラグ系列の追加


# 0 準備 ------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)


# データ確認
m4_monthly %>% print()
m4_monthly %>% group_by(id) %>% tally()


# 1 ラグ系列の追加 --------------------------------------------------------------------

# ラグ系列の追加
m4_monthly_lags <-
  m4_monthly %>%
    group_by(id) %>%
    tk_augment_lags(value, .lags = 1:20)

# 確認
m4_monthly_lags %>% glimpse()
