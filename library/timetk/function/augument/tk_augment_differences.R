# ***************************************************************************************
# Library   : timetk
# Function  : tk_augment_differences
# Created on: 2021/8/8
# URL       : https://business-science.github.io/timetk/reference/tk_augment_differences.html
# ***************************************************************************************


# ＜ポイント＞
# - データフレームに一度に複数の差分系列を作成する
# - difference引数を用いることで2階差分を取得することも可能


# ＜構文＞
# tk_augment_differences(
#  .data,
#  .value,
#  .lags = 1,
#  .differences = 1,
#  .log = FALSE,
#  .names = "auto"
#)


# ＜使用例＞
# 0 準備
# 1 差分系列の作成


# 0 準備 -------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)


# データ確認
m4_monthly %>% print()
m4_monthly %>% group_by(id) %>% tally()


# 1 差分系列の作成 -------------------------------------------------------------------------

# 差分系列の追加
m4_monthly_diff <-
  m4_monthly %>%
    group_by(id) %>%
    tk_augment_differences(value, .lags = 1:20)

# 確認
m4_monthly_diff %>% glimpse()