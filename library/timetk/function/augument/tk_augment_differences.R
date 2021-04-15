# Title     : tk_augment_differences
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/6
# URL       : https://business-science.github.io/timetk/reference/tk_augment_differences.html



# ＜ポイント＞
# - データフレームに複数の差分系列を一度に作成する
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




# 1.使用例 ----------------------------------------------------------

library(tidyverse)
library(timetk)


# データ確認
m4_monthly %>% print()
m4_monthly %>% glimpse()


# データフレームにTimeseries Signatureを作成
m4_monthly %>%
  group_by(id) %>%
  tk_augment_differences(value, .lags = 1:20)
