# Title     : tk_augment_differences
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/6
# URL       : https://business-science.github.io/timetk/reference/tk_augment_differences.html



# ＜ポイント＞
# - データフレームに複数のラグ系列を一度に作成する


# ＜構文＞
# tk_augment_lags(.data, .value, .lags = 1, .names = "auto")




# 1.使用例 ----------------------------------------------------------

library(tidyverse)
library(timetk)


# データ確認
m4_monthly %>% print()
m4_monthly %>% glimpse()


# データフレームにTimeseries Signatureを作成
m4_monthly %>%
  group_by(id) %>%
  tk_augment_lags(value, .lags = 1:20)


