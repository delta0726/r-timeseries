# Title     : tk_augment_fourier
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/6
# URL       : https://business-science.github.io/timetk/reference/tk_augment_fourier.html



# ＜ポイント＞
# - フーリエ級数をデータフレームに追加するための関数


# ＜構文＞
# tk_augment_fourier(.data, .date_var, .periods, .K = 1, .names = "auto")




# 1.使用例 ----------------------------------------------------------

library(tidyverse)
library(timetk)


# データ確認
m4_monthly %>% print()
m4_monthly %>% glimpse()


# データフレームにTimeseries Signatureを作成
m4_monthly %>%
  group_by(id) %>%
  tk_augment_fourier(date, .periods = c(6, 12), .K = 2)


