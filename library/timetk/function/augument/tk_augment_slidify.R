# Title     : tk_augment_slidify
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/23
# URL       : https://business-science.github.io/timetk/reference/tk_augment_slidify.html



# ＜ポイント＞
# - 複数のローリング計算を行ってデータフレームに列を追加する
# - ユーザー定義関数も使用することができる



# ＜構文＞
# tk_augment_slidify(
#   .data,
#   .value,
#   .period,
#   .f,
#   ...,
#   .align = c("center", "left", "right"),
#   .partial = FALSE,
#   .names = "auto"
# )



# 1.使用例 ----------------------------------------------------------

library(tidyverse)
library(tidyquant)
library(timetk)


# ローリング計算
# --- 移動平均
FANG %>%
  select(symbol, date, adjusted) %>%
  group_by(symbol) %>%
  tk_augment_slidify(
      .value  = adjusted,
      # Multiple rolling windows
      .period  = c(10, 30, 60, 90),
      .f       = mean,
      .align   = "right",
      .partial = FALSE,
      .names   = str_c("MA_", c(10, 30, 60, 90))
  )

