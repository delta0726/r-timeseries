# ***************************************************************************************
# Library   : timetk
# Function  : tk_augment_slidify
# Created on: 2021/8/8
# URL       : https://business-science.github.io/timetk/reference/tk_augment_slidify.html
# ***************************************************************************************


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


# ＜使用例＞
# 0 準備
# 1 ローリング系列の追加


# 0 準備 ------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidyquant)
library(timetk)


# データ確認
FANG %>% print()
FANG %>% group_by(symbol) %>% tally()


# 1 ローリング系列の追加 ---------------------------------------------------------------

# 移動平均系列の追加
FANG_slidify <-
  FANG %>%
    select(symbol, date, adjusted) %>%
    group_by(symbol) %>%
    tk_augment_slidify(.value  = adjusted,
                       .period  = c(10, 30, 60, 90),
                       .f       = mean,
                       .align   = "right",
                       .partial = FALSE,
                       .names   = str_c("MA_", c(10, 30, 60, 90)))

# 系列確認
FANG_slidify %>% print(n = 100)
