# Title     : diff_vec
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/6
# URL       : https://business-science.github.io/timetk/reference/diff_vec.html



# ＜ポイント＞
# - {forecast}が提供するBox-Cox変換のラッパー関数
# - 自動でlambdaを計算する機能を備えている




# ＜構文＞
# diff_vec(
#  x,
#  lag = 1,
#  difference = 1,
#  log = FALSE,
#  initial_values = NULL,
#  silent = FALSE
# )
#
#diff_inv_vec(x, lag = 1, difference = 1, log = FALSE, initial_values = NULL)



# 1.準備 ---------------------------------------------------------------------

library(tidyverse)
library(timetk)


# データ準備
d10_daily <- m4_daily %>% filter(id == "D10")


# データ確認
d10_daily %>% print()
d10_daily %>% glimpse()



# 2.ベクトル操作 -------------------------------------------------------------

# 元データ
1:10

# 差分計算
1:10 %>% diff_vec()

# 2階差分
1:10 %>% diff_vec(difference = 2)

# 対数差分
1:10 %>% diff_vec(log = TRUE) %>% exp() - 1



diff_vec(1:10, lag = 2, difference = 2) %>%
    diff_inv_vec(lag = 2, difference = 2, initial_values = 1:4)




# 3.データフレーム操作 ---------------------------------------------------------

#
m4_daily %>%
  group_by(id) %>%
  mutate(difference = diff_vec(value, lag = 1),
         difference_inv = diff_inv_vec(difference, lag = 1, initial_values = value[1]))





