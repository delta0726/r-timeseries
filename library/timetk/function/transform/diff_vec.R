# ***************************************************************************************
# Library   : timetk
# Function  : diff_vec
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/diff_vec.html
# ***************************************************************************************


# ＜ポイント＞
# - 差分系列の作成


# ＜構文＞
# diff_vec(
#  x,
#  lag = 1,
#  difference = 1,
#  log = FALSE,
#  initial_values = NULL,
#  silent = FALSE
# )


# ＜目次＞
# 0 準備
# 1 ベクトル操作
# 2 データフレーム操作


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)


# データ準備
d10_daily <- m4_daily %>% filter(id == "D10")

# データ確認
d10_daily %>% print()
d10_daily %>% glimpse()


# 1 ベクトル操作 -------------------------------------------------------------

# 元データ
1:10

# 差分計算
# --- 1つ前との1階差分
1:10 %>% diff_vec(lag = 1, difference = 1)

# 差分計算
# --- 2つ前との1階差分
1:10 %>% diff_vec(lag = 2, difference = 1)

# 2階差分
# --- 1つ前との2階差分
1:10 %>% diff_vec(lag = 1, difference = 2)

# 対数差分
1:10 %>% diff_vec(log = TRUE)
1:10 %>% diff_vec(log = TRUE) %>% exp() - 1

# 差分の逆変換
diff_vec(1:10, lag = 2, difference = 2) %>%
    diff_inv_vec(lag = 2, difference = 2, initial_values = 1:4)


# 2 データフレーム操作 ---------------------------------------------------------

# 差分系列の追加
m4_daily %>%
  group_by(id) %>%
  mutate(difference = diff_vec(value, lag = 1),
         difference_inv = diff_inv_vec(difference, lag = 1, initial_values = value[1]))
