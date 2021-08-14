# ***************************************************************************************
# Library   : timetk
# Function  : normalize_vec
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/normalize_vec.html
# ***************************************************************************************


# ＜概要＞
# - リスケール系列の作成（0-1変換）


# ＜構文＞
# normalize_vec(x, min = NULL, max = NULL, silent = FALSE)


# ＜目次＞
# 0 準備
# 1 ベクトル操作
# 2 データフレーム操作


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(dplyr)
library(timetk)


# データ準備
d10_daily <- m4_daily %>% filter(id == "D10")


# 1 ベクトル操作 --------------------------------------------------------------

# 基準化
value_norm <- d10_daily$value %>% normalize_vec()

# 確認
d10_daily$value %>% ts.plot()
value_norm %>% ts.plot()

# 統計量
d10_daily$value %>% min()
d10_daily$value %>% max()

# リスケールの逆変換
value <-
  value_norm %>%
    normalize_inv_vec(min = 1781.6,
                      max = 2649.3)


# 2 データフレーム操作 ---------------------------------------------------------

# リスケール系列の追加
m4_daily %>%
  group_by(id) %>%
  mutate(value_std = standardize_vec(value))
