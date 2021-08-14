# ***************************************************************************************
# Library   : timetk
# Function  : box_cox_vec
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/box_cox_vec.html
# ***************************************************************************************


# ＜ポイント＞
# - Box-Cox変換系列の作成
#   --- {forecast}が提供するBox-Cox変換のラッパー関数（lambdaを自動計算する機能を備えている）


# ＜構文＞
# box_cox_vec(x, lambda = "auto", silent = FALSE)


# ＜目次＞
# 0 準備
# 1 ベクトル操作
# 2 データフレーム操作


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)
library(gridExtra)


# データ準備
d10_daily <- m4_daily %>% filter(id == "D10")

# データ確認
d10_daily %>% print()
d10_daily %>% glimpse()


# 1 ベクトル操作 -------------------------------------------------------------

# Box-Cox変換
value_bc <- d10_daily$value %>% box_cox_vec()
value_bc %>% print()


# 2 データフレーム操作 ---------------------------------------------------------

# Box-Cox変換
df_box_cox <-
  d10_daily %>%
    group_by(id) %>%
    mutate(value_bc = box_cox_vec(value, silent = TRUE))

# データ確認
df_box_cox %>% print()
df_box_cox %>% summary()

# プロット作成
p1 <- df_box_cox %>% group_by(id) %>% plot_time_series(date, value, .interactive = FALSE)
p2 <- df_box_cox %>% group_by(id) %>% plot_time_series(date, value_bc, .interactive = FALSE)
grid.arrange(p1, p2)
