# ***************************************************************************************
# Library   : timetk
# Function  : ts_impute_vec
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/ts_impute_vec.html
# ***************************************************************************************


# ＜概要＞
# - 欠損値を補完した系列の追加
#   --- forecast::ts_impute_vec()のラッパー関数


# ＜構文＞
# ts_impute_vec(x, period = 1, lambda = NULL)


# ＜引数＞
# - period : period = 1（季節性なし）  
#          : period > 1（季節性あり）


# ＜目次＞
# 0 準備
# 1 線形補完
# 2 季節性補完
# 3 Box-Cox変換を活用した補完
# 4 データフレーム操作


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidyquant)
library(timetk)

# データ準備
d10_daily <- m4_daily %>% filter(id == "D10")


# 1 線形補完 -------------------------------------------------------------

# ベクトル作成
# --- 異常値と欠損値を含む
values <- c(1, 2, 3, 4*2, 5, 6, 7, NA, 9, 10, 11, 12*2)

# データ確認
values %>% print()
values %>% ts.plot()

# 線形補完
# --- 季節性なし
values_clean_1 <- values %>% ts_impute_vec(period = 1, lambda = NULL)

# プロット比較
tibble(name = "raw", id = 1:12, values = values) %>% 
  bind_rows(tibble(name = "clean", id = 1:12, values = values_clean_1)) %>% 
  ggplot(aes(x = id, y = values, color = name)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~name)


# 2 季節性補完 ------------------------------------------------------------

# クリーニング
# --- 季節性=4
values_clean_4 <- values %>% ts_impute_vec(period = 4, lambda = NULL)

# プロット比較
tibble(name = "raw", id = 1:12, values = values) %>% 
  bind_rows(tibble(name = "clean", id = 1:12, values = values_clean_4)) %>% 
  ggplot(aes(x = id, y = values, color = name)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~name)


# 3 Box-Cox変換を活用した補完 ---------------------------------------------

# クリーニング
# --- Box-Cox変換を内部で行う
values_clean_bc <- values %>% ts_impute_vec(period = 4, lambda = "auto")

# プロット比較
tibble(name = "raw", id = 1:12, values = values) %>% 
  bind_rows(tibble(name = "clean", id = 1:12, values = values_clean_bc)) %>% 
  ggplot(aes(x = id, y = values, color = name)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~name)


# 4 データフレーム操作 ---------------------------------------------------------

# クリーニング系列の追加
m4_daily %>%
  group_by(id) %>%
  mutate(value_clean = ts_impute_vec(value))

