# ***************************************************************************************
# Library   : timetk
# Function  : ts_clean_vec
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/ts_clean_vec.html
# ***************************************************************************************


# ＜概要＞
# - クリーニング系列の追加（異常値や欠損値のクリーニング）
#   --- forecast::tsclean()のラッパー関数


# ＜構文＞
# ts_clean_vec(x, period = 1, lambda = NULL)


# ＜引数＞
# - period : period = 1（季節性なし）  
#          : period > 1（季節性あり）


# ＜目次＞
# 0 準備
# 1 線形クリーニング
# 2 季節性クリーニング
# 3 Box-Cox変換クリーニング
# 4 データフレーム操作


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidyquant)
library(timetk)

# データ準備
d10_daily <- m4_daily %>% filter(id == "D10")


# 1 線形クリーニング ---------------------------------------------------------

# ベクトル作成
# --- 異常値と欠損値を含む
values <- c(1, 2, 3, 4*2, 5, 6, 7, NA, 9, 10, 11, 12*2)

# データ確認
values %>% print()
values %>% ts.plot()

# 線形補完
# --- 季節性なし
values_clean_1 <- values %>% ts_clean_vec(period = 1, lambda = NULL)

# プロット比較
tibble(name = "raw", id = 1:12, values = values) %>% 
  bind_rows(tibble(name = "clean", id = 1:12, values = values_clean_1)) %>% 
  ggplot(aes(x = id, y = values, color = name)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~name)


# 2 季節性クリーニング ------------------------------------------------------

# クリーニング
# --- 季節性=4
values_clean_4 <- values %>% ts_clean_vec(period = 4, lambda = NULL)

# プロット比較
tibble(name = "raw", id = 1:12, values = values) %>% 
  bind_rows(tibble(name = "clean", id = 1:12, values = values_clean_4)) %>% 
  ggplot(aes(x = id, y = values, color = name)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~name)


# 3 Box-Cox変換クリーニング -----------------------------------------------------

# クリーニング
# --- Box-Cox変換を内部で行う
values_clean_bc <- values %>% ts_clean_vec(period = 4, lambda = "auto")

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
  mutate(value_clean = ts_clean_vec(value))
