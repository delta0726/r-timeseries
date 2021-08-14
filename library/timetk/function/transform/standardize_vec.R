# ***************************************************************************************
# Library   : timetk
# Function  : standardize_vec
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/standardize_vec.html
# ***************************************************************************************


# ＜概要＞
# - 基準化系列の作成（Zスコア変換）


# ＜構文＞
# standardize_vec(x, mean = NULL, sd = NULL, silent = FALSE)


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
value_std <- d10_daily$value %>% standardize_vec()

# 確認
d10_daily$value %>% ts.plot()
value_std %>% ts.plot()

# 統計量
d10_daily$value %>% mean()
d10_daily$value %>% sd()

# 基準化の逆変換
value <-
  value_std %>%
    standardize_inv_vec(mean = 2261.60682492582,
                        sd   = 175.603721730477)


# 2 データフレーム操作 ---------------------------------------------------------

# 基準化系列の追加
m4_daily %>%
  group_by(id) %>%
  mutate(value_std = standardize_vec(value))
