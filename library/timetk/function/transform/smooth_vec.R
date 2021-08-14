# ***************************************************************************************
# Library   : timetk
# Function  : smooth_vec
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/smooth_vec.html
# ***************************************************************************************


# ＜概要＞
# -  LOESS変換でスムーズした系列の作成


# ＜構文＞
# smooth_vec(x, period = 30, span = NULL, degree = 2)


# ＜目次＞
# 0 準備
# 1 ベクトル操作
# 2 periodのコントロール
# 3 spanのコントロール
# 4 LOESSと移動平均の比較


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidyquant)
library(timetk)


# データ準備
FB_tbl <-
  FANG %>%
    filter(symbol == "FB") %>%
    select(symbol, date, adjusted)

# データ確認
FB_tbl %>% print()


# 1 ベクトル操作 --------------------------------------------------------------

# LOESS変換
FB_tbl$adjusted %>% smooth_vec()


# 2 periodのコントロール -------------------------------------------------------

# プロット作成
# --- スムージング系列の追加
FB_tbl %>%
  mutate(adjusted_30 = smooth_vec(adjusted, period = 30, degree = 2)) %>%
  ggplot(aes(date, adjusted)) +
  geom_line() +
  geom_line(aes(y = adjusted_30), color = "red")


# 3 spanのコントロール --------------------------------------------------------

# プロット作成
# --- スムージング系列の追加
FB_tbl %>%
  mutate(adjusted_30 = smooth_vec(adjusted, span = 0.75, degree = 2)) %>%
  ggplot(aes(date, adjusted)) +
  geom_line() +
  geom_line(aes(y = adjusted_30), color = "red")


# 4 LOESSと移動平均の比較 ----------------------------------------------------

# データ変換
X_Plot <- 
  FB_tbl %>%
  mutate(adjusted_loess_30 = smooth_vec(adjusted, period = 30, degree = 0), 
         adjusted_ma_30    = slidify_vec(adjusted, .period = 30, 
                                         .f = AVERAGE, .partial = TRUE))

# プロット作成
X_Plot %>%
  ggplot(aes(date, adjusted)) +
  geom_line() +
  geom_line(aes(y = adjusted_loess_30), color = "red") +
  geom_line(aes(y = adjusted_ma_30), color = "blue") +
  labs(title = "Loess vs Moving Average")
