# ***************************************************************************************
# Library   : timetk
# Function  : slidify_vec
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/slidify_vec.html
# ***************************************************************************************


# ＜概要＞
# - slidify()によるローリング計算をベクトルレベルで行う


# ＜構文＞
# slidify_vec(
#   .x,
#   .f,
#   ...,
#   .period = 1,
#   .align = c("center", "left", "right"),
#   .partial = FALSE
# )


# ＜引数＞



# ＜目次＞
# 0 準備
# 1 ベクトル操作
# 2 関数を直接使ったローリング
# 3 ラムダ式を使ったローリング
# 4 欠損値を直前値で補完してローリング


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


# 2 関数を直接使ったローリング -------------------------------------------------

# データ変換
X_Plot <- 
  FB_tbl %>%
  mutate(adjusted_30_ma = slidify_vec(.x      = adjusted,
                                      .period = 30,
                                      .f      = mean,
                                      na.rm   = TRUE,
                                      .align  = "center"))

# プロット作成
X_Plot %>%
  ggplot(aes(date, adjusted)) +
  geom_line() +
  geom_line(aes(y = adjusted_30_ma), color = "blue")



# 3 ラムダ式を使ったローリング----------------------------------------------------

# データ変換
X_Plot <- 
  FB_tbl %>%
    mutate(adjusted_30_ma = slidify_vec(.x      = adjusted,
                                        .period = 30,
                                        .f      = ~ mean(., na.rm = TRUE),
                                        na.rm   = TRUE,
                                        .align  = "center"))
# プロット作成
X_Plot %>%
  ggplot(aes(date, adjusted)) +
  geom_line() +
  geom_line(aes(y = adjusted_30_ma), color = "blue")


# 4 欠損値を直前値で補完してローリング --------------------------------------------


# データ変換
X_Plot <- 
  FB_tbl %>%
    mutate(adjusted = replace(adjusted, sample(row_number(), size = ceiling(0.3 * n()), replace = FALSE), NA)) %>% 
    mutate(adjusted_30_ma = slidify_vec(.x       = adjusted,
                                        .f       = ~ mean(., na.rm = TRUE),
                                        .period  = 30,
                                        .align   = "center",
                                        .partial = TRUE))

# プロット作成
X_Plot %>%
  ggplot(aes(date, adjusted)) +
  geom_line() +
  geom_line(aes(y = adjusted_30_ma), color = "blue")


# 5 LOESSと移動平均の比較 ----------------------------------------------------

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
