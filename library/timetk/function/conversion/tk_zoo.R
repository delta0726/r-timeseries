# ***************************************************************************************
# Library   : timetk
# Function  : tk_zoo
# Created on: 2021/8/
# URL       : https://business-science.github.io/timetk/reference/tk_zoo.html
# ***************************************************************************************


# ＜概要＞
# - zooオブジェクトをtibble形式に変換する


# ＜構文＞
# tk_zoo(data, select = NULL, date_var = NULL, silent = FALSE, ...)


# ＜使用例＞
# 0 準備
# 1 zooオブジェクトの作成
# 2 tk_zooの活用


# 0 準備 ------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)
library(xts)


# データ作成
data_tbl <-
  tibble(date = seq.Date(as.Date("2016-01-01"), by = 1, length.out = 5),
         x    = rep("chr values", 5),
         y    = cumsum(1:5),
         z    = cumsum(11:15) * rnorm(1))

# 確認
data_tbl %>% print()


# 1 zooオブジェクトの作成 -----------------------------------------------------------

# zoo::zoo()による変換
data_tbl %>% select(-c(1,2)) %>% zoo(order.by = data_tbl$date)

# timetk::tk_zoo()による変換
data_tbl %>% tk_zoo()


# 2 tk_zooの活用 ---------------------------------------------------------------------

# tsオブジェクトを経由して変換
data_tbl %>%
  tk_ts(start = 2016, freq = 365) %>%
  tk_zoo()

# 特定系列を指定して変換
data_tbl %>% tk_zoo(select = y, date_var = date)
