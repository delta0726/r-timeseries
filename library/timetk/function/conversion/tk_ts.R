# ***************************************************************************************
# Library   : timetk
# Function  : tk_ts
# Created on: 2021/8/9
# URL       : https://business-science.github.io/timetk/reference/tk_ts.html
# ***************************************************************************************


# ＜概要＞
# - tsオブジェクトをtibble形式に変換する


# ＜構文＞
# tk_ts(
#   data,
#   select = NULL,
#   start = 1,
#   end = numeric(),
#   frequency = 1,
#   deltat = 1,
#   ts.eps = getOption("ts.eps"),
#   silent = FALSE
# )


# ＜使用例＞
# 0 準備
# 1 tsオブジェクトの作成
# 2 tsオブジェクトの要素取得


# 0 準備 ------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)


# データ作成
data_tbl <-
  tibble(date = seq.Date(as.Date("2016-01-01"), by = 1, length.out = 5),
         x    = rep("chr values", 5),
         y    = cumsum(1:5),
         z    = cumsum(11:15) * rnorm(1))

# 確認
data_tbl %>% print()


# 1 tsオブジェクトの作成 -----------------------------------------------------------------

# stats::ts()による変換
data_tbl %>% select(-date) %>% ts(start = 2016)

# timetk::tk_ts()による変換
data_ts <- data_tbl %>% tk_ts(start = 2016)
data_ts %>% print()

# timetk::tk_ts()による変換
# --- 特定系列のみ取得
data_tbl %>% tk_ts(select = y)


# 2 tsオブジェクトの要素取得 ------------------------------------------------------------

# インデックスの取得
data_ts %>% tk_index(timetk_idx = FALSE)

# メインデータの取得
data_ts %>% tk_tbl(timetk_idx = TRUE)
