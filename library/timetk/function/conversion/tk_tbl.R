# ***************************************************************************************
# Library   : timetk
# Function  : tk_tbl
# Created on: 2021/8/9
# URL       : https://business-science.github.io/timetk/reference/tk_tbl.html
# ***************************************************************************************


# ＜概要＞
# - 時系列オブジェクをtibbleオブジェクトに強制変換する
#   --- xts、zoo、ts、timeSeriesなど


# ＜構文＞
# tk_tbl(
#   data,
#   preserve_index = TRUE,
#   rename_index = "index",
#   timetk_idx = FALSE,
#   silent = FALSE,
#   ...
# )


# ＜使用例＞
# 0 準備
# 1 tsオブジェクトの変換
# 2 xtsオブジェクトの変換
# 3 zooregオブジェクトの変換
# 4 zooオブジェクトの変換


# 0 準備 ------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)


# データ作成
# --- データフレームの日付系列
data_tbl <-
  tibble(date = seq.Date(from = as.Date("2010-01-01"), by = 1, length.out = 5),
         x    = seq(100, 120, by = 5))


# 1 tsオブジェクトの変換 ---------------------------------------------------------------

# tsオブジェクトの作成
data_ts <- data_tbl %>% tk_ts(start = c(2010,1), freq = 365)

# 確認
data_ts %>% print()

# データフレームに変換
# --- インデックスは維持されない
data_ts %>% as.data.frame()

# データフレームに変換
# --- インデックスが保持される
# --- インデックスが日付形式で保持される
data_ts %>% tk_tbl()
data_ts %>% tk_tbl(timetk_idx = TRUE)


# 2 xtsオブジェクトの変換 -------------------------------------------------------------

# xtsオブジェクトの作成
data_xts <- data_tbl %>% tk_xts()

# データフレームに変換
# --- インデックスは列名として保持される
# --- tibbleの場合は保持されない
data_xts %>% as.data.frame()

# データフレームに変換
# --- インデックスが日付形式で保持される
# --- インデックスが日付形式で保持される（同じ）
data_xts %>% tk_tbl()
data_xts %>% tk_tbl(timetk_idx = TRUE)


# 3 zooregオブジェクトの変換 -------------------------------------------------------------

# zooregオブジェクトの作成
data_zooreg <- tk_zooreg(1:8, start = zoo::yearqtr(2000), frequency = 4)

# データフレームに変換
# --- インデックスは列名として保持される
data_zooreg %>% as.data.frame()

# データフレームに変換
# --- インデックスがyearqtr形式で保持される
data_zooreg %>% tk_tbl()


# 4 zooオブジェクトの変換 ---------------------------------------------------------------

# zooオブジェクトの作成
data_zoo <- zoo::zoo(1:12, zoo::yearmon(2016 + seq(0, 11)/12))

# データフレームに変換
# --- インデックスは列名として保持される
data_zoo %>% as.data.frame()

# データフレームに変換
# --- インデックスがyearmon形式で保持される
data_zoo %>% tk_tbl()
