# ***************************************************************************************
# Library   : timetk
# Function  : tk_index
# Created on: 2021/8/13
# URL       : https://business-science.github.io/timetk/reference/tk_index.html
# ***************************************************************************************


# ＜ポイント＞
# - tk_index()は、さまざまな時系列オブジェクト/モデル/予測から日付または日時インデックスを抽出する
#   --- tbl、xts、zoo、zooreg、およびtsオブジェクトに使用することができる
#   --- Arima、ets、およびHoltWintersクラスなどの関数オブジェクトからも日付抽出ができる


# ＜構文＞
# tk_index(data, timetk_idx = FALSE, silent = FALSE)


# ＜目次＞
# 0 準備
# 1 日付インデックスの取得
# 2 tsオブジェクトの日付インデックス


# 0 準備 --------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)


# 時系列データセットの作成
data_tbl <-
  tibble(date = seq.Date(from = as.Date("2000-01-01"), by = 1, length.out = 5),
         x    = rnorm(5) * 10,
         y    = 5:1)


# 1 日付インデックスの取得 ------------------------------------------------

# 日付インデックスの取得
data_tbl %>% tk_index()
tk_index(data_ts, timetk_idx = FALSE)



# 2 tsオブジェクトの日付インデックス ---------------------------------------

# tsオブジェクトの作成
data_ts <- data_tbl %>% tk_ts()

# 日付インデックスの取得
data_ts %>% tk_index(timetk_idx = FALSE)
data_ts %>% tk_index(timetk_idx = TRUE)

# データフレームに変換
data_ts %>% tk_tbl(timetk_idx = FALSE)
data_ts %>% tk_tbl(timetk_idx = TRUE)
