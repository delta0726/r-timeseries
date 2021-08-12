# ***************************************************************************************
# Library   : timetk
# Function  : Add / Subtract Timeseries Vector
# Created on: 2021/8/13
# URL       : https://business-science.github.io/timetk/reference/time_arithmetic.html
# ***************************************************************************************


# ＜ポイント＞
# - 日付型や文字列から日付演算の加算/減算を行う
# - xtsなどの特定オブジェクトに変換する必要がない


# ＜構文：加算＞
# add_time(index, period)
# subtract_time(index, period)
#
# index %+time% period
# index %-time% period


# ＜目次＞
# 0 準備
# 1 日付型からの時間演算
# 2 文字列型からの時間演算
# 3 ベクトルからの時間演算


# 0 準備 -------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)


# 1 日付型からの時間演算 -----------------------------------------------------------

# クラス確認
# --- 日付型
as.Date("2021-01-21") %>% class()

# 日付の加算
# --- 日付と自動判定したうえで演算
as.Date("2021-01-21") %>% add_time(period = "1 hour 34 seconds")
as.Date("2021-01-21") %+time% "1 hour 34 seconds"

# 日付の減算
# --- 日付と自動判定したうえで演算
as.Date("2021-01-21") %>% subtract_time(period = "1 hour 34 seconds")
as.Date("2021-01-21") %-time% "1 hour 34 seconds"


# 2 文字列型からの時間演算 ------------------------------------------------------------------

# クラス確認
# --- ただの文字列
"2021" %>% class()

# 日付の加算
# --- 日付と自動判定したうえで演算
add_time(index = "2021", period = "1 hour 34 seconds")
"2021" %+time% "1 hour 34 seconds"

# 日付の減算
# --- 日付と自動判定したうえで演算
subtract_time(index = "2021", period = "1 hour 34 seconds")
"2021" %-time% "1 hour 34 seconds"


# 3 ベクトルからの時間演算 ------------------------------------------------------------------

# 日付ベクトルの作成
index_daily <- tk_make_timeseries("2016", "2016-02-01")
index_daily %>% print()

# 日付演算
# --- 存在しない日付はNAとなる
index_daily %+time% "1 month"
index_daily %-time% "1 month"
