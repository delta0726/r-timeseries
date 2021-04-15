# Title     : Add / Subtract Timeseries Vector
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/23
# URL       : https://business-science.github.io/timetk/reference/time_arithmetic.html



# ＜ポイント＞
# - 日付型や文字列から日付演算の計算を行う
# - xtsなどの特定オブジェクトに変換する必要がない


# ＜構文：加算＞
# add_time(index, period)
# index %+time% period


# ＜構文：減算＞
# subtract_time(index, period)
# index %-time% period



# 1.準備 ------------------------------------------------------------------

library(tidyverse)
library(timetk)



# 2.日付型からの時間演算 ------------------------------------------------------------------

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



# 3.文字列型からの時間演算 ------------------------------------------------------------------

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



# 4.ベクトルからの時間演算 ------------------------------------------------------------------

# 日付ベクトルの作成
index_daily <- tk_make_timeseries("2016", "2016-02-01")
index_daily %>% print()


# 日付演算
# --- 存在しない日付はNAとなる
index_daily %+time% "1 month"
index_daily %-time% "1 month"

