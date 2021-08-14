# ***************************************************************************************
# Library   : timetk
# Function  : tk_make_weekend_sequence
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/tk_make_holiday_sequence.html
# ***************************************************************************************


# ＜概要＞
# - 指定した期間の週末(土日)の日付ベクトルを作成する
#   --- 英語の曜日表記で判定するため、ローケールをEnglishにしておく必要がある


# ＜構文＞
# tk_make_weekend_sequence(start_date, end_date)


# ＜目次＞
# 0 準備
# 1 週末ベクトルの取得


# 0 準備 --------------------------------------------------------------

# ライブラリ
library(dplyr)
library(tidyquant)
library(timetk)

# ローケール設定変更
# --- 英語の曜日表記で判定するため
Sys.setlocale("LC_TIME", "English")


# 1 週末ベクトルの取得 --------------------------------------------------

# 期間指定で週末を取得
tk_make_weekend_sequence(start_date = "2016", end_date   = "2017")

