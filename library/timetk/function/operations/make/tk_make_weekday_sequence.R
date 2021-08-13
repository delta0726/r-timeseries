# ***************************************************************************************
# Library   : timetk
# Function  : tk_make_weekday_sequence
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/tk_make_holiday_sequence.html
# ***************************************************************************************


# ＜概要＞
# - Weekday(平日)の日付ベクトルを作成する


# ＜構文＞
# tk_make_weekday_sequence(
#   start_date,
#   end_date,
#   remove_weekends = TRUE,
#   remove_holidays = FALSE,
#   calendar = c("NYSE", "LONDON", "NERC", "TSX", "ZURICH"),
#   skip_values = NULL,
#   insert_values = NULL
# )


# ＜目次＞
# 0 準備
# 1 休日ベクトルの取得


# 0 準備 --------------------------------------------------------------

# ライブラリ
library(dplyr)
library(tidyquant)
library(timetk)


# 1 平日ベクトルの取得 ------------------------------------------------------------------

# 平日を取得
tk_make_weekday_sequence(start_date = "2021-05", end_date = "2021-05")

# 営業日のみを取得
# --- 休日を除外
tk_make_weekday_sequence(start_date = "2021-05", end_date = "2021-05",
                         remove_holidays = TRUE, calendar = "NYSE")
