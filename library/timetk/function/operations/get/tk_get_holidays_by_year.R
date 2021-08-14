# ***************************************************************************************
# Library   : timetk
# Function  : tk_get_holidays_by_year
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/tk_get_holiday.html
# ***************************************************************************************


# ＜概要＞
# - 休日一覧テーブルを取得する


# ＜構文＞
# tk_get_holidays_by_year(years = year(today()))


# ＜目次＞
# 0 準備
# 1 休日一覧テーブルの取得


# 0 準備 ------------------------------------------------------------

# ライブラリ
library(dplyr)
library(timetk)


# 1 休日一覧テーブルの取得 ---------------------------------------------

# 休日一覧をテーブル抽出
tk_get_holidays_by_year(2020)

# 特定のカレンダーに絞り込み
tk_get_holidays_by_year(2020) %>%
    filter(holiday_name %>% str_detect("US_"))
