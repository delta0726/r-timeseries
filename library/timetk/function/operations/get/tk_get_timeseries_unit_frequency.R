# ***************************************************************************************
# Library   : timetk
# Function  : tk_get_timeseries_unit_frequency
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/tk_get_timeseries_unit_frequency.html
# ***************************************************************************************


# ＜概要＞
# - 日付/時間単位を秒で表現したタイムテーブルの取得


# ＜構文＞
# tk_get_timeseries_unit_frequency()


# ＜目次＞
# 0 準備
# 1 タイムテーブルの取得


# 0 準備 -----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidyquant)
library(timetk)


# 1 タイムテーブルの取得 ---------------------------------------------------------

# タイムテーブルの取得
# --- 日付/時間単位を秒で表現している
tk_get_timeseries_unit_frequency()
