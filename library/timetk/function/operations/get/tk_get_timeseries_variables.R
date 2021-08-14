# ***************************************************************************************
# Library   : timetk
# Function  : tk_get_timeseries_variables
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/tk_get_timeseries_variables.html
# ***************************************************************************************


# ＜概要＞
# - 日付インデックスの列名取得


# ＜構文＞
# tk_get_timeseries_variables(data)


# ＜目次＞
# 0 準備
# 1 日付インデックスの列名取得


# 0 準備 -----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidyquant)
library(timetk)


# データ確認
FANG %>% print()
FANG %>% glimpse()


# 1 日付インデックスの列名取得 ----------------------------------------------------

# 日付インデックスの列名取得
# --- 最初の日付列の列名を取得
FANG %>% tk_get_timeseries_variables()
