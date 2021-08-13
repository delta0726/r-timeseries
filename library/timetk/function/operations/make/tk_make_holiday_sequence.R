# ***************************************************************************************
# Library   : timetk
# Function  : tk_make_holiday_sequence
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/index.html
# ***************************************************************************************



# ＜ポイント＞
# - 指定したカレンダーの休日(週末は含まない)を取得する
#   --- {timeDate}から休日情報を取得（引数にある部の国の取引所のみ対応）


# ＜構文＞
# tk_make_holiday_sequence(
#   start_date,
#   end_date,
#   calendar = c("NYSE", "LONDON", "NERC", "TSX", "ZURICH"),
#   skip_values = NULL,
#   insert_values = NULL
# )



# ＜目次＞
# 0 準備
# 1 指定したカレンダーの休日を取得


# 0 準備 --------------------------------------------------------------

# ライブラリ
library(dplyr)
library(tidyquant)
library(timetk)


# 1 指定したカレンダーの休日を取得 ----------------------------------------

# 期間指定で取得
tk_make_holiday_sequence("2017-04-01", "2017-12-31", calendar = "NYSE")

# 年のみ指定して取得
tk_make_holiday_sequence("2017", calendar = "NYSE")
