# Title     : tk_make_weekend_sequence
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/23
# URL       : https://business-science.github.io/timetk/reference/tk_make_holiday_sequence.html



# ＜ポイント＞
# - 指定した期間の日付ベクトルを作成する




# ＜構文＞
# tk_make_weekend_sequence(start_date, end_date)



# 1.準備 ------------------------------------------------------------------

library(dplyr)
library(tidyquant)
library(timetk)


#
weekends <- tk_make_weekend_sequence(start_date = "2016", end_date   = "2017")
weekends %>% print()

