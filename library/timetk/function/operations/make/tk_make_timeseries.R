# Title     : tk_make_timeseries
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/23
# URL       : https://business-science.github.io/timetk/reference/tk_make_timeseries.html



# ＜ポイント＞
# - 指定した期間の日付ベクトルを作成する




# ＜構文＞
# tk_make_timeseries(
#   start_date,
#   end_date,
#   by,
#   length_out = NULL,
#   include_endpoints = TRUE,
#   skip_values = NULL,
#   insert_values = NULL
# )



# 1.準備 ------------------------------------------------------------------

library(dplyr)
library(tidyquant)
library(timetk)



# 2.単純な日付出力 ------------------------------------------------------------------

# 日付で期間指定
tk_make_timeseries(start_date = "2017-01-01", end_date = "2017-12-31")


# 年のみ指定
tk_make_timeseries(start_date = "2017")


# 年月を指定
tk_make_timeseries(start_date = "2017-02")



# 3.ベクトルの長さを指定 ------------------------------------------------------------------

# 年のみ指定
# --- ベクトルの長さを指定
tk_make_timeseries(start_date = "2012", length_out = 6)


# 年のみ指定
# --- 頻度とベクトルの長さを指定
tk_make_timeseries(start_date = "2012", by = "1 month", length_out = 6)



# 4.長さをキーワードで指定 ------------------------------------------------------------------

# 期間をlength outで指定
tk_make_timeseries(start_date = "2012", by = "1 month",
                   length_out = "1 year 6 months", include_endpoints = FALSE)


# End Pointも出力
tk_make_timeseries(start_date = "2012", by = "1 month",
                   length_out = "1 year 6 months", include_endpoints = TRUE)


tk_make_timeseries(end_date = "2012-01-01", by = "1 month",
                   length_out = "1 year 6 months", include_endpoints = FALSE)



# 4.時間ベクトルの出力 ------------------------------------------------------------------


tk_make_timeseries("2016-01-01 01:01:02", "2016-01-01 01:01:04")


tk_make_timeseries("2017-01-01", "2017-01-02", by = "10 min")


tk_make_timeseries("2017-01-01", by = "30 min", length_out = "6 hours")


tk_make_timeseries("2012-01-01", by = "1 month",
                   length_out = "12 months",
                   include_endpoints = FALSE) # Removes unnecessary last value


tk_make_timeseries(
    "2011-01-01", length_out = 5,
    skip_values   = "2011-01-05",
    insert_values = "2011-01-06"
)


