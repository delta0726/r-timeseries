# Title     : Time Series Conversion
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/12
# URL       : https://business-science.github.io/timetk/articles/TK00_Time_Series_Coercion.html


# ＜ポイント＞
# - データフレームとRの各種時系列オブジェクトを相互変換する関数群
#   --- ts、zoo、xts、irts、mstsなど
# - 各種時系列オブジェクトが持つ関数群を使えるようになる





library(tidyverse)
library(timetk)

q10_quarterly <- m4_quarterly %>% filter(id == "Q10")
q10_quarterly


# date column gets coerced to numeric
ts(q10_quarterly, start = c(2000, 1), freq = 4) %>%
    head()

q10_quarterly_ts <- ts(q10_quarterly$value, start = c(2000, 1), freq  = 4)
q10_quarterly_ts

# No date index attribute
str(q10_quarterly_ts)


# date automatically dropped and user is warned
q10_quarterly_ts_timetk <- tk_ts(q10_quarterly, start = 2000, freq  = 4)

q10_quarterly_ts_timetk

# More attributes including time index, time class, time zone
str(q10_quarterly_ts_timetk)



# Can now retrieve the original date index
timetk_index <- q10_quarterly_ts_timetk %>%
    tk_index(timetk_idx = TRUE)
head(timetk_index)

class(timetk_index)


# Conversion back to tibble using the default index (regularized)
q10_quarterly_ts_timetk %>%
    tk_tbl(index_rename = "date", timetk_idx = FALSE)


# Conversion back to tibble now using the timetk index (date / date-time)
q10_quarterly_timetk <- q10_quarterly_ts_timetk %>%
    tk_tbl(timetk_idx = TRUE) %>%
    rename(date = index)
q10_quarterly_timetk


# Comparing the coerced tibble with the original tibble
identical(q10_quarterly_timetk, q10_quarterly %>% select(-id))


# Start:
q10_quarterly


# End
q10_quarterly_xts <- tk_xts(q10_quarterly)


head(q10_quarterly_xts)