# Title     : ts_clean_vec
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/5
# URL       : https://business-science.github.io/timetk/reference/ts_clean_vec.html



# ＜ポイント＞
# - データフレームにTimeseries Signatureを追加する
#   --- tk_get_timeseries_signature()は日付インデックスを引数とする点で異なる


# ＜構文＞
# ts_clean_vec(x, period = 1, lambda = NULL)




# 1.使用例 ----------------------------------------------------------

library(dplyr)
library(timetk)


# --- VECTOR ----

# データ準備
values <- c(1,2,3, 4*2, 5,6,7, NA, 9,10,11, 12*2)
values



values %>% ts_clean_vec(period = 1, lambda = NULL)

values %>% ts_clean_vec(period = 4, lambda = NULL)

values %>% ts_clean_vec(period = 4, lambda = "auto")
