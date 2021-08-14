# ***************************************************************************************
# Library   : timetk
# Function  : log_interval_vec
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/log_interval_vec.html
# ***************************************************************************************


# ＜概要＞
# - 対数変換してから指定範囲に分布


# ＜構文＞
# log_interval_vec(
#   x,
#   limit_lower = "auto",
#   limit_upper = "auto",
#   offset = 0,
#   silent = FALSE
# )


# ＜目次＞
# 0 準備
# 1 ベクトル操作


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(dplyr)
library(timetk)


# 1 ベクトル操作 --------------------------------------------------------------

#
values_trans <-
  1:10 %>%
    log_interval_vec(limit_lower = 0, limit_upper = 11)

# 確認
values_trans %>% print()
values_trans %>% plot()


values_trans_forecast <- c(values_trans, 3.4, 4.4, 5.4)

#
values_trans_forecast %>%
  log_interval_inv_vec(limit_lower = 0, limit_upper = 11) %>%
  plot()
