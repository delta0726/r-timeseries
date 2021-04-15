# Title     : tk_stl_diagnostics
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/6
# URL       : https://business-science.github.io/timetk/reference/tk_summary_diagnostics.html



# ＜ポイント＞
# - 指定した系列をseason / trend / remainder の3つに分解する


# ＜構文＞
# - tk_stl_diagnostics(
#  .data,
#  .date_var,
#  .value,
#  .frequency = "auto",
#  .trend = "auto",
#  .message = TRUE
# )



# 1.準備 ------------------------------------------------------------------------

library(tidyverse)
library(timetk)



# 2.日次データのSTL分解 ------------------------------------------------------------------------

# データ確認
m4_daily %>% print()
m4_daily %>% glimpse()


# STL分解
# --- 元系列をBox-Cox変換
# --- season / trend / remainder の3つに分解される
m4_daily %>%
    group_by(id) %>%
    tk_stl_diagnostics(date, box_cox_vec(value))


# 検証
# --- observed = season + trend + remainder
m4_daily %>%
  group_by(id) %>%
  tk_stl_diagnostics(date, box_cox_vec(value)) %>%
  ungroup() %>%
  mutate(result = rowSums(.[4:6]))



# 2.週次データのSTL分解 ------------------------------------------------------------------------

# データ確認
m4_weekly %>% print()
m4_weekly %>% glimpse()


# STL分解
m4_weekly %>%
  group_by(id) %>%
  tk_stl_diagnostics(date, box_cox_vec(value), .trend = "2 quarters")


