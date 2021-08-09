# ***************************************************************************************
# Library   : timetk
# Function  : time_series_split
# Created on: 2021/8/8
# URL       : https://business-science.github.io/timetk/reference/time_series_split.html
# ***************************************************************************************


# ＜概要＞
# - 時系列方向にデータセットを訓練データ/テストデータに分割する


# ＜構文＞
# time_series_split(
#   data,
#   date_var = NULL,
#   initial = 5,
#   assess = 1,
#   skip = 1,
#   lag = 0,
#   cumulative = FALSE,
#   slice = 1,
#   ...
# )


# ＜目次＞
# 0 準備
# 1 時系列データ分割


# 0 準備 ----------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)
library(rsample)


# データ準備
# --- 1系列のみ
m750 <- m4_monthly %>% filter(id == "M750")

# 確認
m750 %>% print()
m750 %>% tk_summary_diagnostics() %>% select(1:4)


# 1 時系列データ分割 --------------------------------------------------------------

# データ分割
df_split_1 <-
  m750 %>%
    time_series_split(initial = "10 years", assess = "3 years")

# 確認
df_split_1 %>% training() %>% tk_summary_diagnostics() %>% select(1:4)
df_split_1 %>% testing() %>% tk_summary_diagnostics() %>% select(1:4)


# データ分割
df_split_2 <-
  m750 %>%
    time_series_split(initial = "10 years",
                      assess  = "3 years",
                      skip    = "3 years",
                      slice   = 2)

# 確認
df_split_2 %>% training() %>% tk_summary_diagnostics() %>% select(1:4)
df_split_2 %>% testing() %>% tk_summary_diagnostics() %>% select(1:4)

# データ分割
df_split_2 <-
  m750 %>%
    time_series_split(initial = "10 years",
                      assess  = "3 years",
                      skip    = "3 years",
                      slice   = 2,
                      lag     = "1 year")

# 確認
df_split_3 %>% training() %>% tk_summary_diagnostics() %>% select(1:4)
df_split_3 %>% testing() %>% tk_summary_diagnostics() %>% select(1:4)
